/* main.c -- chibi-scheme command-line app                   */
/* Copyright (c) 2009-2015 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt       */

#ifdef EMSCRIPTEN
#include <emscripten.h>
#endif

#include "chibi/eval.h"
#include "chibi/gc_heap.h"

#define sexp_argv_symbol "command-line"

#define sexp_import_prefix "(import ("
#define sexp_import_suffix "))"
#define sexp_environment_prefix "(environment '("
#define sexp_environment_suffix "))"
#define sexp_trace_prefix "(module-env (load-module '("
#define sexp_trace_suffix ")))"
#define sexp_default_environment "(environment '(scheme small))"
#define sexp_advice_environment "(load-module '(chibi repl))"

#define sexp_version_string "chibi-scheme "sexp_version" \""sexp_release_name"\" "

#ifdef PLAN9
#define exit_failure() exits("ERROR")
#define exit           exits
#else
#define exit_failure() exit(70)
#endif
#define exit_success() exit(0)

#if SEXP_USE_MAIN_HELP
void sexp_usage(int err) {
  printf("usage: chibi-scheme [<options> ...] [<file> <args> ...]\n"
#if SEXP_USE_FOLD_CASE_SYMS
         "  -f           - case-fold symbols\n"
#endif
         "  -q           - \"quick\" load, use the core -xchibi language\n"
         "  -Q           - extra \"quick\" load, -xchibi.primitive\n"
         "  -V           - print version information\n"
#if ! SEXP_USE_BOEHM
         "  -h <size>    - specify the initial heap size\n"
#endif
#if SEXP_USE_MODULES
         "  -A <dir>     - append a module search directory\n"
         "  -I <dir>     - prepend a module search directory\n"
         "  -m <module>  - import a module\n"
         "  -x <module>  - import only a module\n"
#endif
         "  -e <expr>    - evaluate an expression\n"
         "  -p <expr>    - evaluate and print an expression\n"
         "  -r[<main>]   - run a SRFI-22 main\n"
         "  -R[<module>] - run main from a module\n"
#if SEXP_USE_IMAGE_LOADING
         "  -d <file>    - dump an image file and exit\n"
         "  -i <file>    - load an image file\n"
#endif
         );
  if (err == 0) exit_success();
  else exit_failure();
}
#else
#define sexp_usage(err) (err ? exit_failure() : exit_success())
#endif

#if SEXP_USE_PRINT_BACKTRACE_ON_SEGFAULT
#include <execinfo.h>
#include <signal.h>
void sexp_segfault_handler(int sig) {
  void *array[10];
  size_t size;
  /* get void*'s for all entries on the stack */
  size = backtrace(array, 10);
  /* print out all the frames to stderr */
  fprintf(stderr, "Error: signal %d:\n", sig);
  backtrace_symbols_fd(array, size, STDERR_FILENO);
  exit(1);
}
#endif


#if SEXP_USE_GREEN_THREADS
static void sexp_make_unblocking (sexp ctx, sexp port) {
  if (!(sexp_portp(port) && sexp_port_fileno(port) >= 0))
    return;
  if (sexp_port_flags(port) == SEXP_PORT_UNKNOWN_FLAGS)
    sexp_port_flags(port) = fcntl(sexp_port_fileno(port), F_GETFL);
  if (!(sexp_port_flags(port) & O_NONBLOCK))
    if (fcntl(sexp_port_fileno(port), F_SETFL, sexp_port_flags(port) | O_NONBLOCK) == 0)
      sexp_port_flags(port) |= O_NONBLOCK;
}
#endif

static sexp sexp_meta_env (sexp ctx) {
  if (sexp_envp(sexp_global(ctx, SEXP_G_META_ENV)))
    return sexp_global(ctx, SEXP_G_META_ENV);
  return sexp_context_env(ctx);
}

static sexp sexp_param_ref (sexp ctx, sexp env, sexp name) {
  sexp res = sexp_env_ref(ctx, env, name, SEXP_FALSE);
  return sexp_opcodep(res) ? sexp_parameter_ref(ctx, res) : NULL;
}

static sexp sexp_load_standard_params (sexp ctx, sexp e) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  sexp_load_standard_ports(ctx, e, stdin, stdout, stderr, 0);
#if SEXP_USE_GREEN_THREADS
  sexp_make_unblocking(ctx, sexp_param_ref(ctx, e, sexp_global(ctx, SEXP_G_CUR_IN_SYMBOL)));
  sexp_make_unblocking(ctx, sexp_param_ref(ctx, e, sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL)));
  sexp_make_unblocking(ctx, sexp_param_ref(ctx, e, sexp_global(ctx, SEXP_G_CUR_ERR_SYMBOL)));
#endif
  res = sexp_make_env(ctx);
  sexp_env_parent(res) = e;
  sexp_context_env(ctx) = res;
  sexp_set_parameter(ctx, sexp_meta_env(ctx), sexp_global(ctx, SEXP_G_INTERACTION_ENV_SYMBOL), res);
  sexp_gc_release1(ctx);
  return res;
}

static void repl (sexp ctx, sexp env) {
  sexp_gc_var6(obj, tmp, res, in, out, err);
  sexp_gc_preserve6(ctx, obj, tmp, res, in, out, err);
  sexp_context_tracep(ctx) = 1;
  in  = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_IN_SYMBOL));
  out = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL));
  err = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_ERR_SYMBOL));
  if (in == NULL || out == NULL) {
    fprintf(stderr, "Standard I/O ports not found, aborting.  Maybe a bad -x language?\n");
    exit_failure();
  }
  if (err == NULL) err = out;
  sexp_port_sourcep(in) = 1;
  while (1) {
    sexp_write_string(ctx, "> ", out);
    sexp_flush(ctx, out);
    sexp_maybe_block_port(ctx, in, 1);
    obj = sexp_read(ctx, in);
    sexp_maybe_unblock_port(ctx, in);
    if (obj == SEXP_EOF)
      break;
    if (sexp_exceptionp(obj)) {
      sexp_print_exception(ctx, obj, err);
    } else {
      sexp_context_top(ctx) = 0;
      if (!(sexp_idp(obj)||sexp_pairp(obj)||sexp_nullp(obj)))
        obj = sexp_make_lit(ctx, obj);
      tmp = sexp_env_bindings(env);
      res = sexp_eval(ctx, obj, env);
#if SEXP_USE_WARN_UNDEFS
      sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp, res);
#endif
      if (res && sexp_exceptionp(res)) {
        sexp_print_exception(ctx, res, err);
        sexp_stack_trace(ctx, err);
      } else if (res != SEXP_VOID) {
        sexp_write(ctx, res, out);
        sexp_write_char(ctx, '\n', out);
      }
    }
  }
  sexp_gc_release6(ctx);
}

#if ! SEXP_USE_BOEHM
static sexp_uint_t multiplier (char c) {
  switch (sexp_tolower((unsigned char)c)) {
  case 'k': return 1024;
  case 'm': return (1024*1024);
  case 'g': return (1024*1024*1024);
  default:  return 1;
  }
}
#endif

static char* make_import(const char* prefix, const char* mod, const char* suffix) {
  int preflen = strlen(prefix), len = preflen + strlen(mod) + strlen(suffix);
  char *p, *impmod = (char*) malloc(len+1);
  strcpy(impmod, prefix);
  strcpy(impmod+preflen, mod[0] == '(' ? mod + 1 : mod);
  strcpy(impmod+len-strlen(suffix)-(mod[0] == '(' ? 1 : 0),  suffix);
  impmod[len] = '\0';
  for (p=impmod; *p; p++)
    if (*p == '.') *p=' ';
  return impmod;
}

static void check_nonull_arg (int c, char *arg) {
  if (! arg) {
    fprintf(stderr, "chibi-scheme: option '%c' requires an argument\n", c);
    sexp_usage(1);
  }
}

static sexp check_exception (sexp ctx, sexp res) {
  sexp_gc_var4(err, advise, sym, tmp);
  if (res && sexp_exceptionp(res)) {
    sexp_gc_preserve4(ctx, err, advise, sym, tmp);
    tmp = res;
    err = sexp_current_error_port(ctx);
    if (! sexp_oportp(err))
      err = sexp_make_output_port(ctx, stderr, SEXP_FALSE);
    sexp_print_exception(ctx, res, err);
    sexp_stack_trace(ctx, err);
#if SEXP_USE_MAIN_ERROR_ADVISE
    if (sexp_envp(sexp_global(ctx, SEXP_G_META_ENV))) {
      advise = sexp_eval_string(ctx, sexp_advice_environment, -1, sexp_global(ctx, SEXP_G_META_ENV));
      if (sexp_vectorp(advise)) {
        advise = sexp_vector_ref(advise, SEXP_ONE);
        if (sexp_envp(advise)) {
          sym = sexp_intern(ctx, "repl-advise-exception", -1);
          advise = sexp_env_ref(ctx, advise, sym, SEXP_FALSE);
          if (sexp_procedurep(advise))
            sexp_apply(ctx, advise, tmp=sexp_list2(ctx, res, err));
        }
      }
    }
#endif
    sexp_gc_release4(ctx);
    exit_failure();
  }
  return res;
}

static sexp sexp_add_import_binding (sexp ctx, sexp env) {
  sexp_gc_var2(sym, tmp);
  sexp_gc_preserve2(ctx, sym, tmp);
  sym = sexp_intern(ctx, "repl-import", -1);
  tmp = sexp_env_ref(ctx, sexp_meta_env(ctx), sym, SEXP_VOID);
  sym = sexp_intern(ctx, "import", -1);
  sexp_env_define(ctx, env, sym, tmp);
  sexp_gc_release3(ctx);
  return env;
}

static sexp sexp_load_standard_repl_env (sexp ctx, sexp env, sexp k, int bootp) {
  sexp_gc_var1(e);
  sexp_gc_preserve1(ctx, e);
  e = sexp_load_standard_env(ctx, env, k);
  if (!sexp_exceptionp(e)) {
#if SEXP_USE_MODULES
    if (!bootp)
      e = sexp_eval_string(ctx, sexp_default_environment, -1, sexp_global(ctx, SEXP_G_META_ENV));
    if (!sexp_exceptionp(e))
      sexp_add_import_binding(ctx, e);
#endif
    if (!sexp_exceptionp(e))
      e = sexp_load_standard_params(ctx, e);
  }
  sexp_gc_release1(ctx);
  return e;
}

static void do_init_context (sexp* ctx, sexp* env, sexp_uint_t heap_size,
                             sexp_uint_t heap_max_size, sexp_sint_t fold_case) {
  *ctx = sexp_make_eval_context(NULL, NULL, NULL, heap_size, heap_max_size);
  if (! *ctx) {
    fprintf(stderr, "chibi-scheme: out of memory\n");
    exit_failure();
  }
#if SEXP_USE_FOLD_CASE_SYMS
  sexp_global(*ctx, SEXP_G_FOLD_CASE_P) = sexp_make_boolean(fold_case);
#endif
  *env = sexp_context_env(*ctx);
}

#define handle_noarg() if (argv[i][2] != '\0') { \
    fprintf(stderr, "option %c doesn't take any argument but got: %s\n", argv[i][1], argv[i]); \
    exit_failure();                                                     \
  }

#define init_context() if (! ctx) do {                                  \
      do_init_context(&ctx, &env, heap_size, heap_max_size, fold_case); \
      sexp_gc_preserve4(ctx, tmp, sym, args, env);                      \
    } while (0)

#define load_init(bootp) if (! init_loaded++) do {                      \
      init_context();                                                   \
      check_exception(ctx, env=sexp_load_standard_repl_env(ctx, env, SEXP_SEVEN, bootp)); \
    } while (0)

/* static globals for the sake of resuming from within emscripten */
#ifdef EMSCRIPTEN
static sexp sexp_resume_ctx = SEXP_FALSE;
static sexp sexp_resume_proc = SEXP_FALSE;
#endif

sexp run_main (int argc, char **argv) {
#if SEXP_USE_MODULES
  char *impmod;
#endif
  char *arg;
  const char *prefix=NULL, *suffix=NULL, *main_symbol=NULL, *main_module=NULL;
  sexp_sint_t i, j, c, quit=0, print=0, init_loaded=0, mods_loaded=0,
    fold_case=SEXP_DEFAULT_FOLD_CASE_SYMS;
  sexp_uint_t heap_size=0, heap_max_size=SEXP_MAXIMUM_HEAP_SIZE;
  sexp out=SEXP_FALSE, ctx=NULL, ls;
  sexp_gc_var4(tmp, sym, args, env);
  args = SEXP_NULL;
  env = NULL;

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch ((c=argv[i][1])) {
    case 'D':
      init_context();
      arg = (argv[i][2] == '\0') ? argv[++i] : argv[i]+2;
      sym = sexp_intern(ctx, arg, -1);
      ls = sexp_global(ctx, SEXP_G_FEATURES);
      if (sexp_pairp(ls)) {
        for (; sexp_pairp(sexp_cdr(ls)); ls=sexp_cdr(ls))
          ;
        sexp_cdr(ls) = sexp_cons(ctx, sym, SEXP_NULL);
      }
      break;
    case 'e':
    case 'p':
      mods_loaded = 1;
      load_init(0);
      print = (argv[i][1] == 'p');
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      check_nonull_arg('e', arg);
      tmp = check_exception(ctx, sexp_eval_string(ctx, arg, -1, env));
      if (print) {
        if (! sexp_oportp(out))
          out = sexp_eval_string(ctx, "(current-output-port)", -1, env);
        sexp_write(ctx, tmp, out);
        sexp_write_char(ctx, '\n', out);
      }
      quit = 1;
      break;
    case 'l':
      mods_loaded = 1;
      load_init(0);
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      check_nonull_arg('l', arg);
      check_exception(ctx, sexp_load_module_file(ctx, arg, env));
      break;
    case 'x':
      prefix = sexp_environment_prefix;
      suffix = sexp_environment_suffix;
    case 'm':
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      if (c == 'x') {
        if (strcmp(arg, "chibi.primitive") == 0) {
          goto load_primitive;
        } else if (strcmp(arg, "scheme.small") == 0) {
          load_init(0);
          break;
        }
      } else {
        prefix = sexp_import_prefix;
        suffix = sexp_import_suffix;
      }
      mods_loaded = 1;
      load_init(c == 'x');
#if SEXP_USE_MODULES
      check_nonull_arg(c, arg);
      impmod = make_import(prefix, arg, suffix);
      tmp = check_exception(ctx, sexp_eval_string(ctx, impmod, -1, (c=='x' ? sexp_global(ctx, SEXP_G_META_ENV) : env)));
      free(impmod);
      if (c == 'x') {
        sexp_set_parameter(ctx, sexp_global(ctx, SEXP_G_META_ENV), sexp_global(ctx, SEXP_G_INTERACTION_ENV_SYMBOL), tmp);
        sexp_context_env(ctx) = env = tmp;
        sexp_add_import_binding(ctx, env);
        tmp = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL));
        if (tmp != NULL && !sexp_oportp(tmp)) {
          sexp_load_standard_ports(ctx, env, stdin, stdout, stderr, 0);
        }
      }
#endif
      break;
    load_primitive:
    case 'Q':
      init_context();
      mods_loaded = 1;
      if (! init_loaded++)
        sexp_load_standard_ports(ctx, env, stdin, stdout, stderr, 0);
      handle_noarg();
      break;
    case 'q':
      argv[i--] = (char*)"-xchibi";
      break;
    case 'A':
      init_context();
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      check_nonull_arg('A', arg);
      sexp_add_module_directory(ctx, tmp=sexp_c_string(ctx,arg,-1), SEXP_TRUE);
      break;
    case 'I':
      init_context();
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      check_nonull_arg('I', arg);
      sexp_add_module_directory(ctx, tmp=sexp_c_string(ctx,arg,-1), SEXP_FALSE);
      break;
    case '-':
      if (argv[i][2] == '\0') {
        i++;
        goto done_options;
      }
      sexp_usage(1);
    case 'h':
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      check_nonull_arg('h', arg);
#if ! SEXP_USE_BOEHM
      heap_size = strtoul(arg, &arg, 0);
      if (sexp_isalpha((unsigned char)*arg)) heap_size *= multiplier(*arg++);
      if (*arg == '/') {
        heap_max_size = strtoul(arg+1, &arg, 0);
        if (sexp_isalpha((unsigned char)*arg)) heap_max_size *= multiplier(*arg++);
      }
#endif
      break;
#if SEXP_USE_IMAGE_LOADING
    case 'i':
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      if (ctx) {
        fprintf(stderr, "-:i <file>: image files must be loaded first\n");
        exit_failure();
      }
      ctx = sexp_load_image(arg, 0, heap_size, heap_max_size);
      if (!ctx || !sexp_contextp(ctx)) {
        fprintf(stderr, "-:i <file>: couldn't open file for reading: %s\n", arg);
        fprintf(stderr, "            %s\n", sexp_load_image_err());
        exit_failure();
      }
      env = sexp_load_standard_params(ctx, sexp_context_env(ctx));
      init_loaded++;
      break;
    case 'd':
      if (! init_loaded++) {
        init_context();
        env = sexp_load_standard_env(ctx, env, SEXP_SEVEN);
      }
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      if (sexp_save_image(ctx, arg) != SEXP_TRUE) {
        fprintf(stderr, "-d <file>: couldn't save image to file: %s\n", arg);
        fprintf(stderr, "           %s\n", sexp_load_image_err());
        exit_failure();
      }
      quit = 1;
      break;
#endif
    case 'V':
      load_init(1);
      if (! sexp_oportp(out))
        out = sexp_eval_string(ctx, "(current-output-port)", -1, env);
      sexp_write_string(ctx, sexp_version_string, out);
      tmp = sexp_env_ref(ctx, env, sym=sexp_intern(ctx, "*features*", -1), SEXP_NULL);
      sexp_write(ctx, tmp, out);
      sexp_newline(ctx, out);
      return SEXP_TRUE;
#if SEXP_USE_FOLD_CASE_SYMS
    case 'f':
      fold_case = 1;
      init_context();
      sexp_global(ctx, SEXP_G_FOLD_CASE_P) = SEXP_TRUE;
      handle_noarg();
      break;
#endif
    case 'R':
      main_module = argv[i][2] != '\0' ? argv[i]+2 :
        (i+1 < argc && argv[i+1][0] != '-') ? argv[++i] : "chibi.repl";
      if (main_symbol == NULL) main_symbol = "main";
      break;
    case 'r':
      main_symbol = argv[i][2] == '\0' ? "main" : argv[i]+2;
      break;
    case 's':
      init_context(); sexp_global(ctx, SEXP_G_STRICT_P) = SEXP_TRUE;
      handle_noarg();
      break;
    case 't':
      mods_loaded = 1;
      load_init(1);
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
#if SEXP_USE_MODULES
      check_nonull_arg('t', arg);
      suffix = strrchr(arg, '.');
      sym = sexp_intern(ctx, suffix + 1, -1);
      *(char*)suffix = '\0';
      impmod = make_import(sexp_trace_prefix, arg, sexp_trace_suffix);
      tmp = check_exception(ctx, sexp_eval_string(ctx, impmod, -1, sexp_meta_env(ctx)));
      if (!(tmp && sexp_envp(tmp))) {
        fprintf(stderr, "couldn't find library to trace: %s\n", impmod);
      } else if (!((sym = sexp_env_cell(ctx, tmp, sym, 0)))) {
        fprintf(stderr, "couldn't find binding to trace: %s in %s\n", suffix + 1, impmod);
      } else {
        sym = sexp_list1(ctx, sym);
        tmp = check_exception(ctx, sexp_eval_string(ctx, "(environment '(chibi trace))", -1, sexp_meta_env(ctx)));
        tmp = sexp_env_ref(ctx, tmp, sexp_intern(ctx, "trace-cell", -1), 0);
        if (tmp && sexp_procedurep(tmp))
          check_exception(ctx, sexp_apply(ctx, tmp, sym));
      }
      free(impmod);
#endif
      break;
    default:
      fprintf(stderr, "unknown option: %s\n", argv[i]);
      /* ... FALLTHROUGH ... */
    case '?':
      sexp_usage(1);
    }
  }

 done_options:
  if (!quit || main_symbol != NULL) {
    init_context();
    /* build argument list */
    if (i < argc)
      for (j=argc-1; j>=i; j--)
        args = sexp_cons(ctx, tmp=sexp_c_string(ctx,argv[j],-1), args);
    if (i >= argc || main_symbol != NULL)
      args = sexp_cons(ctx, tmp=sexp_c_string(ctx,argv[0],-1), args);
    load_init(i < argc || main_symbol != NULL);
    sexp_set_parameter(ctx, sexp_meta_env(ctx), sym=sexp_intern(ctx, sexp_argv_symbol, -1), args);
    if (i >= argc && main_symbol == NULL) {
      /* no script or main, run interactively */
      repl(ctx, env);
    } else {
#if SEXP_USE_MODULES
      /* load the module or script */
      if (main_module != NULL) {
        impmod = make_import("(load-module '(", main_module, "))");
        env = check_exception(ctx, sexp_eval_string(ctx, impmod, -1, sexp_meta_env(ctx)));
        if (sexp_vectorp(env)) env = sexp_vector_ref(env, SEXP_ONE);
        free(impmod);
        check_exception(ctx, env);
        if (!sexp_envp(env)) {
          fprintf(stderr, "couldn't find module: %s\n", main_module);
          exit_failure();
        }
      } else
#endif
      if (i < argc) {   /* script usage */
#if SEXP_USE_MODULES
        /* reset the environment to have only the `import' and */
        /* `cond-expand' bindings */
        if (!mods_loaded) {
          env = sexp_make_env(ctx);
          sexp_set_parameter(ctx, sexp_meta_env(ctx),
                             sexp_global(ctx, SEXP_G_INTERACTION_ENV_SYMBOL), env);
          sexp_context_env(ctx) = env;
          sym = sexp_intern(ctx, "repl-import", -1);
          tmp = sexp_env_ref(ctx, sexp_meta_env(ctx), sym, SEXP_VOID);
          sym = sexp_intern(ctx, "import", -1);
          check_exception(ctx, sexp_env_define(ctx, env, sym, tmp));
          sym = sexp_intern(ctx, "cond-expand", -1);
          tmp = sexp_env_cell(ctx, sexp_meta_env(ctx), sym, 0);
#if SEXP_USE_RENAME_BINDINGS
          sexp_env_rename(ctx, env, sym, tmp);
#endif
          sexp_env_define(ctx, env, sym, sexp_cdr(tmp));
        }
#endif
        sexp_context_tracep(ctx) = 1;
        tmp = sexp_env_bindings(env);
#if SEXP_USE_MODULES
        /* use scheme load if possible for better stack traces */
        sym = sexp_intern(ctx, "load", -1);
        tmp = sexp_env_ref(ctx, sexp_meta_env(ctx), sym, SEXP_FALSE);
        if (sexp_procedurep(tmp)) {
          sym = sexp_c_string(ctx, argv[i], -1);
          sym = sexp_list2(ctx, sym, env);
          tmp = check_exception(ctx, sexp_apply(ctx, tmp, sym));
        } else
#endif
          tmp = check_exception(ctx, sexp_load(ctx, sym=sexp_c_string(ctx, argv[i], -1), env));
#if SEXP_USE_WARN_UNDEFS
        sexp_warn_undefs(ctx, env, tmp, SEXP_VOID);
#endif
#ifdef EMSCRIPTEN
        if (sexp_applicablep(tmp)) {
          sexp_resume_ctx = ctx;
          sexp_resume_proc = tmp;
          sexp_preserve_object(ctx, sexp_resume_proc);
          emscripten_exit_with_live_runtime();
        }
#endif
      }
      /* SRFI-22: run main if specified */
      if (main_symbol) {
        sym = sexp_intern(ctx, main_symbol, -1);
        tmp = sexp_env_ref(ctx, env, sym, SEXP_FALSE);
        if (sexp_procedurep(tmp)) {
          args = sexp_list1(ctx, args);
          check_exception(ctx, sexp_apply(ctx, tmp, args));
        } else {
          fprintf(stderr, "couldn't find main binding: %s in %s\n", main_symbol, main_module ? main_module : argv[i]);
        }
      }
    }
  }

  sexp_gc_release4(ctx);
  if (sexp_destroy_context(ctx) == SEXP_FALSE) {
    fprintf(stderr, "destroy_context error\n");
    return SEXP_FALSE;
  }
  return SEXP_TRUE;
}

#ifdef EMSCRIPTEN
void sexp_resume() {
  sexp_gc_var1(tmp);
  sexp_gc_preserve1(sexp_resume_ctx, tmp);
  tmp = sexp_list1(sexp_resume_ctx, SEXP_VOID);
  if (sexp_applicablep(sexp_resume_proc)) {
    sexp_resume_proc = check_exception(sexp_resume_ctx, sexp_apply(sexp_resume_ctx, sexp_resume_proc, tmp));
  }
  sexp_gc_release1(sexp_resume_ctx);
}
#endif

int main (int argc, char **argv) {
#if SEXP_USE_PRINT_BACKTRACE_ON_SEGFAULT
  signal(SIGSEGV, sexp_segfault_handler); 
#endif
  sexp_scheme_init();
  if (run_main(argc, argv) == SEXP_FALSE) {
    exit_failure();
  } else {
    exit_success();
  }
  return 0;
}
