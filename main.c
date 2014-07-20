/* main.c -- chibi-scheme command-line app                   */
/* Copyright (c) 2009-2013 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"

#define sexp_argv_symbol "command-line"

#define sexp_import_prefix "(import ("
#define sexp_import_suffix "))"
#define sexp_environment_prefix "(environment '("
#define sexp_environment_suffix "))"
#define sexp_default_environment "(environment '(scheme base))"
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
  // get void*'s for all entries on the stack
  size = backtrace(array, 10);
  // print out all the frames to stderr
  fprintf(stderr, "Error: signal %d:\n", sig);
  backtrace_symbols_fd(array, size, STDERR_FILENO);
  exit(1);
}
#endif

#if SEXP_USE_IMAGE_LOADING

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>

#define SEXP_IMAGE_MAGIC "\a\achibi\n\0"
#define SEXP_IMAGE_MAJOR_VERSION 1
#define SEXP_IMAGE_MINOR_VERSION 1

typedef struct sexp_image_header_t* sexp_image_header;
struct sexp_image_header_t {
  char magic[8];
  short major, minor;
  sexp_abi_identifier_t abi;
  sexp_uint_t size;
  sexp_heap base;
  sexp context;
};

sexp sexp_gc (sexp ctx, size_t *sum_freed);
void sexp_offset_heap_pointers (sexp_heap heap, sexp_heap from_heap, sexp* types, sexp flags);

static sexp sexp_load_image (const char* file, sexp_uint_t heap_size, sexp_uint_t heap_max_size) {
  sexp ctx, flags, *globals, *types;
  int fd;
  sexp_sint_t offset;
  sexp_heap heap;
  sexp_free_list q;
  struct sexp_image_header_t header;
  fd = open(file, O_RDONLY);
  if (fd < 0) {
    fprintf(stderr, "can't open image file: %s\n", file);
    return NULL;
  }
  if (read(fd, &header, sizeof(header)) != sizeof(header))
    return NULL;
  if (memcmp(header.magic, SEXP_IMAGE_MAGIC, sizeof(header.magic)) != 0) {
    fprintf(stderr, "invalid image file magic for %s: %s\n", file, header.magic);
    return NULL;
  } else if (header.major != SEXP_IMAGE_MAJOR_VERSION
             || header.major < SEXP_IMAGE_MINOR_VERSION) {
    fprintf(stderr, "unsupported image version: %d.%d\n",
            header.major, header.minor);
    return NULL;
  } else if (!sexp_abi_compatible(NULL, header.abi, SEXP_ABI_IDENTIFIER)) {
    fprintf(stderr, "unsupported ABI: %s (expected %s)\n",
            header.abi, SEXP_ABI_IDENTIFIER);
    return NULL;
  }
  if (heap_size < header.size) heap_size = header.size;
  heap = (sexp_heap)malloc(sexp_heap_pad_size(heap_size));
  if (!heap) {
    fprintf(stderr, "couldn't malloc heap\n");
    return NULL;
  }
  if (read(fd, heap, header.size) != header.size) {
    fprintf(stderr, "error reading image\n");
    return NULL;
  }
  offset = (sexp_sint_t)((char*)heap - (sexp_sint_t)header.base);
  /* expand the last free chunk if necessary */
  if (heap->size < heap_size) {
    for (q=(sexp_free_list)(((char*)heap->free_list) + offset); q->next;
         q=(sexp_free_list)(((char*)q->next) + offset))
      ;
    if ((char*)q + q->size >= (char*)heap->data + heap->size) {
      /* last free chunk at end of heap */
      q->size += heap_size - heap->size;
    } else {
      /* last free chunk in the middle of the heap */
      q->next = (sexp_free_list)((char*)heap->data + heap->size);
      q = (sexp_free_list)(((char*)q->next) + offset);
      q->size = heap_size - heap->size;
      q->next = NULL;
    }
    heap->size += (heap_size - heap->size);
  }
  ctx = (sexp)(((char*)header.context) + offset);
  globals = sexp_vector_data((sexp)((char*)sexp_context_globals(ctx) + offset));
  types = sexp_vector_data((sexp)((char*)(globals[SEXP_G_TYPES]) + offset));
  flags = sexp_fx_add(SEXP_COPY_LOADP, SEXP_COPY_FREEP);
  sexp_offset_heap_pointers(heap, header.base, types, flags);
  close(fd);
  return ctx;
}

static int sexp_save_image (sexp ctx, const char* path) {
  sexp_heap heap;
  FILE* file;
  struct sexp_image_header_t header;
  file = fopen(path, "w");
  if (!file) {
    fprintf(stderr, "couldn't open image file for writing: %s\n", path);
    return 0;
  }
  heap = sexp_context_heap(ctx);
  memcpy(&header.magic, SEXP_IMAGE_MAGIC, sizeof(header.magic));
  memcpy(&header.abi, SEXP_ABI_IDENTIFIER, sizeof(header.abi));
  header.major = SEXP_IMAGE_MAJOR_VERSION;
  header.minor = SEXP_IMAGE_MINOR_VERSION;
  header.size = heap->size;
  header.base = heap;
  header.context = ctx;
  sexp_gc(ctx, NULL);
  if (! (fwrite(&header, sizeof(header), 1, file) == 1
         && fwrite(heap, heap->size, 1, file) == 1)) {
    fprintf(stderr, "error writing image file\n");
    return 0;
  }
  fclose(file);
  return 1;
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
  int len = strlen(mod) + strlen(prefix) + strlen(suffix);
  char *p, *impmod = (char*) malloc(len+1);
  strcpy(impmod, prefix);
  strcpy(impmod+strlen(prefix), mod);
  strcpy(impmod+len-+strlen(suffix), suffix);
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

static sexp sexp_load_standard_repl_env (sexp ctx, sexp env, sexp k, int bootp) {
  sexp_gc_var3(e, sym, tmp);
  sexp_gc_preserve3(ctx, e, sym, tmp);
  e = sexp_load_standard_env(ctx, env, k);
  if (!sexp_exceptionp(e)) {
#if SEXP_USE_MODULES
    if (!bootp) {
      e = sexp_eval_string(ctx, sexp_default_environment, -1, sexp_global(ctx, SEXP_G_META_ENV));
      sym = sexp_intern(ctx, "repl-import", -1);
      tmp = sexp_env_ref(ctx, sexp_meta_env(ctx), sym, SEXP_VOID);
      sym = sexp_intern(ctx, "import", -1);
      sexp_env_define(ctx, e, sym, tmp);
    }
#endif
    if (!sexp_exceptionp(e)) {
      e = sexp_load_standard_params(ctx, e);
    }
  }
  sexp_gc_release3(ctx);
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

#define init_context() if (! ctx) do {                                  \
      do_init_context(&ctx, &env, heap_size, heap_max_size, fold_case); \
      sexp_gc_preserve4(ctx, tmp, sym, args, env);                      \
    } while (0)

#define load_init(bootp) if (! init_loaded++) do {                      \
      init_context();                                                   \
      check_exception(ctx, env=sexp_load_standard_repl_env(ctx, env, SEXP_SEVEN, bootp)); \
    } while (0)

void run_main (int argc, char **argv) {
#if SEXP_USE_MODULES
  char *impmod;
#endif
  char *arg;
  const char *prefix=NULL, *suffix=NULL, *main_symbol=NULL, *main_module=NULL;
  sexp_sint_t i, j, c, quit=0, print=0, init_loaded=0, mods_loaded=0,
    no_script=0, fold_case=SEXP_DEFAULT_FOLD_CASE_SYMS;
  sexp_uint_t heap_size=0, heap_max_size=SEXP_MAXIMUM_HEAP_SIZE;
  sexp out=SEXP_FALSE, ctx=NULL;
  sexp_gc_var4(tmp, sym, args, env);
  args = SEXP_NULL;
  env = NULL;

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch ((c=argv[i][1])) {
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
        } else if (strcmp(arg, "scheme.base") == 0) {
          load_init(0);
          break;
        }
      } else {
        prefix = sexp_import_prefix;
        suffix = sexp_import_suffix;
      }
      mods_loaded = 1;
      load_init(c == 'x');      /* only load the meta-env if we're */
                                /* explicitly setting a language   */
#if SEXP_USE_MODULES
      check_nonull_arg(c, arg);
      impmod = make_import(prefix, arg, suffix);
      tmp = check_exception(ctx, sexp_eval_string(ctx, impmod, -1, (c=='x' ? sexp_global(ctx, SEXP_G_META_ENV) : env)));
      free(impmod);
      if (c == 'x') {
        sexp_set_parameter(ctx, sexp_global(ctx, SEXP_G_META_ENV), sexp_global(ctx, SEXP_G_INTERACTION_ENV_SYMBOL), env);
        sexp_context_env(ctx) = env = tmp;
        tmp = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL));
        if (tmp != NULL && !sexp_oportp(tmp))
          sexp_load_standard_ports(ctx, env, stdin, stdout, stderr, 0);
      }
#endif
      break;
    load_primitive:
    case 'Q':
      init_context();
      mods_loaded = 1;
      if (! init_loaded++)
        sexp_load_standard_ports(ctx, env, stdin, stdout, stderr, 0);
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
        no_script = 1;
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
      ctx = sexp_load_image(arg, heap_size, heap_max_size);
      if (!ctx) {
        fprintf(stderr, "-:i <file>: couldn't open file for reading: %s\n", arg);
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
      if (!sexp_save_image(ctx, arg))
        exit_failure();
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
      return;
#if SEXP_USE_FOLD_CASE_SYMS
    case 'f':
      fold_case = 1;
      init_context();
      sexp_global(ctx, SEXP_G_FOLD_CASE_P) = SEXP_TRUE;
      break;
#endif
    case 'R':
      main_module = argv[i][2] == '\0' ? "chibi.repl" : argv[i]+2;
      if (main_symbol == NULL) main_symbol = "main";
      break;
    case 'r':
      main_symbol = argv[i][2] == '\0' ? "main" : argv[i]+2;
      break;
    case 's':
      init_context(); sexp_global(ctx, SEXP_G_STRICT_P) = SEXP_TRUE;
      break;
    case 't':
      mods_loaded = 1;
      load_init(0);
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
#if SEXP_USE_MODULES
      suffix = strrchr(arg, '.');
      sym = sexp_intern(ctx, suffix + 1, -1);
      *(char*)suffix = '\0';
      impmod = make_import(sexp_environment_prefix, arg, sexp_environment_suffix);
      tmp = check_exception(ctx, sexp_eval_string(ctx, impmod, -1, sexp_meta_env(ctx)));
      free(impmod);
      sym = sexp_list1(ctx, sexp_env_cell(ctx, tmp, sym, 0));
      tmp = check_exception(ctx, sexp_eval_string(ctx, "(environment '(chibi trace))", -1, sexp_meta_env(ctx)));
      tmp = sexp_env_ref(ctx, tmp, sexp_intern(ctx, "trace-cell", -1), 0);
      check_exception(ctx, sexp_apply(ctx, tmp, sym));
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
    load_init(0);
    /* build argument list */
    if (i < argc)
      for (j=argc-1; j>=i; j--)
        args = sexp_cons(ctx, tmp=sexp_c_string(ctx,argv[j],-1), args);
    if (i >= argc || no_script)
      args = sexp_cons(ctx, tmp=sexp_c_string(ctx,argv[0],-1), args);
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
      if (i < argc && !no_script) {   /* script usage */
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
          check_exception(ctx, sexp_apply(ctx, tmp, sym));
        } else
#endif
          check_exception(ctx, sexp_load(ctx, sym=sexp_c_string(ctx, argv[i], -1), env));
#if SEXP_USE_WARN_UNDEFS
        sexp_warn_undefs(ctx, env, tmp, SEXP_VOID);
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
  sexp_destroy_context(ctx);
}

int main (int argc, char **argv) {
#if SEXP_USE_PRINT_BACKTRACE_ON_SEGFAULT
  signal(SIGSEGV, sexp_segfault_handler); 
#endif
  sexp_scheme_init();
  run_main(argc, argv);
  exit_success();
  return 0;
}
