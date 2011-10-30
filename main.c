/* main.c -- chibi-scheme command-line app                   */
/* Copyright (c) 2009-2011 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"

#define sexp_argv_symbol "*command-line-arguments*"
#define sexp_argv_proc   "(define (command-line-arguments) "sexp_argv_symbol")"

#define sexp_import_prefix "(import ("
#define sexp_import_suffix "))"

#define sexp_version_string "chibi-scheme "sexp_version" \""sexp_release_name"\" "

#ifdef PLAN9
#define exit_failure() exits("ERROR")
#else
#define exit_failure() exit(70)
#endif

#if SEXP_USE_IMAGE_LOADING

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#define SEXP_IMAGE_MAGIC "\x07\x07chibi\n\0"
#define SEXP_IMAGE_MAJOR_VERSION 1
#define SEXP_IMAGE_MINOR_VERSION 1

typedef struct sexp_image_header_t* sexp_image_header;
struct sexp_image_header_t {
  const char magic[8];
  short major, minor;
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
  }
  if (heap_size < header.size) heap_size = header.size;
  heap = (sexp_heap)malloc(sexp_heap_pad_size(heap_size));
  if (read(fd, heap, header.size) != header.size) {
    fprintf(stderr, "error reading image\n");
    return NULL;
  }
  offset = (sexp_sint_t)((char*)heap - (sexp_sint_t)header.base);
  /* expand the last free chunk if necessary */
  if (heap->size < heap_size) {
    for (q=(sexp_free_list)((char*)heap->free_list + offset); q->next;
         q=(sexp_free_list)((char*)q->next + offset))
      ;
    if ((char*)q + q->size >= (char*)heap->data + heap->size) {
      /* last free chunk at end of heap */
      q->size += heap_size - heap->size;
    } else {
      /* last free chunk in the middle of the heap */
      q->next = (sexp_free_list)((char*)heap->data + heap->size);
      q = (sexp_free_list)((char*)q->next + offset);
      q->size = heap_size - heap->size;
      q->next = NULL;
    }
    heap->size += (heap_size - heap->size);
  }
  ctx = (sexp)(header.context + offset);
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

static sexp sexp_param_ref (sexp ctx, sexp env, sexp name) {
  sexp res=sexp_env_ref(env, name, SEXP_FALSE);
  return sexp_opcodep(res) ? sexp_parameter_ref(ctx, res) : SEXP_VOID;
}

static void repl (sexp ctx, sexp env) {
  sexp in, out, err;
  sexp_gc_var3(obj, tmp, res);
  sexp_gc_preserve3(ctx, obj, tmp, res);
  sexp_context_tracep(ctx) = 1;
  in  = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_IN_SYMBOL));
  out = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL));
  err = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_ERR_SYMBOL));
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
      tmp = sexp_env_bindings(env);
      sexp_context_top(ctx) = 0;
      if (!(sexp_idp(obj)||sexp_pairp(obj)))
        obj = sexp_make_lit(ctx, obj);
      res = sexp_eval(ctx, obj, env);
      if (sexp_exceptionp(res)) {
        sexp_print_exception(ctx, res, err);
        sexp_stack_trace(ctx, err);
      } else {
#if SEXP_USE_WARN_UNDEFS
        sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp);
#endif
        if (res != SEXP_VOID) {
          sexp_write(ctx, res, out);
          sexp_write_char(ctx, '\n', out);
        }
      }
    }
  }
  sexp_gc_release3(ctx);
}

static sexp_uint_t multiplier (char c) {
  switch ((tolower)(c)) {
  case 'k': return 1024;
  case 'm': return (1024*1024);
  case 'g': return (1024*1024*1024);
  default:  return 1;
  }
}

static void check_nonull_arg (int c, char *arg) {
  if (! arg) {
    fprintf(stderr, "chibi-scheme: option '%c' requires an argument\n", c);
    exit_failure();
  }
}

static sexp check_exception (sexp ctx, sexp res) {
  sexp err;
  if (res && sexp_exceptionp(res)) {
    err = sexp_current_error_port(ctx);
    if (! sexp_oportp(err))
      err = sexp_make_output_port(ctx, stderr, SEXP_FALSE);
    sexp_print_exception(ctx, res, err);
    sexp_stack_trace(ctx, err);
    exit_failure();
  }
  return res;
}

static sexp sexp_load_standard_repl_env (sexp ctx, sexp env, sexp k) {
  sexp_gc_var3(e, p, res);
  sexp_gc_preserve3(ctx, e, p, res);
  e = sexp_load_standard_env(ctx, env, k);
  if (sexp_exceptionp(e)) return e;
  sexp_load_standard_ports(ctx, e, stdin, stdout, stderr, 0);
#if SEXP_USE_GREEN_THREADS
  p  = sexp_param_ref(ctx, e, sexp_global(ctx, SEXP_G_CUR_IN_SYMBOL));
  if (sexp_portp(p)) sexp_maybe_block_port(ctx, p, 1);
#endif
  res = sexp_make_env(ctx);
  sexp_env_parent(res) = e;
  sexp_set_parameter(ctx, res, sexp_global(ctx, SEXP_G_INTERACTION_ENV_SYMBOL), res);
  sexp_gc_release3(ctx);
  return res;
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
      sexp_gc_preserve2(ctx, tmp, args);                                \
    } while (0)

#define load_init() if (! init_loaded++) do {                           \
      init_context();                                                   \
      check_exception(ctx, env=sexp_load_standard_repl_env(ctx, env, SEXP_SEVEN)); \
    } while (0)

void run_main (int argc, char **argv) {
  char *arg, *impmod, *p;
  sexp out=SEXP_FALSE, env=NULL, ctx=NULL;
  sexp_sint_t i, j, len, quit=0, print=0, init_loaded=0, fold_case=SEXP_DEFAULT_FOLD_CASE_SYMS;
  sexp_uint_t heap_size=0, heap_max_size=SEXP_MAXIMUM_HEAP_SIZE;
  sexp_gc_var2(tmp, args);
  args = SEXP_NULL;

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
    case 'e':
    case 'p':
      load_init();
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
      load_init();
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      check_nonull_arg('l', arg);
      check_exception(ctx, sexp_load_module_file(ctx, arg, env));
      break;
    case 'm':
      load_init();
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      check_nonull_arg('m', arg);
      len = strlen(arg)+strlen(sexp_import_prefix)+strlen(sexp_import_suffix);
      impmod = (char*) malloc(len+1);
      strcpy(impmod, sexp_import_prefix);
      strcpy(impmod+strlen(sexp_import_prefix), arg);
      strcpy(impmod+len-+strlen(sexp_import_suffix), sexp_import_suffix);
      impmod[len] = '\0';
      for (p=impmod; *p; p++)
        if (*p == '.') *p=' ';
      check_exception(ctx, sexp_eval_string(ctx, impmod, -1, env));
      free(impmod);
      break;
    case 'q':
      init_context();
      if (! init_loaded++)
        sexp_load_standard_ports(ctx, env, stdin, stdout, stderr, 0);
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
      i++;
      goto done_options;
    case 'h':
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      check_nonull_arg('h', arg);
      heap_size = strtoul(arg, &arg, 0);
      if (sexp_isalpha(*arg)) heap_size *= multiplier(*arg++);
      if (*arg == '/') {
        heap_max_size = strtoul(arg+1, &arg, 0);
        if (sexp_isalpha(*arg)) heap_max_size *= multiplier(*arg++);
      }
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
      env = sexp_context_env(ctx);
      sexp_load_standard_ports(ctx, env, stdin, stdout, stderr, 0);
      init_loaded++;
      break;
    case 'd':
      load_init();
      arg = ((argv[i][2] == '\0') ? argv[++i] : argv[i]+2);
      if (!sexp_save_image(ctx, arg))
        exit_failure();
      quit = 1;
      break;
#endif
    case 'V':
      load_init();
      if (! sexp_oportp(out))
        out = sexp_eval_string(ctx, "(current-output-port)", -1, env);
      sexp_write_string(ctx, sexp_version_string, out);
      tmp = sexp_env_ref(env, sexp_intern(ctx, "*features*", -1), SEXP_NULL);
      sexp_write(ctx, tmp, out);
      sexp_newline(ctx, out);
      return;
#if SEXP_USE_FOLD_CASE_SYMS
    case 'f':
      fold_case = 1;
      if (ctx) sexp_global(ctx, SEXP_G_FOLD_CASE_P) = SEXP_TRUE;
      break;
#endif
    default:
      fprintf(stderr, "unknown option: %s\n", argv[i]);
      exit_failure();
    }
  }

 done_options:
  if (! quit) {
    load_init();
    if (i < argc)
      for (j=argc-1; j>i; j--)
        args = sexp_cons(ctx, tmp=sexp_c_string(ctx,argv[j],-1), args);
    else
      args = sexp_cons(ctx, tmp=sexp_c_string(ctx,argv[0],-1), args);
    sexp_env_define(ctx, env, sexp_intern(ctx, sexp_argv_symbol, -1), args);
    sexp_eval_string(ctx, sexp_argv_proc, -1, env);
    if (i < argc) {             /* script usage */
      sexp_context_tracep(ctx) = 1;
      check_exception(ctx, sexp_load(ctx, tmp=sexp_c_string(ctx, argv[i], -1), env));
      tmp = sexp_intern(ctx, "main", -1);
      tmp = sexp_env_ref(env, tmp, SEXP_FALSE);
      if (sexp_procedurep(tmp)) {
        args = sexp_list1(ctx, args);
        check_exception(ctx, sexp_apply(ctx, tmp, args));
      }
    } else {
      repl(ctx, env);
    }
  }

  sexp_gc_release2(ctx);
  sexp_destroy_context(ctx);
}

int main (int argc, char **argv) {
  sexp_scheme_init();
  run_main(argc, argv);
  return 0;
}
