/* main.c -- chibi-scheme command-line app              */
/* Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt  */

#ifndef PLAN9
#include <sys/stat.h>
#endif
#include "chibi/eval.h"

char *chibi_module_dir = NULL;

sexp find_module_file (sexp ctx, char *file) {
  sexp res;
  int mlen, flen;
  char *path;
#ifndef PLAN9
  struct stat buf;

  if (! stat(file, &buf))
#endif
    return sexp_c_string(ctx, file, -1);
#ifndef PLAN9
  if (! chibi_module_dir) {
#ifndef PLAN9
    chibi_module_dir = getenv("CHIBI_MODULE_DIR");
    if (! chibi_module_dir)
#endif
      chibi_module_dir = sexp_module_dir;
  }
  mlen = strlen(chibi_module_dir);
  flen = strlen(file);
  path = (char*) malloc(mlen+flen+2);
  memcpy(path, chibi_module_dir, mlen);
  path[mlen] = '/';
  memcpy(path+mlen+1, file, flen);
  path[mlen+flen+1] = '\0';
  if (! stat(path, &buf))
    res = sexp_c_string(ctx, path, mlen+flen+2);
  else
    res = SEXP_FALSE;
  free(path);
  return res;
#endif
}

void repl (sexp ctx) {
  sexp tmp, res, env, in, out, err;
  sexp_gc_var(ctx, obj, s_obj);
  sexp_gc_preserve(ctx, obj, s_obj);
  env = sexp_context_env(ctx);
  sexp_context_tracep(ctx) = 1;
  in = sexp_eval_string(ctx, "(current-input-port)");
  out = sexp_eval_string(ctx, "(current-output-port)");
  err = sexp_eval_string(ctx, "(current-error-port)");
  sexp_port_sourcep(in) = 1;
  while (1) {
    sexp_write_string(ctx, "> ", out);
    sexp_flush(ctx, out);
    obj = sexp_read(ctx, in);
    if (obj == SEXP_EOF)
      break;
    if (sexp_exceptionp(obj)) {
      sexp_print_exception(ctx, obj, err);
    } else {
      tmp = sexp_env_bindings(env);
      sexp_context_top(ctx) = 0;
      res = sexp_eval(ctx, obj);
#if USE_WARN_UNDEFS
      sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp, err);
#endif
      if (res != SEXP_VOID) {
        sexp_write(ctx, res, out);
        sexp_write_char(ctx, '\n', out);
      }
    }
  }
  sexp_gc_release(ctx, obj, s_obj);
}

void run_main (int argc, char **argv) {
  sexp env, out=NULL, res, ctx;
  sexp_uint_t i, quit=0, init_loaded=0;
  sexp_gc_var(ctx, str, s_str);

  ctx = sexp_make_context(NULL, NULL, NULL);
  sexp_gc_preserve(ctx, str, s_str);
  env = sexp_context_env(ctx);
  out = sexp_eval_string(ctx, "(current-output-port)");

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
    case 'e':
    case 'p':
      if (! init_loaded++)
        sexp_load(ctx, str=find_module_file(ctx, sexp_init_file), env);
      res = sexp_read_from_string(ctx, argv[i+1]);
      if (! sexp_exceptionp(res))
        res = sexp_eval(ctx, res);
      if (sexp_exceptionp(res)) {
        sexp_print_exception(ctx, res, out);
        quit = 1;
        break;
      } else if (argv[i][1] == 'p') {
        sexp_write(ctx, res, out);
        sexp_write_char(ctx, '\n', out);
      }
      quit=1;
      i++;
      break;
    case 'l':
      if (! init_loaded++)
        sexp_load(ctx, str=find_module_file(ctx, sexp_init_file), env);
      sexp_load(ctx, str=find_module_file(ctx, argv[++i]), env);
      break;
    case 'q':
      init_loaded = 1;
      break;
    case 'm':
      chibi_module_dir = argv[++i];
      break;
    default:
      errx(1, "unknown option: %s", argv[i]);
    }
  }

  if (! quit) {
    if (! init_loaded)
      res = sexp_load(ctx, str=find_module_file(ctx, sexp_init_file), env);
    if (! sexp_exceptionp(res)) {
      if (i < argc)
        for ( ; i < argc; i++)
          sexp_load(ctx, str=sexp_c_string(ctx, argv[i], -1), env);
      else
        repl(ctx);
    }
  }

  sexp_gc_release(ctx, str, s_str);
}

int main (int argc, char **argv) {
  sexp_scheme_init();
  run_main(argc, argv);
  return 0;
}

