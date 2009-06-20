/* main.c -- chibi-scheme command-line app using        */
/* Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt  */

#include "chibi/eval.h"

void repl (sexp ctx) {
  sexp tmp, res, env, in, out, err;
  sexp_gc_var(ctx, obj, s_obj);
  sexp_gc_preserve(ctx, obj, s_obj);
  env = sexp_context_env(ctx);
  sexp_context_tracep(ctx) = 1;
  in = sexp_eval_string(ctx, "(current-input-port)");
  out = sexp_eval_string(ctx, "(current-output-port)");
  err = sexp_eval_string(ctx, "(current-error-port)");
  while (1) {
    sexp_write_string("> ", out);
    sexp_flush(out);
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
      sexp_warn_undefs(sexp_env_bindings(env), tmp, err);
#endif
      if (res != SEXP_VOID) {
        sexp_write(res, out);
        sexp_write_char('\n', out);
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
#if USE_STRING_STREAMS
    case 'e':
    case 'p':
      if (! init_loaded++)
        sexp_load(ctx, str=sexp_c_string(ctx, sexp_init_file, -1), env);
      res = sexp_read_from_string(ctx, argv[i+1]);
      if (! sexp_exceptionp(res))
        res = sexp_eval(ctx, res);
      if (sexp_exceptionp(res)) {
        sexp_print_exception(ctx, res, out);
      } else if (argv[i][1] == 'p') {
        sexp_write(res, out);
        sexp_write_char('\n', out);
      }
      quit=1;
      i++;
      break;
#endif
    case 'l':
      if (! init_loaded++)
        sexp_load(ctx, str=sexp_c_string(ctx, sexp_init_file, -1), env);
      sexp_load(ctx, str=sexp_c_string(ctx, argv[++i], -1), env);
      break;
    case 'q':
      init_loaded = 1;
      break;
    default:
      errx(1, "unknown option: %s", argv[i]);
    }
  }

  if (! quit) {
    if (! init_loaded)
      sexp_load(ctx, str=sexp_c_string(ctx, sexp_init_file, -1), env);
    if (i < argc)
      for ( ; i < argc; i++)
        sexp_load(ctx, str=sexp_c_string(ctx, argv[i], -1), env);
    else
      repl(ctx);
  }

  sexp_gc_release(ctx, str, s_str);
}

int main (int argc, char **argv) {
  sexp_scheme_init();
  run_main(argc, argv);
  return 0;
}

