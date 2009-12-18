/* main.c -- chibi-scheme command-line app              */
/* Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt  */

#include "chibi/eval.h"

#define sexp_argv_symbol "*command-line-arguments*"
#define sexp_argv_proc   "(define (command-line-arguments) "sexp_argv_symbol")"

static void repl (sexp ctx) {
  sexp in, out, err;
  sexp_gc_var4(obj, tmp, res, env);
  sexp_gc_preserve4(ctx, obj, tmp, res, env);
  env = sexp_context_env(ctx);
  sexp_context_tracep(ctx) = 1;
  in = sexp_eval_string(ctx, "(current-input-port)", env);
  out = sexp_eval_string(ctx, "(current-output-port)", env);
  err = sexp_eval_string(ctx, "(current-error-port)", env);
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
      res = sexp_eval(ctx, obj, env);
      if (sexp_exceptionp(res)) {
        sexp_print_exception(ctx, res, err);
      } else {
#if SEXP_USE_WARN_UNDEFS
        sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp, err);
#endif
        if (res != SEXP_VOID) {
          sexp_write(ctx, res, out);
          sexp_write_char(ctx, '\n', out);
        }
      }
    }
  }
  sexp_gc_release4(ctx);
}

static sexp check_exception (sexp ctx, sexp res) {
  sexp err;
  if (res && sexp_exceptionp(res)) {
    err = sexp_current_error_port(ctx);
    if (! sexp_oportp(err))
      err = sexp_make_output_port(ctx, stderr, SEXP_FALSE);
    sexp_print_exception(ctx, res, err);
    exit_failure();
  }
  return res;
}

void run_main (int argc, char **argv) {
  sexp env, out=NULL, res=SEXP_VOID, ctx;
  sexp_sint_t i, quit=0, init_loaded=0;
  sexp_gc_var2(str, args);

  ctx = sexp_make_eval_context(NULL, NULL, NULL);
  sexp_gc_preserve2(ctx, str, args);
  env = sexp_context_env(ctx);
  out = SEXP_FALSE;
  args = SEXP_NULL;

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
    case 'e':
    case 'p':
      if (! init_loaded++)
        check_exception(ctx, sexp_load_standard_env(ctx, env, SEXP_FIVE));
      res = check_exception(ctx, sexp_read_from_string(ctx, argv[i+1]));
      res = check_exception(ctx, sexp_eval(ctx, res, env));
      if (argv[i][1] == 'p') {
        if (! sexp_oportp(out))
          out = sexp_eval_string(ctx, "(current-output-port)", env);
        sexp_write(ctx, res, out);
        sexp_write_char(ctx, '\n', out);
      }
      quit=1;
      i++;
      break;
    case 'l':
      if (! init_loaded++)
        check_exception(ctx, sexp_load_standard_env(ctx, env, SEXP_FIVE));
      check_exception(ctx, sexp_load_module_file(ctx, argv[++i], env));
      break;
    case 'q':
      init_loaded = 1;
      break;
    case 'm':
      sexp_module_dir = argv[++i];
      break;
    case 's':
      for (argc=argc-1; argc>i+1; argc--)
        args = sexp_cons(ctx, str=sexp_c_string(ctx,argv[argc],-1), args);
      argc++;
      break;
    default:
      errx(1, "unknown option: %s", argv[i]);
    }
  }

  if (! quit) {
    if (! init_loaded)
      check_exception(ctx, sexp_load_standard_env(ctx, env, SEXP_FIVE));
    sexp_env_define(ctx, env, sexp_intern(ctx, sexp_argv_symbol), args);
    sexp_eval_string(ctx, sexp_argv_proc, env);
    if (i < argc)
      for ( ; i < argc; i++)
        check_exception(ctx, sexp_load(ctx, str=sexp_c_string(ctx, argv[i], -1), env));
    else
      repl(ctx);
  }

  sexp_gc_release2(ctx);
}

int main (int argc, char **argv) {
  sexp_scheme_init();
  run_main(argc, argv);
  return 0;
}

