
#include "eval.c"

void repl (sexp context) {
  sexp obj, res, env, in, out, err;
  env = sexp_context_env(context);
  in = env_global_ref(env, the_cur_in_symbol, SEXP_FALSE);
  out = env_global_ref(env, the_cur_out_symbol, SEXP_FALSE);
  err = env_global_ref(env, the_cur_err_symbol, SEXP_FALSE);
  while (1) {
    sexp_write_string("> ", out);
    sexp_flush(out);
    obj = sexp_read(in);
    if (obj == SEXP_EOF)
      break;
    if (sexp_exceptionp(obj)) {
      sexp_print_exception(obj, err);
    } else {
      res = eval_in_context(obj, context);
      if (res != SEXP_VOID) {
        sexp_write(res, out);
        sexp_write_char('\n', out);
      }
    }
  }
}

void run_main (int argc, char **argv) {
  sexp env, obj, out=NULL, res, context, err_handler;
  sexp_uint_t i, quit=0, init_loaded=0;

  env = sexp_make_standard_env(sexp_make_integer(5));
  context = sexp_make_context(NULL, env);
  sexp_context_tailp(context) = 0;
  emit_push(SEXP_VOID, context);
  emit(OP_DONE, context);
  err_handler = sexp_make_procedure(sexp_make_integer(0),
                                    sexp_make_integer(0),
                                    finalize_bytecode(context),
                                    sexp_make_vector(0, SEXP_VOID));
  env_define(env, the_err_handler_symbol, err_handler);

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
#if USE_STRING_STREAMS
    case 'e':
    case 'p':
      if (! init_loaded) {
        sexp_load(sexp_c_string(sexp_init_file), env);
        init_loaded = 1;
      }
      obj = sexp_read_from_string(argv[i+1]);
      res = eval_in_context(obj, context);
      if (argv[i][1] == 'p') {
        if (! out)
          out = env_global_ref(env, the_cur_out_symbol, SEXP_FALSE);
        sexp_write(res, out);
        sexp_write_char('\n', out);
      }
      quit=1;
      i++;
      break;
#endif
    case 'q':
      init_loaded = 1;
      break;
    default:
      errx(1, "unknown option: %s", argv[i]);
    }
  }

  if (! quit) {
    if (! init_loaded)
      sexp_load(sexp_c_string(sexp_init_file), env);
    if (i < argc)
      for ( ; i < argc; i++)
        sexp_load(sexp_c_string(argv[i]), env);
    else
      repl(context);
  }
}

int main (int argc, char **argv) {
  scheme_init();
  run_main(argc, argv);
  return 0;
}

