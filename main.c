
#include "eval.c"

void repl (sexp context) {
  sexp obj, tmp, res, env, in, out, err;
  env = sexp_context_env(context);
  sexp_context_tracep(context) = 1;
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
      tmp = sexp_env_bindings(env);
      res = eval_in_context(obj, context);
#ifdef USE_WARN_UNDEFS
      sexp_warn_undefs(sexp_env_bindings(env), tmp, err);
#endif
      if (res != SEXP_VOID) {
        sexp_write(res, out);
        sexp_write_char('\n', out);
      }
    }
  }
}

void run_main (int argc, char **argv) {
  sexp env, out=NULL, res, context, perr_cell, err_cell, err_handler;
  sexp_uint_t i, quit=0, init_loaded=0;

  env = sexp_make_standard_env(sexp_make_integer(5));
  env_define(env, the_interaction_env_symbol, env);
  out = env_global_ref(env, the_cur_out_symbol, SEXP_FALSE);
  err_cell = env_cell(env, the_cur_err_symbol);
  perr_cell = env_cell(env, sexp_intern("print-exception"));
  context = sexp_make_context(NULL, env);
  sexp_context_tailp(context) = 0;
  if (err_cell && perr_cell && sexp_opcodep(sexp_cdr(perr_cell))) {
    emit(OP_GLOBAL_KNOWN_REF, context);
    emit_word((sexp_uint_t)err_cell, context);
    emit(OP_LOCAL_REF, context);
    emit_word(0, context);
    emit(OP_FCALL2, context);
    emit_word((sexp_uint_t)sexp_opcode_data(sexp_cdr(perr_cell)), context);
  }
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
      if (! init_loaded++)
        sexp_load(sexp_c_string(sexp_init_file), env);
      res = sexp_read_from_string(argv[i+1]);
      if (! sexp_exceptionp(res))
        res = eval_in_context(res, context);
      if (sexp_exceptionp(res)) {
        sexp_print_exception(res, out);
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
        sexp_load(sexp_c_string(sexp_init_file), env);
      sexp_load(sexp_c_string(argv[++i]), env);
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

