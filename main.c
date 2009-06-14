
#include "eval.c"

void repl (sexp ctx) {
  sexp tmp, res, env, in, out, err;
  sexp_gc_var(ctx, obj, s_obj);
  sexp_gc_preserve(ctx, obj, s_obj);
  env = sexp_context_env(ctx);
  sexp_context_tracep(ctx) = 1;
  in = env_global_ref(env, the_cur_in_symbol, SEXP_FALSE);
  out = env_global_ref(env, the_cur_out_symbol, SEXP_FALSE);
  err = env_global_ref(env, the_cur_err_symbol, SEXP_FALSE);
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
      res = eval_in_context(ctx, obj);
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
  sexp env, out=NULL, res, ctx, perr_cell, err_cell, err_handler;
  sexp_uint_t i, quit=0, init_loaded=0;
  sexp_gc_var(ctx, str, s_str);

  ctx = sexp_make_context(NULL, NULL, NULL);
  sexp_gc_preserve(ctx, str, s_str);
  env = sexp_context_env(ctx);
  env_define(ctx, env, the_interaction_env_symbol, env);
  out = env_global_ref(env, the_cur_out_symbol, SEXP_FALSE);
  err_cell = env_cell(env, the_cur_err_symbol);
  perr_cell = env_cell(env, sexp_intern(ctx, "print-exception"));
  sexp_context_tailp(ctx) = 0;
  if (err_cell && perr_cell && sexp_opcodep(sexp_cdr(perr_cell))) {
    emit(ctx, OP_GLOBAL_KNOWN_REF);
    emit_word(ctx, (sexp_uint_t)err_cell);
    emit(ctx, OP_LOCAL_REF);
    emit_word(ctx, 0);
    emit(ctx, OP_FCALL2);
    emit_word(ctx, (sexp_uint_t)sexp_opcode_data(sexp_cdr(perr_cell)));
  }
  emit_push(ctx, SEXP_VOID);
  emit(ctx, OP_DONE);
  err_handler = sexp_make_procedure(ctx,
                                    sexp_make_integer(0),
                                    sexp_make_integer(0),
                                    finalize_bytecode(ctx),
                                    sexp_make_vector(ctx, 0, SEXP_VOID));
  env_define(ctx, env, the_err_handler_symbol, err_handler);

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
        res = eval_in_context(ctx, res);
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
  scheme_init();
  run_main(argc, argv);
  return 0;
}

