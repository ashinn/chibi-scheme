
#define simplify_it(it) it = simplify(ctx, it, substs, lambda)

static sexp simplify (sexp ctx, sexp ast, sexp init_substs, sexp lambda) {
  int check;
  sexp ls1, ls2, ctx2;
  sexp_gc_var3(res, substs, tmp);
  sexp_gc_preserve3(ctx, res, substs, tmp);
  res = ast;
  substs = init_substs;
 loop:
  switch (sexp_pointerp(res) ? sexp_pointer_tag(res) : 0) {
  case SEXP_PAIR:
    for (ls1=res; sexp_pairp(ls1); ls1=sexp_cdr(ls1))
      simplify_it(sexp_car(ls1));
    if (sexp_opcodep(sexp_car(res))) {
      if (sexp_opcode_class(sexp_car(res)) == OPC_ARITHMETIC) {
        for (check=1, ls1=sexp_cdr(res); sexp_pairp(ls1); ls1=sexp_cdr(ls1)) {
          if (sexp_pointerp(sexp_car(ls1)) && ! sexp_litp(sexp_car(ls1))) {
            check = 0;
            break;
          }
        }
        if (check) {
          ctx2 = sexp_make_eval_context(ctx, NULL, sexp_context_env(ctx));
          generate(ctx2, res);
          res = finalize_bytecode(ctx2);
          tmp = sexp_make_vector(ctx2, 0, SEXP_VOID);
          res = sexp_make_procedure(ctx, SEXP_ZERO, SEXP_ZERO, res, tmp);
          if (! sexp_exceptionp(res))
            res = sexp_apply(ctx2, res, SEXP_NULL);
        }
      }
    } else if (lambda && sexp_lambdap(sexp_car(res))) { /* let */
      if (sexp_nullp(sexp_cdr(res))
          && sexp_nullp(sexp_lambda_params(sexp_car(res)))
          && sexp_nullp(sexp_lambda_defs(sexp_car(res))))
        res = sexp_lambda_body(sexp_car(res));
    }
    break;
  case SEXP_LAMBDA:
    sexp_lambda_body(res) = simplify(ctx, sexp_lambda_body(res), substs, res);
    break;
  case SEXP_CND:
    tmp = simplify(ctx, sexp_cnd_test(res), substs, lambda);
    if (sexp_litp(tmp) || ! sexp_pointerp(tmp)) {
      res = sexp_not((sexp_litp(tmp) ? sexp_lit_value(tmp) : tmp))
        ? sexp_cnd_fail(res) : sexp_cnd_pass(res);
      goto loop;
    } else {
      sexp_cnd_test(res) = tmp;
      simplify_it(sexp_cnd_pass(res));
      simplify_it(sexp_cnd_fail(res));
    }
    break;
  case SEXP_REF:
    tmp = sexp_ref_name(res);
    for (ls1=substs; sexp_pairp(ls1); ls1=sexp_cdr(ls1))
      if ((sexp_caar(ls1) == tmp) && (sexp_cadar(ls1) == sexp_ref_loc(res))) {
        res = sexp_cddar(ls1);
        break;
      }
    break;
  case SEXP_SET:
    simplify_it(sexp_set_value(res));
    break;
  case SEXP_SEQ:
    ls1 = NULL;
    for (ls2=sexp_seq_ls(res); sexp_pairp(ls2); ls2=sexp_cdr(ls2)) {
      tmp = simplify(ctx, sexp_car(ls2), substs, lambda);
      if (sexp_pairp(sexp_cdr(ls2))
          && (sexp_litp(tmp) || ! sexp_pointerp(tmp) || sexp_refp(tmp)
              || sexp_lambdap(tmp))) {
        if (ls1)
          sexp_cdr(ls1) = sexp_cdr(ls2);
        else
          sexp_seq_ls(res) = sexp_cdr(ls2);
      } else {
        sexp_car(ls2) = tmp;
        ls1 = ls2;
      }
    }
    if (sexp_pairp(sexp_seq_ls(res)) && sexp_nullp(sexp_cdr(sexp_seq_ls(res))))
      res = sexp_car(sexp_seq_ls(res));
    break;
  case SEXP_SYMBOL:
    fprintf(stderr, "WARNING: raw symbol\n");
    break;
  }
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_simplify (sexp ctx, sexp ast) {
  return simplify(ctx, ast, SEXP_NULL, NULL);
}

