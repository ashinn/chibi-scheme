
#define simplify_it(it) ((it) = simplify(ctx, it, substs, lambda))

static sexp simplify (sexp ctx, sexp ast, sexp init_substs, sexp lambda) {
  int check;
  sexp ls1, ls2, p1, p2, sv, ctx2;
  sexp_gc_var4(res, substs, tmp, app);
  sexp_gc_preserve4(ctx, res, substs, tmp, app);
  res = ast;                    /* return the ast as-is by default */
  substs = init_substs;

 loop:
  switch (sexp_pointerp(res) ? sexp_pointer_tag(res) : 0) {

  case SEXP_PAIR:
    /* don't simplify the operator if it's a lambda because we
       simplify that as a special case below, with the appropriate
       substs list */
    app = sexp_list1(ctx, sexp_lambdap(sexp_car(res)) ? sexp_car(res)
                     : (tmp=simplify(ctx, sexp_car(res), substs, lambda)));
    for (ls1=sexp_cdr(res); sexp_pairp(ls1); ls1=sexp_cdr(ls1))
      sexp_push(ctx, app, tmp=simplify(ctx, sexp_car(ls1), substs, lambda));
    app = sexp_nreverse(ctx, app);
    if (sexp_opcodep(sexp_car(app))) {
      if (sexp_opcode_class(sexp_car(app)) == OPC_ARITHMETIC) {
        for (check=1, ls1=sexp_cdr(app); sexp_pairp(ls1); ls1=sexp_cdr(ls1)) {
          if (sexp_pointerp(sexp_car(ls1)) && ! sexp_litp(sexp_car(ls1))) {
            check = 0;
            break;
          }
        }
        if (check) {
          ctx2 = sexp_make_eval_context(ctx, NULL, sexp_context_env(ctx));
          generate(ctx2, app);
          app = finalize_bytecode(ctx2);
          if (! sexp_exceptionp(app)) {
            tmp = sexp_make_vector(ctx2, 0, SEXP_VOID);
            app = sexp_make_procedure(ctx, SEXP_ZERO, SEXP_ZERO, app, tmp);
            if (! sexp_exceptionp(app))
              app = sexp_apply(ctx2, app, SEXP_NULL);
          }
        }
      }
    } else if (lambda && sexp_lambdap(sexp_car(app))) { /* let */
      p1 = NULL;
      p2 = sexp_lambda_params(sexp_car(app));
      ls1 = app;
      ls2 = sexp_cdr(app);
      sv = sexp_lambda_sv(sexp_car(app));
      for ( ; sexp_pairp(ls2); ls2=sexp_cdr(ls2), p2=sexp_cdr(p2)) {
        if (sexp_not(sexp_memq(ctx, sexp_car(p2), sv))
            && (! sexp_pointerp(sexp_car(ls2)) || sexp_litp(sexp_car(ls2))
                || (sexp_refp(sexp_car(ls2))
                    && sexp_lambdap(sexp_ref_loc(sexp_car(ls2)))))) {
          tmp = sexp_cons(ctx, sexp_car(app), sexp_car(ls2));
          tmp = sexp_cons(ctx, sexp_car(p2), tmp);
          sexp_push(ctx, substs, tmp);
          sexp_cdr(ls1) = sexp_cdr(ls2);
          if (p1)
            sexp_cdr(p1) = sexp_cdr(p2);
          else
            sexp_lambda_params(sexp_car(app)) = sexp_cdr(p2);
        } else {
          p1 = p2;
          ls1 = ls2;
        }
      }
      sexp_lambda_body(sexp_car(app))
        = simplify(ctx, sexp_lambda_body(sexp_car(app)), substs, sexp_car(app));
      if (sexp_nullp(sexp_cdr(app))
          && sexp_nullp(sexp_lambda_params(sexp_car(app)))
          && sexp_nullp(sexp_lambda_defs(sexp_car(app))))
        app = sexp_lambda_body(sexp_car(app));
    }
    res = app;
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
    app = SEXP_NULL;
    for (ls2=sexp_seq_ls(res); sexp_pairp(ls2); ls2=sexp_cdr(ls2)) {
      tmp = simplify(ctx, sexp_car(ls2), substs, lambda);
      if (! (sexp_pairp(sexp_cdr(ls2))
             && (sexp_litp(tmp) || ! sexp_pointerp(tmp) || sexp_refp(tmp)
                 || sexp_lambdap(tmp))))
        sexp_push(ctx, app, tmp);
    }
    if (sexp_pairp(app) && sexp_nullp(sexp_cdr(app)))
      res = sexp_car(app);
    else
      sexp_seq_ls(res) = sexp_nreverse(ctx, app);
    break;

  }

  sexp_gc_release4(ctx);
  return res;
}

sexp sexp_simplify (sexp ctx, sexp ast) {
  return simplify(ctx, ast, SEXP_NULL, NULL);
}

