/*  simplify.c -- basic simplification pass                   */
/*  Copyright (c) 2010-2013 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"

#if SEXP_USE_SIMPLIFY

#define simplify_it(it) ((it) = simplify(ctx, it, substs, lambda))

static sexp simplify (sexp ctx, sexp ast, sexp init_substs, sexp lambda) {
  int check;
  sexp ls1, ls2, p1, p2, sv;
  sexp_gc_var5(res, substs, tmp, app, ctx2);
  if (!sexp_pointerp(ast))
    return ast;
  sexp_gc_preserve5(ctx, res, substs, tmp, app, ctx2);
  res = ast;                    /* return the ast as-is by default */
  substs = init_substs;

  switch (sexp_pointer_tag(res)) {

  case SEXP_PAIR:
    /* don't simplify the operator if it's a lambda because we
       simplify that as a special case below, with the appropriate
       substs list */
    app = sexp_list1(ctx, sexp_lambdap(sexp_car(res)) ? sexp_car(res)
                     : (tmp=simplify(ctx, sexp_car(res), substs, lambda)));
    sexp_pair_source(app) = sexp_pair_source(res);
    for (ls1=sexp_cdr(res); sexp_pairp(ls1); ls1=sexp_cdr(ls1)) {
      sexp_push(ctx, app, tmp=simplify(ctx, sexp_car(ls1), substs, lambda));
      if (sexp_pairp(app)) sexp_pair_source(app) = sexp_pair_source(ls1);
    }
    app = sexp_nreverse(ctx, app);
    /* app now holds a copy of the list, and is the default result
       (res = app below) if we don't replace it with a simplification */
    if (sexp_opcodep(sexp_car(app))) {
      /* opcode app - right now we just constant fold arithmetic */
      if (sexp_opcode_class(sexp_car(app)) == SEXP_OPC_ARITHMETIC) {
        for (check=1, ls1=sexp_cdr(app); sexp_pairp(ls1); ls1=sexp_cdr(ls1)) {
          if (sexp_pointerp(sexp_car(ls1)) && ! sexp_litp(sexp_car(ls1))) {
            check = 0;
            break;
          }
        }
        if (check) {
          ctx2 = sexp_make_eval_context(ctx, NULL, sexp_context_env(ctx), 0, 0);
          sexp_generate(ctx2, 0, 0, 0, app);
          res = sexp_complete_bytecode(ctx2);
          if (! sexp_exceptionp(res)) {
            tmp = sexp_make_vector(ctx2, 0, SEXP_VOID);
            tmp = sexp_make_procedure(ctx2, SEXP_ZERO, SEXP_ZERO, res, tmp);
            if (! sexp_exceptionp(tmp)) {
              tmp = sexp_apply(ctx2, tmp, SEXP_NULL);
              if (! sexp_exceptionp(tmp))
                app = sexp_make_lit(ctx2, tmp);
            }
          }
        }
      }
    } else if (lambda && sexp_lambdap(sexp_car(app))) { /* let */
      p1 = NULL;
      p2 = sexp_lambda_params(sexp_car(app));
      ls1 = app;
      ls2 = sexp_cdr(app);
      sv = sexp_lambda_sv(sexp_car(app));
      /* inline constants and immutable references */
      if (sexp_length(ctx, p2) == sexp_length(ctx, ls2)) {
        for ( ; sexp_pairp(ls2); ls2=sexp_cdr(ls2), p2=sexp_cdr(p2)) {
          if (sexp_not(sexp_memq(ctx, sexp_car(p2), sv))
              && (! sexp_pointerp(sexp_car(ls2)) || sexp_litp(sexp_car(ls2))
                  /* disable inline references for now */
                  || (0 && sexp_refp(sexp_car(ls2))
                      && sexp_lambdap(sexp_ref_loc(sexp_car(ls2)))
                      && sexp_not(sexp_memq(ctx, sexp_ref_name(sexp_car(ls2)),
                                            sexp_lambda_sv(sexp_ref_loc(sexp_car(ls2)))))))) {
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
        /* TODO: Revisit this - it causes GC problems in rare cases. */
        /* if (sexp_nullp(sexp_cdr(app)) */
        /*     && sexp_nullp(sexp_lambda_params(sexp_car(app))) */
        /*     && sexp_nullp(sexp_lambda_defs(sexp_car(app)))) */
        /*   app = sexp_lambda_body(sexp_car(app)); */
      } else if (sexp_lambdap(sexp_car(app))) {
        sexp_lambda_body(sexp_car(app))
          = simplify(ctx, sexp_lambda_body(sexp_car(app)), substs, sexp_car(app));
      }
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
      res = simplify(ctx, res, substs, lambda);
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

  sexp_gc_release5(ctx);
  return res;
}

sexp sexp_simplify (sexp ctx, sexp self, sexp_sint_t n, sexp ast) {
  return simplify(ctx, ast, SEXP_NULL, NULL);
}

static int usedp (sexp lambda, sexp var, sexp x) {
  sexp ls;
 loop:
  switch (sexp_pointerp(x) ? sexp_pointer_tag(x) : 0) {
  case SEXP_REF:
    return sexp_ref_name(x) == var && sexp_ref_loc(x) == lambda;
  case SEXP_SET:
    x = sexp_set_value(x);
    goto loop;
  case SEXP_LAMBDA:
    x = sexp_lambda_body(x);
    goto loop;
  case SEXP_CND:
    if (usedp(lambda, var, sexp_cnd_test(x))
        || usedp(lambda, var, sexp_cnd_pass(x)))
      return 1;
    x = sexp_cnd_fail(x);
    goto loop;
  case SEXP_SEQ:
    x = sexp_seq_ls(x);
  case SEXP_PAIR:
    for (ls=x; sexp_pairp(ls); ls=sexp_cdr(ls))
      if (usedp(lambda, var, sexp_car(ls)))
        return 1;
  }
  return 0;
}

int sexp_rest_unused_p (sexp lambda) {
  sexp var;
  for (var=sexp_lambda_params(lambda); sexp_pairp(var); var=sexp_cdr(var))
    ;
  if (sexp_nullp(var)) return 0;
  return !usedp(lambda, var, sexp_lambda_body(lambda));
}

#endif
