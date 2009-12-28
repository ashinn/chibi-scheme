/*  env.c -- SRFI-98 environment interface               */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#ifdef __APPLE__
#include <crt_externs.h>
#define environ (*_NSGetEnviron())
#else
extern char **environ;
#endif

#include <chibi/eval.h>

sexp sexp_get_environment_variable (sexp ctx, sexp str) {
  char *cstr;
  if (! sexp_stringp(str))
    return sexp_type_exception(ctx, "get-environment-variable: not a string", str);
  cstr = getenv(sexp_string_data(str));
  return cstr ? sexp_c_string(ctx, cstr, -1) : SEXP_FALSE;
}

sexp sexp_get_environment_variables (sexp ctx) {
  int i;
  char **env, *cname, *cval;
  sexp_gc_var3(res, name, val);
  sexp_gc_preserve3(ctx, res, name, val);
  res = SEXP_NULL;
  env = environ;
  for (i=0; env[i]; i++) {
    cname = env[i];
    cval = strchr(cname, '=');
    if (cval) {
      name = sexp_c_string(ctx, cname, cval-cname);
      val = sexp_c_string(ctx, cval+1, -1);
      val = sexp_cons(ctx, name, val);
      res = sexp_cons(ctx, val, res);
    }
  }
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_init_library (sexp ctx, sexp env) {
  sexp_define_foreign(ctx, env, "get-environment-variable", 1, sexp_get_environment_variable);
  sexp_define_foreign(ctx, env, "get-environment-variables", 0, sexp_get_environment_variables);
  return SEXP_VOID;
}

