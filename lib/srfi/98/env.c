/* env.c -- SRFI-98 environment interface                    */
/* Copyright (c) 2009-2012 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt       */

#ifdef __APPLE__
#include <crt_externs.h>
#define environ (*_NSGetEnviron())
#else
#ifndef PLAN9
extern char **environ;
#endif
#endif

#include <chibi/eval.h>

sexp sexp_get_environment_variable (sexp ctx, sexp self, sexp_sint_t n, sexp str) {
  char *cstr;
  if (! sexp_stringp(str))
    return sexp_type_exception(ctx, self, SEXP_STRING, str);
  cstr = getenv(sexp_string_data(str));
  return cstr ? sexp_c_string(ctx, cstr, -1) : SEXP_FALSE;
}

sexp sexp_get_environment_variables (sexp ctx, sexp self, sexp_sint_t n) {
  int i;
  char **env, *cname, *cval;
  sexp_gc_var3(res, name, val);
  sexp_gc_preserve3(ctx, res, name, val);
  res = SEXP_NULL;
#ifndef PLAN9
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
#endif
  sexp_gc_release3(ctx);
  return res;
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return sexp_global(ctx, SEXP_G_ABI_ERROR);
  sexp_define_foreign(ctx, env, "get-environment-variable", 1, sexp_get_environment_variable);
  sexp_define_foreign(ctx, env, "get-environment-variables", 0, sexp_get_environment_variables);
  return SEXP_VOID;
}

