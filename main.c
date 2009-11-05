/* main.c -- chibi-scheme command-line app              */
/* Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt  */

#ifdef PLAN9
#define file_exists_p(path, buf) (stat(path, buf, 128) >= 0)
#else
#include <sys/stat.h>
#define file_exists_p(path, buf) (! stat(path, buf))
#endif

#include "chibi/eval.h"

char *chibi_module_dir = NULL;

sexp find_module_file (sexp ctx, char *file) {
  sexp res;
  int mlen, flen;
  char *path;
#ifdef PLAN9
  unsigned char buf[128];
#else
  struct stat buf_str;
  struct stat *buf = &buf_str;
#endif

  if (file_exists_p(file, buf))
    return sexp_c_string(ctx, file, -1);
  if (! chibi_module_dir) {
#ifndef PLAN9
    chibi_module_dir = getenv("CHIBI_MODULE_DIR");
    if (! chibi_module_dir)
#endif
      chibi_module_dir = sexp_module_dir;
  }
  mlen = strlen(chibi_module_dir);
  flen = strlen(file);
  path = (char*) malloc(mlen+flen+2);
  memcpy(path, chibi_module_dir, mlen);
  path[mlen] = '/';
  memcpy(path+mlen+1, file, flen);
  path[mlen+flen+1] = '\0';
  if (file_exists_p(path, buf))
    res = sexp_c_string(ctx, path, mlen+flen+2);
  else
    res = SEXP_FALSE;
  free(path);
  return res;
}

sexp sexp_load_module_file (sexp ctx, char *file, sexp env) {
  sexp res = SEXP_VOID;
  sexp_gc_var2(path, irr);
  sexp_gc_preserve2(ctx, path, irr);
  path = find_module_file(ctx, file);
  if (! sexp_stringp(path)) {
    path = sexp_c_string(ctx, chibi_module_dir, -1);
    irr = sexp_cons(ctx, path, SEXP_NULL);
    path = sexp_c_string(ctx, file, -1);
    irr = sexp_cons(ctx, path, irr);
    res = sexp_user_exception(ctx,
                              SEXP_FALSE,
                              "couldn't find file to load in ./ or module dir",
                              irr);
  } else {
    res = sexp_load(ctx, path, env);
  }
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_init_environments (sexp ctx) {
  sexp res, env;
  sexp_gc_var1(confenv);
  env = sexp_context_env(ctx);
  res = sexp_load_module_file(ctx, sexp_init_file, env);
  if (! sexp_exceptionp(res)) {
    res = SEXP_UNDEF;
    sexp_gc_preserve1(ctx, confenv);
    confenv = sexp_make_env(ctx);
    sexp_env_copy(ctx, confenv, env, SEXP_FALSE);
    sexp_load_module_file(ctx, sexp_config_file, confenv);
    sexp_env_define(ctx, env, sexp_intern(ctx, "*config-env*"), confenv);
    sexp_env_define(ctx, confenv, sexp_intern(ctx, "*config-env*"), confenv);
    sexp_gc_release1(ctx);
  }
  return res;
}

void repl (sexp ctx) {
  sexp tmp, res, env, in, out, err;
  sexp_gc_var1(obj);
  sexp_gc_preserve1(ctx, obj);
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
#if USE_WARN_UNDEFS
      sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp, err);
#endif
      if (res != SEXP_VOID) {
        sexp_write(ctx, res, out);
        sexp_write_char(ctx, '\n', out);
      }
    }
  }
  sexp_gc_release1(ctx);
}

void run_main (int argc, char **argv) {
  sexp env, out=NULL, res=SEXP_VOID, ctx;
  sexp_uint_t i, quit=0, init_loaded=0;
  sexp_gc_var1(str);

  ctx = sexp_make_context(NULL, NULL, NULL);
  sexp_gc_preserve1(ctx, str);
  env = sexp_context_env(ctx);
  out = sexp_eval_string(ctx, "(current-output-port)", env);

  /* parse options */
  for (i=1; i < argc && argv[i][0] == '-'; i++) {
    switch (argv[i][1]) {
    case 'e':
    case 'p':
      if (! init_loaded++)
        sexp_init_environments(ctx);
      res = sexp_read_from_string(ctx, argv[i+1]);
      if (! sexp_exceptionp(res))
        res = sexp_eval(ctx, res, env);
      if (sexp_exceptionp(res)) {
        sexp_print_exception(ctx, res, out);
        quit = 1;
        break;
      } else if (argv[i][1] == 'p') {
        sexp_write(ctx, res, out);
        sexp_write_char(ctx, '\n', out);
      }
      quit=1;
      i++;
      break;
    case 'l':
      if (! init_loaded++)
        sexp_init_environments(ctx);
      sexp_load_module_file(ctx, argv[++i], env);
      break;
    case 'q':
      init_loaded = 1;
      break;
    case 'm':
      chibi_module_dir = argv[++i];
      break;
    default:
      errx(1, "unknown option: %s", argv[i]);
    }
  }

  if (! quit) {
    if (! init_loaded)
      res = sexp_init_environments(ctx);
    if (res && sexp_exceptionp(res))
      sexp_print_exception(ctx, res,
                           sexp_eval_string(ctx, "(current-error-port)", env));
    if (i < argc)
      for ( ; i < argc; i++)
        res = sexp_load(ctx, str=sexp_c_string(ctx, argv[i], -1), env);
    else
      repl(ctx);
  }

  sexp_gc_release1(ctx);
}

int main (int argc, char **argv) {
  sexp_scheme_init();
  run_main(argc, argv);
  return 0;
}

