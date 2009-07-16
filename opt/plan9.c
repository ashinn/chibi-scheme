/*  plan9.c -- extended Plan 9 system utils              */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

sexp sexp_rand (sexp ctx) {
  return sexp_make_integer(rand());
}

sexp sexp_srand (sexp ctx, sexp seed) {
  srand(sexp_unbox_integer(seed));
  return SEXP_VOID;
}

sexp sexp_fdopen (sexp ctx, sexp fd, sexp mode) {
  FILE *f;
  if (! sexp_integerp(fd))
    return sexp_type_exception(ctx, "fdopen: not an integer", fd);
  if (! sexp_stringp(mode))
    return sexp_type_exception(ctx, "fdopen: not a mode string", mode);
  f = fdopen(sexp_unbox_integer(fd), sexp_string_data(mode));
  if (! f)
    return sexp_user_exception(ctx, SEXP_FALSE, "fdopen failed", fd);
  /* maybe use fd2path to get the name of the fd */
  if (sexp_string_data(mode)[0] == 'w')
    return sexp_make_output_port(ctx, f, SEXP_FALSE);
  else
    return sexp_make_input_port(ctx, f, SEXP_FALSE);
}

sexp sexp_fileno (sexp ctx, sexp port) {
  if (! sexp_portp(port))
    return sexp_type_exception(ctx, "fileno: not a port", port);
  return sexp_make_integer(fileno(sexp_port_stream(port)));
}

sexp sexp_fork (sexp ctx) {
  return sexp_make_integer(fork());
}

sexp sexp_exec (sexp ctx, sexp name, sexp args) {
  int i, len = sexp_unbox_integer(sexp_length(ctx, args));
  char **argv = malloc((len+1)*sizeof(char*));
  for (i=0; i<len; i++, args=sexp_cdr(args))
    argv[i] = sexp_string_data(sexp_car(args));
  argv[len] = NULL;
  exec(sexp_string_data(name), argv);
  return SEXP_VOID;             /* won't really return */
}

void sexp_exits (sexp ctx, sexp msg) {
  exits(sexp_string_data(sexp_stringp(msg)
                         ? msg : sexp_write_to_string(ctx, msg)));
}

sexp sexp_dup (sexp ctx, sexp oldfd, sexp newfd) {
  return sexp_make_integer(dup(sexp_unbox_integer(oldfd),
                               sexp_unbox_integer(newfd)));
}

sexp sexp_pipe (sexp ctx) {
  int fds[2];
  pipe(fds);
  return sexp_list2(ctx, sexp_make_integer(fds[0]), sexp_make_integer(fds[1]));
}

sexp sexp_sleep (sexp ctx, sexp msecs) {
  if (! sexp_integerp(msecs))
    return sexp_type_exception(ctx, "sleep: not an integer", msecs);
  sleep(sexp_unbox_integer(msecs));
  return SEXP_VOID;
}

sexp sexp_getenv (sexp ctx, sexp name) {
  char *value;
  if (! sexp_stringp(name))
    return sexp_type_exception(ctx, "getenv: not a string", name);
  value = getenv(sexp_string_data(name));
  return ((! value) ? SEXP_FALSE : sexp_c_string(ctx, value, -1));
}

sexp sexp_getwd (sexp ctx) {
  char buf[512];
  getwd(buf, 512);
  return sexp_c_string(ctx, buf, -1);
}

sexp sexp_chdir (sexp ctx, sexp path) {
  if (! sexp_stringp(path))
    return sexp_type_exception(ctx, "chdir: not a string", path);
  chdir(sexp_string_data(path));
  return SEXP_VOID;
}

sexp sexp_getuser (sexp ctx) {
  return sexp_c_string(ctx, getuser(), -1);
}

sexp sexp_sysname (sexp ctx) {
  return sexp_c_string(ctx, sysname(), -1);
}

sexp sexp_wait (sexp ctx) {     /* just return (pid msg) */
  Waitmsg *wmsg;
  sexp res;
  sexp_gc_var(ctx, msg, s_msg);
  sexp_gc_preserve(ctx, msg, s_msg);
  wmsg = wait();
  msg = sexp_c_string(ctx, wmsg->msg, -1);
  res = sexp_list2(ctx, sexp_make_integer(wmsg->pid), msg);
  sexp_gc_release(ctx, msg, s_msg);
  return res;
}

sexp sexp_postnote (sexp ctx, sexp pid, sexp note) {
  if (! sexp_integerp(pid))
    return sexp_type_exception(ctx, "postnote: not an integer", pid);
  if (! sexp_stringp(note))
    return sexp_type_exception(ctx, "postnote: not a string", note);
  postnote(PNPROC, sexp_unbox_integer(pid), sexp_string_data(note));
  return SEXP_VOID;
}

