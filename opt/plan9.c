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

/**********************************************************************/
/* 9p interface */

typedef struct sexp_plan9_srv {
  sexp context, auth, attach, walk, walk1, clone, open, create, remove,
    read, write, stat, wstat, flush, destroyfid, destroyreq, end;
} *sexp_plan9_srv;

void sexp_build_srv (sexp ctx, sexp_plan9_srv s, sexp ls) {
  s->context = ctx;
  s->auth = s->attach = s->walk = s->walk1 = s->clone = s->open
    = s->create = s->remove = s->read = s->write = s->stat = s->wstat
    = s->flush = s->destroyfid = s->destroyreq = s->end = SEXP_FALSE;
  for ( ; sexp_pairp(ls) && sexp_pairp(sexp_cdr(ls)); ls=sexp_cddr(ls)) {
    if (sexp_car(ls) == sexp_intern(ctx, "auth:")) {
      s->auth = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "attach:")) {
      s->attach = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "walk:")) {
      s->walk = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "walk1:")) {
      s->walk1 = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "clone:")) {
      s->clone = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "open:")) {
      s->open = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "create:")) {
      s->create = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "remove:")) {
      s->remove = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "read:")) {
      s->read = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "write:")) {
      s->write = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "stat:")) {
      s->stat = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "wstat:")) {
      s->wstat = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "flush:")) {
      s->flush = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "destroyfid:")) {
      s->destroyfid = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "destroyreq:")) {
      s->destroyreq = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "end:")) {
      s->end = sexp_cadr(ls);
    }
  }
}

void sexp_run_9p_handler (Req *r, sexp handler) {
  sexp_plan9_srv s = (sexp_plan9_srv)r->srv->aux;
  sexp ctx = s->context;
  sexp_gc_var(ctx, ptr, s_ptr);
  sexp_gc_var(ctx, args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, r);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  sexp_apply(ctx, handler, args);
  sexp_gc_release(ctx, ptr, s_ptr);
}

#define sexp_def_9p_handler(name, field)                                \
  void name (Req *r) {                                                  \
    sexp_run_9p_handler(r, ((sexp_plan9_srv)r->srv->aux)->field);       \
  }

sexp_def_9p_handler(sexp_9p_auth, auth)
sexp_def_9p_handler(sexp_9p_attach, attach)
sexp_def_9p_handler(sexp_9p_walk, walk)
sexp_def_9p_handler(sexp_9p_open, open)
sexp_def_9p_handler(sexp_9p_create, create)
sexp_def_9p_handler(sexp_9p_remove, remove)
sexp_def_9p_handler(sexp_9p_read, read)
sexp_def_9p_handler(sexp_9p_write, write)
sexp_def_9p_handler(sexp_9p_stat, stat)
sexp_def_9p_handler(sexp_9p_wstat, wstat)
sexp_def_9p_handler(sexp_9p_flush, flush)

char* sexp_9p_walk1 (Fid *fid, char *name, Qid *qid) {
  sexp_plan9_srv s = (sexp_plan9_srv)fid->pool->srv->aux;
  sexp res, ctx = s->context;
  sexp_gc_var(ctx, ptr, s_ptr);
  sexp_gc_var(ctx, args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, qid);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  ptr = sexp_c_string(ctx, name, -1);
  args = sexp_cons(ctx, ptr, args);
  ptr = sexp_make_cpointer(ctx, fid);
  args = sexp_cons(ctx, ptr, args);
  res = sexp_apply(ctx, s->walk1, args);
  sexp_gc_release(ctx, ptr, s_ptr);
  return sexp_stringp(res) ? sexp_string_data(res) : nil;
}

char* sexp_9p_clone (Fid *oldfid, Fid *newfid) {
  sexp_plan9_srv s = (sexp_plan9_srv)oldfid->pool->srv->aux;
  sexp res, ctx = s->context;
  sexp_gc_var(ctx, ptr, s_ptr);
  sexp_gc_var(ctx, args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, oldfid);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  ptr = sexp_make_cpointer(ctx, newfid);
  args = sexp_cons(ctx, ptr, args);
  res = sexp_apply(ctx, s->clone, args);
  sexp_gc_release(ctx, ptr, s_ptr);
  return sexp_stringp(res) ? sexp_string_data(res) : nil;
}

void sexp_9p_destroyfid (Fid *fid) {
  sexp_plan9_srv s = (sexp_plan9_srv)fid->pool->srv->aux;
  sexp ctx = s->context;
  sexp_gc_var(ctx, ptr, s_ptr);
  sexp_gc_var(ctx, args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, fid);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  sexp_apply(ctx, s->destroyfid, args);
  sexp_gc_release(ctx, ptr, s_ptr);
}

void sexp_9p_destroyreq (Req *r) {
  sexp_plan9_srv s = (sexp_plan9_srv)r->srv->aux;
  sexp ctx = s->context;
  sexp_gc_var(ctx, ptr, s_ptr);
  sexp_gc_var(ctx, args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, r);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  sexp_apply(ctx, s->destroyreq, args);
  sexp_gc_release(ctx, ptr, s_ptr);
}

void sexp_9p_end (Srv *srv) {
  sexp_plan9_srv s = (sexp_plan9_srv)srv->aux;
  sexp ctx = s->context;
  sexp_gc_var(ctx, ptr, s_ptr);
  sexp_gc_var(ctx, args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, srv);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  sexp_apply(ctx, s->end, args);
  sexp_gc_release(ctx, ptr, s_ptr);
}

sexp sexp_postmountsrv (sexp ctx, sexp ls, sexp name, sexp mtpt, sexp flags) {
  Srv s;
  struct sexp_plan9_srv p9s;
  if (! sexp_listp(ctx, ls))
    return sexp_type_exception(ctx, "postmountsrv: not a list", ls);
  if (! sexp_stringp(name))
    return sexp_type_exception(ctx, "postmountsrv: not a string", name);
  if (! sexp_stringp(mtpt))
    return sexp_type_exception(ctx, "postmountsrv: not a string", mtpt);
  if (! sexp_integerp(flags))
    return sexp_type_exception(ctx, "postmountsrv: not an integer", flags);
  sexp_build_srv(ctx, &p9s, ls);
  s.aux = &p9s;
  s.auth = &sexp_9p_auth;
  s.attach = &sexp_9p_attach;
  s.walk = &sexp_9p_walk;
  s.walk1 = &sexp_9p_walk1;
  s.clone = &sexp_9p_clone;
  s.open = &sexp_9p_open;
  s.create = &sexp_9p_create;
  s.remove = &sexp_9p_remove;
  s.read = &sexp_9p_read;
  s.write = &sexp_9p_write;
  s.stat = &sexp_9p_stat;
  s.wstat = &sexp_9p_wstat;
  s.flush = &sexp_9p_flush;
  s.destroyfid = &sexp_9p_destroyfid;
  s.destroyreq = &sexp_9p_destroyreq;
  s.end = &sexp_9p_end;
  postmountsrv(&s, sexp_string_data(name), sexp_string_data(mtpt),
               sexp_unbox_integer(flags));
  return SEXP_UNDEF;
}

sexp sexp_9p_req_offset (sexp ctx, sexp req) {
  return sexp_make_integer(ctx, (Req*)sexp_cpointer_value(req)->ifcall.offset);
}

sexp sexp_9p_req_count (sexp ctx, sexp req) {
  return sexp_make_integer(ctx, (Req*)sexp_cpointer_value(req)->ifcall.count);
}

sexp sexp_9p_req_path (sexp ctx, sexp req) {
  return sexp_c_string(ctx, (Req*)sexp_cpointer_value(req)->fid.qid.path, -1);
}

sexp sexp_9p_req_fid (sexp ctx, sexp req) {
  return sexp_make_cpointer(ctx, (Req*)sexp_cpointer_value(req)->fid);
}

sexp sexp_9p_req_newfid (sexp ctx, sexp req) {
  return sexp_make_cpointer(ctx, (Req*)sexp_cpointer_value(req)->newfid);
}

sexp sexp_9p_respond (sexp ctx, sexp req, sexp err) {
  char *cerr = sexp_stringp(err) ? sexp_string_data(err) : nil;
  respond(sexp_cpointer_value(req), cerr);
  return SEXP_VOID;
}

sexp sexp_9p_responderror (sexp ctx, sexp req) {
  responderror(sexp_cpointer_value(req));
  return SEXP_VOID;
}

