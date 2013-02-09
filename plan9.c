/*  plan9.c -- extended Plan 9 system utils                   */
/*  Copyright (c) 2009-2012 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt       */

#include "chibi/eval.h"

sexp sexp_rand (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_make_fixnum(rand());
}

sexp sexp_srand (sexp ctx, sexp self, sexp_sint_t n, sexp seed) {
  srand(sexp_unbox_fixnum(seed));
  return SEXP_VOID;
}

sexp sexp_file_exists_p (sexp ctx, sexp self, sexp_sint_t n, sexp path) {
  int res;
  uchar statbuf[STATMAX];
  if (! sexp_stringp(path))
    return sexp_type_exception(ctx, self, SEXP_STRING, path);
  res = stat(sexp_string_data(path), statbuf, sizeof(statbuf));
  return (res < 0) ? SEXP_FALSE : SEXP_TRUE;
}

sexp sexp_fdopen (sexp ctx, sexp self, sexp_sint_t n, sexp fd, sexp mode) {
  FILE *f;
  if (! sexp_integerp(fd))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, fd);
  if (! sexp_stringp(mode))
    return sexp_type_exception(ctx, self, SEXP_STRING, mode);
  f = fdopen(sexp_unbox_fixnum(fd), sexp_string_data(mode));
  if (! f)
    return sexp_user_exception(ctx, SEXP_FALSE, "fdopen failed", fd);
  /* maybe use fd2path to get the name of the fd */
  if (sexp_string_data(mode)[0] == 'w')
    return sexp_make_output_port(ctx, f, SEXP_FALSE);
  else
    return sexp_make_input_port(ctx, f, SEXP_FALSE);
}

sexp sexp_fileno (sexp ctx, sexp self, sexp_sint_t n, sexp port) {
  if (! sexp_portp(port))
    return sexp_type_exception(ctx, self, SEXP_IPORT, port);
  return sexp_make_fixnum(fileno(sexp_port_stream(port)));
}

sexp sexp_fork (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_make_fixnum(fork());
}

sexp sexp_exec (sexp ctx, sexp self, sexp_sint_t n, sexp name, sexp args) {
  int i, len = sexp_unbox_fixnum(sexp_length(ctx, args));
  char **argv = malloc((len+1)*sizeof(char*));
  for (i=0; i<len; i++, args=sexp_cdr(args))
    argv[i] = sexp_string_data(sexp_car(args));
  argv[len] = NULL;
  exec(sexp_string_data(name), argv);
  return SEXP_VOID;             /* won't really return */
}

void sexp_exits (sexp ctx, sexp self, sexp_sint_t n, sexp msg) {
  exits(sexp_string_data(sexp_stringp(msg)
                         ? msg : sexp_write_to_string(ctx, msg)));
}

sexp sexp_dup (sexp ctx, sexp self, sexp_sint_t n, sexp oldfd, sexp newfd) {
  return sexp_make_fixnum(dup(sexp_unbox_fixnum(oldfd),
                               sexp_unbox_fixnum(newfd)));
}

sexp sexp_pipe (sexp ctx, sexp self, sexp_sint_t n) {
  int fds[2];
  pipe(fds);
  return sexp_list2(ctx, sexp_make_fixnum(fds[0]), sexp_make_fixnum(fds[1]));
}

sexp sexp_sleep (sexp ctx, sexp self, sexp_sint_t n, sexp msecs) {
  if (! sexp_integerp(msecs))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, msecs);
  sleep(sexp_unbox_fixnum(msecs));
  return SEXP_VOID;
}

sexp sexp_getenv (sexp ctx, sexp self, sexp_sint_t n, sexp name) {
  char *value;
  if (! sexp_stringp(name))
    return sexp_type_exception(ctx, self, SEXP_STRING, name);
  value = getenv(sexp_string_data(name));
  return ((! value) ? SEXP_FALSE : sexp_c_string(ctx, value, -1));
}

sexp sexp_getwd (sexp ctx, sexp self, sexp_sint_t n) {
  char buf[512];
  getwd(buf, 512);
  return sexp_c_string(ctx, buf, -1);
}

sexp sexp_chdir (sexp ctx, sexp self, sexp_sint_t n, sexp path) {
  if (! sexp_stringp(path))
    return sexp_type_exception(ctx, self, SEXP_STRING, path);
  chdir(sexp_string_data(path));
  return SEXP_VOID;
}

sexp sexp_getuser (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_c_string(ctx, getuser(), -1);
}

sexp sexp_sysname (sexp ctx, sexp self, sexp_sint_t n) {
  return sexp_c_string(ctx, sysname(), -1);
}

sexp sexp_wait (sexp ctx, sexp self, sexp_sint_t n) { /* just return (pid msg) */
  Waitmsg *wmsg;
  sexp res;
  sexp_gc_var(msg, s_msg);
  sexp_gc_preserve(ctx, msg, s_msg);
  wmsg = wait();
  msg = sexp_c_string(ctx, wmsg->msg, -1);
  res = sexp_list2(ctx, sexp_make_fixnum(wmsg->pid), msg);
  sexp_gc_release(ctx, msg, s_msg);
  return res;
}

sexp sexp_postnote (sexp ctx, sexp self, sexp_sint_t n, sexp pid, sexp note) {
  if (! sexp_integerp(pid))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, pid);
  if (! sexp_stringp(note))
    return sexp_type_exception(ctx, self, SEXP_STRING, note);
  postnote(PNPROC, sexp_unbox_fixnum(pid), sexp_string_data(note));
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
    if (sexp_car(ls) == sexp_intern(ctx, "auth:", -1)) {
      s->auth = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "attach:", -1)) {
      s->attach = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "walk:", -1)) {
      s->walk = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "walk1:", -1)) {
      s->walk1 = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "clone:", -1)) {
      s->clone = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "open:", -1)) {
      s->open = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "create:", -1)) {
      s->create = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "remove:", -1)) {
      s->remove = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "read:", -1)) {
      s->read = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "write:", -1)) {
      s->write = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "stat:", -1)) {
      s->stat = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "wstat:", -1)) {
      s->wstat = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "flush:", -1)) {
      s->flush = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "destroyfid:", -1)) {
      s->destroyfid = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "destroyreq:", -1)) {
      s->destroyreq = sexp_cadr(ls);
    } else if (sexp_car(ls) == sexp_intern(ctx, "end:", -1)) {
      s->end = sexp_cadr(ls);
    }
  }
}

void sexp_run_9p_handler (Req *r, sexp handler) {
  sexp_plan9_srv s = (sexp_plan9_srv)r->srv->aux;
  sexp ctx = s->context;
  sexp_gc_var(ptr, s_ptr);
  sexp_gc_var(args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, r, SEXP_FALSE, 0);
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
  sexp_gc_var(ptr, s_ptr);
  sexp_gc_var(args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, qid, SEXP_FALSE, 0);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  ptr = sexp_c_string(ctx, name, -1);
  args = sexp_cons(ctx, ptr, args);
  ptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, fid, SEXP_FALSE, 0);
  args = sexp_cons(ctx, ptr, args);
  res = sexp_apply(ctx, s->walk1, args);
  sexp_gc_release(ctx, ptr, s_ptr);
  return sexp_stringp(res) ? sexp_string_data(res) : nil;
}

char* sexp_9p_clone (Fid *oldfid, Fid *newfid) {
  sexp_plan9_srv s = (sexp_plan9_srv)oldfid->pool->srv->aux;
  sexp res, ctx = s->context;
  sexp_gc_var(ptr, s_ptr);
  sexp_gc_var(args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, oldfid, SEXP_FALSE, 0);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  ptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, newfid, SEXP_FALSE, 0);
  args = sexp_cons(ctx, ptr, args);
  res = sexp_apply(ctx, s->clone, args);
  sexp_gc_release(ctx, ptr, s_ptr);
  return sexp_stringp(res) ? sexp_string_data(res) : nil;
}

void sexp_9p_destroyfid (Fid *fid) {
  sexp_plan9_srv s = (sexp_plan9_srv)fid->pool->srv->aux;
  sexp ctx = s->context;
  sexp_gc_var(ptr, s_ptr);
  sexp_gc_var(args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, fid, SEXP_FALSE, 0);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  sexp_apply(ctx, s->destroyfid, args);
  sexp_gc_release(ctx, ptr, s_ptr);
}

void sexp_9p_destroyreq (Req *r) {
  sexp_plan9_srv s = (sexp_plan9_srv)r->srv->aux;
  sexp ctx = s->context;
  sexp_gc_var(ptr, s_ptr);
  sexp_gc_var(args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, r, SEXP_FALSE, 0);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  sexp_apply(ctx, s->destroyreq, args);
  sexp_gc_release(ctx, ptr, s_ptr);
}

void sexp_9p_end (Srv *srv) {
  sexp_plan9_srv s = (sexp_plan9_srv)srv->aux;
  sexp ctx = s->context;
  sexp_gc_var(ptr, s_ptr);
  sexp_gc_var(args, s_args);
  sexp_gc_preserve(ctx, ptr, s_ptr);
  sexp_gc_preserve(ctx, args, s_args);
  ptr = sexp_make_cpointer(ctx, SEXP_CPOINTER, srv, SEXP_FALSE, 0);
  args = sexp_cons(ctx, ptr, SEXP_NULL);
  sexp_apply(ctx, s->end, args);
  sexp_gc_release(ctx, ptr, s_ptr);
}

sexp sexp_postmountsrv (sexp ctx, sexp self, sexp_sint_t n, sexp ls, sexp name, sexp mtpt, sexp flags) {
  Srv s;
  struct sexp_plan9_srv p9s;
  if (! sexp_listp(ctx, ls))
    return sexp_type_exception(ctx, self, SEXP_PAIR, ls);
  if (! sexp_stringp(name))
    return sexp_type_exception(ctx, self, SEXP_STRING, name);
  if (! sexp_stringp(mtpt))
    return sexp_type_exception(ctx, self, SEXP_STRING, mtpt);
  if (! sexp_integerp(flags))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, flags);
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
               sexp_unbox_fixnum(flags));
  return SEXP_UNDEF;
}

sexp sexp_9p_req_offset (sexp ctx, sexp self, sexp_sint_t n, sexp req) {
  return sexp_make_integer(ctx, ((Req*)sexp_cpointer_value(req))->ifcall.offset);
}

sexp sexp_9p_req_count (sexp ctx, sexp self, sexp_sint_t n, sexp req) {
  return sexp_make_integer(ctx, ((Req*)sexp_cpointer_value(req))->ifcall.count);
}

#if 0
sexp sexp_9p_req_path (sexp ctx, sexp self, sexp_sint_t n, sexp req) {
  return sexp_c_string(ctx, ((Req*)sexp_cpointer_value(req))->fid->qid.path, -1);
}
#endif

sexp sexp_9p_req_fid (sexp ctx, sexp self, sexp_sint_t n, sexp req) {
  return sexp_make_cpointer(ctx, SEXP_CPOINTER, ((Req*)sexp_cpointer_value(req))->fid, SEXP_FALSE, 0);
}

sexp sexp_9p_req_newfid (sexp ctx, sexp self, sexp_sint_t n, sexp req) {
  return sexp_make_cpointer(ctx, SEXP_CPOINTER, ((Req*)sexp_cpointer_value(req))->newfid, SEXP_FALSE, 0);
}

sexp sexp_9p_respond (sexp ctx, sexp self, sexp_sint_t n, sexp req, sexp err) {
  char *cerr = sexp_stringp(err) ? sexp_string_data(err) : nil;
  respond(sexp_cpointer_value(req), cerr);
  return SEXP_VOID;
}

sexp sexp_9p_responderror (sexp ctx, sexp self, sexp_sint_t n, sexp req) {
  responderror(sexp_cpointer_value(req));
  return SEXP_VOID;
}

