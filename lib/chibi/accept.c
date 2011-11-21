
/* chibi-ffi should probably be able to detect these patterns automatically, */
/* but for now we manually check two special cases - accept should check for */
/* EWOULDBLOCK and block on the socket, and listen should automatically make */
/* sockets non-blocking. */

sexp sexp_accept (sexp ctx, sexp self, sexp arg0, sexp arg1, sexp arg2) {
  sexp f;
  socklen_t tmp;
  int res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  if (! (sexp_pointerp(arg1) && (sexp_pointer_tag(arg1) == sexp_type_tag(sexp_sockaddr_type_t))))
    return sexp_type_exception(ctx, self, sexp_type_tag(sexp_sockaddr_type_t), arg1);
  if (! sexp_exact_integerp(arg2))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg2);
  tmp = sexp_sint_value(arg2);
  res = accept(sexp_sint_value(arg0), (struct sockaddr*)sexp_cpointer_value(arg1), &tmp);
#if SEXP_USE_GREEN_THREADS
  if (res < 0 && errno == EWOULDBLOCK) {
    f = sexp_global(ctx, SEXP_G_THREADS_BLOCKER);
    if (sexp_opcodep(f)) {
      ((sexp_proc2)sexp_opcode_func(f))(ctx, f, 1, arg0);
      return sexp_global(ctx, SEXP_G_IO_BLOCK_ERROR);
    }
  }
#endif
  return sexp_make_integer(ctx, res);
}

/* If we're listening on a socket from Scheme, we most likely want it */
/* to be non-blocking. */

sexp sexp_listen (sexp ctx, sexp self, sexp arg0, sexp arg1) {
  int fd, res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  if (! sexp_exact_integerp(arg1))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg1);
  fd = sexp_sint_value(arg0);
  res = listen(fd, sexp_sint_value(arg1));
#if SEXP_USE_GREEN_THREADS
  if (res >= 0)
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
#endif
  return sexp_make_integer(ctx, res);
}
