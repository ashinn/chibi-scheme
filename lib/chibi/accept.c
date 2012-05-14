
/* chibi-ffi should probably be able to detect these patterns automatically, */
/* but for now we manually check two special cases - accept should check for */
/* EWOULDBLOCK and block on the socket, and listen should automatically make */
/* sockets non-blocking. */

sexp sexp_accept (sexp ctx, sexp self, int sock, struct sockaddr* addr, socklen_t len) {
#if SEXP_USE_GREEN_THREADS
  sexp f;
#endif
  int res;
  res = accept(sock, addr, &len);
#if SEXP_USE_GREEN_THREADS
  if (res < 0 && errno == EWOULDBLOCK) {
    f = sexp_global(ctx, SEXP_G_THREADS_BLOCKER);
    if (sexp_opcodep(f)) {
      ((sexp_proc2)sexp_opcode_func(f))(ctx, f, 1, sexp_make_fixnum(sock));
      return sexp_global(ctx, SEXP_G_IO_BLOCK_ERROR);
    }
  }
  if (res >= 0)
    fcntl(res, F_SETFL, fcntl(res, F_GETFL) | O_NONBLOCK);
#endif
  return sexp_make_fileno(ctx, sexp_make_fixnum(res), SEXP_FALSE);
}

/* If we're listening on a socket from Scheme, we most likely want it */
/* to be non-blocking. */

sexp sexp_listen (sexp ctx, sexp self, sexp fileno, sexp backlog) {
  int fd, res;
  sexp_assert_type(ctx, sexp_filenop, SEXP_FILENO, fileno);
  sexp_assert_type(ctx, sexp_fixnump, SEXP_FIXNUM, backlog);
  fd = sexp_fileno_fd(fileno);
  res = listen(fd, sexp_unbox_fixnum(backlog));
#if SEXP_USE_GREEN_THREADS
  if (res >= 0)
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
#endif
  return (res == 0) ? SEXP_TRUE : SEXP_FALSE;
}
