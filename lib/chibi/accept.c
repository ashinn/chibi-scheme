
/* chibi-ffi should probably be able to detect these patterns automatically, */
/* but for now we manually check two special cases - accept should check for */
/* EWOULDBLOCK and block on the socket, and listen should automatically make */
/* sockets non-blocking. */

sexp sexp_make_fileno_sock(sexp ctx, sexp self, sexp_sint_t fd, SOCKET_TYPE sock) {
  sexp res = sexp_make_fileno(ctx, sexp_make_fixnum(fd), SEXP_FALSE);
  sexp_fileno_sock(res) = sock;
  return res;
}

sexp sexp_socket(sexp ctx, sexp self, int af, int type, int protocol) {
#ifdef _WIN32
  SOCKET_TYPE sock = WSASocket(af, type, protocol, NULL, 0, 0);
  int fd = _open_osfhandle(sock, _O_RDWR | _O_BINARY);
#else
  SOCKET_TYPE sock = socket(af, type, protocol);
  int fd = sock;
#endif  
  return sexp_make_fileno_sock(ctx, self, fd, sock);
}

sexp sexp_accept(sexp ctx, sexp self, SOCKET_TYPE sock, struct sockaddr* addr, socklen_t len) {
#if SEXP_USE_GREEN_THREADS
  sexp f;
#endif
  SOCKET_TYPE res = accept(sock, addr, &len);
#if SEXP_USE_GREEN_THREADS
  if (res < 0 && errno == EWOULDBLOCK) {
    f = sexp_global(ctx, SEXP_G_THREADS_BLOCKER);
    if (sexp_applicablep(f)) {
      sexp_apply2(ctx, f, sexp_make_fixnum(sock), SEXP_FALSE);
      return sexp_global(ctx, SEXP_G_IO_BLOCK_ERROR);
    }
  }
#ifdef _WIN32
  if (res >= 0)
    ioctlsocket(res, FIONBIO, &mode);
#else
  if (res >= 0)
    fcntl(res, F_SETFL, fcntl(res, F_GETFL) | O_NONBLOCK);
#endif
#endif
#ifdef _WIN32
  int fd = _open_osfhandle(res, _O_RDWR | _O_BINARY);
#else
  int fd = res;
  int i = errno;
#endif 
  return sexp_make_fileno_sock(ctx, self, fd, res);
}

/* likewise sendto and recvfrom should suspend the thread gracefully */

#define sexp_zerop(x) ((x) == SEXP_ZERO || (sexp_flonump(x) && sexp_flonum_value(x) == 0.0))

sexp sexp_sendto (sexp ctx, sexp self, SOCKET_TYPE sock, const void* buffer, size_t len, int flags, struct sockaddr* addr, socklen_t addr_len, sexp timeout) {
#if SEXP_USE_GREEN_THREADS
  sexp f;
#endif
  ssize_t res;
  res = sendto(sock, buffer, len, flags, addr, addr_len);
#if SEXP_USE_GREEN_THREADS
  if (res < 0 && errno == EWOULDBLOCK && !sexp_zerop(timeout)) {
    f = sexp_global(ctx, SEXP_G_THREADS_BLOCKER);
    if (sexp_applicablep(f)) {
      sexp_apply2(ctx, f, sexp_make_fixnum(sock), timeout);
      return sexp_global(ctx, SEXP_G_IO_BLOCK_ONCE_ERROR);
    }
  }
#endif
  return sexp_make_fixnum(res);
}

sexp sexp_recvfrom (sexp ctx, sexp self, SOCKET_TYPE sock, void* buffer, size_t len, int flags, struct sockaddr* addr, socklen_t addr_len, sexp timeout) {
#if SEXP_USE_GREEN_THREADS
  sexp f;
#endif
  ssize_t res;
  res = recvfrom(sock, buffer, len, flags, addr, &addr_len);
#if SEXP_USE_GREEN_THREADS
  if (res < 0 && errno == EWOULDBLOCK && !sexp_zerop(timeout)) {
    f = sexp_global(ctx, SEXP_G_THREADS_BLOCKER);
    if (sexp_applicablep(f)) {
      sexp_apply2(ctx, f, sexp_make_fixnum(sock), timeout);
      return sexp_global(ctx, SEXP_G_IO_BLOCK_ONCE_ERROR);
    }
  }
#endif
  return sexp_make_fixnum(res);
}

/* If we're binding or listening on a socket from Scheme, we most */
/* likely want it to be non-blocking. */

sexp sexp_bind (sexp ctx, sexp self, SOCKET_TYPE fd, struct sockaddr* addr, socklen_t addr_len) {
  int res = bind(fd, addr, addr_len);
#if SEXP_USE_GREEN_THREADS
#ifdef _WIN32
  if (res >= 0)
    ioctlsocket(fd, FIONBIO, &mode);
#else
  if (res >= 0)
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
#endif
#endif
  return (res == 0) ? SEXP_TRUE : SEXP_FALSE;
}

sexp sexp_listen (sexp ctx, sexp self, SOCKET_TYPE fd, int backlog) {
  int res;
  unsigned long mode = 1; 
  res = listen(fd, backlog);
#if SEXP_USE_GREEN_THREADS
#ifdef _WIN32
  if (res >= 0)
    ioctlsocket(fd, FIONBIO, &mode);
#else
  if (res >= 0)
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
#endif
#endif
  return (res == 0) ? SEXP_TRUE : SEXP_FALSE;
}

/* Additional utilities. */

sexp sexp_sockaddr_name (sexp ctx, sexp self, struct sockaddr* addr) {
  char buf[INET6_ADDRSTRLEN];
  /* struct sockaddr_in *sa = (struct sockaddr_in *)addr; */
  /* unsigned char *ptr = (unsigned char *)&(sa->sin_addr); */
  /* snprintf(buf, sizeof(buf), "%d.%d.%d.%d", ptr[0], ptr[1], ptr[2], ptr[3]); */
  inet_ntop(addr->sa_family,
            (addr->sa_family == AF_INET6 ?
             (void*)(&(((struct sockaddr_in6 *)addr)->sin6_addr)) :
             (void*)(&(((struct sockaddr_in *)addr)->sin_addr))),
            buf, INET6_ADDRSTRLEN);
  return sexp_c_string(ctx, buf, -1);
}

int sexp_sockaddr_port (sexp ctx, sexp self, struct sockaddr* addr) {
  struct sockaddr_in *sa = (struct sockaddr_in *)addr;
  return ntohs(sa->sin_port);
}

int sexp_setsockopt(SOCKET_TYPE socket, int level, int option_name, int *option_value, socklen_t option_len) {
#ifdef _WIN32
  return setsockopt(socket, level, option_name, (const char*)option_value, option_len);
#else
  return setsockopt(socket, level, option_name, (const void*)option_value, option_len);
#endif
}

int sexp_getsockopt(SOCKET_TYPE socket, int level, int option_name, int *option_value, socklen_t *option_len) {
#ifdef _WIN32
  return getsockopt(socket, level, option_name, (char*)option_value, option_len);
#else
  return getsockopt(socket, level, option_name, (void*)option_value, option_len);
#endif
}

