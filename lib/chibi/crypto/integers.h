#ifndef CHIBI_CRYPTO_INTEGERS_H
#define CHIBI_CRYPTO_INTEGERS_H

#if __STDC_VERSION__ >= 199901L /* C99 */
# include <inttypes.h>
  typedef uint32_t sexp_uint32_t;
  typedef uint8_t  sexp_uint8_t;
#else
# include <limits.h>
#
# if UCHAR_MAX == 255
    typedef unsigned char sexp_uint8_t;
# else
#   error Could not find 8-bit type
# endif
#
# if UINT_MAX == 4294967295U
    typedef unsigned int sexp_uint32_t;
# elif ULONG_MAX == 4294967295UL
    typedef unsigned long sexp_uint32_t;
# elif USHRT_MAX == 4294967295U
    typedef unsigned short sexp_uint32_t;
# else
#   error Could not find 32-bit type
# endif
#endif

#endif /* CHIBI_CRYPTO_INTEGERS_H */
