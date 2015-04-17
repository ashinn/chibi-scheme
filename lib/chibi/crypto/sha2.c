/* sha2.c -- SHA-2 digest algorithm implementation           */
/* Copyright (c) 2015 Alexei Lozovsky.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt       */

#include "integers.h"

/*
 * SHA-2 algorithms are described in RFC 6234:
 *
 *    http://tools.ietf.org/html/rfc6234
 */

/* Initial hash vector for SHA-224 */
static const sexp_uint32_t h224[8] = {
  0xC1059ED8UL, 0x367CD507UL, 0x3070DD17UL, 0xF70E5939UL,
  0xFFC00B31UL, 0x68581511UL, 0x64F98FA7UL, 0xBEFA4FA4UL,
};

/* Initial hash vector for SHA-256 */
static const sexp_uint32_t h256[8] = {
  0x6A09E667UL, 0xBB67AE85UL, 0x3C6EF372UL, 0xA54FF53AUL,
  0x510E527FUL, 0x9B05688CUL, 0x1F83D9ABUL, 0x5BE0CD19UL,
};

/* Round constants for SHA-224/256 */
static const sexp_uint32_t k256[64] = {
  0x428A2F98UL, 0x71374491UL, 0xB5C0FBCFUL, 0xE9B5DBA5UL,
  0x3956C25BUL, 0x59F111F1UL, 0x923F82A4UL, 0xAB1C5ED5UL,
  0xD807AA98UL, 0x12835B01UL, 0x243185BEUL, 0x550C7DC3UL,
  0x72BE5D74UL, 0x80DEB1FEUL, 0x9BDC06A7UL, 0xC19BF174UL,
  0xE49B69C1UL, 0xEFBE4786UL, 0x0FC19DC6UL, 0x240CA1CCUL,
  0x2DE92C6FUL, 0x4A7484AAUL, 0x5CB0A9DCUL, 0x76F988DAUL,
  0x983E5152UL, 0xA831C66DUL, 0xB00327C8UL, 0xBF597FC7UL,
  0xC6E00BF3UL, 0xD5A79147UL, 0x06CA6351UL, 0x14292967UL,
  0x27B70A85UL, 0x2E1B2138UL, 0x4D2C6DFCUL, 0x53380D13UL,
  0x650A7354UL, 0x766A0ABBUL, 0x81C2C92EUL, 0x92722C85UL,
  0xA2BFE8A1UL, 0xA81A664BUL, 0xC24B8B70UL, 0xC76C51A3UL,
  0xD192E819UL, 0xD6990624UL, 0xF40E3585UL, 0x106AA070UL,
  0x19A4C116UL, 0x1E376C08UL, 0x2748774CUL, 0x34B0BCB5UL,
  0x391C0CB3UL, 0x4ED8AA4AUL, 0x5B9CCA4FUL, 0x682E6FF3UL,
  0x748F82EEUL, 0x78A5636FUL, 0x84C87814UL, 0x8CC70208UL,
  0x90BEFFFAUL, 0xA4506CEBUL, 0xBEF9A3F7UL, 0xC67178F2UL,
};

/* = SHA-224/256 implementation ===================================== */

#define ror32(v, a) (((v) >> (a)) | ((v) << (32 - (a))))

static void sha_224_256_round (const sexp_uint8_t chunk[64],
                               sexp_uint32_t hash[8]) {
  int i;
  sexp_uint32_t w[64];
  sexp_uint32_t tmp1, tmp2;
  sexp_uint32_t a, b, c, d, e, f, g, h;
  /* Initialize schedule array */
  for (i = 0; i < 16; i++) {
    w[i] = (chunk[4*i + 0] << 24)
         | (chunk[4*i + 1] << 16)
         | (chunk[4*i + 2] <<  8)
         | (chunk[4*i + 3] <<  0);
  }
  for (i = 16; i < 64; i++) {
    w[i] = w[i - 16]
         + (ror32(w[i-15], 7) ^ ror32(w[i-15], 18) ^ (w[i-15] >> 3))
         + w[i - 7]
         + (ror32(w[i-2], 17) ^ ror32(w[i-2], 19) ^ (w[i-2] >> 10));
  }
  /* Initialize working variables */
  a = hash[0]; b = hash[1]; c = hash[2]; d = hash[3];
  e = hash[4]; f = hash[5]; g = hash[6]; h = hash[7];
  /* Main loop */
  for (i = 0; i < 64; i++) {
    tmp1 = h
         + (ror32(e, 6) ^ ror32(e, 11) ^ ror32(e, 25))
         + ((e & f) ^ ((~e) & g))
         + k256[i]
         + w[i];
    tmp2 = (ror32(a, 2) ^ ror32(a, 13) ^ ror32(a, 22))
         + ((a & b) ^ (a & c) ^ (b & c));
    h = g; g = f; f = e; e = d + tmp1;
    d = c; c = b; b = a; a = tmp1 + tmp2;
  }
  /* Update hash values */
  hash[0] += a; hash[1] += b; hash[2] += c; hash[3] += d;
  hash[4] += e; hash[5] += f; hash[6] += g; hash[7] += h;
}

static void sha_224_256_remainder (sexp_uint8_t chunk[64], sexp_uint_t offset,
                                   sexp_uint_t len_bits, sexp_uint32_t hash[8]) {
  int i;
  /* Pad with '1' bit and zeros */
  chunk[offset] = 0x80;
  /* If we can't fit the length, use an additional chunk */
  if (offset >= 56) {
    sha_224_256_round(chunk, hash);
    memset(chunk, 0, 64);
  }
  /* Append the message length in bits as big-endian 64-bit integer */
  for (i = 63; i >= 56; i--) {
    chunk[i] = len_bits & 0xFF;
    len_bits >>= 8;
  }
  sha_224_256_round(chunk, hash);
}

/* = Handling data sources for SHA-224/256 ========================== */

static sexp sha_224_256_buffer (const sexp_uint8_t *source, sexp_uint_t len,
                                const sexp_uint32_t h[8], sexp_uint32_t hash[8]) {
  sexp_uint8_t buffer[64];
  sexp_uint_t offset, remaining;
  /* Initialize hash values */
  hash[0] = h[0]; hash[1] = h[1]; hash[2] = h[2]; hash[3] = h[3];
  hash[4] = h[4]; hash[5] = h[5]; hash[6] = h[6]; hash[7] = h[7];
  /* Process whole chunks */
  offset = 0;
  if (len >= 64)
    for ( ; offset <= (len - 64); offset += 64)
      sha_224_256_round(source + offset, hash);
  /* Copy remainder into buffer (if any) */
  memset(buffer, 0, 64);
  remaining = 0;
  if (offset < len) {
    for ( ; remaining < (len - offset); remaining++)
      buffer[remaining] = source[offset + remaining];
  }
  /* Process tha remainder */
  sha_224_256_remainder(buffer, remaining, len * 8, hash);
  return SEXP_VOID;
}

/* Exported by (chibi io) */
extern sexp sexp_read_u8 (sexp ctx, sexp self, sexp in);

static sexp read_sha_224_256_chunk (sexp ctx, sexp self, sexp port,
                                    sexp_uint8_t buffer[64]) {
  sexp res;
  sexp_uint_t read;
  memset(buffer, 0, 64);
  for (read = 0; read < 64; read++) {
    res = sexp_read_u8(ctx, self, port);
    if (sexp_exceptionp(res))
      return res;
    if (res == SEXP_EOF)
      break;
    buffer[read] = sexp_unbox_fixnum(res);
  }
  return sexp_make_fixnum(read);
}

static sexp sha_224_256_stream (sexp ctx, sexp self, sexp port,
                                const sexp_uint32_t h[8], sexp_uint32_t hash[8]) {
  sexp res;
  sexp_uint_t read, len;
  sexp_uint8_t buffer[64];
  /* Initialize hash values */
  hash[0] = h[0]; hash[1] = h[1]; hash[2] = h[2]; hash[3] = h[3];
  hash[4] = h[4]; hash[5] = h[5]; hash[6] = h[6]; hash[7] = h[7];
  /* Read and process whole chunks, leave remainder in the buffer */
  len = 0;
  while (1) {
    res = read_sha_224_256_chunk(ctx, self, port, buffer);
    if (sexp_exceptionp(res))
      return res;
    read = sexp_unbox_fixnum(res);
    len += read;
    if (read < 64)
      break;
    sha_224_256_round(buffer, hash);
  }
  /* Process tha remainder */
  sha_224_256_remainder(buffer, read, len * 8, hash);
  return SEXP_VOID;
}

/* = Handling data sources for SHA-224/256 ========================== */

static sexp sha_224_256_process (sexp ctx, sexp self, sexp src,
                                 const sexp_uint32_t h[8], sexp_uint32_t hash[8]) {
  if (sexp_bytesp(src))
    return sha_224_256_buffer((sexp_uint8_t*) sexp_bytes_data(src), sexp_bytes_length(src), h, hash);
  if (sexp_stringp(src))
    return sha_224_256_buffer((sexp_uint8_t*) sexp_string_data(src), sexp_string_size(src), h, hash);
  if (sexp_iportp(src))
    return sha_224_256_stream(ctx, self, src, h, hash);
  return sexp_xtype_exception(ctx, self, "expected string, bytevector, or input port", src);
}

static const char *hex = "0123456789abcdef";

static sexp sha_224_256_hash_string (sexp ctx, sexp self,
                                     const sexp_uint32_t hash[8], int count) {
  sexp res;
  int i, j;
  sexp_uint32_t next_word;
  /* Allocate string of target length */
  res = sexp_make_string(ctx, sexp_make_fixnum(count * 8), sexp_make_character('\0'));
  if (sexp_exceptionp(res))
    return res;
  /* Write 32-bit words as nibbles in big-endian order */
  for (i = 0; i < count; i++) {
    next_word = hash[i];
    for (j = 7; j >= 0; j--) {
      sexp_string_data(res)[8*i + j] = hex[next_word & 0xF];
      next_word >>= 4;
    }
  }
  return res;
}

/* = Scheme entry points ============================================ */

sexp sexp_sha_224 (sexp ctx, sexp self, sexp src) {
  sexp res;
  sexp_uint32_t hash[8];
  res = sha_224_256_process(ctx, self, src, h224, hash);
  if (sexp_exceptionp(res))
    return res;
  return sha_224_256_hash_string(ctx, self, hash, 7);
}

sexp sexp_sha_256 (sexp ctx, sexp self, sexp src) {
  sexp res;
  sexp_uint32_t hash[8];
  res = sha_224_256_process(ctx, self, src, h256, hash);
  if (sexp_exceptionp(res))
    return res;
  return sha_224_256_hash_string(ctx, self, hash, 8);
}
