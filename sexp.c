/*  sexp.c -- standalone sexp library implementation     */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include "sexp.h"

/* optional huffman-compressed immediate symbols */
#ifdef USE_HUFF_SYMS
struct huff_entry {
  unsigned char len;
  unsigned short bits;
};
#include "sexp-hufftabs.c"
static struct huff_entry huff_table[] = {
#include "sexp-huff.c"
};
#endif

static int sexp_initialized_p = 0;

static sexp the_dot_symbol;
static sexp the_quote_symbol;
static sexp the_quasiquote_symbol;
static sexp the_unquote_symbol;
static sexp the_unquote_splicing_symbol;

static char sexp_separators[] = {
  /* 1  2  3  4  5  6  7  8  9  a  b  c  d  e  f         */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, /* x0_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x1_ */
  1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, /* x2_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, /* x3_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x4_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, /* x5_ */
};

static int is_separator(int c) {
  /* return (!((c-9)&(~3))) | (~(c^4)); */
  return 0<c && c<0x60 && sexp_separators[c];
}

static sexp* symbol_table = NULL;
static unsigned long symbol_table_primes[] = {
  97, 389, 1543, 6151, 12289, 24593, 49157, 98317, 196613, 393241,
  786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653,
  100663319, 201326611, 402653189, 805306457, 1610612741};
static int symbol_table_prime_index = 0;
static int symbol_table_count = 0;

void sexp_free (sexp obj) {
  int len, i;
  sexp *elts;
  if (SEXP_POINTERP(obj)) {
    switch (obj->tag) {
    case SEXP_PAIR:
      sexp_free(SEXP_CAR(obj));
      sexp_free(SEXP_CDR(obj));
      break;
    case SEXP_VECTOR:
      len = sexp_vector_length(obj);
      elts = sexp_vector_data(obj);
      for (i=0; i<len; i++) {
        sexp_free(elts[i]);
      }
      SEXP_FREE(elts);
      break;
    case SEXP_STRING:
    case SEXP_SYMBOL:
      SEXP_FREE(sexp_string_data(obj));
      break;
    }
    SEXP_FREE(obj);
  }
}

/*************************** list utilities ***************************/

sexp sexp_cons(sexp head, sexp tail) {
  sexp pair = SEXP_NEW();
  if (! pair) return SEXP_ERROR;
  pair->tag = SEXP_PAIR;
  pair->data1 = (void*) head;
  pair->data2 = (void*) tail;
  return pair;
}

sexp sexp_car(sexp obj) {
  return (SEXP_PAIRP(obj)) ? SEXP_CAR(obj) : SEXP_ERROR;
}

sexp sexp_cdr(sexp obj) {
  return (SEXP_PAIRP(obj)) ? SEXP_CDR(obj) : SEXP_ERROR;
}

int sexp_listp (sexp obj) {
  while (SEXP_PAIRP(obj))
    obj = SEXP_CDR(obj);
  return (obj == SEXP_NULL);
}

int sexp_list_index (sexp ls, sexp elt) {
  int i=0;
  while (SEXP_PAIRP(ls)) {
    if (SEXP_CAR(ls) == elt)
      return i;
    ls = SEXP_CDR(ls);
    i++;
  }
  return -1;
}

sexp sexp_memq (sexp x, sexp ls) {
  while (SEXP_PAIRP(ls))
    if (x == SEXP_CAR(ls))
      return ls;
    else
      ls = SEXP_CDR(ls);
  return SEXP_FALSE;
}

sexp sexp_assq (sexp x, sexp ls) {
  while (SEXP_PAIRP(ls))
    if (x == SEXP_CAAR(ls))
      return ls;
    else
      ls = SEXP_CDR(ls);
  return SEXP_FALSE;
}

sexp sexp_lset_diff(sexp a, sexp b) {
  sexp res = SEXP_NULL;
  for ( ; SEXP_PAIRP(a); a=SEXP_CDR(a))
    if (! sexp_list_index(b, SEXP_CAR(a)) >= 0)
      res = sexp_cons(SEXP_CAR(a), res);
  return res;
}

sexp sexp_reverse(sexp ls) {
  sexp res = SEXP_NULL;
  for ( ; SEXP_PAIRP(ls); ls=SEXP_CDR(ls))
    res = sexp_cons(SEXP_CAR(ls), res);
  return res;
}

sexp sexp_nreverse(sexp ls) {
  sexp a, b, tmp;
  if (ls == SEXP_NULL) {
    return ls;
  } else if (! SEXP_PAIRP(ls)) {
    return SEXP_ERROR;
  } else {
    b=ls;
    a=SEXP_CDR(ls);
    SEXP_CDR(b) = SEXP_NULL;
    for ( ; SEXP_PAIRP(a); b=a, a=tmp) {
      tmp=SEXP_CDR(a);
      SEXP_CDR(a) = b;
    }
    return b;
  }
}

sexp sexp_append(sexp a, sexp b) {
  for (a=sexp_reverse(a); SEXP_PAIRP(a); a=SEXP_CDR(a))
    b = sexp_cons(SEXP_CAR(a), b);
  return b;
}

sexp sexp_list(int count, ...) {
  sexp res = SEXP_NULL;
  int i;
  va_list ap;
  va_start(ap, count);
  for (i=0; i<count; i++)
    res = sexp_cons(va_arg(ap, sexp), res);
  va_end(ap);
  return sexp_nreverse(res);
}

unsigned long sexp_length(sexp ls) {
  sexp x;
  unsigned long res;
  for (res=0, x=ls; SEXP_PAIRP(x); res++, x=SEXP_CDR(x))
    ;
  return res;
}

/********************* strings, symbols, vectors **********************/

sexp sexp_make_flonum(double f) {
  sexp x = SEXP_NEW();
  if (! x) return SEXP_ERROR;
  x->tag = SEXP_FLONUM;
  sexp_flonum_value(x) = f;
  return x;
}

sexp sexp_make_string(char *str) {
  sexp s = SEXP_NEW();
  if (! s) return SEXP_ERROR;
  unsigned long len = strlen(str);
  char *mystr = SEXP_ALLOC(len+1);
  if (! mystr) { SEXP_FREE(s); return SEXP_ERROR; }
  memcpy(mystr, str, len+1);
  s->tag = SEXP_STRING;
  s->data1 = (void*) len;
  s->data2 = (void*) mystr;
  return s;
}

#define FNV_PRIME 16777619
#define FNV_OFFSET_BASIS 2166136261uL

int sexp_string_hash(char *str, int acc) {
  while (*str) {acc *= FNV_PRIME; acc ^= *str++;}
  return acc;
}

sexp sexp_intern(char *str) {
  struct huff_entry he;
  sexp_uint_t len, res=FNV_OFFSET_BASIS, space=3, newbits, i, d, cell;
  char c, *mystr, *p=str;
  sexp sym, *newtable;

#ifdef USE_HUFF_SYMS
  res = 0;
  for (p=str; c=*p; p++) {
    he = huff_table[c];
    newbits = he.len;
    if ((space+newbits) > (sizeof(sexp)*8)) {
      goto normal_intern;
    }
    res |= (((sexp_uint_t) he.bits) << space);
    space += newbits;
  }
  return (sexp) (res + SEXP_ISYMBOL_TAG);
#endif

 normal_intern:
  res = sexp_string_hash(p, res);
  d = symbol_table_primes[symbol_table_prime_index];
  cell = res % d;
  for (i=0; i<d; i++) {
    if (! symbol_table[cell]) {
      break;
    } else if (strncmp(str,
                       sexp_symbol_data(symbol_table[cell]),
                       sexp_symbol_length(symbol_table[cell])) == 0) {
      return symbol_table[cell];
    }
    cell = (cell * res + 1) % d;
  }
  symbol_table_count++;

 resize:
  if (symbol_table_count*5 > d*4) {
    newtable = SEXP_ALLOC(symbol_table_primes[symbol_table_prime_index++]
                          * sizeof(sexp));
    SEXP_FREE(symbol_table);
    symbol_table = newtable;
  }

 new_entry:
  sym = SEXP_NEW();
  if (! sym) return SEXP_ERROR;
  len = strlen(str);
  mystr = SEXP_ALLOC(len+1);
  if (! mystr) { SEXP_FREE(sym); return SEXP_ERROR; }
  memcpy(mystr, str, len+1);
  sym->tag = SEXP_SYMBOL;
  sym->data1 = (void*) len;
  sym->data2 = (void*) mystr;
  symbol_table[cell] = (sexp) (((sexp_uint_t)sym) + 3);
  return symbol_table[cell];
}

sexp sexp_make_vector(unsigned long len, sexp dflt) {
  int i;
  sexp v = SEXP_NEW();
  if (v == NULL) return SEXP_ERROR;
  sexp *x = (void*) SEXP_ALLOC(len*sizeof(sexp));
  if (x == NULL) return SEXP_ERROR;
  for (i=0; i<len; i++) {
    x[i] = dflt;
  }
  v->tag = SEXP_VECTOR;
  v->data1 = (void*) len;
  v->data2 = (void*) x;
  return v;
}

sexp sexp_list_to_vector(sexp ls) {
  sexp vec = sexp_make_vector(sexp_length(ls), SEXP_FALSE);
  if (vec == SEXP_ERROR) return vec;
  sexp x;
  sexp *elts = sexp_vector_data(vec);
  int i;
  for (i=0, x=ls; SEXP_PAIRP(x); i++, x=SEXP_CDR(x))
    elts[i] = SEXP_CAR(x);
  return vec;
}

sexp sexp_vector(int count, ...) {
  sexp vec = sexp_make_vector(count, SEXP_FALSE);
  if (vec == SEXP_ERROR) return vec;
  sexp *elts = sexp_vector_data(vec);
  va_list ap;
  int i;

  va_start(ap, count);
  for (i=0; i<count; i++)
    elts[i] = va_arg(ap, sexp);
  va_end(ap);
  return vec;
}

/************************ reading and writing *************************/

#ifdef USE_STRING_STREAMS

int sstream_read(void *vec, char *dst, int n) {
  int len = (int) sexp_vector_ref((sexp) vec, sexp_make_integer(1));
  int pos = (int) sexp_vector_ref((sexp) vec, sexp_make_integer(2));
  if (pos >= len) return 0;
  if (n > (len - pos)) n = (len - pos);
  memcpy(dst+pos, sexp_vector_ref((sexp) vec, sexp_make_integer(0)), n);
  sexp_vector_set((sexp) vec, sexp_make_integer(2), (sexp)n);
  return n;
}

int sstream_write(void *vec, const char *src, int n) {
  return n;
}

off_t sstream_seek(void *vec, off_t offset, int whence) {
  int pos;
  if (whence == SEEK_SET) {
    pos = offset;
  } else if (whence == SEEK_CUR) {
    pos = (int) sexp_vector_ref((sexp) vec, sexp_make_integer(2)) + offset;
  } else {                      /* SEEK_END */
    pos = (int) sexp_vector_ref((sexp) vec, sexp_make_integer(1)) + offset;
  }
  sexp_vector_set((sexp) vec, sexp_make_integer(2), (sexp)pos);
  return pos;
}

int sstream_close(void *vec) {
  sexp_free((sexp)vec);
}

sexp sexp_make_input_port(FILE* in) {
  sexp p = SEXP_NEW();
  if (p == NULL) return SEXP_ERROR;
  p->tag = SEXP_IPORT;
  p->data1 = in;
  return p;
}

sexp sexp_make_output_port(FILE* out) {
  sexp p = SEXP_NEW();
  if (p == NULL) return SEXP_ERROR;
  p->tag = SEXP_OPORT;
  p->data1 = out;
  return p;
}

sexp sexp_make_input_string_port(sexp str) {
  FILE *in = fmemopen(sexp_string_data(str), sexp_string_length(str), "r");
  return sexp_make_input_port(in);
}

sexp sexp_make_output_string_port() {
  return SEXP_ERROR;
}

sexp sexp_get_output_string(sexp port) {
  return SEXP_ERROR;
}

#endif

void sexp_write (sexp obj, sexp out) {
  unsigned long len, i, c, res;
  sexp x, *elts;
  char *str;

  if (! obj) {
    sexp_write_string("#<null>", out);
  } else if (SEXP_POINTERP(obj)) {
    switch (obj->tag) {
    case SEXP_PAIR:
      sexp_write_char('(', out);
      sexp_write(SEXP_CAR(obj), out);
      for (x=SEXP_CDR(obj); SEXP_PAIRP(x); x=SEXP_CDR(x)) {
        sexp_write_char(' ', out);
        sexp_write(SEXP_CAR(x), out);
      }
      if (! SEXP_NULLP(x)) {
        sexp_write_string(" . ", out);
        sexp_write(x, out);
      }
      sexp_write_char(')', out);
      break;
    case SEXP_VECTOR:
      len = sexp_vector_length(obj);
      elts = sexp_vector_data(obj);
      if (len == 0) {
        sexp_write_string("#()", out);
      } else {
        sexp_write_string("#(", out);
        sexp_write(out, elts[0]);
        for (i=1; i<len; i++) {
          sexp_write_char(' ', out);
          sexp_write(out, elts[i]);
        }
        sexp_write_char(')', out);
      }
      break;
    case SEXP_FLONUM:
      sexp_printf(out, "%g", sexp_flonum_value(obj)); break;
    case SEXP_PROCEDURE:
      sexp_write_string("#<procedure>", out); break;
    case SEXP_IPORT:
      sexp_write_string("#<input-port>", out); break;
    case SEXP_OPORT:
      sexp_write_string("#<output-port>", out); break;
    case SEXP_BYTECODE:
      sexp_write_string("#<bytecode>", out); break;
    case SEXP_ENV:
      sexp_write_string("#<env>", out); break;
    case SEXP_STRING:
      sexp_write_char('"', out);
      i = sexp_string_length(obj);
      str = sexp_string_data(obj);
      /* FALLTHROUGH */
    case SEXP_SYMBOL:
      if (obj->tag != SEXP_STRING) {
        i = sexp_symbol_length(obj);
        str = sexp_symbol_data(obj);
      }
      for ( ; i>=0; str++, i--) {
        if (str[0] == '\\')
          sexp_write_char('\\', out);
        sexp_write_char(str[0], out);
      }
      if (obj->tag == SEXP_STRING)
        sexp_write_char('"', out);
      break;
    }
  } else if (SEXP_INTEGERP(obj)) {
    sexp_printf(out, "%d", sexp_unbox_integer(obj));
  } else if (SEXP_CHARP(obj)) {
      if (33 <= sexp_unbox_character(obj) < 127) {
        sexp_printf(out, "#\\%c", sexp_unbox_character(obj));
      } else {
        sexp_printf(out, "#\\x%02d", sexp_unbox_character(obj));
      }
  } else if (SEXP_SYMBOLP(obj)) {

#ifdef USE_HUFF_SYMS
    if (((sexp_uint_t)obj&7)==7) {
      c = ((sexp_uint_t)obj)>>3;
      while (c) {
#include "sexp-unhuff.c"
        sexp_write_char(res, out);
      }
    }
#endif

  } else {
    switch ((sexp_uint_t) obj) {
    case (sexp_uint_t) SEXP_NULL:
      sexp_write_string("()", out); break;
    case (sexp_uint_t) SEXP_TRUE:
      sexp_write_string("#t", out); break;
    case (sexp_uint_t) SEXP_FALSE:
      sexp_write_string("#f", out); break;
    case (sexp_uint_t) SEXP_EOF:
      sexp_write_string("#<eof>", out); break;
    case (sexp_uint_t) SEXP_UNDEF:
      sexp_write_string("#<undef>", out); break;
    default:
      sexp_write_string("#<error>", out);
    }
  }
}

char* sexp_read_string(sexp in) {
  char *buf, *tmp, *res;
  int c, len, size=128;

  buf = SEXP_ALLOC(size);       /* XXXX grow! */
  tmp = buf;

  for (c=sexp_read_char(in); c != '"'; c=sexp_read_char(in)) {
    if (c == EOF) {
      SEXP_FREE(buf);
      return NULL;
    } else if (c == '\\') {
      c=sexp_read_char(in);
      switch (c) {
      case 'n': c = '\n'; break;
      case 't': c = '\t'; break;
      }
      *tmp++ = c;
    } else {
      *tmp++ = c;
    }
  }

  *tmp++ = '\0';
  len = tmp - buf;
  res = SEXP_ALLOC(len);
  memcpy(res, buf, len);
  SEXP_FREE(buf);
  return res;
}

char* sexp_read_symbol(sexp in, int init) {
  char *buf, *tmp, *res;
  int c, len, size=128;

  buf = SEXP_ALLOC(size);
  tmp = buf;

  if (init != EOF)
    *tmp++ = init;

  while (1) {
    c=sexp_read_char(in);
    if (c == EOF || is_separator(c)) {
      sexp_push_char(c, in);
      break;
    }
    *tmp++ = c;
  }

  *tmp++ = '\0';
  len = tmp - buf;
  res = SEXP_ALLOC(len);
  memcpy(res, buf, len);
  SEXP_FREE(buf);
  return res;
}

sexp sexp_read_float_tail(sexp in, long whole) {
  double res = 0.0, scale=0.1;
  int c;
  for (c=sexp_read_char(in); isdigit(c); c=sexp_read_char(in), scale*=0.1)
    res += ((c<='9') ? (c - '0') : ((toupper(c) - 'A') + 10))*scale;
  sexp_push_char(c, in);
  return sexp_make_flonum(whole + res);
}

sexp sexp_read_number(sexp in, int base) {
  sexp tmp;
  long res = 0, negativep = 0, c;

  c = sexp_read_char(in);
  if (c == '-') {
    negativep = 1;
  } else if (isdigit(c)) {
    res = c - '0';
  }

  for (c=sexp_read_char(in); isxdigit(c); c=sexp_read_char(in))
    res = res * base + ((c<='9') ? (c - '0') : ((toupper(c) - 'A') + 10));
  if (c=='.') {
    if (base != 10) {
      fprintf(stderr, "decimal found in non-base 10");
      return SEXP_ERROR;
    }
    tmp = sexp_read_float_tail(in, res);
    if (negativep && SEXP_FLONUMP(tmp))
      sexp_flonum_value(tmp) = -1 * sexp_flonum_value(tmp);
    return tmp;
  } else {
    sexp_push_char(c, in);
  }

  return sexp_make_integer(negativep ? -res : res);
}

sexp sexp_read_raw (sexp in) {
  sexp res, tmp, tmp2;
  char *str;
  int c1, c2;

 scan_loop:
  switch (c1 = sexp_read_char(in)) {
  case EOF:
    res = SEXP_EOF;
    break;
  case ';':
    while ((c1 = sexp_read_char(in)) != EOF)
      if (c1 == '\n')
        break;
    /* fallthrough */
  case ' ':
  case '\t':
  case '\n':
    goto scan_loop;
  case '\'':
    res = sexp_read(in);
    res = sexp_list2(the_quote_symbol, res);
    break;
  case '`':
    res = sexp_read(in);
    res = sexp_list2(the_quasiquote_symbol, res);
    break;
  case ',':
    if ((c1 = sexp_read_char(in)) == '@') {
      res = sexp_read(in);
      res = sexp_list2(the_unquote_splicing_symbol, res);
    } else {
      sexp_push_char(c1, in);
      res = sexp_read(in);
      res = sexp_list2(the_unquote_symbol, res);
    }
    break;
  case '"':
    str = sexp_read_string(in);
    res = sexp_make_string(str);
    SEXP_FREE(str);
    break;
  case '(':
    res = SEXP_NULL;
    tmp = sexp_read_raw(in);
    while ((tmp != SEXP_ERROR) && (tmp != SEXP_EOF) && (tmp != SEXP_CLOSE)) {
      if (tmp == SEXP_RAWDOT) {
        if (res == SEXP_NULL) {
          fprintf(stderr, "sexp: dot before any elements in list\n");
          return SEXP_ERROR;
        } else {
          tmp = sexp_read_raw(in);
          if (sexp_read(in) != SEXP_CLOSE) {
            fprintf(stderr, "sexp: multiple tokens in dotted tail\n");
            sexp_free(res);
            return SEXP_ERROR;
          } else {
            tmp2 = res;
            res = sexp_nreverse(res);
            SEXP_CDR(tmp2) = tmp;
            return res;
          }
        }
      } else {
        res = sexp_cons(tmp, res);
        tmp = sexp_read_raw(in);
      }
    }
    if (tmp != SEXP_CLOSE) {
      sexp_free(res);
      res = SEXP_ERROR;
    }
    res = sexp_nreverse(res);
    break;
  case '#':
    switch (c1=sexp_read_char(in)) {
    case 'b':
      res = sexp_read_number(in, 2); break;
    case 'o':
      res = sexp_read_number(in, 8); break;
    case 'd':
      res = sexp_read_number(in, 10); break;
    case 'x':
      res = sexp_read_number(in, 16); break;
/*     case 'e': */
/*     case 'i': */
    case 'f':
    case 't':
      c2 = sexp_read_char(in);
      if (c2 == EOF || is_separator(c2)) {
        res = (c1 == 't' ? SEXP_TRUE : SEXP_FALSE);
      } else {
        fprintf(stderr, "sexp: invalid syntax #%c%c\n", c1, c2);
        res = SEXP_ERROR;
      }
      sexp_push_char(c2, in);
      break;
    case ';':
      sexp_read_raw(in);
      goto scan_loop;
    case '(':
      sexp_push_char(c1, in);
      res = sexp_read(in);
      if (! sexp_listp(res)) {
        if (res != SEXP_ERROR) {
          fprintf(stderr, "sexp: dotted list not allowed in vector syntax\n");
          sexp_free(res);
          res = SEXP_ERROR;
        }
      } else {
        res = sexp_list_to_vector(res);
      }
      break;
    default:
      fprintf(stderr, "sexp: invalid syntax #%c\n", c1);
      res = SEXP_ERROR;
      break;
    }
    break;
  case '.':
    c1 = sexp_read_char(in);
    if (c1 == EOF || is_separator(c1)) {
      res = SEXP_RAWDOT;
    } else if (isdigit(c1)) {
      sexp_push_char(c1,in );
      res = sexp_read_float_tail(in, 0);
    } else {
      sexp_push_char(c1, in);
      str = sexp_read_symbol(in, '.');
      res = sexp_intern(str);
      SEXP_FREE(str);
    }
    break;
  case ')':
    res = SEXP_CLOSE;
    break;
  case '+':
  case '-':
    c2 = sexp_read_char(in);
    if (c2 == '.' || isdigit(c2)) {
      sexp_push_char(c2, in);
      res = sexp_read_number(in, 10);
      if (c1 == '-') res = sexp_mul(res, -1);
    } else {
      sexp_push_char(c2, in);
      str = sexp_read_symbol(in, c1);
      res = sexp_intern(str);
      SEXP_FREE(str);
    }
    break;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    sexp_push_char(c1, in);
    res = sexp_read_number(in, 10);
    break;
  default:
    str = sexp_read_symbol(in, c1);
    res = sexp_intern(str);
    SEXP_FREE(str);
    break;
  }
  return res;
}

sexp sexp_read (sexp in) {
  sexp res = sexp_read_raw(in);
  if ((res == SEXP_CLOSE) || (res == SEXP_RAWDOT))
    res = SEXP_ERROR;
  return res;
}

sexp sexp_read_from_string(char *str) {
  sexp s = sexp_make_string(str);
  sexp in = sexp_make_input_string_port(s);
  sexp res = sexp_read(in);
  sexp_free(s);
  sexp_free(in);
  return res;
}

void sexp_init() {
  if (! sexp_initialized_p) {
    sexp_initialized_p = 1;
#ifdef USE_BOEHM
    GC_init();
#endif
    symbol_table = SEXP_ALLOC(symbol_table_primes[0]*sizeof(sexp));
    the_dot_symbol = sexp_intern(".");
    the_quote_symbol = sexp_intern("quote");
    the_quasiquote_symbol = sexp_intern("quasiquote");
    the_unquote_symbol = sexp_intern("unquote");
    the_unquote_splicing_symbol = sexp_intern("unquote-splicing");
  }
}

