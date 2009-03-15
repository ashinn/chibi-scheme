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
static sexp the_empty_vector;

static char sexp_separators[] = {
  /* 1  2  3  4  5  6  7  8  9  a  b  c  d  e  f         */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, /* x0_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x1_ */
  1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, /* x2_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, /* x3_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x4_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, /* x5_ */
};

#define digit_value(c) (((c)<='9') ? ((c) - '0') : ((toupper(c) - 'A') + 10))

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
  if (sexp_pointerp(obj)) {
    switch (obj->tag) {
    case SEXP_PAIR:
      sexp_free(sexp_car(obj));
      sexp_free(sexp_cdr(obj));
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
  sexp pair = SEXP_ALLOC(sexp_sizeof(pair));
  pair->tag = SEXP_PAIR;
  sexp_car(pair) = head;
  sexp_cdr(pair) = tail;
  return pair;
}

int sexp_listp (sexp obj) {
  while (sexp_pairp(obj))
    obj = sexp_cdr(obj);
  return (obj == SEXP_NULL);
}

int sexp_list_index (sexp ls, sexp elt) {
  int i=0;
  while (sexp_pairp(ls)) {
    if (sexp_car(ls) == elt)
      return i;
    ls = sexp_cdr(ls);
    i++;
  }
  return -1;
}

sexp sexp_memq (sexp x, sexp ls) {
  while (sexp_pairp(ls))
    if (x == sexp_car(ls))
      return ls;
    else
      ls = sexp_cdr(ls);
  return SEXP_FALSE;
}

sexp sexp_assq (sexp x, sexp ls) {
  while (sexp_pairp(ls))
    if (x == sexp_caar(ls))
      return ls;
    else
      ls = sexp_cdr(ls);
  return SEXP_FALSE;
}

sexp sexp_lset_diff(sexp a, sexp b) {
  sexp res = SEXP_NULL;
  for ( ; sexp_pairp(a); a=sexp_cdr(a))
    if (! sexp_list_index(b, sexp_car(a)) >= 0)
      res = sexp_cons(sexp_car(a), res);
  return res;
}

sexp sexp_reverse(sexp ls) {
  sexp res = SEXP_NULL;
  for ( ; sexp_pairp(ls); ls=sexp_cdr(ls))
    res = sexp_cons(sexp_car(ls), res);
  return res;
}

sexp sexp_nreverse(sexp ls) {
  sexp a, b, tmp;
  if (ls == SEXP_NULL) {
    return ls;
  } else if (! sexp_pairp(ls)) {
    return SEXP_ERROR;
  } else {
    b = ls;
    a = sexp_cdr(ls);
    sexp_cdr(b) = SEXP_NULL;
    for ( ; sexp_pairp(a); b=a, a=tmp) {
      tmp = sexp_cdr(a);
      sexp_cdr(a) = b;
    }
    return b;
  }
}

sexp sexp_append(sexp a, sexp b) {
  for (a=sexp_reverse(a); sexp_pairp(a); a=sexp_cdr(a))
    b = sexp_cons(sexp_car(a), b);
  return b;
}

sexp sexp_length(sexp ls) {
  sexp_uint_t res=0;
  for ( ; sexp_pairp(ls); res++, ls=sexp_cdr(ls))
    ;
  return sexp_make_integer(res);
}

/********************* strings, symbols, vectors **********************/

sexp sexp_make_flonum(double f) {
  sexp x = SEXP_ALLOC(sexp_sizeof(flonum));
  x->tag = SEXP_FLONUM;
  sexp_flonum_value(x) = f;
  return x;
}

sexp sexp_make_string(char *str) {
  sexp s = SEXP_ALLOC(sexp_sizeof(string));
  sexp_uint_t len = strlen(str);
  char *mystr = SEXP_ALLOC(len+1);
  memcpy(mystr, str, len+1);
  s->tag = SEXP_STRING;
  sexp_string_length(s) = len;
  sexp_string_data(s) = mystr;
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

#if USE_HUFF_SYMS
  res = 0;
  for ( ; (c=*p); p++) {
    he = huff_table[(unsigned char)c];
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

  if (symbol_table_count*5 > d*4) {
    fprintf(stderr, "resizing symbol table!!!!!\n");
    newtable = SEXP_ALLOC(symbol_table_primes[symbol_table_prime_index++]
                          * sizeof(sexp));
    /* XXXX rehash */
    SEXP_FREE(symbol_table);
    symbol_table = newtable;
  }

  sym = SEXP_ALLOC(sexp_sizeof(symbol));
  len = strlen(str);
  mystr = SEXP_ALLOC(len+1);
  memcpy(mystr, str, len+1);
  mystr[len]=0;
  sym->tag = SEXP_SYMBOL;
  sexp_symbol_length(sym) = len;
  sexp_symbol_data(sym) = mystr;
  symbol_table[cell] = sym;
  return symbol_table[cell];
}

sexp sexp_make_vector(sexp len, sexp dflt) {
  sexp v, *x;
  int i, clen = sexp_unbox_integer(len);
  if (! clen) return the_empty_vector;
  v = SEXP_ALLOC(sexp_sizeof(vector));
  x = (sexp*) SEXP_ALLOC(clen*sizeof(sexp));
  for (i=0; i<clen; i++) {
    x[i] = dflt;
  }
  v->tag = SEXP_VECTOR;
  sexp_vector_length(v) = clen;
  sexp_vector_data(v) = x;
  return v;
}

sexp sexp_list_to_vector(sexp ls) {
  sexp x, vec = sexp_make_vector(sexp_length(ls), SEXP_UNDEF);
  sexp *elts = sexp_vector_data(vec);
  int i;
  for (i=0, x=ls; sexp_pairp(x); i++, x=sexp_cdr(x))
    elts[i] = sexp_car(x);
  return vec;
}

sexp sexp_vector(int count, ...) {
  sexp vec = sexp_make_vector(sexp_make_integer(count), SEXP_UNDEF);
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

#if USE_STRING_STREAMS

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
  return 0;
}

sexp sexp_make_input_port(FILE* in) {
  sexp p = SEXP_ALLOC(sexp_sizeof(port));
  p->tag = SEXP_IPORT;
  sexp_port_stream(p) = in;
  return p;
}

sexp sexp_make_output_port(FILE* out) {
  sexp p = SEXP_ALLOC(sexp_sizeof(port));
  p->tag = SEXP_OPORT;
  sexp_port_stream(p) = out;
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
  unsigned long len, c, res;
  long i=0;
  sexp x, *elts;
  char *str=NULL;

  if (! obj) {
    sexp_write_string("#<null>", out);
  } else if (sexp_pointerp(obj)) {
    switch (obj->tag) {
    case SEXP_PAIR:
      sexp_write_char('(', out);
      sexp_write(sexp_car(obj), out);
      for (x=sexp_cdr(obj); sexp_pairp(x); x=sexp_cdr(x)) {
        sexp_write_char(' ', out);
        sexp_write(sexp_car(x), out);
      }
      if (! sexp_nullp(x)) {
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
        sexp_write(elts[0], out);
        for (i=1; i<len; i++) {
          sexp_write_char(' ', out);
          sexp_write(elts[i], out);
        }
        sexp_write_char(')', out);
      }
      break;
    case SEXP_FLONUM:
      sexp_printf(out, "%g", sexp_flonum_value(obj)); break;
    case SEXP_PROCEDURE:
      sexp_printf(out, "#<procedure: %p>", obj); break;
    case SEXP_IPORT:
      sexp_write_string("#<input-port>", out); break;
    case SEXP_OPORT:
      sexp_write_string("#<output-port>", out); break;
    case SEXP_CORE:
      sexp_write_string("#<core-form>", out); break;
    case SEXP_OPCODE:
      sexp_write_string("#<opcode>", out); break;
    case SEXP_BYTECODE:
      sexp_write_string("#<bytecode>", out); break;
    case SEXP_ENV:
      sexp_write_string("#<env>", out); break;
    case SEXP_MACRO:
      sexp_write_string("#<macro>", out); break;
    case SEXP_STRING:
      sexp_write_char('"', out);
      i = sexp_string_length(obj);
      str = sexp_string_data(obj);
      /* ... FALLTHROUGH ... */
    case SEXP_SYMBOL:
      if (obj->tag != SEXP_STRING) {
        i = sexp_symbol_length(obj);
        str = sexp_symbol_data(obj);
      }
      for ( ; i>0; str++, i--) {
        if (str[0] == '\\')
          sexp_write_char('\\', out);
        sexp_write_char(str[0], out);
      }
      if (obj->tag == SEXP_STRING)
        sexp_write_char('"', out);
      break;
    }
  } else if (sexp_integerp(obj)) {
    sexp_printf(out, "%ld", sexp_unbox_integer(obj));
  } else if (sexp_charp(obj)) {
    if ((33 <= sexp_unbox_character(obj)) && (sexp_unbox_character(obj) < 127))
      sexp_printf(out, "#\\%c", sexp_unbox_character(obj));
    else
      sexp_printf(out, "#\\x%02d", sexp_unbox_character(obj));
  } else if (sexp_symbolp(obj)) {

#if USE_HUFF_SYMS
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
    case (sexp_uint_t) SEXP_ERROR:
      sexp_write_string("#<error>", out); break;
    default:
      sexp_printf(out, "#<invalid: %p>", obj);
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
    res += digit_value(c)*scale;
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
    res = res * base + digit_value(c);
  if (c=='.') {
    if (base != 10) {
      fprintf(stderr, "decimal found in non-base 10");
      return SEXP_ERROR;
    }
    tmp = sexp_read_float_tail(in, res);
    if (negativep && sexp_flonump(tmp))
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
    /* ... FALLTHROUGH ... */
  case ' ':
  case '\t':
  case '\r':
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
          if (sexp_read_raw(in) != SEXP_CLOSE) {
            fprintf(stderr, "sexp: multiple tokens in dotted tail\n");
            sexp_free(res);
            return SEXP_ERROR;
          } else {
            tmp2 = res;
            res = sexp_nreverse(res);
            sexp_cdr(tmp2) = tmp;
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
    case '\\':
      c1 = sexp_read_char(in);
      c2 = sexp_read_char(in);
      if (c2 == EOF || is_separator(c2)) {
        sexp_push_char(c2, in);
        res = sexp_make_character(c1);
      } else if ((c1 == 'x' || c1 == 'X') && isxdigit(c2)) {
        c1 = sexp_read_char(in);
        res = sexp_make_character(16 * digit_value(c2) + digit_value(c1));
      } else {
        str = sexp_read_symbol(in, c1);
        if (strcasecmp(str, "space") == 0)
          res = sexp_make_character(' ');
        else if (strcasecmp(str, "newline") == 0)
          res = sexp_make_character('\r');
        else if (strcasecmp(str, "return") == 0)
          res = sexp_make_character('\r');
        else if (strcasecmp(str, "tab") == 0)
          res = sexp_make_character('\t');
        else {
          fprintf(stderr, "unknown character name: '%s'\n", str);
          res = SEXP_ERROR;
        }
      }
      break;
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
#if USE_BOEHM
    GC_init();
#endif
    symbol_table = SEXP_ALLOC(symbol_table_primes[0]*sizeof(sexp));
    the_dot_symbol = sexp_intern(".");
    the_quote_symbol = sexp_intern("quote");
    the_quasiquote_symbol = sexp_intern("quasiquote");
    the_unquote_symbol = sexp_intern("unquote");
    the_unquote_splicing_symbol = sexp_intern("unquote-splicing");
    the_empty_vector = SEXP_ALLOC(sexp_sizeof(vector));
    the_empty_vector->tag = SEXP_VECTOR;
    sexp_vector_length(the_empty_vector) = 0;
    sexp_vector_data(the_empty_vector) = NULL;
  }
}

