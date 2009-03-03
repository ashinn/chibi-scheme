/*  sexp.c -- sexp library implementation */
/*  Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt */

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

static int initialized_p = 0;

static sexp the_dot_symbol;
static sexp the_quote_symbol;
static sexp the_quasiquote_symbol;
static sexp the_unquote_symbol;
static sexp the_unquote_splicing_symbol;
static sexp the_lambda_symbol;
static sexp the_begin_symbol;
static sexp the_define_symbol;
static sexp the_set_x_symbol;
static sexp the_if_symbol;

static char separators[] = {
  /* 1  2  3  4  5  6  7  8  9  a  b  c  d  e  f         */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, /* x0_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x1_ */
  1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, /* x2_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, /* x3_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x4_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, /* x5_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x6_ */
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* x7_ */
};

static int is_separator(int c) {
  /* return (!((c-9)&(~3))) | (~(c^4)); */
  return 0<c && c<128 && separators[c];
}

static sexp* symbol_table = NULL;
static unsigned long symbol_table_primes[] = {
  97, 389, 1543, 6151, 12289, 24593, 49157, 98317, 196613, 393241,
  786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653,
  100663319, 201326611, 402653189, 805306457, 1610612741};
static int symbol_table_prime_index = 0;
static int symbol_table_count = 0;

void free_sexp (sexp obj) {
  int len, i;
  sexp *elts;
  if (SEXP_POINTERP(obj)) {
    switch (obj->tag) {
    case SEXP_PAIR:
      free_sexp(car(obj));
      free_sexp(cdr(obj));
      break;
    case SEXP_VECTOR:
      len = vector_length(obj);
      elts = vector_data(obj);
      for (i=0; i<len; i++) {
        free_sexp(elts[i]);
      }
      SEXP_FREE(elts);
      break;
    case SEXP_STRING:
    case SEXP_SYMBOL:
      SEXP_FREE(string_data(obj));
      break;
    }
    SEXP_FREE(obj);
  }
}

/*************************** list utilities ***************************/

sexp cons(sexp head, sexp tail) {
  sexp pair = SEXP_NEW();
  if (! pair) return SEXP_ERROR;
  pair->tag = SEXP_PAIR;
  pair->data1 = (void*) head;
  pair->data2 = (void*) tail;
  return pair;
}

sexp car(sexp obj) {
  return (SEXP_PAIRP(obj)) ? SEXP_CAR(obj) : SEXP_ERROR;
}

sexp cdr(sexp obj) {
  return (SEXP_PAIRP(obj)) ? SEXP_CDR(obj) : SEXP_ERROR;
}

sexp set_car(sexp obj, sexp val) {
  if (SEXP_PAIRP(obj))
    return SEXP_CAR(obj) = val;
  else {
    sexp_debug("error: set-car! not a pair: ", obj);
    return SEXP_ERROR;
  }
}

sexp set_cdr(sexp obj, sexp val) {
  if (SEXP_PAIRP(obj))
    return SEXP_CDR(obj) = val;
  else
    return SEXP_ERROR;
}

int listp (sexp obj) {
  while (SEXP_PAIRP(obj))
    obj = SEXP_CDR(obj);
  return (obj == SEXP_NULL);
}

int list_index (sexp ls, sexp elt) {
  int i=0;
  while (SEXP_PAIRP(ls)) {
    if (SEXP_CAR(ls) == elt)
      return i;
    ls = SEXP_CDR(ls);
    i++;
  }
  return -1;
}

sexp memq (sexp x, sexp ls) {
  while (SEXP_PAIRP(ls))
    if (x == SEXP_CAR(ls))
      return ls;
    else
      ls = SEXP_CDR(ls);
  return SEXP_FALSE;
}

sexp assq (sexp x, sexp ls) {
  while (SEXP_PAIRP(ls))
    if (x == SEXP_CAAR(ls))
      return ls;
    else
      ls = SEXP_CDR(ls);
  return SEXP_FALSE;
}

sexp lset_diff(sexp a, sexp b) {
  sexp res = SEXP_NULL;
  for ( ; SEXP_PAIRP(a); a=SEXP_CDR(a))
    if (! list_index(b, SEXP_CAR(a)) >= 0)
      res = cons(SEXP_CAR(a), res);
  return res;
}

sexp reverse(sexp ls) {
  sexp res = SEXP_NULL;
  for ( ; SEXP_PAIRP(ls); ls=SEXP_CDR(ls))
    res = cons(SEXP_CAR(ls), res);
  return res;
}

sexp nreverse(sexp ls) {
  sexp a, b, tmp;
  if (ls == SEXP_NULL) {
    return ls;
  } else if (! SEXP_PAIRP(ls)) {
    return SEXP_ERROR;
  } else {
    b=ls;
    a=cdr(ls);
    set_cdr(b, SEXP_NULL);
    for ( ; SEXP_PAIRP(a); b=a, a=tmp) {
      tmp=cdr(a);
      set_cdr(a, b);
    }
    return b;
  }
}

sexp append(sexp a, sexp b) {
  for (a=reverse(a); SEXP_PAIRP(a); a=SEXP_CDR(a))
    b = cons(SEXP_CAR(a), b);
  return b;
}

sexp list(int count, ...) {
  sexp res = SEXP_NULL;
  int i;
  va_list ap;
  va_start(ap, count);
  for (i=0; i<count; i++)
    res = cons(va_arg(ap, sexp), res);
  va_end(ap);
  return nreverse(res);
}

unsigned long length(sexp ls) {
  sexp x;
  unsigned long res;
  for (res=0, x=ls; SEXP_PAIRP(x); res++, x=cdr(x))
    ;
  return res;
}

/********************* strings, symbols, vectors **********************/

sexp make_string(char *str) {
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

int string_hash(char *str, int acc) {
  while (*str) {acc *= FNV_PRIME; acc ^= *str++;}
  return acc;
}

sexp intern(char *str) {
  struct huff_entry he;
  unsigned long len, res=FNV_OFFSET_BASIS, space=3, newbits, i, d, cell;
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
    res |= (((unsigned long) he.bits) << space);
    space += newbits;
  }
  return (sexp) (res + SEXP_ISYMBOL_TAG);
#endif

 normal_intern:
  res = string_hash(p, res);
  d = symbol_table_primes[symbol_table_prime_index];
  cell = res % d;
  for (i=0; i<d; i++) {
    if (! symbol_table[cell]) {
      break;
    } else if (strncmp(str,
                symbol_data(symbol_table[cell]),
                symbol_length(symbol_table[cell])) == 0) {
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
  symbol_table[cell] = (sexp) (((unsigned long)sym) + 3);
  return symbol_table[cell];
}

sexp make_vector(unsigned long len, sexp dflt) {
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

sexp list_to_vector(sexp ls) {
  sexp vec = make_vector(length(ls), SEXP_FALSE);
  if (vec == SEXP_ERROR) return vec;
  sexp x;
  sexp *elts = vector_data(vec);
  int i;
  for (i=0, x=ls; SEXP_PAIRP(x); i++, x=cdr(x))
    elts[i] = car(x);
  return vec;
}

sexp vector(int count, ...) {
  sexp vec = make_vector(count, SEXP_FALSE);
  if (vec == SEXP_ERROR) return vec;
  sexp *elts = vector_data(vec);
  va_list ap;
  int i;

  va_start(ap, count);
  for (i=0; i<count; i++)
    elts[i] = va_arg(ap, sexp);
  va_end(ap);
  return vec;
}

/************************ reading and writing *************************/

void write_sexp (FILE *out, sexp obj) {
  unsigned long len, i, c, res;
  sexp x, *elts;

  if (! obj) {
    fprintf(out, "#<null>");
  } else if (SEXP_POINTERP(obj)) {
    switch (obj->tag) {
    case SEXP_PAIR:
      fprintf(out, "(");
      write_sexp(out, car(obj));
      for (x=cdr(obj); SEXP_PAIRP(x); x=cdr(x)) {
        fprintf(out, " ");
        write_sexp(out, car(x));
      }
      if (! SEXP_NULLP(x)) {
        fprintf(out, " . ");
        write_sexp(out, x);
      }
      fprintf(out, ")");
      break;
    case SEXP_VECTOR:
      len = vector_length(obj);
      elts = vector_data(obj);
      if (len == 0) {
        fprintf(out, "#()");
      } else {
        fprintf(out, "#(");
        write_sexp(out, elts[0]);
        for (i=1; i<len; i++) {
          fprintf(out, " ");
          write_sexp(out, elts[i]);
        }
        fprintf(out, ")");
      }
      break;
    case SEXP_PROCEDURE:
      fprintf(out, "#<procedure>");
      break;
    case SEXP_BYTECODE:
      fprintf(out, "#<bytecode>");
      break;
    case SEXP_ENV:
      fprintf(out, "#<env>");
      break;
    case SEXP_STRING:
      fprintf(out, "\"");
      /* FALLTHROUGH */
    case SEXP_SYMBOL:
      fprintf(out, "%s", string_data(obj));
      if (obj->tag == SEXP_STRING)
        fprintf(out, "\"");
      break;
    }
  } else if (SEXP_INTEGERP(obj)) {
      fprintf(out, "%d", unbox_integer(obj));
  } else if (SEXP_CHARP(obj)) {
      if (33 <= unbox_character(obj) < 127) {
        fprintf(out, "#\\%c", unbox_character(obj));
      } else {
        fprintf(out, "#\\x%02d", unbox_character(obj));
      }
  } else if (SEXP_SYMBOLP(obj)) {

#ifdef USE_HUFF_SYMS
    if (((unsigned long)obj&7)==7) {
      c = ((unsigned long)obj)>>3;
      while (c) {
#include "sexp-unhuff.c"
        putc(res, out);
      }
    } else
#endif

      fprintf(out, "%s", symbol_data(obj));
  } else {
    switch ((unsigned long) obj) {
    case (int) SEXP_NULL:
      fprintf(out, "()");
      break;
    case (int) SEXP_TRUE:
      fprintf(out, "#t");
      break;
    case (int) SEXP_FALSE:
      fprintf(out, "#f");
      break;
    case (int) SEXP_EOF:
      fprintf(out, "#<eof>");
      break;
    case (int) SEXP_UNDEF:
      fprintf(out, "#<undef>");
      break;
    default:
      fprintf(out, "#<error>");
    }
  }
}

char* read_string(FILE *in) {
  char *buf, *tmp, *res;
  char c;
  int len;

  buf = SEXP_ALLOC(128);
  tmp = buf;

  for (c=fgetc(in); (c != EOF) && (c != '"'); c=fgetc(in)) {
    if (c == '\\') {
      c=fgetc(in);
      switch (c) {
      case 'n':
        c = '\n';
      case 't':
        c = '\t';
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

char* read_symbol(FILE *in, int init) {
  char *buf, *tmp, *res;
  char c;
  int len;

  buf = SEXP_ALLOC(128);
  tmp = buf;

  if (init != EOF)
    *tmp++ = init;

  while (1) {
    c=fgetc(in);
    if (c == EOF || is_separator(c)) {
      ungetc(c, in);
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

int read_number(FILE *in) {
  int res = 0;
  int negativep = 0;
  char c;

  c = fgetc(in);
  if (c == '-') {
    negativep = 1;
  } else if (isdigit(c)) {
    res = c - '0';
  }

  for (c=fgetc(in); isdigit(c); c=fgetc(in)) {
    res = res * 10 + (c - '0');
  }
  ungetc(c, in);

  return negativep ? -res : res;
}

sexp read_sexp_raw (FILE *in) {
  sexp res, tmp, tmp2;
  char *str;
  int c1, c2;

 scan_loop:
  switch (c1 = fgetc(in)) {
  case EOF:
    res = SEXP_EOF;
    break;
  case ';':
    while ((c1 = fgetc(in)) != EOF)
      if (c1 == '\n')
        break;
    /* fallthrough */
  case ' ':
  case '\t':
  case '\n':
    goto scan_loop;
    break;
  case '\'':
    res = read_sexp(in);
    res = list2(the_quote_symbol, res);
    break;
  case '`':
    res = read_sexp(in);
    res = list2(the_quasiquote_symbol, res);
    break;
  case ',':
    if ((c1 = fgetc(in)) == '@') {
      res = read_sexp(in);
      res = list2(the_unquote_splicing_symbol, res);
    } else {
      ungetc(c1, in);
      res = read_sexp(in);
      res = list2(the_unquote_symbol, res);
    }
    break;
  case '"':
    str = read_string(in);
    res = make_string(str);
    SEXP_FREE(str);
    break;
  case '(':
    res = SEXP_NULL;
    tmp = read_sexp_raw(in);
    while ((tmp != SEXP_ERROR) && (tmp != SEXP_EOF) && (tmp != SEXP_CLOSE)) {
      if (tmp == SEXP_RAWDOT) {
        /* dotted list */
        free_sexp(tmp);
        tmp = read_sexp_raw(in);
        if (read_sexp(in) != SEXP_CLOSE) {
          fprintf(stderr, "sexp: multiple tokens in dotted tail\n");
          res = SEXP_ERROR;
        } else {
          tmp2 = res;
          res = nreverse(res);
          set_cdr(tmp2, tmp);
          return res;
        }
      } else {
        res = cons(tmp, res);
        tmp = read_sexp_raw(in);
      }
    }
    if (tmp != SEXP_CLOSE) {
      free_sexp(res);
      res = SEXP_ERROR;
    }
    res = nreverse(res);
    break;
  case '#':
    switch (c1=fgetc(in)) {
/*     case 'b': */
/*     case 'd': */
/*     case 'o': */
/*     case 'x': */
/*     case 'e': */
/*     case 'i': */
    case 'f':
    case 't':
      c2 = fgetc(in);
      if (c2 == EOF || is_separator(c2)) {
        res = (c1 == 't' ? SEXP_TRUE : SEXP_FALSE);
      } else {
        fprintf(stderr, "sexp: invalid syntax #%c%c\n", c1, c2);
        res = SEXP_ERROR;
      }
      ungetc(c2, in);
      break;
    case '(':
      ungetc(c1, in);
      res = read_sexp(in);
      if (! listp(res)) {
        if (res != SEXP_ERROR) {
          fprintf(stderr, "sexp: dotted list not allowed in vector syntax\n");
          free_sexp(res);
          res = SEXP_ERROR;
        }
      } else {
        res = list_to_vector(res);
      }
      break;
    default:
      fprintf(stderr, "sexp: invalid syntax #%c\n", c1);
      res = SEXP_ERROR;
      break;
    }
    break;
  case '.':
    c1 = fgetc(in);
    if (c1 == EOF || is_separator(c1)) {
      res = SEXP_RAWDOT;
    } else if (isdigit(c1)) {
      ungetc(c1,in );
      /* res = read_float_tail(in); */
      res = SEXP_ERROR;
    } else {
      ungetc(c1, in);
      str = read_symbol(in, '.');
      res = intern(str);
      SEXP_FREE(str);
    }
    break;
  case ')':
    res = SEXP_CLOSE;
    break;
  case '+':
  case '-':
    fprintf(stderr, "plus/minus: %c\n", c1);
    c2 = fgetc(in);
    if (c2 == '.' || isdigit(c2)) {
      ungetc(c2, in);
      res = make_integer(read_number(in) * ((c1 == '-') ? -1 : 1));
    } else {
      fprintf(stderr, "... symbol: %c\n", c2);
      ungetc(c2, in);
      str = read_symbol(in, c1);
      res = intern(str);
      SEXP_FREE(str);
    }
    break;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    ungetc(c1, in);
    res = make_integer(read_number(in));
    break;
  default:
    str = read_symbol(in, c1);
    res = intern(str);
    SEXP_FREE(str);
    break;
  }
  return res;
}

sexp read_sexp (FILE *in) {
  sexp res = read_sexp_raw(in);
  if ((res == SEXP_CLOSE) || (res == SEXP_RAWDOT))
    res = SEXP_ERROR;
  return res;
}

void sexp_init() {
  if (! initialized_p) {
    initialized_p = 1;
#ifdef USE_BOEHM
    GC_init();
#endif
    symbol_table = SEXP_ALLOC(symbol_table_primes[0]*sizeof(sexp));
    the_dot_symbol = intern(".");
    the_quote_symbol = intern("quote");
    the_quasiquote_symbol = intern("quasiquote");
    the_unquote_symbol = intern("unquote");
    the_unquote_splicing_symbol = intern("unquote-splicing");
    the_lambda_symbol = intern("lambda");
    the_begin_symbol = intern("begin");
    the_define_symbol = intern("define");
    the_set_x_symbol = intern("set!");
    the_if_symbol = intern("if");
  }
}

