
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* simple tagging
 *   ends in  00:  pointer
 *            01:  fixnum
 *           011:  symbol
 *           111:  immediate symbol
 *          0110:  char
 *          1110:  other immediate object (NULL, TRUE, FALSE)
 */

#define SEXP_FIXNUM_BITS 2
#define SEXP_IMMEDIATE_BITS 3
#define SEXP_EXTENDED_BITS 4

#define SEXP_FIXNUM_MASK 3
#define SEXP_IMMEDIATE_MASK 7
#define SEXP_EXTENDED_MASK 15

#define SEXP_POINTER_TAG 0
#define SEXP_FIXNUM_TAG 1
#define SEXP_LSYMBOL_TAG 3
#define SEXP_ISYMBOL_TAG 7
#define SEXP_CHAR_TAG 6

enum sexp_types {
  SEXP_FIXNUM,
  SEXP_CHAR,
  SEXP_BOOLEAN,
  SEXP_PAIR,
  SEXP_SYMBOL,
  SEXP_STRING,
  SEXP_VECTOR,
  SEXP_PROCEDURE,
  SEXP_ENV,
  SEXP_BYTECODE,
  SEXP_CORE,
  SEXP_OPCODE,
};

typedef struct sexp_struct {
  char tag;
  void *data1;
  void *data2;
} *sexp;

#include "sexp-hufftabs.c"

static int initialized_p = 0;

/* static sexp the_dot_symbol; */
static sexp the_quote_symbol;
static sexp the_quasiquote_symbol;
static sexp the_unquote_symbol;
static sexp the_unquote_splicing_symbol;
static sexp the_lambda_symbol;
static sexp the_begin_symbol;
static sexp the_define_symbol;
static sexp the_set_x_symbol;
static sexp the_if_symbol;

#define MAKE_IMMEDIATE(n)  ((sexp) ((n<<4) + 14))
#define SEXP_NULL   MAKE_IMMEDIATE(0)
#define SEXP_FALSE  MAKE_IMMEDIATE(1)
#define SEXP_TRUE   MAKE_IMMEDIATE(2)
#define SEXP_EOF    MAKE_IMMEDIATE(3)
#define SEXP_UNDEF  MAKE_IMMEDIATE(4)
#define SEXP_ERROR  MAKE_IMMEDIATE(5)
#define SEXP_CLOSE  MAKE_IMMEDIATE(6) /* internal use */
#define SEXP_RAWDOT MAKE_IMMEDIATE(7) /* internal use */

#define SEXP_NULLP(x)    ((x) == SEXP_NULL)
#define SEXP_POINTERP(x) (((unsigned long)(x) & SEXP_FIXNUM_MASK) == SEXP_POINTER_TAG)
#define SEXP_INTEGERP(x) (((unsigned long)(x) & SEXP_FIXNUM_MASK) == SEXP_FIXNUM_TAG)
#define SEXP_ISYMBOLP(x) (((unsigned long)(x) & SEXP_IMMEDIATE_MASK) == SEXP_ISYMBOL_TAG)
#define SEXP_CHARP(x)    (((unsigned long)(x) & SEXP_EXTENDED_MASK) == SEXP_CHAR_TAG)
#define SEXP_BOOLEANP(x) (((x) == SEXP_TRUE) || ((x) == SEXP_FALSE))

#define SEXP_PAIRP(x)     (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_PAIR)
#define SEXP_STRINGP(x)   (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_STRING)
#define SEXP_LSYMBOLP(x)  (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_SYMBOL)
#define SEXP_VECTORP(x)   (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_VECTOR)
#define SEXP_PROCEDUREP(x) (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_PROCEDURE)
#define SEXP_ENVP(x)      (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_ENV)
#define SEXP_BYTECODEP(x) (SEXP_POINTERP(x) && ((sexp)(x))->tag ==SEXP_BYTECODE)
#define SEXP_COREP(x)     (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_CORE)
#define SEXP_OPCODEP(x)   (SEXP_POINTERP(x) && ((sexp)(x))->tag == SEXP_OPCODE)

#define SEXP_SYMBOLP(x)  (SEXP_ISYMBOLP(x) || SEXP_LSYMBOLP(x))

/* #define SEXP_DOTP(x)     (SEXP_SYMBOLP(x) && (strncmp(string_data(x), ".", 2) == 0)) */
/* #define SEXP_DOTP(x)     (x==the_dot_symbol) */
#define SEXP_DOTP(x)     (((unsigned long)(x))==((0x5D00<<SEXP_IMMEDIATE_BITS)+SEXP_ISYMBOL_TAG))

#define SEXP_ALLOC(size) (malloc(size))
#define SEXP_FREE        free
#define SEXP_NEW()       ((sexp) SEXP_ALLOC(sizeof(struct sexp_struct)))

#define make_integer(n)    ((sexp) (((long) n<<SEXP_FIXNUM_BITS) + SEXP_FIXNUM_TAG))
#define unbox_integer(n)   ((long) n>>SEXP_FIXNUM_BITS)
#define make_character(n)  ((sexp) (((long) n<<SEXP_EXTENDED_BITS) + SEXP_CHAR_TAG))
#define unbox_character(n) ((long) n>>SEXP_EXTENDED_BITS)

#define vector_length(x) ((unsigned long) x->data1)
#define vector_data(x)   ((sexp*) x->data2)

#define vector_ref(x, i) (vector_data(x)[unbox_integer(i)])
#define vector_set(x, i, v) (vector_data(x)[unbox_integer(i)] = (v))

#define procedure_code(x) ((bytecode) ((sexp)x)->data1)
#define procedure_vars(x)   ((sexp) ((sexp)x)->data2)

#define string_length(x) ((unsigned long) x->data1)
#define string_data(x)   ((char*) x->data2)

#define symbol_pointer(x) ((sexp) (((unsigned long)x)-SEXP_LSYMBOL_TAG))
#define symbol_length(x)  ((unsigned long) (symbol_pointer(x)->data1))
#define symbol_data(x)    ((char*) (symbol_pointer(x)->data2))

#define sexp_add(a, b) ((sexp)(((unsigned long)a)+((unsigned long)b)-SEXP_FIXNUM_TAG))
#define sexp_sub(a, b) ((sexp)(((unsigned long)a)-((unsigned long)b)+SEXP_FIXNUM_TAG))
#define sexp_mul(a, b) ((sexp)((((((unsigned long)a)-SEXP_FIXNUM_TAG)*(((unsigned long)b)>>SEXP_FIXNUM_BITS))+SEXP_FIXNUM_TAG)))
#define sexp_div(a, b) ((sexp)(((((unsigned long)a)>>SEXP_FIXNUM_BITS)/(((unsigned long)b)>>SEXP_FIXNUM_BITS))<<SEXP_FIXNUM_BITS)+SEXP_FIXNUM_TAG)
#define sexp_mod(a, b) ((sexp)(((((unsigned long)a)>>SEXP_FIXNUM_BITS)%(((unsigned long)b)>>SEXP_FIXNUM_BITS))<<SEXP_FIXNUM_BITS)+SEXP_FIXNUM_TAG)

sexp cons(sexp head, sexp tail) {
  sexp pair = SEXP_NEW();
  if (! pair) return SEXP_ERROR;
  pair->tag = SEXP_PAIR;
  pair->data1 = (void*) head;
  pair->data2 = (void*) tail;
  return pair;
}

#define list2(a, b)       cons(a, cons(b, SEXP_NULL))
#define list3(a, b, c)    cons(a, cons(b, cons(c, SEXP_NULL)))
#define list4(a, b, c, d) cons(a, cons(b, cons(c, cons(d, SEXP_NULL))))

#define SEXP_CAR(x)       (((sexp)x)->data1)
#define SEXP_CDR(x)       (((sexp)x)->data2)

#define SEXP_CAAR(x)      (SEXP_CAR(SEXP_CAR(x)))
#define SEXP_CADR(x)      (SEXP_CAR(SEXP_CDR(x)))
#define SEXP_CDAR(x)      (SEXP_CDR(SEXP_CAR(x)))
#define SEXP_CDDR(x)      (SEXP_CDR(SEXP_CDR(x)))

#define SEXP_CADDR(x)     (SEXP_CAR(SEXP_CDDR(x)))
#define SEXP_CDDDR(x)     (SEXP_CDR(SEXP_CDDR(x)))
#define SEXP_CADDDR(x)    (SEXP_CADR(SEXP_CDDR(x)))
#define SEXP_CDDDDR(x)    (SEXP_CDDR(SEXP_CDDR(x)))

sexp read_sexp (FILE *in);

/* separators: space, tab, newline, ; () [] , ' " */
/* 9 10 11 12 13 32 34 39 40 41 44 59 91 93 */
/* 0 1  2  3  4  23 25 30 31 32 35 50 82 84 */
/* 0000000 */
/* 0000001 */
/* 0000010 */
/* 0000011 */
/* 0000100 */
/* 0010111 */
/* 0011001 */
/* 0011110 */
/* 0011111 */
/* 0100000 */
/* 0100011 */
/* 0110010 */
/* 1010010 */
/* 1010100 */

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

static int is_separator (int c) {
  /* return (!((c-9)&(~3))) | (~(c^4)); */
  return 0<c && c<128 && separators[c];
}

sexp car(sexp obj) {
  return (SEXP_PAIRP(obj)) ? SEXP_CAR(obj) : SEXP_ERROR;
}

sexp cdr(sexp obj) {
  return (SEXP_PAIRP(obj)) ? SEXP_CDR(obj) : SEXP_ERROR;
}

sexp set_car(sexp obj, sexp val) {
  if (SEXP_PAIRP(obj)) {
    return SEXP_CAR(obj) = val;
  } else {
    return SEXP_ERROR;
  }
}

sexp set_cdr(sexp obj, sexp val) {
  if (SEXP_PAIRP(obj)) {
    return SEXP_CDR(obj) = val;
  } else {
    return SEXP_ERROR;
  }
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
  sexp a;
  sexp b;
  sexp tmp;

  if (ls == SEXP_NULL) {
    return ls;
  } else if (! SEXP_PAIRP(ls)) {
    return SEXP_ERROR;
  } else {
    b = ls;
    a=cdr(ls);
    set_cdr(b, SEXP_NULL);
    for ( ; SEXP_PAIRP(a); ) {
      tmp = cdr(a);
      set_cdr(a, b);
      b = a;
      a = tmp;
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
  for (i=0; i<count; i++) {
    res = cons(va_arg(ap, sexp), res);
  }
  va_end(ap);
  return nreverse(res);
}

sexp memq (sexp x, sexp ls) {
  while (SEXP_PAIRP(ls)) {
    if (x == SEXP_CAR(ls))
      return ls;
    else
      ls = SEXP_CDR(ls);
  }
  return SEXP_FALSE;
}

sexp assq (sexp x, sexp ls) {
  while (SEXP_PAIRP(ls)) {
    if (x == SEXP_CAAR(ls))
      return ls;
    else
      ls = SEXP_CDR(ls);
  }
  return SEXP_FALSE;
}

unsigned long length(sexp ls) {
  sexp x;
  unsigned long res;
  for (res=0, x=ls; SEXP_PAIRP(x); res++, x=cdr(x))
    ;
  return res;
}

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

struct huff_entry {
  unsigned char len;
  unsigned short bits;
};

static struct huff_entry huff_table[] = {
#include "sexp-huff.c"
};

/* http://planetmath.org/encyclopedia/GoodHashTablePrimes.html */
static sexp* symbol_table = NULL;
static unsigned long symbol_table_primes[] = {
  97, 389, 1543, 6151, 12289, 24593, 49157, 98317, 196613, 393241,
  786433, 1572869, 3145739, 6291469, 12582917, 25165843, 50331653,
  100663319, 201326611, 402653189, 805306457, 1610612741};
static int symbol_table_prime_index = 0;
static int symbol_table_count = 0;

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
  /* fprintf(stderr, "immediate symbol: %x\n", res); */
  return (sexp) (res + SEXP_ISYMBOL_TAG);

 normal_intern:
  /* fprintf(stderr, "normal intern\n"); */
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
    fprintf(stderr, "resizing symbol table\n");
    newtable = malloc(symbol_table_primes[symbol_table_prime_index++]
                      * sizeof(sexp));
    free(symbol_table);
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
  for (i=0, x=ls; SEXP_PAIRP(x); i++, x=cdr(x)) {
    elts[i] = car(x);
  }
  return vec;
}

sexp vector(int count, ...) {
  sexp vec = make_vector(count, SEXP_FALSE);
  if (vec == SEXP_ERROR) return vec;
  sexp *elts = vector_data(vec);
  va_list ap;
  int i;

  va_start(ap, count);
  for (i=0; i<count; i++) {
    elts[i] = va_arg(ap, sexp);
  }
  va_end(ap);
  return vec;
}

void write_sexp (FILE *out, sexp obj) {
  unsigned long len, i, c, res;
  sexp x;

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
      sexp *elts = vector_data(obj);
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
      if (obj->tag == SEXP_STRING) {
        fprintf(out, "\"");
      }
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

    if (((unsigned long)obj&7)==7) {

      c = ((unsigned long)obj)>>3;

      while (c) {
#include "sexp-unhuff.c"
        putc(res, out);
      }

    } else {
      fprintf(out, "%s", symbol_data(obj));
    }

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

void* free_sexp (sexp obj) {
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
      free(string_data(obj));
      break;
    }

    SEXP_FREE(obj);
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
    free(str);
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
      free(str);
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
      free(str);
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
    free(str);
    break;
  }
  return res;
}

sexp read_sexp (FILE *in) {
  sexp res = read_sexp_raw(in);
  if ((res == SEXP_CLOSE) || (res == SEXP_RAWDOT)) {
    res = SEXP_ERROR;
  }
}

void sexp_init() {

  if (! initialized_p) {

    initialized_p = 1;

    symbol_table = malloc(symbol_table_primes[0]*sizeof(sexp));

    /* the_dot_symbol = intern("."); */
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

/* ******************************************************************** */

#ifdef HAVE_ERR_H
#include <err.h>
#else
#define errx(code, msg, ...) (fprintf(stderr,msg"\n",__VA_ARGS__), exit(code))
#endif

#define INIT_BCODE_SIZE 128
#define INIT_STACK_SIZE 1024

typedef struct bytecode {
  char tag;
  unsigned int len;
  unsigned char data[];
} *bytecode;

/* env binding: #(id chain offset flags) */
/* chain is the index into the closure parent list (0 for current lambda) */
/* macros/constants have a value instead of chain */
typedef struct env {
  char tag;
  struct env *parent;
  sexp bindings;
} *env;

enum core_form_names {
  CORE_DEFINE,
  CORE_SET,
  CORE_LAMBDA,
  CORE_IF,
  CORE_BEGIN,
  CORE_QUOTE,
  CORE_DEFINE_SYNTAX,
  CORE_LET_SYNTAX,
  CORE_LETREC_SYNTAX,
};

typedef struct core_form {
  char tag;
  char* name;
  char code;
} *core_form;

static struct core_form core_forms[] = {
  {SEXP_CORE, "define", CORE_DEFINE},
  {SEXP_CORE, "set!", CORE_SET},
  {SEXP_CORE, "lambda", CORE_LAMBDA},
  {SEXP_CORE, "if", CORE_IF},
  {SEXP_CORE, "begin", CORE_BEGIN},
  {SEXP_CORE, "quote", CORE_QUOTE},
  {SEXP_CORE, "define-syntax", CORE_DEFINE_SYNTAX},
  {SEXP_CORE, "let-syntax", CORE_LET_SYNTAX},
  {SEXP_CORE, "letrec-syntax", CORE_LETREC_SYNTAX},
};

enum opcode_classes {
  OPC_GENERIC,
  OPC_TYPE_PREDICATE,
  OPC_PREDICATE,
  OPC_ARITHMETIC,
  OPC_ARITHMETIC_INV,
  OPC_ARITHMETIC_CMP,
  OPC_CONSTRUCTOR,
};

/* #define OP_UNSAFE(op) ((op)+128) */

enum opcode_names {
  OP_NOOP,                      /* 0 */
  OP_STACK_REF,                 /* 1 */
  OP_STACK_SET,                 /* 2 */
  OP_GLOBAL_REF,                /* 3 */
  OP_GLOBAL_SET,                /* 4 */
  OP_CLOSURE_REF,               /* 5 */
  OP_CLOSURE_SET,               /* 6 */
  OP_VECTOR_REF,                /* 7 */
  OP_VECTOR_SET,                /* 8 */
  OP_MAKE_PROCEDURE,
  OP_MAKE_VECTOR,
  OP_PUSH,
  OP_DUP,                       /* C */
  OP_DROP,
  OP_SWAP,
  OP_CAR,
  OP_CDR,                       /* 10 */
  OP_SET_CAR,                   /* 11 */
  OP_SET_CDR,                   /* 12 */
  OP_CONS,
  OP_ADD,                       /* 14 */
  OP_SUB,
  OP_MUL,                       /* 16 */
  OP_DIV,
  OP_MOD,                       /* 18 */
  OP_NEG,
  OP_INV,                       /* 1A */
  OP_LT,
  OP_CALL,                      /* 1C */
  OP_JUMP_UNLESS,
  OP_JUMP,                      /* 1E */
  OP_RET,
  OP_DONE,
};

static const char* reverse_opcode_names[] =
  {"NOOP", "STACK_REF", "STACK_SET", "GLOBAL_REF", "GLOBAL_SET", "CLOSURE_REF",
   "CLOSURE_SET", "VECTOR_REF", "VECTOR_SET", "MAKE_PROCEDURE", "MAKE_VECTOR",
   "PUSH", "DUP", "DROP", "SWAP", "CAR", "CDR", "SET_CAR", "SET_CDR", "CONS",
   "ADD", "SUB", "MUL", "DIV", "MOD", "NEG", "INV", "LT", "CALL",
   "JUMP_UNLESS", "JUMP", "RET", "DONE"
  };

typedef struct opcode {
  char tag;
  char op_class;
  char op_name;
  char num_args;
  char var_args_p;
  char arg1_type;
  char arg2_type;
  char* name;
  char op_inverse;
  sexp proc;
} *opcode;

static struct opcode opcodes[] = {
{SEXP_OPCODE, OPC_TYPE_PREDICATE, OP_CAR, 1, 0, SEXP_PAIR, 0, "car", 0, NULL},
{SEXP_OPCODE, OPC_TYPE_PREDICATE, OP_CDR, 1, 0, SEXP_PAIR, 0, "cdr", 0, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC,     OP_ADD, 0, 1, SEXP_FIXNUM, 0, "+", 0, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC_INV, OP_SUB, 0, 1, SEXP_FIXNUM, 0, "-", OP_NEG, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC,     OP_MUL, 0, 1, SEXP_FIXNUM, 0, "*", 0, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC_INV, OP_DIV, 0, 1, SEXP_FIXNUM, 0, "/", OP_INV, 0},
{SEXP_OPCODE, OPC_ARITHMETIC,     OP_MOD, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, "%", 0, NULL},
{SEXP_OPCODE, OPC_ARITHMETIC_CMP, OP_LT,  0, 1, SEXP_FIXNUM, 0, "<", 0, NULL},
{SEXP_OPCODE, OPC_CONSTRUCTOR,    OP_CONS, 2, 0, 0, 0, "cons", 0, NULL},
{SEXP_OPCODE, OPC_CONSTRUCTOR,    OP_MAKE_VECTOR, 2, 0, SEXP_FIXNUM, 0, "make-vector", 0, NULL},
{SEXP_OPCODE, OPC_CONSTRUCTOR,    OP_MAKE_PROCEDURE, 2, 0, 0, 0, "make-procedure", 0, NULL},
};

void disasm (bytecode bc) {
  unsigned char *ip=bc->data, opcode;
 loop:
  opcode = *ip++;
  if (opcode*sizeof(char*) < sizeof(reverse_opcode_names)) {
    fprintf(stderr, "  %s ", reverse_opcode_names[opcode]);
  } else {
    fprintf(stderr, "  <unknown> %d ", opcode);
  }
  switch (opcode) {
  case OP_STACK_REF:
  case OP_STACK_SET:
  case OP_CLOSURE_REF:
  case OP_CLOSURE_SET:
    fprintf(stderr, "%d", (long) ((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_GLOBAL_REF:
  case OP_GLOBAL_SET:
  case OP_CALL:
  case OP_PUSH:
    write_sexp(stderr, ((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
  case OP_JUMP:
  case OP_JUMP_UNLESS:
    fprintf(stderr, "%d", ip[0]);
    ip++;
    break;
  }
  fprintf(stderr, "\n");
  if ((! (opcode == OP_RET) || (opcode == OP_DONE))
      && (ip - bc->data < bc->len))
    goto loop;
}

sexp env_cell(env e, sexp key) {
  sexp ls, res=NULL;

  do {
    for (ls=e->bindings; SEXP_PAIRP(ls); ls=SEXP_CDR(ls)) {
      if (SEXP_CAAR(ls) == key) {
        res = SEXP_CAR(ls);
        break;
      }
    }
    e = e->parent;
  } while (e && ! res);

  return res;
}

sexp make_procedure(sexp bc, sexp vars) {
  sexp proc = SEXP_NEW();
  if (! proc) return SEXP_ERROR;
  proc->tag = SEXP_PROCEDURE;
  proc->data1 = (void*) bc;
  proc->data2 = (void*) vars;
  return proc;
}

int env_global_p (env e, sexp id) {
  while (e->parent) {
    if (assq(id, e->bindings) != SEXP_FALSE)
      return 0;
    else
      e = e->parent;
  }
  return 1;
}

void env_define(env e, sexp key, sexp value) {
  sexp cell = env_cell(e, key);
  if (cell) {
    SEXP_CDR(cell) = value;
  } else {
    e->bindings = cons(cons(key, value), e->bindings);
  }
}

env extend_env_closure (env e, sexp fv) {
  int i;
  env e2 = (env) malloc(sizeof(struct env));
  e2->tag = SEXP_ENV;
  e2->parent = e;
  e2->bindings = SEXP_NULL;
  for (i=0; SEXP_PAIRP(fv); fv = SEXP_CDR(fv), i++) {
    e2->bindings = cons(cons(SEXP_CAR(fv), make_integer(i)), e2->bindings);
  }
  return e2;
}

env make_standard_env() {
  int i;
  env e = (env) malloc(sizeof(struct env));
  e->tag = SEXP_ENV;
  e->parent = NULL;
  e->bindings = SEXP_NULL;
  for (i=0; i<(sizeof(core_forms)/sizeof(struct core_form)); i++) {
    env_define(e, intern(core_forms[i].name), (sexp)(&core_forms[i]));
  }
  for (i=0; i<(sizeof(opcodes)/sizeof(struct opcode)); i++) {
    env_define(e, intern(opcodes[i].name), (sexp)(&opcodes[i]));
  }
  return e;
}

/* ******************************************************************** */

/* char *buffncpy(char *buf, unsigned int n, unsigned int len) { */
/*   char *res; */
/*   if (n==len) { */
/*     res = buf; */
/*   } else { */
/*     res = (char*) malloc(n); */
/*     strncpy(res, buf, n); */
/*     free(buf); */
/*   } */
/*   return res; */
/* } */

/* char *buffngrow(char *buf, unsigned int newlen) { */
/*   char *tmp = (char*) malloc(newlen); */
/*   strncpy(tmp, buf, newlen/2); */
/*   free(buf); */
/*   return tmp; */
/* } */

void print_bytecode (bytecode bc) {
  int i;
  fprintf(stderr, "bytecode @ %p, data @ %p, length = %d\n", bc, bc->data, bc->len);
  for (i=0; i+16 < bc->len; i+=8) {
    fprintf(stderr, "%02x: %02x %02x %02x %02x %02x %02x %02x %02x   ", i,
            bc->data[i], bc->data[i+1], bc->data[i+2], bc->data[i+3],
            bc->data[i+4], bc->data[i+5], bc->data[i+6], bc->data[i+7]);
    i += 8;
    fprintf(stderr, "%02x %02x %02x %02x %02x %02x %02x %02x\n",
            bc->data[i], bc->data[i+1], bc->data[i+2], bc->data[i+3],
            bc->data[i+4], bc->data[i+5], bc->data[i+6], bc->data[i+7]);
  }
  if (i != bc->len) {
    fprintf(stderr, "%02x:", i);
    for ( ; i < bc->len; i++) {
      if ((i % 8) == 0 && (i % 16) != 0)
        fprintf(stderr, "  ");
      fprintf(stderr, " %02x", bc->data[i]);
    }
    fprintf(stderr, "\n");
  }
}

void print_stack (sexp *stack, int top) {
  int i;
  for (i=0; i<top; i++) {
    fprintf(stderr, "  %02d: ", i);
    fflush(stderr);
    write_sexp(stderr, stack[i]);
    fprintf(stderr, "\n");
  }
}

void shrink_bcode(bytecode *bc, unsigned int i) {
  bytecode tmp;
  if ((*bc)->len != i) {
    fprintf(stderr, "shrinking to %d\n", i);
    tmp = (bytecode) malloc(sizeof(struct bytecode) + i);
    tmp->tag = SEXP_BYTECODE;
    tmp->len = i;
    memcpy(tmp->data, (*bc)->data, i);
    SEXP_FREE(*bc);
    *bc = tmp;
  }
}

void emit(bytecode *bc, unsigned int *i, char c)  {
  bytecode tmp;
  if ((*bc)->len < (*i)+1) {
    fprintf(stderr, "expanding (%d < %d)\n", (*bc)->len, (*i)+1);
    tmp = (bytecode) malloc(sizeof(unsigned int) + (*bc)->len*2);
    tmp->len = (*bc)->len*2;
    memcpy(tmp->data, (*bc)->data, (*bc)->len);
    SEXP_FREE(*bc);
    *bc = tmp;
  }
  (*bc)->data[(*i)++] = c;
}

void emit_word(bytecode *bc, unsigned int *i, unsigned long val)  {
  bytecode tmp;
  if ((*bc)->len < (*i)+4) {
    tmp = (bytecode) malloc(sizeof(unsigned int) + (*bc)->len*2);
    tmp->len = (*bc)->len*2;
    memcpy(tmp->data, (*bc)->data, (*bc)->len);
    SEXP_FREE(*bc);
    *bc = tmp;
  }
  *((unsigned long*)(&((*bc)->data[*i]))) = val;
  *i += sizeof(unsigned long);
}

bytecode compile(sexp params, sexp obj, env e, sexp fv, sexp sv, int done_p);
void analyze_app (sexp obj, bytecode *bc, unsigned int *i,
                  env e, sexp params, sexp fv, sexp sv, unsigned int *d);
void analyze_lambda (sexp name, sexp formals, sexp body,
                     bytecode *bc, unsigned int *i, env e,
                     sexp params, sexp fv, sexp sv, unsigned int *d);
void analyze_var_ref (sexp name, bytecode *bc, unsigned int *i, env e,
                      sexp params, sexp fv, sexp sv, unsigned int *d);

void analyze(sexp obj, bytecode *bc, unsigned int *i, env e,
             sexp params, sexp fv, sexp sv, unsigned int *d) {
  int tmp1, tmp2;
  env e2 = e;
  sexp o1, o2, cell;

  if (SEXP_PAIRP(obj)) {
    /* fprintf(stderr, ":: pair\n"); */
    if (SEXP_SYMBOLP(SEXP_CAR(obj))) {
      fprintf(stderr, ":: symbol application\n");
      o1 = env_cell(e, SEXP_CAR(obj));
      /* fprintf(stderr, ":: => %p\n", o1); */
      if (! o1)
        errx(1, "unknown operator: %s", SEXP_CAR(obj));
      o1 = SEXP_CDR(o1);
      /* fprintf(stderr, ":: => %p\n", o1); */
      if (SEXP_COREP(o1)) {
        /* core form */
        fprintf(stderr, ":: core form\n");
        switch (((core_form)o1)->code) {
        case CORE_LAMBDA:
          fprintf(stderr, ":: lambda\n");
          analyze_lambda(SEXP_FALSE, SEXP_CADR(obj), SEXP_CDDR(obj),
                         bc, i, e, params, fv, sv, d);
          break;
        case CORE_DEFINE:
          fprintf(stderr, "compiling global set: %p\n", SEXP_CADR(obj));
          if ((((core_form)o1)->code == CORE_DEFINE)
              && SEXP_PAIRP(SEXP_CADR(obj))) {
            analyze_lambda(SEXP_CAR(SEXP_CADR(obj)),
                           SEXP_CDR(SEXP_CADR(obj)),
                           SEXP_CDDR(obj),
                           bc, i, e, params, fv, sv, d);
          } else {
            analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d);
          }
          emit(bc, i, OP_GLOBAL_SET);
          emit_word(bc, i, (unsigned long) (SEXP_PAIRP(SEXP_CADR(obj))
                                            ? SEXP_CAR(SEXP_CADR(obj))
                                            : SEXP_CADR(obj)));
          emit(bc, i, OP_PUSH);
          (*d)++;
          emit_word(bc, i, (unsigned long) SEXP_UNDEF);
          break;
        case CORE_SET:
          fprintf(stderr, "set!: "); write_sexp(stderr, SEXP_CADR(obj));
          fprintf(stderr, " sv: ");  write_sexp(stderr, sv);
          fprintf(stderr, "\n");
          analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d);
          analyze_var_ref(SEXP_CADR(obj), bc, i, e, params, fv, SEXP_NULL, d);
          emit(bc, i, OP_SET_CAR);
          break;
        case CORE_BEGIN:
          for (o2 = SEXP_CDR(obj); SEXP_PAIRP(o2); o2 = SEXP_CDR(o2)) {
            analyze(SEXP_CAR(o2), bc, i, e, params, fv, sv, d);
            if (SEXP_PAIRP(SEXP_CDR(o2))) emit(bc, i, OP_DROP);
          }
          break;
        case CORE_IF:
          fprintf(stderr, "test clause: %d\n", *i);
          analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d);
          emit(bc, i, OP_JUMP_UNLESS);              /* jumps if test fails */
          tmp1 = *i;
          emit(bc, i, 0);
          fprintf(stderr, "pass clause: %d\n", *i);
          analyze(SEXP_CADDR(obj), bc, i, e, params, fv, sv, d);
          emit(bc, i, OP_JUMP);
          tmp2 = *i;
          emit(bc, i, 0);
          ((signed char*) (*bc)->data)[tmp1] = (*i)-tmp1-1;    /* patch */
          fprintf(stderr, "fail clause: %d\n", *i);
          if (SEXP_PAIRP(SEXP_CDDDR(obj))) {
            analyze(SEXP_CADDDR(obj), bc, i, e, params, fv, sv, d);
          } else {
            emit(bc, i, OP_PUSH);
            (*d)++;
            emit_word(bc, i, (unsigned long) SEXP_UNDEF);
          }
          ((signed char*) (*bc)->data)[tmp2] = (*i)-tmp2-1;    /* patch */
          break;
        case CORE_QUOTE:
          emit(bc, i, OP_PUSH);
          (*d)++;
          emit_word(bc, i, (unsigned long)SEXP_CADR(obj));
          break;
        default:
          errx(1, "unknown core form: %s", ((core_form)o1)->code);
        }
      } else if (SEXP_OPCODEP(o1)) {
        fprintf(stderr, ":: opcode\n");
        /* direct opcode */
        /* verify arity */
        switch (((opcode)o1)->op_class) {
        case OPC_TYPE_PREDICATE:
        case OPC_PREDICATE:
        case OPC_ARITHMETIC:
        case OPC_ARITHMETIC_INV:
        case OPC_ARITHMETIC_CMP:
          if (SEXP_NULLP(SEXP_CDR(obj))) {
            errx(1, "unknown opcode class: %d", ((opcode)o1)->op_class);
          } else if (SEXP_NULLP(SEXP_CDDR(obj))) {
            if (((opcode)o1)->op_class == OPC_ARITHMETIC_INV) {
              analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d);
              emit(bc, i, ((opcode)o1)->op_inverse);
            } else {
              analyze(SEXP_CADR(obj), bc, i, e, params, fv, sv, d);
            }
          } else {
            /* fprintf(stderr, ":: class: %d\n", ((opcode)o1)->op_class); */
            for (o2 = reverse(SEXP_CDR(obj)); SEXP_PAIRP(o2); o2 = SEXP_CDR(o2)) {
              /* fprintf(stderr, ":: arg: %d\n", SEXP_CAR(o2)); */
              analyze(SEXP_CAR(o2), bc, i, e, params, fv, sv, d);
            }
            fprintf(stderr, ":: name: %d\n", ((opcode)o1)->op_name);
            emit(bc, i, ((opcode)o1)->op_name);
            (*d) -= length(SEXP_CDDR(obj));
          }
          break;
        default:
          errx(1, "unknown opcode class: %d", ((opcode)o1)->op_class);
        }
      } else {
        /* function call */
        analyze_app(obj, bc, i, e, params, fv, sv, d);
      }
    } else if (SEXP_PAIRP(SEXP_CAR(obj))) {
      o2 = env_cell(e, SEXP_CAAR(obj));
/*       if (o2 */
/*           && SEXP_COREP(SEXP_CDR(o2)) */
/*           && (((core_form)SEXP_CDR(o2))->code == CORE_LAMBDA)) { */
/*         /\* let *\/ */
/*       } else { */
        /* computed application */
      analyze_app(obj, bc, i, e, params, fv, sv, d);
/*       } */
    } else {
      errx(1, "invalid operator: %s", SEXP_CAR(obj));
    }
  } else if (SEXP_SYMBOLP(obj)) {
    analyze_var_ref(obj, bc, i, e, params, fv, sv, d);
  } else {
    fprintf(stderr, "push: %d\n", (unsigned long)obj);
    emit(bc, i, OP_PUSH);
    emit_word(bc, i, (unsigned long)obj);
    (*d)++;
  }
}

void analyze_var_ref (sexp obj, bytecode *bc, unsigned int *i, env e,
                      sexp params, sexp fv, sexp sv, unsigned int *d) {
  int tmp;
  /* variable reference */
  /* cell = env_cell(e, obj); */
  fprintf(stderr, "symbol lookup, param length: %d sv: ", length(params));
  write_sexp(stderr, sv);
  fprintf(stderr, "\n");
  if ((tmp = list_index(params, obj)) >= 0) {
    fprintf(stderr, "compiling local ref: %p => %d (d = %d)\n", obj, tmp, *d);
    emit(bc, i, OP_STACK_REF);
    emit_word(bc, i, tmp + *d + 4);
    (*d)++;
  } else if ((tmp = list_index(fv, obj)) >= 0) {
    fprintf(stderr, "compiling closure ref: %p => %d\n", obj, tmp);
    emit(bc, i, OP_CLOSURE_REF);
    emit_word(bc, i, tmp);
    (*d)++;
  } else {
    fprintf(stderr, "compiling global ref: %p\n", obj);
    emit(bc, i, OP_GLOBAL_REF);
    emit_word(bc, i, (unsigned long) obj);
    (*d)++;
  }
  if (list_index(sv, obj) >= 0) {
    fprintf(stderr, "mutable variables, fetching CAR\n");
    emit(bc, i, OP_CAR);
  }
}

void analyze_app (sexp obj, bytecode *bc, unsigned int *i,
                  env e, sexp params, sexp fv, sexp sv, unsigned int *d) {
  sexp o1;
  unsigned long len = length(SEXP_CDR(obj));

  /* push the arguments onto the stack */
  for (o1 = reverse(SEXP_CDR(obj)); SEXP_PAIRP(o1); o1 = SEXP_CDR(o1)) {
    analyze(SEXP_CAR(o1), bc, i, e, params, fv, sv, d);
  }

  /* push the operator onto the stack */
  analyze(SEXP_CAR(obj), bc, i, e, params, fv, sv, d);

  /* make the call */
  emit(bc, i, OP_CALL);
  emit_word(bc, i, (unsigned long) make_integer(len));
}

sexp free_vars (env e, sexp formals, sexp obj, sexp fv) {
  sexp o1;
  if (SEXP_SYMBOLP(obj)) {
    if (env_global_p(e, obj)
        || (list_index(formals, obj) >= 0)
        || (list_index(fv, obj) >= 0))
      return fv;
    else
      return cons(obj, fv);
  } else if (SEXP_PAIRP(obj)) {
    if (SEXP_SYMBOLP(SEXP_CAR(obj))) {
      if ((o1 = env_cell(e, SEXP_CAR(obj)))
          && SEXP_COREP(o1)
          && (((core_form)SEXP_CDR(o1))->code == CORE_LAMBDA)) {
        return free_vars(e, SEXP_CADR(obj), SEXP_CADDR(obj), fv);
      }
    }
    while (SEXP_PAIRP(obj)) {
      fv = free_vars(e, formals, SEXP_CAR(obj), fv);
      obj = SEXP_CDR(obj);
    }
    return fv;
  } else {
    return fv;
  }
}

sexp set_vars (env e, sexp formals, sexp obj, sexp sv) {
  sexp tmp;
  if (SEXP_NULLP(formals))
    return sv;
  if (SEXP_PAIRP(obj)) {
    if (SEXP_SYMBOLP(SEXP_CAR(obj))) {
      if ((tmp = env_cell(e, SEXP_CAR(obj))) && SEXP_COREP(SEXP_CDR(tmp))) {
        if (((core_form)SEXP_CDR(tmp))->code == CORE_LAMBDA) {
          formals = lset_diff(formals, SEXP_CADR(obj));
          return set_vars(e, formals, SEXP_CADDR(obj), sv);
        } else if (((core_form)SEXP_CDR(tmp))->code == CORE_SET) {
          if ((list_index(formals, SEXP_CADR(obj)) >= 0)
              && ! (list_index(sv, SEXP_CADR(obj)) >= 0)) {
            fprintf(stderr, "found set! "); write_sexp(stderr, SEXP_CADR(obj));
            fprintf(stderr, "\n");
            sv = cons(SEXP_CADR(obj), sv);
            return set_vars(e, formals, SEXP_CADDR(obj), sv);
          }
        }
      }
    }
    while (SEXP_PAIRP(obj)) {
      sv = set_vars(e, formals, SEXP_CAR(obj), sv);
      obj = SEXP_CDR(obj);
    }
  }
  return sv;
}

void analyze_lambda (sexp name, sexp formals, sexp body,
                     bytecode *bc, unsigned int *i, env e,
                     sexp params, sexp fv, sexp sv, unsigned int *d) {
  sexp obj;
  sexp fv2 = free_vars(e, formals, body, SEXP_NULL), ls;
  env e2 = extend_env_closure(e, formals);
  int k;
  fprintf(stderr, "%d free-vars\n", length(fv2));
  write_sexp(stderr, fv2);
  fprintf(stderr, "\n");
  obj = (sexp) compile(formals, body, e2, fv2, sv, 0);
  emit(bc, i, OP_PUSH);
  emit_word(bc, i, (unsigned long) SEXP_UNDEF);
  emit(bc, i, OP_PUSH);
  emit_word(bc, i, (unsigned long) make_integer(length(fv2)));
  emit(bc, i, OP_MAKE_VECTOR);
  (*d)++;
  for (ls=fv2, k=0; SEXP_PAIRP(ls); ls=SEXP_CDR(ls), k++) {
    analyze_var_ref(SEXP_CAR(ls), bc, i, e, params, fv, SEXP_NULL, d);
    emit(bc, i, OP_PUSH);
    emit_word(bc, i, (unsigned long) make_integer(k));
    emit(bc, i, OP_STACK_REF);
    emit_word(bc, i, 3);
    emit(bc, i, OP_VECTOR_SET);
    emit(bc, i, OP_DROP);
    (*d)--;
  }
  emit(bc, i, OP_PUSH);
  emit_word(bc, i, (unsigned long) obj);
  emit(bc, i, OP_MAKE_PROCEDURE);
}

sexp vm(bytecode bc, env e, sexp* stack, unsigned int top) {
  unsigned char *ip=bc->data;
  sexp cp, tmp;
  int i;

 loop:
  /* fprintf(stderr, "opcode: %d, ip: %d\n", *ip, ip); */
  /* print_bytecode(bc); */
  switch (*ip++) {
  case OP_NOOP:
    fprintf(stderr, "noop\n");
    break;
  case OP_GLOBAL_REF:
    fprintf(stderr, "global ref: ip: %p => %p: ", ip, ((sexp*)ip)[0]);
    fflush(stderr);
    write_sexp(stderr, ((sexp*)ip)[0]);
    fprintf(stderr, "\n");
    tmp = env_cell(e, ((sexp*)ip)[0]);
    stack[top++]=SEXP_CDR(tmp);
    ip += sizeof(sexp);
    break;
  case OP_GLOBAL_SET:
    fprintf(stderr, "global set: %p: ", ((sexp*)ip)[0]);
    fflush(stderr);
    write_sexp(stderr, ((sexp*)ip)[0]);
    fprintf(stderr, "\n");
    env_define(e, ((sexp*)ip)[0], stack[--top]);
    ip += sizeof(sexp);
    break;
  case OP_STACK_REF:
    fprintf(stderr, "stack ref: ip=%p,  %d - %d => ",
            ip, top, (unsigned long) ((sexp*)ip)[0]);
    fflush(stderr);
    write_sexp(stderr, stack[top - (unsigned int) ((sexp*)ip)[0]]);
    fprintf(stderr, "\n");
    stack[top] = stack[top - (unsigned int) ((sexp*)ip)[0]];
    ip += sizeof(sexp);
    top++;
    break;
  case OP_STACK_SET:
    stack[top - (unsigned int) ((sexp*)ip)[0]] = stack[top-1];
    stack[top-1] = SEXP_UNDEF;
    ip += sizeof(sexp);
    break;
  case OP_CLOSURE_REF:
    fprintf(stderr, "closure-ref %d => ", ((sexp*)ip)[0]);
    fflush(stderr);
    write_sexp(stderr, vector_ref(cp,((sexp*)ip)[0]));
    fprintf(stderr, "\n");
    stack[top++]=vector_ref(cp,((sexp*)ip)[0]);
    ip += sizeof(sexp);
    break;
/*   case OP_CLOSURE_SET: */
/*     cp[*ip++]=stack[--top]; */
/*     break; */
  case OP_VECTOR_REF:
    stack[top-2]=vector_ref(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_VECTOR_SET:
    fprintf(stderr, "vector-set! %p %d => ", stack[top-1], unbox_integer(stack[top-2]));
    write_sexp(stderr, stack[top-3]);
    fprintf(stderr, "\n");
    vector_set(stack[top-1], stack[top-2], stack[top-3]);
    stack[top-3]=SEXP_UNDEF;
    top-=2;
    break;
  case OP_MAKE_PROCEDURE:
    stack[top-2]=make_procedure(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_MAKE_VECTOR:
    stack[top-2]=make_vector(unbox_integer(stack[top-1]), stack[top-2]);
    top--;
    break;
  case OP_PUSH:
    /* fprintf(stderr, " (push)\n"); */
    stack[top++]=((sexp*)ip)[0];
    ip += sizeof(sexp);
    break;
  case OP_DUP:
    stack[top]=stack[top-1];
    top++;
    break;
  case OP_DROP:
    top--;
    break;
  case OP_SWAP:
    tmp = stack[top-2];
    stack[top-2]=stack[top-1];
    stack[top-1]=tmp;
    break;
  case OP_CAR:
    stack[top-1]=car(stack[top-1]);
    break;
  case OP_CDR:
    stack[top-1]=cdr(stack[top-1]);
    break;
  case OP_SET_CAR:
    set_car(stack[top-1], stack[top-2]);
    stack[top-2]=SEXP_UNDEF;
    top--;
    break;
  case OP_SET_CDR:
    set_cdr(stack[top-1], stack[top-2]);
    stack[top-2]=SEXP_UNDEF;
    top--;
    break;
  case OP_CONS:
    stack[top-2]=cons(stack[top-1], stack[top-2]);
    top--;
    break;
  case OP_ADD:
    fprintf(stderr, "OP_ADD %d %d\n", stack[top-1], stack[top-2]);
    stack[top-2]=sexp_add(stack[top-1],stack[top-2]);
    top--;
    break;
  case OP_SUB:
    stack[top-2]=sexp_sub(stack[top-1],stack[top-2]);
    top--;
    break;
  case OP_MUL:
    stack[top-2]=sexp_mul(stack[top-2],stack[top-1]);
    top--;
    break;
  case OP_DIV:
    stack[top-2]=sexp_div(stack[top-2],stack[top-1]);
    top--;
    break;
  case OP_MOD:
    stack[top-2]=sexp_mod(stack[top-2],stack[top-1]);
    top--;
    break;
  case OP_LT:
    stack[top-2]=((stack[top-2] < stack[top-1]) ? SEXP_TRUE : SEXP_FALSE);
    top--;
    break;
  case OP_CALL:
    fprintf(stderr, "CALL\n");
    i = (unsigned long) ((sexp*)ip)[0];
    tmp = stack[top-1];
    if (! SEXP_PROCEDUREP(tmp))
      errx(2, "non-procedure application: %p", tmp);
    stack[top-1] = (sexp) i;
    stack[top] = (sexp) (ip+4);
    stack[top+1] = cp;
    top+=2;
    bc = procedure_code(tmp);
    print_bytecode(bc);
    ip = bc->data;
    cp = procedure_vars(tmp);
    fprintf(stderr, "... calling procedure at %p\ncp: ", ip);
    write_sexp(stderr, cp);
    fprintf(stderr, "\n");
    /* print_stack(stack, top); */
    break;
  case OP_JUMP_UNLESS:
    fprintf(stderr, "JUMP UNLESS, stack top is %d\n", stack[top-1]);
    if (stack[--top] == SEXP_FALSE) {
      fprintf(stderr, "test passed, jumping to + %d => %d\n", ((signed char*)ip)[0], ip + ((signed char*)ip)[0]);
      ip += ((signed char*)ip)[0];
    } else {
      fprintf(stderr, "test failed, not jumping\n");
      ip++;
    }
    break;
  case OP_JUMP:
    fprintf(stderr, "jumping to + %d => %d\n", ((signed char*)ip)[0], ip + ((signed char*)ip)[0]);
    ip += ((signed char*)ip)[0];
    break;
  case OP_RET:
    fprintf(stderr, "returning @ %d: ", top-1);
    fflush(stderr);
    write_sexp(stderr, stack[top-1]);
    fprintf(stderr, "...\n");
    print_stack(stack, top);
    /*                      top-1  */
    /* stack: args ... n ip result */
    cp = stack[top-2];
    fprintf(stderr, "1\n");
    ip = (unsigned char*) stack[top-3];
    fprintf(stderr, "2\n");
    i = unbox_integer(stack[top-4]);
    fprintf(stderr, "3 (i=%d)\n", i);
    stack[top-i-4] = stack[top-1];
    fprintf(stderr, "4\n");
    top = top-i-3;
    fprintf(stderr, "... done returning\n");
    break;
  case OP_DONE:
    fprintf(stderr, "finally returning @ %d: ", top-1);
    fflush(stderr);
    write_sexp(stderr, stack[top-1]);
    fprintf(stderr, "\n");
    goto end_loop;
  default:
    fprintf(stderr, "unknown opcode: %d\n", *(ip-1));
    stack[top] = SEXP_ERROR;
    goto end_loop;
  }
  fprintf(stderr, "looping\n");
  goto loop;

 end_loop:
  return stack[top-1];
}

bytecode compile(sexp params, sexp obj, env e, sexp fv, sexp sv, int done_p) {
  unsigned int i = 0, j, d = 0;
  bytecode bc = (bytecode) malloc(sizeof(struct bytecode)+INIT_BCODE_SIZE);
  sexp sv2 = set_vars(e, params, obj, SEXP_NULL), ls;
  fprintf(stderr, "set-vars: "); write_sexp(stderr, sv2); fprintf(stderr, "\n");
  bc->tag = SEXP_BYTECODE;
  bc->len = INIT_BCODE_SIZE;
  fprintf(stderr, "analyzing\n");
  for (ls=params; SEXP_PAIRP(ls); ls=SEXP_CDR(ls)) {
    if ((j = list_index(sv2, SEXP_CAR(ls)) >= 0)) {
      fprintf(stderr, "consing mutable var\n");
      emit(&bc, &i, OP_PUSH);
      emit_word(&bc, &i, (unsigned long) SEXP_NULL);
      emit(&bc, &i, OP_STACK_REF);
      emit_word(&bc, &i, j+3);
      emit(&bc, &i, OP_CONS);
      emit(&bc, &i, OP_STACK_SET);
      emit_word(&bc, &i, j+4);
      emit(&bc, &i, OP_DROP);
    }
  }
  sv = append(sv2, sv);
  for ( ; SEXP_PAIRP(obj); obj=SEXP_CDR(obj)) {
    fprintf(stderr, "loop: "); write_sexp(stderr, obj); fprintf(stderr, "\n");
    analyze(SEXP_CAR(obj), &bc, &i, e, params, fv, sv, &d);
    if (SEXP_PAIRP(SEXP_CDR(obj))) emit(&bc, &i, OP_DROP);
  }
  emit(&bc, &i, done_p ? OP_DONE : OP_RET);
  /* fprintf(stderr, "shrinking\n"); */
  shrink_bcode(&bc, i);
  fprintf(stderr, "done compiling:\n");
  print_bytecode(bc);
  disasm(bc);
  return bc;
}

sexp eval_in_stack(sexp obj, env e, sexp* stack, unsigned int top) {
  bytecode bc = compile(SEXP_NULL, cons(obj, SEXP_NULL), e, SEXP_NULL, SEXP_NULL, 1);
  fprintf(stderr, "evaling\n");
  return vm(bc, e, stack, top);
}

sexp eval(sexp obj, env e) {
  sexp* stack = (sexp*) malloc(sizeof(sexp) * INIT_STACK_SIZE);
  sexp res = eval_in_stack(obj, e, stack, 0);
  free(stack);
  return res;
}

int main (int argc, char **argv) {
  sexp obj, res, *stack;
  env e;

  sexp_init();
  e = make_standard_env();
  stack = (sexp*) malloc(sizeof(sexp) * INIT_STACK_SIZE);

  /* repl */
  fprintf(stdout, "> ");
  fflush(stdout);
  while ((obj = read_sexp(stdin)) != SEXP_EOF) {
    write_sexp(stdout, obj);
    fprintf(stdout, "\n => ");
    res = eval_in_stack(obj, e, stack, 0);
    /* fprintf(stderr, " (=> %d)\n", res); */
    write_sexp(stdout, res);
    fprintf(stdout, "\n> ");
    fflush(stdout);
  }
  fprintf(stdout, "\n");
  return 0;
}

