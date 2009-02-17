
/* #include <ctype.h> */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* simple tagging
 *   ends in  00:  pointer
 *             1:  fixnum
 *           010:  symbol
 *          0110:  char
 *          1110:  other immediate object (NULL, TRUE, FALSE)
 */

enum sexp_tags {
  SEXP_PAIR,
  SEXP_SYMBOL,
  SEXP_STRING,
  SEXP_VECTOR,
};

/* would need a proper header for GC */
typedef struct sexp_struct {
  char tag;
  void *data1;
  void *data2;
} *sexp;

#define MAKE_IMMEDIATE(n)  ((sexp) ((n<<3) + 6))
#define SEXP_NULL   MAKE_IMMEDIATE(0)
#define SEXP_FALSE  MAKE_IMMEDIATE(1)
#define SEXP_TRUE   MAKE_IMMEDIATE(2)
#define SEXP_EOF    MAKE_IMMEDIATE(3)
#define SEXP_UNDEF  MAKE_IMMEDIATE(4)
#define SEXP_CLOSE  MAKE_IMMEDIATE(5) /* internal use */
#define SEXP_ERROR  MAKE_IMMEDIATE(6)

#define SEXP_NULLP(x)    ((x) == SEXP_NULL)
#define SEXP_POINTERP(x) (((int) x & 3) == 0)
#define SEXP_INTEGERP(x) (((int) x & 3) == 1)
#define SEXP_CHARP(x)    (((int) x & 7) == 2)

#define SEXP_PAIRP(x)    (SEXP_POINTERP(x) && (x)->tag == SEXP_PAIR)
#define SEXP_SYMBOLP(x)  (SEXP_POINTERP(x) && (x)->tag == SEXP_SYMBOL)
#define SEXP_STRINGP(x)  (SEXP_POINTERP(x) && (x)->tag == SEXP_STRING)

#define SEXP_ALLOC(size) (malloc(size))
#define SEXP_FREE        free
#define SEXP_NEW()       ((sexp) SEXP_ALLOC(sizeof(sexp)))

#define make_integer(n)    ((sexp) (((int) n<<2) + 1))
#define unbox_integer(n)   ((int) n>>2)
#define make_character(n)  ((sexp) (((int) n<<3) + 2))
#define unbox_character(n) ((int) n>>3)

#define vector_length(x) ((int) x->data1)
#define vector_data(x)   ((sexp*) x->data2)

#define string_length(x) ((int) x->data1)
#define string_data(x)   ((char*) x->data2)

sexp cons(sexp head, sexp tail) {
  sexp pair = SEXP_NEW();
  if (! pair) return SEXP_ERROR;
  pair->tag = SEXP_PAIR;
  pair->data1 = (void*) head;
  pair->data2 = (void*) tail;
  return pair;
}

sexp car(sexp obj) {
  return (SEXP_PAIRP(obj)) ? obj->data1 : SEXP_ERROR;
}

sexp cdr(sexp obj) {
  return (SEXP_PAIRP(obj)) ? obj->data2 : SEXP_ERROR;
}

sexp set_car(sexp obj, sexp val) {
  if (SEXP_PAIRP(obj)) {
    return obj->data1 = val;
  } else {
    return SEXP_ERROR;
  }
}

sexp set_cdr(sexp obj, sexp val) {
  if (SEXP_PAIRP(obj)) {
    return obj->data2 = val;
  } else {
    return SEXP_ERROR;
  }
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

sexp list(int count, ...) {
  sexp res = SEXP_NULL;
  sexp elt;
  int i;
  va_list ap;

  va_start(ap, count);
  for (i=0; i<count; i++) {
    res = cons(va_arg(ap, sexp), res);
  }
  va_end(ap);
  return nreverse(res);
}

int length(sexp ls) {
  sexp x;
  int res;
  for (res=0, x=ls; SEXP_PAIRP(x); res++, x=cdr(x))
    ;
  return res;
}

sexp make_string(char *str) {
  sexp s = SEXP_NEW();
  if (! s) return SEXP_ERROR;
  int len = strlen(str);
  char *mystr = SEXP_ALLOC(len+1);
  if (! mystr) { SEXP_FREE(s); return SEXP_ERROR; }
  strncpy(mystr, str, len+1);
  s->tag = SEXP_STRING;
  s->data1 = (void*) len;
  s->data2 = (void*) mystr;
  return s;
}

sexp intern(char *str) {
  sexp sym = SEXP_NEW();
  if (! sym) return SEXP_ERROR;
  int len = strlen(str);
  char *mystr = SEXP_ALLOC(len+1);
  if (! mystr) { SEXP_FREE(sym); return SEXP_ERROR; }
  strncpy(mystr, str, len+1);
  sym->tag = SEXP_SYMBOL;
  sym->data1 = (void*) len;
  sym->data2 = (void*) mystr;
  return sym;
}

sexp make_vector(int len, sexp dflt) {
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
  int len, i;
  sexp x;

  if (SEXP_POINTERP(obj)) {

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

  } else {

    switch ((int) obj) {
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

enum sexp_tokens {
  TOK_OPEN_LIST,
  TOK_OPEN_VECTOR,
  TOK_CLOSE,
  TOK_START_STRING,
  TOK_SYMBOL,
  TOK_NUMBER,
  TOK_QUOTE,
  TOK_QUASIQUOTE,
  TOK_UNQUOTE,
  TOK_UNQUOTE_SPLICING,
  TOK_EOF,
  TOK_TRUE,
  TOK_FALSE,
  TOK_ERROR,
};

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
  strncpy(res, buf, len);
  SEXP_FREE(buf);
  return res;
}

char* read_symbol(FILE *in) {
  char *buf, *tmp, *res;
  char c;
  int len;

  buf = SEXP_ALLOC(128);
  tmp = buf;

  while (1) {
    c=fgetc(in);
    switch (c) {
    case '(': case ')': case ';': case ' ': case '\t': case '\r': case '\n':
    case '\'': case '"': case ',': case EOF:
      ungetc(c, in);
      goto done;
      break;
    }
    *tmp++ = c;
  }
 done:

  *tmp++ = '\0';
  len = tmp - buf;
  res = SEXP_ALLOC(len);
  strncpy(res, buf, len);
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

int read_token (FILE *in) {
  int c;
 scan_loop:
  while (isspace(c=fgetc(in)))
    ;
  if (c == EOF)
    return TOK_EOF;
  switch (c) {
  case ';':
    while ((c=fgetc(in)) != '\n')
      ;
    goto scan_loop;
    break;
  case '\'':
    return TOK_QUOTE;
  case '`':
    return TOK_QUASIQUOTE;
  case '"':
    return TOK_START_STRING;
  case '(':
    return TOK_OPEN_LIST;
  case ')':
    return TOK_CLOSE;
  case ',':
    c = fgetc(in);
    if (c == '@') {
      return TOK_UNQUOTE_SPLICING;
    } else {
      ungetc(c, in);
      return TOK_UNQUOTE;
    }
  case '#':
    c = fgetc(in);
    switch (c) {
    case '(':
      return TOK_OPEN_VECTOR;
    case 't':
      return TOK_TRUE;
    case 'f':
      return TOK_FALSE;
/*     case ';': */
/*       read_sexp(in); */
/*       goto scan_loop; */
/*     case 'b': */
/*       return TOK_BINARY; */
/*     case 'o': */
/*       return TOK_OCTAL; */
/*     case 'o': */
/*       return TOK_DECIMAL; */
/*     case 'x': */
/*       return TOK_HEXADECIMAL; */
/*     case 'e': */
/*       return TOK_EXACT; */
/*     case 'i': */
/*       return TOK_INEXACT; */
    default:
      return TOK_ERROR;
    }
/*   case '+': */
/*   case '-': */
  default:
    ungetc(c, in);
    return (isdigit(c) || c == '+' || c == '-') ? TOK_NUMBER : TOK_SYMBOL;
  }
}

sexp read_sexp (FILE *in) {
  sexp res, tmp, tmp2;
  char *str;
  int tok = read_token(in);

  switch (tok) {
  case TOK_EOF:
    res = SEXP_EOF;
    break;
  case TOK_TRUE:
    res = SEXP_TRUE;
    break;
  case TOK_FALSE:
    res = SEXP_FALSE;
    break;
  case TOK_QUOTE:
    res = read_sexp(in);
    res = list(2, intern("quote"), res);
    break;
  case TOK_QUASIQUOTE:
    res = read_sexp(in);
    res = list(2, intern("quasiquote"), res);
    break;
  case TOK_UNQUOTE:
    res = read_sexp(in);
    res = list(2, intern("unquote"), res);
    break;
  case TOK_UNQUOTE_SPLICING:
    res = read_sexp(in);
    res = list(2, intern("unquote-splicing"), res);
    break;
  case TOK_OPEN_LIST:
  case TOK_OPEN_VECTOR:
    res = SEXP_NULL;
    tmp = read_sexp(in);
    while ((tmp != SEXP_ERROR) && (tmp != SEXP_EOF) && (tmp != SEXP_CLOSE)) {
      if ((tok == TOK_OPEN_LIST) && SEXP_SYMBOLP(tmp)
          && (strncmp(string_data(tmp), ".", 2) == 0)) {
        /* dotted list */
        free_sexp(tmp);
        tmp = read_sexp(in);
        if (read_token(in) != TOK_CLOSE) {
          res = SEXP_ERROR;
        } else {
          tmp2 = res;
          res = nreverse(res);
          set_cdr(tmp2, tmp);
          return res;
        }
      } else {
        res = cons(tmp, res);
        tmp = read_sexp(in);
      }
    }
    if (tmp != SEXP_CLOSE) {
      free_sexp(res);
      res = SEXP_ERROR;
    }
    res = nreverse(res);
    if (tok == TOK_OPEN_VECTOR) {
      tmp = res;
      res = list_to_vector(tmp);
      free_sexp(tmp);
    }
    break;
  case TOK_START_STRING:
    str = read_string(in);
    res = make_string(str);
    free(str);
    break;
  case TOK_SYMBOL:
    str = read_symbol(in);
    res = intern(str);
    free(str);
    break;
  case TOK_NUMBER:
    res = make_integer(read_number(in));
    break;
  case TOK_CLOSE:
    res = SEXP_CLOSE;
    break;
  case TOK_ERROR:
  default:
    res = SEXP_ERROR;
    break;
  }
  return res;
}

int main (int argc, char **argv) {
  sexp obj;

  /* sample object */
/*   write_sexp(stdout, list(6, */
/*                           intern("foo"), */
/*                           make_integer(2), */
/*                           make_string("bar"), */
/*                           make_character('d'), */
/*                           vector(2, intern("baz"), intern("qux")), */
/*                           SEXP_TRUE)); */
/*   fprintf(stdout, "\n"); */

  /* rpl (repl without the eval) */
  fprintf(stdout, "> ");
  fflush(stdout);
  while ((obj = read_sexp(stdin)) != SEXP_EOF) {
    write_sexp(stdout, obj);
    fprintf(stdout, "\n> ");
    fflush(stdout);
  }
  fprintf(stdout, "\n");
  return 0;
}
