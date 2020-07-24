/*  json.c -- fast json I/O                                  */
/*  Copyright (c) 2020 Alex Shinn.      All rights reserved. */
/*  Copyright (c) 2020 Ekaitz Zarraga.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt      */

#include <chibi/eval.h>

static int digit_value (int c) {
  return (((c)<='9') ? ((c) - '0') : ((sexp_tolower(c) - 'a') + 10));
}

sexp json_read (sexp ctx, sexp self, sexp in);

sexp sexp_json_read_exception (sexp ctx, sexp self, const char* msg, sexp in, sexp ir) {
  sexp res;
  sexp_gc_var4(sym, name, str, irr);
  sexp_gc_preserve4(ctx, sym, name, str, irr);
  name = (sexp_port_name(in) ? sexp_port_name(in) : SEXP_FALSE);
  name = sexp_cons(ctx, name, sexp_make_fixnum(sexp_port_line(in)));
  str = sexp_c_string(ctx, msg, -1);
  irr = ((sexp_pairp(ir) || sexp_nullp(ir)) ? ir : sexp_list1(ctx, ir));
  res = sexp_make_exception(ctx, sym = sexp_intern(ctx, "json-read", -1),
                            str, irr, SEXP_FALSE, name);
  sexp_gc_release4(ctx);
  return res;
}

sexp sexp_json_write_exception (sexp ctx, sexp self, const char* msg, sexp obj) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  tmp = sexp_list1(ctx, obj);
  res = sexp_user_exception(ctx, self, msg, tmp);
  sexp_gc_release2(ctx);
  return res;
}

sexp json_read_number (sexp ctx, sexp self, sexp in) {
  double res = 0, scale = 1;
  int sign = 1, inexactp = 0, scale_sign = 1, ch;
  ch = sexp_read_char(ctx, in);
  if (ch == '+') {
    ch = sexp_read_char(ctx, in);
  } else if (ch == '-') {
    ch = sexp_read_char(ctx, in);
    sign = -1;
  }
  for ( ; ch != EOF && isdigit(ch); ch = sexp_read_char(ctx, in))
    res = res * 10 + ch - '0';
  if (ch == '.') {
    inexactp = 1;
    for (ch = sexp_read_char(ctx, in); isdigit(ch); scale *= 10, ch = sexp_read_char(ctx, in))
      res = res * 10 + ch - '0';
    res /= scale;
  } else if (ch == 'e') {
    inexactp = 1;
    ch = sexp_read_char(ctx, in);
    if (ch == '+') {
      ch = sexp_read_char(ctx, in);
    } else if (ch == '-') {
      ch = sexp_read_char(ctx, in);
      scale_sign = -1;
    }
    for (scale=0; isdigit(ch); ch = sexp_read_char(ctx, in))
      scale = scale * 10 + ch - '0';
    res *= pow(10.0, scale_sign * scale);
  }
  if (ch != EOF) sexp_push_char(ctx, ch, in);
  return (inexactp || fabs(res) > SEXP_MAX_FIXNUM) ?
    sexp_make_flonum(ctx, sign * res) :
    sexp_make_fixnum(sign * res);  /* always return inexact? */
}

sexp json_read_literal (sexp ctx, sexp self, sexp in, char* name, sexp value) {
  int ch;
  for (++name; *name; )
    if (*(name++) != (ch = sexp_read_char(ctx, in)))
      sexp_json_read_exception(ctx, self, "unexpected character in json", in, sexp_make_character(ch));
  return value;
}

#define USEQ_LEN 4

long decode_useq(sexp ctx, sexp in) {
  long result = 0, i, ch;
  for (i=0; i < USEQ_LEN; i++) {
    ch = sexp_read_char(ctx, in);
    if (!isxdigit(ch)) {
      sexp_push_char(ctx, ch, in);
      return -1;
    }
    result = (result << 4) + digit_value(ch);
  }
  return result;
}

#define INIT_STRING_BUFFER_SIZE 128

sexp json_read_string (sexp ctx, sexp self, sexp in) {
  sexp_sint_t size=INIT_STRING_BUFFER_SIZE;
  char initbuf[INIT_STRING_BUFFER_SIZE];
  char *buf=initbuf, *tmp;
  int i=0, ch, len;
  long utfchar, utfchar2;
  sexp res = SEXP_VOID;
  for (ch = sexp_read_char(ctx, in); ch != '"'; ch = sexp_read_char(ctx, in)) {
    if (ch == EOF) {
      res = sexp_json_read_exception(ctx, self, "unterminated string in json", in, SEXP_NULL);
      break;
    }
    if (i+4 >= size) {       /* expand buffer w/ malloc(), later free() it */
      tmp = (char*) sexp_malloc(size*2);
      if (!tmp) {res = sexp_global(ctx, SEXP_G_OOM_ERROR); break;}
      memcpy(tmp, buf, i);
      if (size != INIT_STRING_BUFFER_SIZE) free(buf);
      buf = tmp;
      size *= 2;
    }
    if (ch == '\\') {
      ch = sexp_read_char(ctx, in);
      switch (ch) {
      case 'n':
        buf[i++] = '\n';
        break;
      case 't':
        buf[i++] = '\t';
        break;
      case 'u':
        utfchar = decode_useq(ctx, in);
        if (0xd800 <= utfchar && utfchar <= 0xdbff) {
          ch = sexp_read_char(ctx, in);
          if (ch == '\\') {
            ch = sexp_read_char(ctx, in);
            if (ch == 'u') {
              /* high surrogate followed by another unicode escape */
              utfchar2 = decode_useq(ctx, in);
              if (0xdc00 <= utfchar2 && utfchar2 <= 0xdfff) {
                /* merge low surrogate (otherwise high is left unpaired) */
                utfchar = 0x10000 + (((utfchar - 0xd800) << 10) | (utfchar2 - 0xdc00));
              } else {
                return sexp_json_read_exception(ctx, self, "invalid \\u sequence", in, SEXP_NULL);
              }
            } else {
              sexp_push_char(ctx, ch, in);
              sexp_push_char(ctx, '\\', in);
            }
          } else {
            sexp_push_char(ctx, ch, in);
          }
        }
        if (utfchar < 0) {
          return sexp_json_read_exception(ctx, self, "invalid \\u sequence", in, SEXP_NULL);
        } else {
          len = sexp_utf8_char_byte_count(utfchar);
          sexp_utf8_encode_char((unsigned char*)buf + i, len, utfchar);
          i += len;
        }
        break;
      default:
        buf[i++] = ch;
        break;
      }
    } else {
      buf[i++] = ch;
    }
  }
  if (!sexp_exceptionp(res)) {
    buf[i] = '\0';
    res = sexp_c_string(ctx, buf, i);
    if (sexp_stringp(res)) sexp_immutablep(res) = 1;
  }
  if (size != INIT_STRING_BUFFER_SIZE) free(buf);
  return res;
}

sexp json_read_array (sexp ctx, sexp self, sexp in) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  int comma = 1, ch;
  res = SEXP_NULL;
  while (1) {
    ch = sexp_read_char(ctx, in);
    if (ch == EOF) {
      res = sexp_json_read_exception(ctx, self, "unterminated array in json", in, SEXP_NULL);
      break;
    } else if (ch == ']') {
      if (comma && res != SEXP_NULL) {
        res = sexp_json_read_exception(ctx, self, "missing value after comma in json", in, SEXP_NULL);
      } else {
        res = sexp_nreverse(ctx, res);
        res = sexp_list_to_vector(ctx, res);
      }
      break;
    } else if (ch == ',' && comma) {
      res = sexp_json_read_exception(ctx, self, "unexpected comma in json array", in, SEXP_NULL);
      break;
    } else if (ch == ',') {
      comma = 1;
    } else if (!isspace(ch)) {
      if (comma) {
        sexp_push_char(ctx, ch, in);
        tmp = json_read(ctx, self, in);
        if (sexp_exceptionp(tmp)) {
          res = tmp;
          break;
        }
        res = sexp_cons(ctx, tmp, res);
        comma = 0;
      } else {
        res = sexp_json_read_exception(ctx, self, "unexpected value in json array", in, SEXP_NULL);
        break;
      }
    }
  }
  sexp_gc_release2(ctx);
  return res;
}

sexp json_read_object (sexp ctx, sexp self, sexp in) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  int comma = 1, ch;
  res = SEXP_NULL;
  while (1) {
    ch = sexp_read_char(ctx, in);
    if (ch == EOF) {
      res = sexp_json_read_exception(ctx, self, "unterminated object in json", in, SEXP_NULL);
      break;
    } else if (ch == '}') {
      if (comma && res != SEXP_NULL) {
        res = sexp_json_read_exception(ctx, self, "missing value after comma in json object", in, SEXP_NULL);
      } else {
        res = sexp_nreverse(ctx, res);
      }
      break;
    } else if (ch == ',' && comma) {
      res = sexp_json_read_exception(ctx, self, "unexpected comma in json object", in, SEXP_NULL);
      break;
    } else if (ch == ',') {
      comma = 1;
    } else if (!isspace(ch)) {
      if (comma) {
        sexp_push_char(ctx, ch, in);
        tmp = json_read(ctx, self, in);
        if (sexp_exceptionp(tmp)) {
          res = tmp;
          break;
        } else if (sexp_stringp(tmp)) {
          tmp = sexp_string_to_symbol(ctx, tmp);
        }
        tmp = sexp_cons(ctx, tmp, SEXP_VOID);
        for (ch = sexp_read_char(ctx, in); isspace(ch); ch = sexp_read_char(ctx, in))
          ;
        if (ch != ':') {
          res = sexp_json_read_exception(ctx, self, "missing colon in json object", in, sexp_make_character(ch));
          break;
        }
        sexp_cdr(tmp) = json_read(ctx, self, in);
        if (sexp_exceptionp(sexp_cdr(tmp))) {
          res = sexp_cdr(tmp);
          break;
        }
        res = sexp_cons(ctx, tmp, res);
        comma = 0;
      } else {
        res = sexp_json_read_exception(ctx, self, "unexpected value in json object", in, SEXP_NULL);
        break;
      }
    }
  }
  sexp_gc_release2(ctx);
  return res;
}

sexp json_read (sexp ctx, sexp self, sexp in) {
  sexp res;
  int ch = ' ';
  while (isspace(ch))
    ch = sexp_read_char(ctx, in);
  switch (ch) {
  case '{':
    res = json_read_object(ctx, self, in);
    break;
  case '[':
    res = json_read_array(ctx, self, in);
    break;
  case '"':
    res = json_read_string(ctx, self, in);
    break;
  case '-': case '+':
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    sexp_push_char(ctx, ch, in);
    res = json_read_number(ctx, self, in);
    break;
  case 'n': case 'N':
    res = json_read_literal(ctx, self, in, "null", SEXP_VOID);
    break;
  case 't': case 'T':
    res = json_read_literal(ctx, self, in, "true", SEXP_TRUE);
    break;
  case 'f': case 'F':
    res = json_read_literal(ctx, self, in, "false", SEXP_FALSE);
    break;
  case '}':
    res = sexp_json_read_exception(ctx, self, "unexpected closing brace in json", in, SEXP_NULL);
    break;
  case ']':
    res = sexp_json_read_exception(ctx, self, "unexpected closing bracket in json", in, SEXP_NULL);
    break;
  default:
    res = sexp_json_read_exception(ctx, self, "unexpected character in json", in, sexp_make_character(ch));
    break;
  }
  return res;
}

sexp sexp_json_read (sexp ctx, sexp self, sexp_sint_t n, sexp in) {
  sexp_assert_type(ctx, sexp_iportp, SEXP_IPORT, in);
  return json_read(ctx, self, in);
}


sexp json_write (sexp ctx, sexp self, sexp obj, sexp out);

#define FLONUM_SIGNIFICANT_DIGITS 10
#define FLONUM_EXP_MAX_DIGITS 3
sexp json_write_flonum(sexp ctx, sexp self, const sexp obj, sexp out) {
  if (sexp_infp(obj) || sexp_nanp(obj)) {
    return sexp_json_write_exception(ctx, self, "unable to encode number", obj);
  }
  /* Extra space for signs (x2), dot, E and \0 */
  char cout[FLONUM_SIGNIFICANT_DIGITS + FLONUM_EXP_MAX_DIGITS + 5];
  snprintf(cout, sizeof(cout), "%.*G", FLONUM_SIGNIFICANT_DIGITS, sexp_flonum_value(obj));
  sexp_write_string(ctx, cout, out);
  return SEXP_VOID;
}


sexp json_write_string(sexp ctx, sexp self, const sexp obj, sexp out) {
  char cout[32];  /* oversized to avoid snprintf warnings */
  unsigned long ch, chh, chl;
  sexp i, end = sexp_make_string_cursor(sexp_string_size(obj));

  sexp_write_char(ctx, '"', out);
  for (i = sexp_make_string_cursor(0); i < end;
       i = sexp_string_cursor_next(obj, i)) {
    ch = sexp_unbox_character(sexp_string_cursor_ref(ctx, obj, i));
    if (ch < 0x7F) {
      switch (ch) {
        case '\\':
          sexp_write_string(ctx, "\\\\", out);
          break;
        case '\b':
          sexp_write_string(ctx, "\\b", out);
          break;
        case '\f':
          sexp_write_string(ctx, "\\f", out);
          break;
        case '\n':
          sexp_write_string(ctx, "\\n", out);
          break;
        case '\r':
          sexp_write_string(ctx, "\\r", out);
          break;
        case '\t':
          sexp_write_string(ctx, "\\t", out);
          break;
        default:
          sexp_write_char(ctx, ch, out);
          break;
      }
    } else if (ch <= 0xFFFF) {
      snprintf(cout, sizeof(cout), "\\u%04lX", ch);
      sexp_write_string(ctx, cout, out);
    } else {
      // Surrogate pair
      chh = (0xD800 - (0x10000 >> 10) + ((ch) >> 10));
      chl = (0xDC00 + ((ch) & 0x3FF));
      if (chh > 0xFFFF || chl > 0xFFFF) {
        return sexp_json_write_exception(ctx, self, "unable to encode string", obj);
      }
      snprintf(cout, sizeof(cout), "\\u%04lX\\u%04lX", chh, chl);
      sexp_write_string(ctx, cout, out);
    }
  }
  sexp_write_char(ctx, '"', out);

  return SEXP_VOID;
}

sexp json_write_array(sexp ctx, sexp self, const sexp obj, sexp out) {
  sexp tmp;
  int len = sexp_vector_length(obj), i;
  sexp_write_string(ctx, "[", out);
  for (i = 0; i < len; ++i) {
    tmp = json_write(ctx, self, sexp_vector_ref(obj, sexp_make_fixnum(i)), out);
    if (sexp_exceptionp(tmp)) {
      return tmp;
    }
    if (i < len - 1) {
      sexp_write_char(ctx, ',', out);
    }
  }
  sexp_write_string(ctx, "]", out);
  return SEXP_VOID;
}

sexp json_write_object(sexp ctx, sexp self, const sexp obj, sexp out) {
  sexp ls, cur, key, val, tmp;
  if (sexp_length(ctx, obj) == SEXP_FALSE)
    return sexp_json_write_exception(ctx, self, "unable to encode circular list", obj);
  sexp_write_char(ctx, '{', out);
  for (ls = obj; sexp_pairp(ls); ls = sexp_cdr(ls)) {
    if (ls != obj)
      sexp_write_char(ctx, ',', out);
    cur = sexp_car(ls);
    if (!sexp_pairp(cur))
      return sexp_json_write_exception(ctx, self, "unable to encode key-value pair: not a pair", obj);
    key = sexp_car(cur);
    if (!sexp_symbolp(key))
      return sexp_json_write_exception(ctx, self, "unable to encode key: not a symbol", key);
    tmp = json_write(ctx, self, key, out);
    if (sexp_exceptionp(tmp))
      return tmp;
    sexp_write_char(ctx, ':', out);
    val = sexp_cdr(cur);
    tmp = json_write(ctx, self, val, out);
    if (sexp_exceptionp(tmp))
      return tmp;
  }
  sexp_write_char(ctx, '}', out);
  return SEXP_VOID;
}

sexp json_write (sexp ctx, sexp self, const sexp obj, sexp out) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_VOID;
  if (sexp_symbolp(obj)) {
    res = sexp_symbol_to_string(ctx, obj);
    res = json_write_string(ctx, self, res, out);
  } else if (sexp_stringp(obj)) {
    res = json_write_string(ctx, self, obj, out);
  } else if (sexp_listp(ctx, obj) == SEXP_TRUE) {
    res = json_write_object(ctx, self, obj, out);
  } else if (sexp_vectorp(obj)) {
    res = json_write_array(ctx, self, obj, out);
  } else if (sexp_fixnump(obj)) {
    res = sexp_write(ctx, obj, out);
  } else if (sexp_flonump(obj)) {
    res = json_write_flonum(ctx, self, obj, out);
#if SEXP_USE_BIGNUMS
  } else if (sexp_bignump(obj)) {
    res = sexp_make_flonum(ctx, sexp_bignum_to_double(obj));
    res = json_write_flonum(ctx, self, res, out);
#endif
  } else if (obj == SEXP_FALSE) {
    sexp_write_string(ctx, "false", out);
  } else if (obj == SEXP_TRUE) {
    sexp_write_string(ctx, "true", out);
  } else if (obj == SEXP_NULL) {
    sexp_write_string(ctx, "null", out);
  } else if (sexp_pairp(obj)) {
    res = sexp_json_write_exception(ctx, self, "unable to encode elemente: key-value pair out of object", obj);
  } else {
    res = sexp_json_write_exception(ctx, self, "unable to encode element", obj);
  }
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_json_write (sexp ctx, sexp self, sexp_sint_t n, sexp obj, sexp out) {
  sexp_assert_type(ctx, sexp_oportp, SEXP_OPORT, out);
  return json_write(ctx, self, obj, out);
}


sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_define_foreign(ctx, env, "json-read", 1, sexp_json_read);
  sexp_define_foreign(ctx, env, "json-write", 2, sexp_json_write);
  return SEXP_VOID;
}
