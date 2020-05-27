/*  json.c -- fast json parser and unparser                  */
/*  Copyright (c) 2019 Alex Shinn.      All rights reserved. */
/*  Copyright (c) 2020 Ekaitz Zarraga.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt      */

#include <chibi/eval.h>

static int digit_value (int c) {
  return (((c)<='9') ? ((c) - '0') : ((sexp_tolower(c) - 'a') + 10));
}

sexp parse_json (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len);

sexp sexp_json_parse_exception (sexp ctx, sexp self, const char* msg, sexp str, const int pos) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  tmp = sexp_list2(ctx, str, sexp_make_fixnum(pos));
  res = sexp_user_exception(ctx, self, msg, tmp);
  sexp_gc_release2(ctx);
  return res;
}

sexp sexp_json_unparse_exception (sexp ctx, sexp self, const char* msg, sexp obj) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  tmp = sexp_list1(ctx, obj);
  res = sexp_user_exception(ctx, self, msg, tmp);
  sexp_gc_release2(ctx);
  return res;
}

sexp parse_json_number (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len) {
  double res = 0, scale = 1;
  int j = *i, sign = 1, inexactp = 0, scale_sign = 1;
  if (s[j] == '+') {
    ++j;
  } else if (s[j] == '-') {
    ++j;
    sign = -1;
  }
  while (j < len && isdigit(s[j]))
    res = res * 10 + s[j++] - '0';
  if (j < len && s[j] == '.') {
    inexactp = 1;
    for (++j; j < len && isdigit(s[j]); scale *= 10)
      res = res * 10 + s[j++] - '0';
    res /= scale;
  } else if (j < len && sexp_tolower(s[j]) == 'e') {
    inexactp = 1;
    if (j+1 < len) {
      if (s[j+1] == '+') {
        ++j;
      } else if (s[j+1] == '-') {
        ++j;
        scale_sign = -1;
      }
    }
    for (++j, scale=0; j < len && isdigit(s[j]); )
      scale = scale * 10 + s[j++] - '0';
    res *= pow(10.0, scale_sign * scale);
  }
  *i = j;
  return (inexactp || fabs(res) > SEXP_MAX_FIXNUM) ?
    sexp_make_flonum(ctx, sign * res) :
    sexp_make_fixnum(sign * res);  /* always return inexact? */
}

sexp parse_json_literal (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len, const char* name, int namelen, sexp value) {
  sexp res;
  if (strncasecmp(s+*i, name, namelen) == 0 && (*i+namelen >= len || !isalnum(s[*i+namelen]))) {
    res = value;
    *i += namelen;
  } else {
    res = sexp_json_parse_exception(ctx, self, "unexpected character in json at", str, *i);
  }
  return res;
}

#define USEQ_LEN 4

long decode_useq(const char* s) {
  long result = 0, i;
  for (i=0; i < USEQ_LEN; i++) {
    if (!isxdigit(s[i]))
      return -1;
    result = (result << 4) + digit_value(s[i]);
  }
  return result;
}

sexp parse_json_string (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  int from = *i, to = *i;
  long utfchar, utfchar2;
  res = SEXP_NULL;
  for ( ; s[to] != '"' && !sexp_exceptionp(res); ++to) {
    if (to+1 >= len) {
      res = sexp_json_parse_exception(ctx, self, "unterminated string in json started at", str, *i);
      break;
    }
    if (s[to] == '\\') {
      tmp = sexp_c_string(ctx, s+from, to-from);
      res = sexp_stringp(res) ? sexp_list2(ctx, tmp, res) : sexp_cons(ctx, tmp, res);
      switch (s[++to]) {
      case 'n':
        tmp = sexp_c_string(ctx, "\n", -1);
        res = sexp_cons(ctx, tmp, res);
        from = to+1;
        break;
      case 't':
        tmp = sexp_c_string(ctx, "\t", -1);
        res = sexp_cons(ctx, tmp, res);
        from = to+1;
        break;
      case 'u':
        utfchar = decode_useq(s+to+1);
        to += USEQ_LEN;
        if (0xd800 <= utfchar && utfchar <= 0xdbff && s[to+1] == '\\' && s[to+2] == 'u') {
          /* high surrogate followed by another unicode escape */
          utfchar2 = decode_useq(s+to+3);
          if (0xdc00 <= utfchar2 && utfchar2 <= 0xdfff) {
            /* merge low surrogate (otherwise high is left unpaired) */
            utfchar = 0x10000 + (((utfchar - 0xd800) << 10) | (utfchar2 - 0xdc00));
            to += USEQ_LEN + 2;
          }
        }
        if (utfchar < 0) {
          res = sexp_json_parse_exception(ctx, self, "invalid \\u sequence at", str, to - USEQ_LEN);
        } else {
          tmp = sexp_make_string(ctx, sexp_make_fixnum(1), sexp_make_character(utfchar));
          res = sexp_cons(ctx, tmp, res);
          from = to + 1;
        }
        break;
      default:
        from = to;
        break;
      }
    }
  }
  if (!sexp_exceptionp(res)) {
    tmp = sexp_c_string(ctx, s+from, to-from);
    if (res == SEXP_NULL) {
      res = tmp;
    } else {
      res = sexp_stringp(res) ? sexp_list2(ctx, tmp, res) : sexp_cons(ctx, tmp, res);
      res = sexp_nreverse(ctx, res);
      res = sexp_string_concatenate(ctx, res, SEXP_FALSE);
    }
  }
  *i = to+1;
  sexp_gc_release2(ctx);
  return res;
}

sexp parse_json_array (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  int j = *i;
  int comma = 1;
  res = SEXP_NULL;
  while (1) {
    if (j >= len) {
      res = sexp_json_parse_exception(ctx, self, "unterminated array in json started at", str, *i);
      break;
    } else if (s[j] == ']') {
      if (comma && res != SEXP_NULL) {
        res = sexp_json_parse_exception(ctx, self, "missing value after comma in json array at", str, j);
      } else {
        res = sexp_nreverse(ctx, res);
        res = sexp_list_to_vector(ctx, res);
      }
      ++j;
      break;
    } else if (s[j] == ',' && comma) {
      res = sexp_json_parse_exception(ctx, self, "unexpected comma in json array at", str, j);
      break;
    } else if (s[j] == ',') {
      comma = 1;
      ++j;
    } else if (isspace(s[j])) {
      ++j;
    } else {
      if (comma) {
        tmp = parse_json(ctx, self, str, s, &j, len);
        if (sexp_exceptionp(tmp)) {
          res = tmp;
          break;
        }
        res = sexp_cons(ctx, tmp, res);
        comma = 0;
      } else {
        res = sexp_json_parse_exception(ctx, self, "unexpected value in json array at", str, j);
        break;
      }
    }
  }
  *i = j;
  sexp_gc_release2(ctx);
  return res;
}

sexp parse_json_object (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  int j = *i;
  int comma = 1;
  res = SEXP_NULL;
  while (1) {
    if (j >= len) {
      res = sexp_json_parse_exception(ctx, self, "unterminated object in json started at", str, *i);
      break;
    } else if (s[j] == '}') {
      if (comma && res != SEXP_NULL) {
        res = sexp_json_parse_exception(ctx, self, "missing value after comma in json object at", str, j);
      } else {
        res = sexp_nreverse(ctx, res);
      }
      ++j;
      break;
    } else if (s[j] == ',' && comma) {
      res = sexp_json_parse_exception(ctx, self, "unexpected comma in json object at", str, j);
      break;
    } else if (s[j] == ',') {
      comma = 1;
      ++j;
    } else if (isspace(s[j])) {
      ++j;
    } else {
      if (comma) {
        tmp = parse_json(ctx, self, str, s, &j, len);
        if (sexp_exceptionp(tmp)) {
          res = tmp;
          break;
        } else if (sexp_stringp(tmp)) {
          tmp = sexp_string_to_symbol(ctx, tmp);
        }
        tmp = sexp_cons(ctx, tmp, SEXP_VOID);
        while (j < len && isspace(s[j]))
          ++j;
        if (s[j] != ':') {
          res = sexp_json_parse_exception(ctx, self, "missing colon in json object at", str, j);
          break;
        }
        ++j;
        sexp_cdr(tmp) = parse_json(ctx, self, str, s, &j, len);
        if (sexp_exceptionp(sexp_cdr(tmp))) {
          res = sexp_cdr(tmp);
          break;
        }
        res = sexp_cons(ctx, tmp, res);
        comma = 0;
      } else {
        res = sexp_json_parse_exception(ctx, self, "unexpected value in json object at", str, j);
        break;
      }
    }
  }
  *i = j;
  sexp_gc_release2(ctx);
  return res;
}

sexp parse_json (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len) {
  sexp res;
  int j = *i;
  while (j < len && isspace(s[j]))
    ++j;
  switch (s[j]) {
  case '{':
    ++j;
    res = parse_json_object(ctx, self, str, s, &j, len);
    break;
  case '[':
    ++j;
    res = parse_json_array(ctx, self, str, s, &j, len);
    break;
  case '"':
    ++j;
    res = parse_json_string(ctx, self, str, s, &j, len);
    break;
  case '-': case '+':
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    res = parse_json_number(ctx, self, str, s, &j, len);
    break;
  case 'n': case 'N':
    res = parse_json_literal(ctx, self, str, s, &j, len, "null", 4, SEXP_VOID);
    break;
  case 't': case 'T':
    res = parse_json_literal(ctx, self, str, s, &j, len, "true", 4, SEXP_TRUE);
    break;
  case 'f': case 'F':
    res = parse_json_literal(ctx, self, str, s, &j, len, "false", 5, SEXP_FALSE);
    break;
  case '}':
    res = sexp_json_parse_exception(ctx, self, "unexpected closing brace in json at", str, j);
    break;
  case ']':
    res = sexp_json_parse_exception(ctx, self, "unexpected closing bracket in json at", str, j);
    break;
  default:
    res = sexp_json_parse_exception(ctx, self, "unexpected character in json at", str, j);
    break;
  }
  *i = j;
  return res;
}

sexp sexp_parse_json (sexp ctx, sexp self, sexp_sint_t n, sexp str) {
  const char *s;
  int i=0, len;
  sexp_assert_type(ctx, sexp_stringp, SEXP_STRING, str);
  s = sexp_string_data(str);
  len = sexp_string_size(str);
  return parse_json(ctx, self, str, s, &i, len);
}




sexp unparse_json (sexp ctx, sexp self, sexp obj);


sexp unparse_json_fixnum(sexp ctx, sexp self, const sexp obj) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  res = SEXP_VOID;
  int sign = 1;
  long num = sexp_unbox_fixnum(obj);
  char digit;
  if (num == 0) {
    res = sexp_c_string(ctx, "0", -1);
  } else {
    if (num < 0) {
      sign = -1;
      num = labs(num);
    }
    while (num > 0) {
      digit = '0' + num%10;
      num /= 10;

      tmp = sexp_c_string(ctx, &digit, 1);
      res = sexp_cons(ctx, tmp, res);
    }
    if (sign==-1) {
      tmp = sexp_c_string(ctx, "-", -1);
      res = sexp_cons(ctx, tmp, res);
    }
    res = sexp_string_concatenate(ctx, res, SEXP_FALSE);
  }
  sexp_gc_release2(ctx);
  return res;
}


#define FLONUM_SIGNIFICANT_DIGITS 10
#define FLONUM_EXP_MAX_DIGITS 3
sexp unparse_json_flonum(sexp ctx, sexp self, const sexp obj) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  res = SEXP_VOID;
  char cout[FLONUM_SIGNIFICANT_DIGITS + FLONUM_EXP_MAX_DIGITS + 5];
    // Extra space for signs (x2), dot, E and \0

  if (sexp_infp(obj) || sexp_nanp(obj)) {
    res = sexp_json_unparse_exception(ctx, self, "unable to encode number", obj);
    sexp_gc_release2(ctx);
    return res;
  }

  snprintf(cout, sizeof(cout), "%.*G", FLONUM_SIGNIFICANT_DIGITS, sexp_flonum_value(obj));
  res = sexp_c_string(ctx, cout, -1);
  sexp_gc_release2(ctx);
  return res;
}


sexp unparse_json_string(sexp ctx, sexp self, const sexp obj) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  res = SEXP_VOID;

  tmp = sexp_c_string(ctx, "\"", -1);
  res = sexp_cons(ctx, tmp, res);

  char cout[32];  /* oversized to avoid snprintf warnings */
  unsigned long ch, chh, chl;

  sexp_uint_t len = sexp_string_length(obj);
  for(sexp_uint_t i=0; i!= len; i++) {
    ch = sexp_unbox_character(sexp_string_ref(ctx, obj, sexp_make_fixnum(i)));
    if(ch < 0x7F) {
      switch(ch) {
        case '\\':
          snprintf(cout, sizeof(cout), "\\\\");
          break;
        case '/':
          snprintf(cout, sizeof(cout), "\\/");
          break;
        case '\b':
          snprintf(cout, sizeof(cout), "\\b");
          break;
        case '\f':
          snprintf(cout, sizeof(cout), "\\f");
          break;
        case '\n':
          snprintf(cout, sizeof(cout), "\\n");
          break;
        case '\r':
          snprintf(cout, sizeof(cout), "\\r");
          break;
        case '\t':
          snprintf(cout, sizeof(cout), "\\t");
          break;
        default:
          snprintf(cout, sizeof(cout), "%c", (int)ch);
          break;
      }
    } else if(ch <= 0xFFFF) {
      snprintf(cout, sizeof(cout), "\\u%04lX", ch);
    } else {
      // Surrogate pair
      chh = (0xD800 - (0x10000 >> 10) + ((ch) >> 10));
      chl = (0xDC00 + ((ch) & 0x3FF));
      if (chh > 0xFFFF || chl > 0xFFFF) {
        res = sexp_json_unparse_exception(ctx, self, "unable to encode string", obj);
        sexp_gc_release2(ctx);
        return res;
      }
      snprintf(cout, sizeof(cout), "\\u%04lX\\u%04lX", chh, chl);
    }
    tmp = sexp_c_string(ctx, cout, -1);
    res = sexp_cons(ctx, tmp, res);
  }

  tmp = sexp_c_string(ctx, "\"", -1);
  res = sexp_cons(ctx, tmp, res);

  res = sexp_nreverse(ctx, res);
  res = sexp_string_concatenate(ctx, res, SEXP_FALSE);

  sexp_gc_release2(ctx);
  return res;
}

sexp unparse_json_array(sexp ctx, sexp self, const sexp obj) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  res = SEXP_VOID;

  tmp = sexp_c_string(ctx, "[", -1);
  res = sexp_cons(ctx, tmp, res);

  int len = sexp_vector_length(obj);
  for (int i=0; i!=len; i++) {
    tmp = unparse_json(ctx, self, sexp_vector_ref(obj, sexp_make_fixnum(i)));
    if (sexp_exceptionp(tmp)) {
      sexp_gc_release2(ctx);
      return tmp;
    }
    res = sexp_cons(ctx, tmp, res);

    if (i != len-1) {
      tmp = sexp_c_string(ctx, ",", -1);
      res = sexp_cons(ctx, tmp, res);
    }
  }

  tmp = sexp_c_string(ctx, "]", -1);
  res = sexp_cons(ctx, tmp, res);

  res = sexp_nreverse(ctx, res);
  res = sexp_string_concatenate(ctx, res, SEXP_FALSE);

  sexp_gc_release2(ctx);
  return res;
}


sexp unparse_json_object(sexp ctx, sexp self, const sexp obj) {
  sexp_gc_var6(res, tmp, it, cur, key, val);
  sexp_gc_preserve6(ctx, res, tmp, it, cur, key, val);
  res = SEXP_VOID;

  tmp = sexp_c_string(ctx, "{", -1);
  res = sexp_cons(ctx, tmp, res);

  int len = sexp_unbox_fixnum(sexp_length(ctx, obj));
  it = obj;
  for (int i=0; i!=len; i++) {
    cur = sexp_car(it);
    if (!sexp_pairp(cur)) {
      res = sexp_json_unparse_exception(ctx, self, "unable to encode key-value pair: not a pair", obj);
      goto except;
    }

    // Key
    key = sexp_car(cur);
    if (!(sexp_symbolp(key) /*|| sexp_stringp(key)*/)) {
      res = sexp_json_unparse_exception(ctx, self, "unable to encode key: not a symbol", key);
      goto except;
    }
    tmp = unparse_json(ctx, self, key);
    if (sexp_exceptionp(tmp)) {
      res = tmp;
      goto except;
    }
    res = sexp_cons(ctx, tmp, res);

    // Separator
    tmp = sexp_c_string(ctx, ":", -1);
    res = sexp_cons(ctx, tmp, res);

    // Value
    val = sexp_cdr(cur);
    tmp = unparse_json(ctx, self, val);
    res = sexp_cons(ctx, tmp, res);

    if (i != len-1) {
      tmp = sexp_c_string(ctx, ",", -1);
      res = sexp_cons(ctx, tmp, res);
    }
    it = sexp_cdr(it);
  }

  tmp = sexp_c_string(ctx, "}", -1);
  res = sexp_cons(ctx, tmp, res);

  res = sexp_nreverse(ctx, res);
  res = sexp_string_concatenate(ctx, res, SEXP_FALSE);
except:
  sexp_gc_release6(ctx);
  return res;
}

sexp unparse_json (sexp ctx, sexp self, sexp obj) {
  sexp_gc_var1(res);
  sexp_gc_preserve1(ctx, res);
  res = SEXP_VOID;

  if( sexp_symbolp(obj) ) {
    obj = sexp_symbol_to_string(ctx, obj);
    res = unparse_json_string(ctx, self, obj);
  } else if (sexp_stringp(obj)) {
    res = unparse_json_string(ctx, self, obj);
  } else if (sexp_listp(ctx, obj) == SEXP_TRUE) {
    res = unparse_json_object(ctx, self, obj);
  } else if (sexp_vectorp(obj)) {
    res = unparse_json_array(ctx, self, obj);
  } else if(sexp_fixnump(obj)) {
    res = unparse_json_fixnum(ctx, self, obj);
  } else if (sexp_flonump(obj)) {
    res = unparse_json_flonum(ctx, self, obj); // OTHER TYPES? bignum etc?
  } else if (obj == SEXP_FALSE) {
    res = sexp_c_string(ctx, "false", -1);
  } else if (obj == SEXP_TRUE) {
    res = sexp_c_string(ctx, "true", -1);
  } else if (obj == SEXP_NULL) {
    res = sexp_c_string(ctx, "null", -1);
  } else if (sexp_pairp(obj)) {
    res = sexp_json_unparse_exception(ctx, self, "unable to encode elemente: key-value pair out of object", obj);
  } else {
    res = sexp_json_unparse_exception(ctx, self, "unable to encode element", obj);
  }
  sexp_gc_release1(ctx);
  return res;
}

sexp sexp_unparse_json (sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
  return unparse_json(ctx, self, obj);
}


sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_define_foreign(ctx, env, "parse-json", 1, sexp_parse_json);
  sexp_define_foreign(ctx, env, "unparse-json", 1, sexp_unparse_json);
  return SEXP_VOID;
}
