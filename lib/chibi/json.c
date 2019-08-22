/*  json.c -- fast json parser                           */
/*  Copyright (c) 2019 Alex Shinn.  All rights reserved. */
/*  BSD-style license: http://synthcode.com/license.txt  */

#include <chibi/eval.h>

sexp parse_json (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len);

sexp sexp_json_exception (sexp ctx, sexp self, const char* msg, sexp str, const int pos) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  tmp = sexp_list2(ctx, str, sexp_make_fixnum(pos));
  res = sexp_user_exception(ctx, self, msg, tmp);
  sexp_gc_release2(ctx);
  return res;
}

sexp parse_json_number (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len) {
  sexp_sint_t res = 0;
  int j = *i;
  while (j < len && isdigit(s[j]))
    res = res * 10 + s[j++] - '0';
  *i = j;
  return sexp_make_fixnum(res);
}

sexp parse_json_literal (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len, const char* name, int namelen, sexp value) {
  sexp res;
  if (strncasecmp(s+*i, name, namelen) == 0 && (*i+namelen >= len || !isalnum(s[*i+namelen]))) {
    res = value;
    *i += namelen;
  } else {
    res = sexp_json_exception(ctx, self, "unexpected character in json at", str, *i);
  }
  return res;
}

sexp parse_json_string (sexp ctx, sexp self, sexp str, const char* s, int* i, const int len) {
  sexp_gc_var2(res, tmp);
  sexp_gc_preserve2(ctx, res, tmp);
  int from = *i, to = *i;
  res = SEXP_NULL;
  for ( ; s[to] != '"'; ++to) {
    if (to+1 >= len) {
      res = sexp_json_exception(ctx, self, "unterminated string in json started at", str, *i);
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
      res = sexp_json_exception(ctx, self, "unterminated array in json started at", str, *i);
      break;
    } else if (s[j] == ']') {
      if (comma && res != SEXP_NULL) {
        res = sexp_json_exception(ctx, self, "missing value after comma in json array at", str, j);
      } else {
        res = sexp_nreverse(ctx, res);
        res = sexp_list_to_vector(ctx, res);
      }
      ++j;
      break;
    } else if (s[j] == ',' && comma) {
      res = sexp_json_exception(ctx, self, "unexpected comma in json array at", str, j);
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
        res = sexp_json_exception(ctx, self, "unexpected value in json array at", str, j);
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
      res = sexp_json_exception(ctx, self, "unterminated object in json started at", str, *i);
      break;
    } else if (s[j] == '}') {
      if (comma && res != SEXP_NULL) {
        res = sexp_json_exception(ctx, self, "missing value after comma in json object at", str, j);
      } else {
        res = sexp_nreverse(ctx, res);
      }
      ++j;
      break;
    } else if (s[j] == ',' && comma) {
      res = sexp_json_exception(ctx, self, "unexpected comma in json object at", str, j);
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
          res = sexp_json_exception(ctx, self, "missing colon in json object at", str, j);
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
        res = sexp_json_exception(ctx, self, "unexpected value in json object at", str, j);
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
    res = sexp_json_exception(ctx, self, "unexpected closing brace in json at", str, j);
    break;
  case ']':
    res = sexp_json_exception(ctx, self, "unexpected closing bracket in json at", str, j);
    break;
  default:
    res = sexp_json_exception(ctx, self, "unexpected character in json at", str, j);
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

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, const sexp_abi_identifier_t abi) {
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return SEXP_ABI_ERROR;
  sexp_define_foreign(ctx, env, "parse-json", 1, sexp_parse_json);
  return SEXP_VOID;
}
