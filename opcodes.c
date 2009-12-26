
#define _OP(c,o,n,m,t,u,i,s,d,f)                                \
  {.tag=SEXP_OPCODE,                                            \
      .value={.opcode={c, o, n, m, t, u, i, s, d, NULL, NULL, f}}}
#define _FN(o,n,m,t,u,s,d,f) _OP(SEXP_OPC_FOREIGN, o, n, m, t, u, 0, s, d, (sexp_proc1)f)
#define _FN0(s, d, f) _FN(SEXP_OP_FCALL0, 0, 0, 0, 0, s, d, f)
#define _FN1(t, s, d, f) _FN(SEXP_OP_FCALL1, 1, 0, t, 0, s, d, f)
#define _FN1OPT(t, s, d, f) _FN(SEXP_OP_FCALL1, 0, 1, t, u, s, d, f)
#define _FN1OPTP(t, s, d, f) _FN(SEXP_OP_FCALL1, 0, 3, t, 0, s, d, f)
#define _FN2(t, u, s, d, f) _FN(SEXP_OP_FCALL2, 2, 0, t, u, s, d, f)
#define _FN2OPT(t, u, s, d, f) _FN(SEXP_OP_FCALL2, 1, 1, t, u, s, d, f)
#define _FN2OPTP(t, u, s, d, f) _FN(SEXP_OP_FCALL2, 1, 3, t, u, s, d, f)
#define _FN3(t, u, s, d, f) _FN(SEXP_OP_FCALL3, 3, 0, t, u, s, d, f)
#define _FN4(t, u, s, d, f) _FN(SEXP_OP_FCALL4, 4, 0, t, u, s, d, f)
#define _FN5(t, u, s, d, f) _FN(SEXP_OP_FCALL5, 5, 0, t, u, s, d, f)
#define _FN6(t, u, s, d, f) _FN(SEXP_OP_FCALL6, 6, 0, t, u, s, d, f)
#define _PARAM(n, a, t) _OP(SEXP_OPC_PARAMETER, SEXP_OP_NOOP, 0, 3, t, 0, 0, n, a, 0)

static struct sexp_struct opcodes[] = {
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_CAR, 1, 0, SEXP_PAIR, 0, 0, "car", 0, NULL),
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_SET_CAR, 2, 0, SEXP_PAIR, 0, 0, "set-car!", 0, NULL),
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_CDR, 1, 0, SEXP_PAIR, 0, 0, "cdr", 0, NULL),
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_SET_CDR, 2, 0, SEXP_PAIR, 0, 0, "set-cdr!", 0, NULL),
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_VECTOR_REF,2,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-ref", 0, NULL),
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_VECTOR_SET,3,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-set!", 0, NULL),
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_VECTOR_LENGTH,1,0, SEXP_VECTOR, 0, 0,"vector-length", 0, NULL),
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_STRING_REF,2,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-ref", 0, NULL),
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_STRING_SET,3,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-set!", 0, NULL),
_OP(SEXP_OPC_ACCESSOR, SEXP_OP_STRING_LENGTH,1,0, SEXP_STRING, 0, 0,"string-length", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_FIX2FLO, 1, 0, 0, 0, 0, "exact->inexact", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_FLO2FIX, 1, 0, 0, 0, 0, "inexact->exact", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_CHAR2INT, 1, 0, SEXP_CHAR, 0, 0, "char->integer", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_INT2CHAR, 1, 0, SEXP_FIXNUM, 0, 0, "integer->char", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_CHAR_UPCASE, 1, 0, SEXP_CHAR, 0, 0, "char-upcase", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_CHAR_DOWNCASE, 1, 0, SEXP_CHAR, 0, 0, "char-downcase", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_ADD, 0, 1, SEXP_FIXNUM, 0, 0, "+", sexp_make_fixnum(0), NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_MUL, 0, 1, SEXP_FIXNUM, 0, 0, "*", sexp_make_fixnum(1), NULL),
_OP(SEXP_OPC_ARITHMETIC_INV, SEXP_OP_SUB, 0, 1, SEXP_FIXNUM, 0, SEXP_OP_NEGATIVE, "-", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_INV, SEXP_OP_DIV, 0, 1, SEXP_FIXNUM, 0, SEXP_OP_INVERSE, "/", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_QUOTIENT, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, 0, "quotient", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_REMAINDER, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, 0, "remainder", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_LT,  2, 1, SEXP_FIXNUM, 0, 0, "<", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_LE,  2, 1, SEXP_FIXNUM, 0, 0, "<=", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_LT,  2, 1, SEXP_FIXNUM, 0, 1, ">", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_LE,  2, 1, SEXP_FIXNUM, 0, 1, ">=", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_EQN, 2, 1, SEXP_FIXNUM, 0, 0, "=", 0, NULL),
_OP(SEXP_OPC_PREDICATE,      SEXP_OP_EQ,  2, 0, 0, 0, 0, "eq?", 0, NULL),
_OP(SEXP_OPC_CONSTRUCTOR,    SEXP_OP_CONS, 2, 0, 0, 0, 0, "cons", 0, NULL),
_OP(SEXP_OPC_CONSTRUCTOR,    SEXP_OP_MAKE_VECTOR, 1, 1, SEXP_FIXNUM, 0, 0, "make-vector", SEXP_VOID, NULL),
_OP(SEXP_OPC_CONSTRUCTOR,    SEXP_OP_MAKE_PROCEDURE, 4, 0, 0, 0, 0, "make-procedure", 0, NULL),
_OP(SEXP_OPC_CONSTRUCTOR,    SEXP_OP_MAKE_EXCEPTION, 5, 0, 0, 0, 0, "make-exception", 0, NULL),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_NULLP,  1, 0, 0, 0, 0, "null?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_EOFP,  1, 0, 0, 0, 0, "eof-object?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_SYMBOLP,  1, 0, 0, 0, 0, "symbol?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_CHARP,  1, 0, 0, 0, 0, "char?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_FIXNUMP,  1, 0, 0, 0, 0, "fixnum?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, 0, 0, 0, "pair?", sexp_make_fixnum(SEXP_PAIR), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, 0, 0, 0, "string?", sexp_make_fixnum(SEXP_STRING), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, 0, 0, 0, "vector?", sexp_make_fixnum(SEXP_VECTOR), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, 0, 0, 0, "flonum?", sexp_make_fixnum(SEXP_FLONUM), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, 0, 0, 0, "bignum?", sexp_make_fixnum(SEXP_BIGNUM), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, 0, 0, 0, "closure?", sexp_make_fixnum(SEXP_PROCEDURE), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, 0, 0, 0, "opcode?", sexp_make_fixnum(SEXP_OPCODE), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, 0, 0, 0, "input-port?", sexp_make_fixnum(SEXP_IPORT), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, 0, 0, 0, "output-port?", sexp_make_fixnum(SEXP_OPORT), 0),
_OP(SEXP_OPC_GENERIC, SEXP_OP_APPLY1, 2, 0, SEXP_PROCEDURE, SEXP_PAIR, 0, "apply1", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_CALLCC, 1, SEXP_PROCEDURE, 0, 0, 0, "%call/cc", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_RAISE, 1, SEXP_STRING, 0, 0, 0, "raise", 0, NULL),
_OP(SEXP_OPC_IO, SEXP_OP_WRITE_CHAR, 1, 3, 0, SEXP_OPORT, 0, "write-char", (sexp)"*current-output-port*", NULL),
_OP(SEXP_OPC_IO, SEXP_OP_NEWLINE, 0, 3, 0, SEXP_OPORT, 0, "newline", (sexp)"*current-output-port*", NULL),
_OP(SEXP_OPC_IO, SEXP_OP_READ_CHAR, 0, 3, 0, SEXP_IPORT, 0, "read-char", (sexp)"*current-input-port*", NULL),
_OP(SEXP_OPC_IO, SEXP_OP_PEEK_CHAR, 0, 3, 0, SEXP_IPORT, 0, "peek-char", (sexp)"*current-input-port*", NULL),
_FN1OPTP(SEXP_IPORT, "read", (sexp)"*current-input-port*", sexp_read),
_FN2OPTP(0, SEXP_OPORT, "write", (sexp)"*current-output-port*", sexp_write),
_FN2OPTP(0, SEXP_OPORT, "display", (sexp)"*current-output-port*", sexp_display),
_FN1OPTP(SEXP_OPORT, "flush-output", (sexp)"*current-output-port*", sexp_flush_output),
_FN2(0, 0, "equal?", 0, sexp_equalp),
_FN1(0, "list?", 0, sexp_listp),
_FN1(0, "identifier?", 0, sexp_identifierp),
_FN1(0, "identifier->symbol", 0, sexp_syntactic_closure_expr),
_FN4(0, SEXP_ENV, "identifier=?", 0, sexp_identifier_eq),
_FN1(SEXP_PAIR, "length", 0, sexp_length),
_FN1(SEXP_PAIR, "reverse", 0, sexp_reverse),
_FN1(SEXP_PAIR, "reverse!", 0, sexp_nreverse),
_FN2(SEXP_PAIR, SEXP_PAIR, "append2", 0, sexp_append2),
_FN1(SEXP_PAIR, "list->vector", 0, sexp_list_to_vector),
_FN1(SEXP_STRING, "open-input-file", 0, sexp_open_input_file),
_FN1(SEXP_STRING, "open-output-file", 0, sexp_open_output_file),
_FN1(SEXP_IPORT, "close-input-port", 0, sexp_close_port),
_FN1(SEXP_OPORT, "close-output-port", 0, sexp_close_port),
_FN0("make-environment", 0, sexp_make_env),
_FN1(SEXP_FIXNUM, "null-environment", 0, sexp_make_null_env),
_FN1(SEXP_FIXNUM, "scheme-report-environment", 0, sexp_make_standard_env),
_FN2OPTP(0, SEXP_ENV, "eval", (sexp)"*interaction-environment*", sexp_eval),
_FN2OPTP(SEXP_STRING, SEXP_ENV, "load", (sexp)"*interaction-environment*", sexp_load),
_FN4(SEXP_ENV, SEXP_ENV, "%env-copy!", 0, sexp_env_copy),
_FN2(SEXP_EXCEPTION, SEXP_OPORT, "print-exception", 0, sexp_print_exception),
_FN1(SEXP_EXCEPTION, "exception-type", 0, sexp_exception_type_func),
_FN2OPT(SEXP_FIXNUM, SEXP_CHAR, "make-string", sexp_make_character(' '), sexp_make_string),
_FN3(SEXP_STRING, SEXP_STRING, "string-cmp", 0, sexp_string_cmp),
_FN3(SEXP_STRING, SEXP_FIXNUM, "substring", 0, sexp_substring),
_FN1(SEXP_STRING, "string->symbol", 0, sexp_string_to_symbol),
_FN1(SEXP_PAIR, "string-concatenate", 0, sexp_string_concatenate),
_FN2(0, SEXP_PAIR, "memq", 0, sexp_memq),
_FN2(0, SEXP_PAIR, "assq", 0, sexp_assq),
_FN3(SEXP_ENV, SEXP_PAIR, "make-syntactic-closure", 0, sexp_make_synclo),
_FN1(0, "strip-syntactic-closures", 0, sexp_strip_synclos),
_PARAM("current-input-port", (sexp)"*current-input-port*", SEXP_IPORT),
_PARAM("current-output-port", (sexp)"*current-output-port*", SEXP_OPORT),
_PARAM("current-error-port", (sexp)"*current-error-port*", SEXP_OPORT),
_PARAM("current-exception-handler", (sexp)"*current-exception-handler*", SEXP_PROCEDURE),
_PARAM("interaction-environment", (sexp)"*interaction-environment*", SEXP_ENV),
_FN0("open-output-string", 0, sexp_make_output_string_port),
_FN1(SEXP_STRING, "open-input-string", 0, sexp_make_input_string_port),
_FN1(SEXP_OPORT, "get-output-string", 0, sexp_get_output_string),
#if SEXP_USE_MATH
_FN1(0, "exp", 0, sexp_exp),
_FN1(0, "log", 0, sexp_log),
_FN1(0, "sin", 0, sexp_sin),
_FN1(0, "cos", 0, sexp_cos),
_FN1(0, "tan", 0, sexp_tan),
_FN1(0, "asin", 0, sexp_asin),
_FN1(0, "acos", 0, sexp_acos),
_FN1(0, "atan1", 0, sexp_atan),
_FN1(0, "sqrt", 0, sexp_sqrt),
_FN1(0, "round", 0, sexp_round),
_FN1(0, "truncate", 0, sexp_trunc),
_FN1(0, "floor", 0, sexp_floor),
_FN1(0, "ceiling", 0, sexp_ceiling),
_FN2(0, 0, "expt", 0, sexp_expt),
#endif
#if SEXP_USE_TYPE_DEFS
_FN2(SEXP_STRING, SEXP_FIXNUM, "register-simple-type", 0, sexp_register_simple_type),
_FN2(SEXP_STRING, SEXP_FIXNUM, "make-type-predicate", 0, sexp_make_type_predicate),
_FN2(SEXP_STRING, SEXP_FIXNUM, "make-constructor", 0, sexp_make_constructor),
_FN3(SEXP_STRING, SEXP_FIXNUM, "make-getter", 0, sexp_make_getter),
_FN3(SEXP_STRING, SEXP_FIXNUM, "make-setter", 0, sexp_make_setter),
#endif
#if PLAN9
#include "opt/plan9-opcodes.c"
#endif
#if SEXP_USE_MODULES
_FN1(SEXP_ENV, "env-exports", 0, sexp_env_exports),
_FN1(SEXP_STRING, "find-module-file", 0, sexp_find_module_file_op),
_FN2(SEXP_STRING, SEXP_ENV, "load-module-file", 0, sexp_load_module_file_op),
_FN2(SEXP_STRING, SEXP_BOOLEAN, "add-module-directory", 0, sexp_add_module_directory),
#endif
};

