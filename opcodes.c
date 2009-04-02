
#define _OP(c,o,n,m,t,u,i,s,d,p) {.tag=SEXP_OPCODE, .value={.opcode={c, o, n, m, t, u, i, s, d, p}}}
#define _FN(o,n,t,u,s,f) _OP(OPC_FOREIGN, o, n, 0, t, u, 0, s, (sexp)f, NULL)
#define _FN0(s, f) _FN(OP_FCALL0, 0, 0, 0, s, f)
#define _FN1(t, s, f) _FN(OP_FCALL1, 1, t, 0, s, f)
#define _FN2(t, u, s, f) _FN(OP_FCALL2, 2, t, u, s, f)
#define _FN3(t, u, s, f) _FN(OP_FCALL3, 3, t, u, s, f)
#define _FN4(t, u, s, f) _FN(OP_FCALL4, 4, t, u, s, f)
#define _PARAM(n,a,t) _OP(OPC_PARAMETER, OP_NOOP, 0, 2, t, 0, 0, n, a, NULL)

static struct sexp_struct opcodes[] = {
_OP(OPC_ACCESSOR, OP_CAR, 1, 0, SEXP_PAIR, 0, 0, "car", NULL, NULL),
_OP(OPC_ACCESSOR, OP_SET_CAR, 2, 0, SEXP_PAIR, 0, 0, "set-car!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_CDR, 1, 0, SEXP_PAIR, 0, 0, "cdr", NULL, NULL),
_OP(OPC_ACCESSOR, OP_SET_CDR, 2, 0, SEXP_PAIR, 0, 0, "set-cdr!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_VECTOR_REF,2,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-ref", NULL, NULL),
_OP(OPC_ACCESSOR, OP_VECTOR_SET,3,0, SEXP_VECTOR, SEXP_FIXNUM, 0,"vector-set!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_VECTOR_LENGTH,1,0, SEXP_VECTOR, 0, 0,"vector-length", NULL, NULL),
_OP(OPC_ACCESSOR, OP_STRING_REF,2,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-ref", NULL, NULL),
_OP(OPC_ACCESSOR, OP_STRING_SET,3,0, SEXP_STRING, SEXP_FIXNUM, 0,"string-set!", NULL, NULL),
_OP(OPC_ACCESSOR, OP_STRING_LENGTH,1,0, SEXP_STRING, 0, 0,"string-length", NULL, NULL),
_OP(OPC_GENERIC, OP_FIX2FLO, 1, 0, 0, 0, 0, "exact->inexact", NULL, NULL),
_OP(OPC_GENERIC, OP_FLO2FIX, 1, 0, 0, 0, 0, "inexact->exact", NULL, NULL),
_OP(OPC_GENERIC, OP_CHAR2INT, 1, 0, SEXP_CHAR, 0, 0, "char->integer", NULL, NULL),
_OP(OPC_GENERIC, OP_INT2CHAR, 1, 0, SEXP_FIXNUM, 0, 0, "integer->char", NULL, NULL),
_OP(OPC_GENERIC, OP_CHAR_UPCASE, 1, 0, SEXP_CHAR, 0, 0, "char-upcase", NULL, NULL),
_OP(OPC_GENERIC, OP_CHAR_DOWNCASE, 1, 0, SEXP_CHAR, 0, 0, "char-downcase", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_ADD, 0, 1, SEXP_FIXNUM, 0, 0, "+", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_MUL, 0, 1, SEXP_FIXNUM, 0, 0, "*", NULL, NULL),
_OP(OPC_ARITHMETIC_INV, OP_SUB, 0, 1, SEXP_FIXNUM, 0, OP_NEGATIVE, "-", NULL, NULL),
_OP(OPC_ARITHMETIC_INV, OP_DIV, 0, 1, SEXP_FIXNUM, 0, OP_INVERSE, "/", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_QUOTIENT, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, 0, "quotient", NULL, NULL),
_OP(OPC_ARITHMETIC,     OP_REMAINDER, 2, 0, SEXP_FIXNUM, SEXP_FIXNUM, 0, "remainder", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_LT,  0, 1, SEXP_FIXNUM, 0, 0, "<", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_LE,  0, 1, SEXP_FIXNUM, 0, 0, "<=", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_LT,  0, 1, SEXP_FIXNUM, 0, 1, ">", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_LE,  0, 1, SEXP_FIXNUM, 0, 1, ">=", NULL, NULL),
_OP(OPC_ARITHMETIC_CMP, OP_EQN,  0, 1, SEXP_FIXNUM, 0, 0, "=", NULL, NULL),
_OP(OPC_PREDICATE,      OP_EQ,  2, 0, 0, 0, 0, "eq?", NULL, NULL),
_OP(OPC_CONSTRUCTOR,    OP_CONS, 2, 0, 0, 0, 0, "cons", NULL, NULL),
_OP(OPC_CONSTRUCTOR,    OP_MAKE_VECTOR, 2, 0, SEXP_FIXNUM, 0, 0, "make-vector", NULL, NULL),
_OP(OPC_CONSTRUCTOR,    OP_MAKE_PROCEDURE, 4, 0, 0, 0, 0, "make-procedure", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_NULLP,  1, 0, 0, 0, 0, "null?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_EOFP,  1, 0, 0, 0, 0, "eof-object?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_SYMBOLP,  1, 0, 0, 0, 0, "symbol?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_CHARP,  1, 0, 0, 0, 0, "char?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_INTEGERP,  1, 0, 0, 0, 0, "fixnum?", NULL, NULL),
_OP(OPC_TYPE_PREDICATE, OP_TYPEP,  1, 0, 0, 0, 0, "pair?", (sexp)SEXP_PAIR, NULL),
_OP(OPC_TYPE_PREDICATE, OP_TYPEP,  1, 0, 0, 0, 0, "string?", (sexp)SEXP_STRING, NULL),
_OP(OPC_TYPE_PREDICATE, OP_TYPEP,  1, 0, 0, 0, 0, "vector?", (sexp)SEXP_VECTOR, NULL),
_OP(OPC_TYPE_PREDICATE, OP_TYPEP,  1, 0, 0, 0, 0, "flonum?", (sexp)SEXP_FLONUM, NULL),
_OP(OPC_TYPE_PREDICATE, OP_TYPEP,  1, 0, 0, 0, 0, "procedure?", (sexp)SEXP_PROCEDURE, NULL),
_OP(OPC_TYPE_PREDICATE, OP_TYPEP,  1, 0, 0, 0, 0, "input-port?", (sexp)SEXP_IPORT, NULL),
_OP(OPC_TYPE_PREDICATE, OP_TYPEP,  1, 0, 0, 0, 0, "output-port?", (sexp)SEXP_OPORT, NULL),
_OP(OPC_GENERIC, OP_APPLY1, 2, 0, SEXP_PROCEDURE, SEXP_PAIR, 0, "apply1", NULL, NULL),
_OP(OPC_GENERIC, OP_CALLCC, 1, SEXP_PROCEDURE, 0, 0, 0, "call-with-current-continuation", NULL, NULL),
_OP(OPC_GENERIC, OP_ERROR, 1, SEXP_STRING, 0, 0, 0, "error", NULL, NULL),
_OP(OPC_IO, OP_WRITE, 1, 3, 0, SEXP_OPORT, 0, "write", (sexp)"*current-output-port*", NULL),
_OP(OPC_IO, OP_DISPLAY, 1, 3, 0, SEXP_OPORT, 0, "display", (sexp)"*current-output-port*", NULL),
_OP(OPC_IO, OP_WRITE_CHAR, 1, 3, 0, SEXP_OPORT, 0, "write-char", (sexp)"*current-output-port*", NULL),
_OP(OPC_IO, OP_NEWLINE, 0, 3, 0, SEXP_OPORT, 0, "newline", (sexp)"*current-output-port*", NULL),
_OP(OPC_IO, OP_FLUSH_OUTPUT, 0, 3, 0, SEXP_OPORT, 0, "flush-output", (sexp)"*current-output-port*", NULL),
_OP(OPC_IO, OP_READ, 0, 3, 0, SEXP_IPORT, 0, "read", (sexp)"*current-input-port*", NULL),
_OP(OPC_IO, OP_READ_CHAR, 0, 3, 0, SEXP_IPORT, 0, "read-char", (sexp)"*current-input-port*", NULL),
_OP(OPC_IO, OP_PEEK_CHAR, 0, 3, 0, SEXP_IPORT, 0, "peek-char", (sexp)"*current-input-port*", NULL),
_OP(OPC_GENERIC, OP_EVAL, 1, 3, 0, 0, 0, "eval", (sexp)"*interaction-environment*", NULL),
_FN2(0, 0, "equal?", sexp_equalp),
_FN1(0, "list?", sexp_listp),
_FN1(0, "identifier?", sexp_identifierp),
_FN4(0, SEXP_ENV, "identifier=?", sexp_identifier_eq),
_FN1(SEXP_PAIR, "length", sexp_length),
_FN1(SEXP_PAIR, "reverse", sexp_reverse),
_FN1(SEXP_PAIR, "list->vector", sexp_list_to_vector),
_FN1(SEXP_STRING, "open-input-file", sexp_open_input_file),
_FN1(SEXP_STRING, "open-output-file", sexp_open_output_file),
_FN1(SEXP_IPORT, "close-input-port", sexp_close_port),
_FN1(SEXP_OPORT, "close-output-port", sexp_close_port),
_FN1(SEXP_FIXNUM, "null-environment", sexp_make_null_env),
_FN1(SEXP_FIXNUM, "scheme-report-environment", sexp_make_standard_env),
_FN2(0, SEXP_ENV, "%load", sexp_load),
_FN2(SEXP_FIXNUM, SEXP_CHAR, "make-string", sexp_make_string),
_FN2(SEXP_STRING, SEXP_STRING, "string-cmp", sexp_string_cmp),
_FN2(SEXP_STRING, SEXP_STRING, "string-cmp-ci", sexp_string_cmp_ci),
_FN3(SEXP_STRING, SEXP_FIXNUM, "substring", sexp_substring),
_FN1(SEXP_PAIR, "string-concatenate", sexp_string_concatenate),
_FN2(0, SEXP_PAIR, "memq", sexp_memq),
_FN2(0, SEXP_PAIR, "assq", sexp_assq),
_FN3(SEXP_ENV, SEXP_PAIR, "make-syntactic-closure", sexp_make_synclo),
_PARAM("current-input-port", (sexp)"*current-input-port*", SEXP_IPORT),
_PARAM("current-output-port", (sexp)"*current-output-port*", SEXP_OPORT),
_PARAM("current-error-port", (sexp)"*current-error-port*", SEXP_OPORT),
_PARAM("current-error-handler", (sexp)"*current-error-handler*", SEXP_PROCEDURE),
_PARAM("interaction-environment", (sexp)"*interaction-environment*", SEXP_ENV),
#if USE_MATH
_FN1(0, "exp", sexp_exp),
_FN1(0, "log", sexp_log),
_FN1(0, "sin", sexp_sin),
_FN1(0, "cos", sexp_cos),
_FN1(0, "tan", sexp_tan),
_FN1(0, "asin", sexp_asin),
_FN1(0, "acos", sexp_acos),
_FN1(0, "atan", sexp_atan),
_FN1(0, "sqrt", sexp_sqrt),
_FN1(0, "round", sexp_round),
_FN1(0, "truncate", sexp_trunc),
_FN1(0, "floor", sexp_floor),
_FN1(0, "ceiling", sexp_ceiling),
_FN2(0, 0, "expt", sexp_expt),
#endif
#if USE_STRING_STREAMS
_FN0("open-output-string", sexp_make_output_string_port),
_FN1(SEXP_STRING, "open-input-string", sexp_make_input_string_port),
_FN1(SEXP_OPORT, "get-output-string", sexp_get_output_string),
#endif
#if USE_DEBUG
_FN2(SEXP_PROCEDURE, SEXP_OPORT, "disasm", sexp_disasm),
#endif
};

