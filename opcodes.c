
#include "chibi/eval.h"

#define _I(n) sexp_make_fixnum(n)

#define _OP(c,o,n,m,rt,a1,a2,a3,i,s,d,f) \
 {c, o, n, m, i, (sexp)s, d, NULL, NULL, rt, a1, a2, a3, NULL, NULL, SEXP_FALSE, f}

#define _GETTER(name, type, index) \
  {SEXP_OPC_GETTER, SEXP_OP_SLOT_REF, 1, 0, 0, (sexp)name, _I(type), _I(index), NULL, _I(SEXP_OBJECT), _I(type), NULL, NULL, NULL, NULL, SEXP_FALSE, NULL}
#define _SETTER(name, type, index) \
  {SEXP_OPC_SETTER, SEXP_OP_SLOT_SET, 2, 0, 0, (sexp)name, _I(type), _I(index), NULL, SEXP_VOID, _I(type), _I(SEXP_OBJECT), NULL, NULL, NULL, SEXP_FALSE, NULL}

#define _PARAM(n, t) \
  _OP(SEXP_OPC_PARAMETER, SEXP_OP_PARAMETER_REF, 0, 1, t, t, SEXP_FALSE, SEXP_FALSE, 0, n, SEXP_FALSE, 0)

#if SEXP_USE_IMAGE_LOADING
#define _FN(o,n,m,rt,a1,a2,a3,s,d,f) {SEXP_OPC_FOREIGN, o, n, m, 0, (sexp)s, d, (sexp)#f, NULL, rt, a1, a2, a3, NULL, NULL, SEXP_FALSE, (sexp_proc1)f}
#else
#define _FN(o,n,m,rt,a1,a2,a3,s,d,f) _OP(SEXP_OPC_FOREIGN, o, n, m, rt, a1, a2, a3, 0, s, d, (sexp_proc1)f)
#endif

#define _FN0(rt, s, d, f) _FN(SEXP_OP_FCALL0, 0, 0, rt, SEXP_FALSE, SEXP_FALSE, SEXP_FALSE, s, d, f)
#define _FN1(rt, a1, s, d, f) _FN(SEXP_OP_FCALL1, 1, 0, rt, a1, SEXP_FALSE, SEXP_FALSE, s, d, f)
#define _FN1OPT(rt, a1, s, d, f) _FN(SEXP_OP_FCALL1, 0, 1, rt, a1, SEXP_FALSE, SEXP_FALSE, s, d, f)
#define _FN1OPTP(rt, a1, s, d, f) _FN(SEXP_OP_FCALL1, 0, 3, rt, a1, SEXP_FALSE, SEXP_FALSE, s, d, f)
#define _FN2(rt, a1, a2, s, d, f) _FN(SEXP_OP_FCALL2, 2, 0, rt, a1, a2, SEXP_FALSE, s, d, f)
#define _FN2OPT(rt, a1, a2, s, d, f) _FN(SEXP_OP_FCALL2, 1, 1, rt, a1, a2, SEXP_FALSE, s, d, f)
#define _FN2OPTP(rt, a1, a2, s, d, f) _FN(SEXP_OP_FCALL2, 1, 3, rt, a1, a2, SEXP_FALSE, s, d, f)
#define _FN3(rt, a1, a2, a3, s, d, f) _FN(SEXP_OP_FCALL3, 3, 0, rt, a1, a2, a3, s, d, f)
#define _FN3OPT(rt, a1, a2, a3, s, d, f) _FN(SEXP_OP_FCALL3, 2, 1, rt, a1, a2, a3, s, d, f)
#define _FN4(rt, a1, a2, a3, s, d, f) _FN(SEXP_OP_FCALL4, 4, 0, rt, a1, a2, a3, s, d, f)
#define _FN5(rt, a1, a2, a3, s, d, f) _FN(SEXP_OP_FCALLN, 5, 0, rt, a1, a2, a3, s, d, f)

static struct sexp_opcode_struct opcodes[] = {
_PARAM("current-input-port", _I(SEXP_IPORT)),
_PARAM("current-output-port", _I(SEXP_OPORT)),
_PARAM("current-error-port", _I(SEXP_OPORT)),
_PARAM("current-exception-handler", _I(SEXP_PROCEDURE)),
_PARAM("interaction-environment", _I(SEXP_ENV)),
_PARAM("command-line", SEXP_NULL),
_OP(SEXP_OPC_GETTER, SEXP_OP_CAR, 1, 0, _I(SEXP_OBJECT), _I(SEXP_PAIR), SEXP_FALSE, SEXP_FALSE, 0, "car", 0, NULL),
_OP(SEXP_OPC_SETTER, SEXP_OP_SET_CAR, 2, 0, SEXP_VOID, _I(SEXP_PAIR), _I(SEXP_OBJECT), SEXP_FALSE, 0, "set-car!", 0, NULL),
_OP(SEXP_OPC_GETTER, SEXP_OP_CDR, 1, 0, _I(SEXP_OBJECT), _I(SEXP_PAIR), SEXP_FALSE, SEXP_FALSE, 0, "cdr", 0, NULL),
_OP(SEXP_OPC_SETTER, SEXP_OP_SET_CDR, 2, 0, SEXP_VOID, _I(SEXP_PAIR), _I(SEXP_OBJECT), SEXP_FALSE, 0, "set-cdr!", 0, NULL),
_GETTER("pair-source", SEXP_PAIR, 2),
_SETTER("pair-source-set!", SEXP_PAIR, 2),
_OP(SEXP_OPC_GETTER, SEXP_OP_VECTOR_REF, 2, 0, _I(SEXP_OBJECT), _I(SEXP_VECTOR), _I(SEXP_FIXNUM), SEXP_FALSE, 0,"vector-ref", 0, NULL),
_OP(SEXP_OPC_SETTER, SEXP_OP_VECTOR_SET, 3, 0, SEXP_VOID, _I(SEXP_VECTOR), _I(SEXP_FIXNUM), _I(SEXP_OBJECT), 0,"vector-set!", 0, NULL),
_OP(SEXP_OPC_GETTER, SEXP_OP_VECTOR_LENGTH, 1, 0, _I(SEXP_FIXNUM), _I(SEXP_VECTOR), SEXP_FALSE, SEXP_FALSE, 0,"vector-length", 0, NULL),
_OP(SEXP_OPC_GETTER, SEXP_OP_BYTES_REF, 2, 0, _I(SEXP_FIXNUM), _I(SEXP_BYTES), _I(SEXP_FIXNUM), SEXP_FALSE, 0,"bytevector-u8-ref", 0, NULL),
_OP(SEXP_OPC_SETTER, SEXP_OP_BYTES_SET, 3, 0, SEXP_VOID, _I(SEXP_BYTES), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), 0,"bytevector-u8-set!", 0, NULL),
_OP(SEXP_OPC_GETTER, SEXP_OP_BYTES_LENGTH, 1, 0, _I(SEXP_FIXNUM), _I(SEXP_BYTES), SEXP_FALSE, SEXP_FALSE, 0,"bytevector-length", 0, NULL),
#if SEXP_USE_UTF8_STRINGS
_OP(SEXP_OPC_GETTER, SEXP_OP_STRING_REF, 2, 0, _I(SEXP_CHAR), _I(SEXP_STRING), _I(SEXP_FIXNUM), SEXP_FALSE, 0,"string-cursor-ref", 0, NULL),
_OP(SEXP_OPC_GETTER, SEXP_OP_STRING_CURSOR_NEXT, 2, 0, _I(SEXP_FIXNUM), _I(SEXP_STRING), _I(SEXP_FIXNUM), SEXP_FALSE, 0,"string-cursor-next", 0, NULL),
_OP(SEXP_OPC_GETTER, SEXP_OP_STRING_CURSOR_PREV, 2, 0, _I(SEXP_FIXNUM), _I(SEXP_STRING), _I(SEXP_FIXNUM), SEXP_FALSE, 0,"string-cursor-prev", 0, NULL),
_OP(SEXP_OPC_GETTER, SEXP_OP_STRING_SIZE, 1, 0, _I(SEXP_FIXNUM), _I(SEXP_STRING), SEXP_FALSE, SEXP_FALSE, 0,"string-size", 0, NULL),
#else
_OP(SEXP_OPC_GETTER, SEXP_OP_STRING_REF, 2, 0, _I(SEXP_CHAR), _I(SEXP_STRING), _I(SEXP_FIXNUM), SEXP_FALSE, 0,"string-ref", 0, NULL),
#endif
#if SEXP_USE_MUTABLE_STRINGS
#if SEXP_USE_UTF8_STRINGS
_OP(SEXP_OPC_SETTER, SEXP_OP_STRING_SET, 3, 0, SEXP_VOID, _I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_CHAR), 0,"string-cursor-set!", 0, NULL),
#else
_OP(SEXP_OPC_SETTER, SEXP_OP_STRING_SET, 3, 0, SEXP_VOID, _I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_CHAR), 0,"string-set!", 0, NULL),
#endif
#endif
_OP(SEXP_OPC_GETTER, SEXP_OP_STRING_LENGTH, 1, 0, _I(SEXP_FIXNUM), _I(SEXP_STRING), SEXP_FALSE, SEXP_FALSE, 0,"string-length", 0, NULL),
#if SEXP_USE_NATIVE_X86
_FN1(_I(SEXP_FLONUM), _I(SEXP_FIXNUM), "exact->inexact", 0, sexp_exact_to_inexact),
_FN1(_I(SEXP_FIXNUM), _I(SEXP_FLONUM), "inexact->exact", 0, sexp_inexact_to_exact),
_FN1(_I(SEXP_CHAR), _I(SEXP_CHAR), "char-upcase", 0, sexp_char_upcase),
_FN1(_I(SEXP_CHAR), _I(SEXP_CHAR), "char-downcase", 0, sexp_char_downcase),
#else
_OP(SEXP_OPC_GENERIC, SEXP_OP_FIX2FLO, 1, 0, _I(SEXP_FLONUM), _I(SEXP_NUMBER), SEXP_FALSE, SEXP_FALSE, 0, "exact->inexact", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_FLO2FIX, 1, 0, _I(SEXP_FIXNUM), _I(SEXP_NUMBER), SEXP_FALSE, SEXP_FALSE, 0, "inexact->exact", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_CHAR_UPCASE, 1, 0, _I(SEXP_CHAR), _I(SEXP_CHAR), SEXP_FALSE, SEXP_FALSE, 0, "char-upcase", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_CHAR_DOWNCASE, 1, 0, _I(SEXP_CHAR), _I(SEXP_CHAR), SEXP_FALSE, SEXP_FALSE, 0, "char-downcase", 0, NULL),
#endif
_OP(SEXP_OPC_GENERIC, SEXP_OP_CHAR2INT, 1, 0, _I(SEXP_FIXNUM), _I(SEXP_CHAR), SEXP_FALSE, SEXP_FALSE, 0, "char->integer", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_INT2CHAR, 1, 0, _I(SEXP_CHAR), _I(SEXP_FIXNUM), SEXP_FALSE, SEXP_FALSE, 0, "integer->char", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_ADD, 0, 1, _I(SEXP_NUMBER), _I(SEXP_NUMBER), _I(SEXP_NUMBER), SEXP_FALSE, 0, "+", SEXP_ZERO, NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_MUL, 0, 1, _I(SEXP_NUMBER), _I(SEXP_NUMBER), _I(SEXP_NUMBER), SEXP_FALSE, 0, "*", SEXP_ONE, NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_SUB, 1, 1, _I(SEXP_NUMBER), _I(SEXP_NUMBER), _I(SEXP_NUMBER), SEXP_FALSE, 1, "-", SEXP_ZERO, NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_DIV, 1, 1, _I(SEXP_NUMBER), _I(SEXP_NUMBER), _I(SEXP_NUMBER), SEXP_FALSE, 1, "/", SEXP_ONE, NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_QUOTIENT, 2, 0, _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), SEXP_FALSE, 0, "quotient", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC,     SEXP_OP_REMAINDER, 2, 0, _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), SEXP_FALSE, 0, "remainder", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_LT,  2, 1, _I(SEXP_BOOLEAN), _I(SEXP_NUMBER), _I(SEXP_NUMBER), SEXP_FALSE, 0, "<", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_LE,  2, 1, _I(SEXP_BOOLEAN), _I(SEXP_NUMBER), _I(SEXP_NUMBER), SEXP_FALSE, 0, "<=", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_LT,  2, 1, _I(SEXP_BOOLEAN), _I(SEXP_NUMBER), _I(SEXP_NUMBER), SEXP_FALSE, 1, ">", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_LE,  2, 1, _I(SEXP_BOOLEAN), _I(SEXP_NUMBER), _I(SEXP_NUMBER), SEXP_FALSE, 1, ">=", 0, NULL),
_OP(SEXP_OPC_ARITHMETIC_CMP, SEXP_OP_EQN, 2, 1, _I(SEXP_BOOLEAN), _I(SEXP_NUMBER), _I(SEXP_NUMBER), SEXP_FALSE, 0, "=", 0, NULL),
_OP(SEXP_OPC_PREDICATE,      SEXP_OP_EQ,  2, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), _I(SEXP_OBJECT), SEXP_FALSE, 0, "eq?", 0, NULL),
_OP(SEXP_OPC_CONSTRUCTOR,    SEXP_OP_CONS, 2, 0, _I(SEXP_PAIR), _I(SEXP_OBJECT), _I(SEXP_OBJECT), SEXP_FALSE, 0, "cons", 0, NULL),
_OP(SEXP_OPC_CONSTRUCTOR,    SEXP_OP_MAKE_VECTOR, 1, 1, _I(SEXP_VECTOR), _I(SEXP_FIXNUM), _I(SEXP_OBJECT), SEXP_FALSE, 0, "make-vector", SEXP_VOID, NULL),
_OP(SEXP_OPC_CONSTRUCTOR,    SEXP_OP_MAKE_EXCEPTION, 5, 0, _I(SEXP_EXCEPTION), _I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_OBJECT), 0, "make-exception", 0, NULL),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_ISA,  2, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), _I(SEXP_OBJECT), SEXP_FALSE, 0, "is-a?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_NULLP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "null?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_EOFP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "eof-object?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_SYMBOLP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "symbol?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_CHARP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "char?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_FIXNUMP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "fixnum?", NULL, 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "pair?", _I(SEXP_PAIR), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "string?", _I(SEXP_STRING), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "vector?", _I(SEXP_VECTOR), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "bytevector?", _I(SEXP_BYTES), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "fileno?", _I(SEXP_FILENO), 0),
#if SEXP_USE_IMMEDIATE_FLONUMS
_FN1(_I(SEXP_BOOLEAN), _I(SEXP_OBJECT), "flonum?", 0, sexp_flonump_op),
#else
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "flonum?", _I(SEXP_FLONUM), 0),
#endif
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "bignum?", _I(SEXP_BIGNUM), 0),
#if SEXP_USE_RATIOS
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "ratio?", _I(SEXP_RATIO), 0),
_FN1(_I(SEXP_FIXNUM), _I(SEXP_RATIO), "ratio-numerator", 0, sexp_ratio_numerator_op),
_FN1(_I(SEXP_FIXNUM), _I(SEXP_RATIO), "ratio-denominator", 0, sexp_ratio_denominator_op),
#endif
#if SEXP_USE_COMPLEX
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "%complex?", _I(SEXP_COMPLEX), 0),
_FN1(_I(SEXP_NUMBER), _I(SEXP_COMPLEX), "complex-real", 0, sexp_complex_real_op),
_FN1(_I(SEXP_NUMBER), _I(SEXP_COMPLEX), "complex-imag", 0, sexp_complex_imag_op),
#endif
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "closure?", _I(SEXP_PROCEDURE), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "opcode?", _I(SEXP_OPCODE), 0),
_OP(SEXP_OPC_TYPE_PREDICATE, SEXP_OP_TYPEP,  1, 0, _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "input-port?", _I(SEXP_IPORT), 0),
_FN1(_I(SEXP_BOOLEAN), _I(SEXP_IPORT), "output-port?", 0, sexp_port_outputp_op),
_FN1(_I(SEXP_BOOLEAN), _I(SEXP_IPORT), "binary-port?", 0, sexp_port_binaryp_op),
_FN1(_I(SEXP_BOOLEAN), _I(SEXP_IPORT), "port-open?", 0, sexp_port_openp_op),
_OP(SEXP_OPC_GENERIC, SEXP_OP_APPLY1, 2, 16, _I(SEXP_OBJECT), _I(SEXP_PROCEDURE), SEXP_NULL, SEXP_FALSE, 0, "apply1", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_CALLCC, 1, 0, _I(SEXP_OBJECT), _I(SEXP_PROCEDURE), SEXP_FALSE, SEXP_FALSE, 0, "%call/cc", 0, NULL),
_OP(SEXP_OPC_GENERIC, SEXP_OP_RAISE, 1, 0, _I(SEXP_OBJECT), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "raise", 0, NULL),
#if SEXP_USE_NATIVE_X86
_FN2OPTP(SEXP_VOID, _I(SEXP_CHAR), _I(SEXP_OPORT), "write-char", (sexp)"current-output-port", sexp_write_char_op),
_FN1OPTP(SEXP_VOID, _I(SEXP_IPORT), "read-char", (sexp)"current-input-port", sexp_read_char_op),
_FN1OPTP(SEXP_VOID, _I(SEXP_IPORT), "peek-char", (sexp)"current-input-port", sexp_peek_char_op),
_FN5(_I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_OBJECT), "five", 0, sexp_five),
#else
_OP(SEXP_OPC_IO, SEXP_OP_WRITE_CHAR, 1, 3, SEXP_VOID, _I(SEXP_CHAR), _I(SEXP_OPORT), SEXP_FALSE, 0, "write-char", (sexp)"current-output-port", NULL),
_OP(SEXP_OPC_IO, SEXP_OP_WRITE_STRING, 2, 3, SEXP_VOID, _I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_OPORT), 0, "%write-string", (sexp)"current-output-port", NULL),
_OP(SEXP_OPC_IO, SEXP_OP_READ_CHAR, 0, 3, _I(SEXP_CHAR), _I(SEXP_IPORT), SEXP_FALSE, SEXP_FALSE, 0, "read-char", (sexp)"current-input-port", NULL),
_OP(SEXP_OPC_IO, SEXP_OP_PEEK_CHAR, 0, 3, _I(SEXP_CHAR), _I(SEXP_IPORT), SEXP_FALSE, SEXP_FALSE, 0, "peek-char", (sexp)"current-input-port", NULL),
#endif
_FN1OPTP(_I(SEXP_BOOLEAN), _I(SEXP_IPORT), "char-ready?", (sexp)"current-input-port", sexp_char_ready_p),
_FN1OPTP(_I(SEXP_OBJECT), _I(SEXP_IPORT), "read", (sexp)"current-input-port", sexp_read_op),
_FN2OPTP(SEXP_VOID,_I(SEXP_OBJECT), _I(SEXP_OPORT), "write", (sexp)"current-output-port", sexp_write_op),
_FN1OPTP(SEXP_VOID, _I(SEXP_OPORT), "flush-output", (sexp)"current-output-port", sexp_flush_output_op),
_FN2(_I(SEXP_BOOLEAN), _I(SEXP_OBJECT), _I(SEXP_OBJECT), "equal?", 0, sexp_equalp_op),
_FN4(_I(SEXP_BOOLEAN), _I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_OBJECT), "equal?/bounded", 0, sexp_equalp_bound),
_FN1(_I(SEXP_BOOLEAN), _I(SEXP_OBJECT), "list?", 0, sexp_listp_op),
_FN1(_I(SEXP_BOOLEAN), _I(SEXP_OBJECT), "identifier?", 0, sexp_identifierp_op),
_FN1(_I(SEXP_SYMBOL), _I(SEXP_OBJECT), "identifier->symbol", 0, sexp_syntactic_closure_expr_op),
_FN4(_I(SEXP_BOOLEAN), _I(SEXP_OBJECT), _I(SEXP_ENV), _I(SEXP_OBJECT), "identifier=?", 0, sexp_identifier_eq_op),
_FN1(_I(SEXP_FIXNUM), SEXP_NULL, "length*", 0, sexp_length_op),
_FN1(SEXP_NULL, SEXP_NULL, "reverse", 0, sexp_reverse_op),
_FN1(SEXP_NULL, SEXP_NULL, "reverse!", 0, sexp_nreverse_op),
_FN2(SEXP_NULL, SEXP_NULL, SEXP_NULL, "append2", 0, sexp_append2_op),
_FN1(_I(SEXP_VECTOR), SEXP_NULL, "list->vector", 0, sexp_list_to_vector_op),
_FN1(_I(SEXP_IPORT), _I(SEXP_STRING), "open-input-file", 0, sexp_open_input_file_op),
_FN1(_I(SEXP_OPORT), _I(SEXP_STRING), "open-output-file", 0, sexp_open_output_file_op),
_FN1(_I(SEXP_IPORT), _I(SEXP_STRING), "open-binary-input-file", 0, sexp_open_binary_input_file),
_FN1(_I(SEXP_OPORT), _I(SEXP_STRING), "open-binary-output-file", 0, sexp_open_binary_output_file),
_FN1(SEXP_VOID, _I(SEXP_IPORT), "close-input-port", 0, sexp_close_port_op),
_FN1(SEXP_VOID, _I(SEXP_OPORT), "close-output-port", 0, sexp_close_port_op),
_FN0(_I(SEXP_ENV), "make-environment", 0, sexp_make_env_op),
_FN1(_I(SEXP_ENV), _I(SEXP_FIXNUM), "null-environment", 0, sexp_make_null_env_op),
_FN1(_I(SEXP_ENV), _I(SEXP_FIXNUM), "scheme-report-environment", 0, sexp_make_standard_env_op),
_FN2OPTP(_I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_ENV), "compile", (sexp)"interaction-environment", sexp_compile_op),
_FN2OPTP(_I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_ENV), "generate", (sexp)"interaction-environment", sexp_generate_op),
_FN2OPTP(SEXP_VOID, _I(SEXP_STRING), _I(SEXP_ENV), "%load", (sexp)"interaction-environment", sexp_load_op),
_FN4(SEXP_VOID, _I(SEXP_ENV), _I(SEXP_ENV), _I(SEXP_OBJECT), "%import", 0, sexp_env_import_op),
_FN2OPTP(SEXP_VOID, _I(SEXP_EXCEPTION), _I(SEXP_OPORT), "print-exception", (sexp)"current-error-port", sexp_print_exception_op),
_FN1OPTP(SEXP_VOID, _I(SEXP_OPORT), "print-stack-trace", (sexp)"current-error-port", sexp_stack_trace_op),
_FN3OPT(SEXP_VOID, _I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_OBJECT), "warn-undefs", SEXP_FALSE, sexp_warn_undefs_op),
_FN1(_I(SEXP_OBJECT), _I(SEXP_EXCEPTION), "exception-type", 0, sexp_exception_type_op),
_FN2OPT(_I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_CHAR), "make-string", sexp_make_character(' '), sexp_make_string_op),
_FN2OPT(_I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), "make-bytevector", SEXP_ZERO, sexp_make_bytes_op),
_FN2OPT(_I(SEXP_NUMBER), _I(SEXP_STRING), _I(SEXP_FIXNUM), "string->number", SEXP_TEN, sexp_string_to_number_op),
_FN3(_I(SEXP_FIXNUM), _I(SEXP_STRING), _I(SEXP_STRING), _I(SEXP_BOOLEAN), "string-cmp", 0, sexp_string_cmp_op),
_FN1(_I(SEXP_SYMBOL), _I(SEXP_STRING), "string->symbol", 0, sexp_string_to_symbol_op),
_FN1(_I(SEXP_STRING), _I(SEXP_SYMBOL), "symbol->string", 0, sexp_symbol_to_string_op),
_FN2OPT(_I(SEXP_STRING), SEXP_NULL, _I(SEXP_STRING), "string-concatenate", SEXP_FALSE, sexp_string_concatenate_op),
_FN2(_I(SEXP_OBJECT), _I(SEXP_OBJECT), SEXP_NULL, "memq", 0, sexp_memq_op),
_FN2(_I(SEXP_OBJECT), _I(SEXP_OBJECT), SEXP_NULL, "assq", 0, sexp_assq_op),
_FN3(_I(SEXP_SYNCLO), _I(SEXP_ENV), SEXP_NULL, _I(SEXP_OBJECT), "make-syntactic-closure", 0, sexp_make_synclo_op),
_FN1(_I(SEXP_OBJECT), _I(SEXP_OBJECT), "strip-syntactic-closures", 0, sexp_strip_synclos),
_FN0(_I(SEXP_OPORT), "open-output-string", 0, sexp_make_output_string_port_op),
_FN1(_I(SEXP_IPORT), _I(SEXP_STRING), "open-input-string", 0, sexp_make_input_string_port_op),
_FN1(_I(SEXP_STRING), _I(SEXP_OPORT), "get-output-string", 0, sexp_get_output_string_op),
_FN2OPT(_I(SEXP_IPORT), _I(SEXP_FIXNUM), _I(SEXP_BOOLEAN), "open-input-file-descriptor", SEXP_FALSE, sexp_open_input_file_descriptor),
_FN2OPT(_I(SEXP_OPORT), _I(SEXP_FIXNUM), _I(SEXP_BOOLEAN), "open-output-file-descriptor", SEXP_FALSE, sexp_open_output_file_descriptor),
_FN2(_I(SEXP_VOID), _I(SEXP_IPORT), _I(SEXP_FIXNUM), "set-port-line!", 0, sexp_set_port_line_op),
_FN2OPT(_I(SEXP_OBJECT), _I(SEXP_PROCEDURE), _I(SEXP_FIXNUM), "register-optimization!", _I(600), sexp_register_optimization),
#if SEXP_USE_MATH
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "exp", 0, sexp_exp),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "ln", 0, sexp_log),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "sin", 0, sexp_sin),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "cos", 0, sexp_cos),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "tan", 0, sexp_tan),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "asin", 0, sexp_asin),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "acos", 0, sexp_acos),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "atan1", 0, sexp_atan),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "sqrt", 0, sexp_sqrt),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "round", 0, sexp_round),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "truncate", 0, sexp_trunc),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "floor", 0, sexp_floor),
_FN1(_I(SEXP_NUMBER), _I(SEXP_NUMBER), "ceiling", 0, sexp_ceiling),
#endif
_FN2(_I(SEXP_NUMBER), _I(SEXP_NUMBER), _I(SEXP_NUMBER), "expt", 0, sexp_expt_op),
#if SEXP_USE_UTF8_STRINGS
_FN2(_I(SEXP_FIXNUM), _I(SEXP_STRING), _I(SEXP_FIXNUM), "string-index->offset", 0, sexp_string_index_to_offset),
_FN2(_I(SEXP_CHAR), _I(SEXP_STRING), _I(SEXP_FIXNUM), "string-ref", 0, sexp_string_utf8_index_ref),
#if SEXP_USE_MUTABLE_STRINGS
_FN3(SEXP_VOID, _I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_CHAR), "string-set!", 0, sexp_string_utf8_index_set),
#endif
_FN3OPT(_I(SEXP_STRING), _I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), "substring-cursor", SEXP_FALSE, sexp_substring_op),
_FN3OPT(_I(SEXP_STRING), _I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), "substring", SEXP_FALSE, sexp_utf8_substring_op),
#else
_FN3OPT(_I(SEXP_STRING), _I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), "substring", SEXP_FALSE, sexp_substring_op),
#endif
_FN3OPT(_I(SEXP_BYTES), _I(SEXP_BYTES), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), "subbytes", SEXP_FALSE, sexp_subbytes_op),
#if SEXP_USE_FOLD_CASE_SYMS
_FN1(SEXP_VOID, _I(SEXP_IPORT), "port-fold-case?", 0, sexp_get_port_fold_case),
_FN2(SEXP_VOID, _I(SEXP_IPORT), _I(SEXP_BOOLEAN), "set-port-fold-case!", 0, sexp_set_port_fold_case),
#endif
#if SEXP_USE_OBJECT_BRACE_LITERALS
_FN2(_I(SEXP_TYPE), _I(SEXP_STRING), _I(SEXP_OBJECT), "lookup-type", 0, sexp_lookup_type_op),
#endif
#if SEXP_USE_TYPE_DEFS
_FN3(_I(SEXP_TYPE), _I(SEXP_STRING), _I(SEXP_TYPE), SEXP_NULL, "register-simple-type", 0, sexp_register_simple_type_op),
_FN2(_I(SEXP_OPCODE), _I(SEXP_STRING), _I(SEXP_FIXNUM), "make-type-predicate", 0, sexp_make_type_predicate_op),
_FN2(_I(SEXP_OPCODE), _I(SEXP_STRING), _I(SEXP_FIXNUM), "make-constructor", 0, sexp_make_constructor_op),
_FN3(_I(SEXP_OPCODE), _I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), "make-getter", 0, sexp_make_getter_op),
_FN3(_I(SEXP_OPCODE), _I(SEXP_STRING), _I(SEXP_FIXNUM), _I(SEXP_FIXNUM), "make-setter", 0, sexp_make_setter_op),
_FN2(_I(SEXP_OPCODE), _I(SEXP_TYPE), _I(SEXP_SYMBOL), "type-slot-offset", 0, sexp_type_slot_offset_op),
_OP(SEXP_OPC_GETTER, SEXP_OP_SLOTN_REF, 3, 0, _I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_FIXNUM), 0, "slot-ref", 0, NULL),
_OP(SEXP_OPC_SETTER, SEXP_OP_SLOTN_SET, 4, 0, SEXP_VOID, _I(SEXP_OBJECT), _I(SEXP_OBJECT), _I(SEXP_FIXNUM), 0,"slot-set!", 0, NULL),
#endif
#if PLAN9
#include "opt/plan9-opcodes.c"
#endif
#if SEXP_USE_MODULES
_FN0(_I(SEXP_ENV), "current-environment", 0, sexp_current_environment),
_FN0(_I(SEXP_ENV), "%meta-env", 0, sexp_meta_environment),
_FN1(SEXP_NULL, _I(SEXP_ENV), "env-exports", 0, sexp_env_exports_op),
_FN0(_I(SEXP_PAIR), "current-module-path", 0, sexp_current_module_path_op),
_FN1(_I(SEXP_STRING), _I(SEXP_STRING), "find-module-file", 0, sexp_find_module_file_op),
_FN2(SEXP_VOID, _I(SEXP_STRING), _I(SEXP_ENV), "load-module-file", 0, sexp_load_module_file_op),
_FN2(SEXP_VOID, _I(SEXP_STRING), _I(SEXP_BOOLEAN), "add-module-directory", 0, sexp_add_module_directory_op),
#endif
#if SEXP_USE_GREEN_THREADS
_FN1OPT(_I(SEXP_OBJECT), _I(SEXP_OBJECT), "%dk", SEXP_FALSE, sexp_dk),
_OP(SEXP_OPC_GENERIC, SEXP_OP_YIELD, 0, 0, SEXP_VOID, SEXP_FALSE, SEXP_FALSE, SEXP_FALSE, 0, "yield!", 0, NULL),
#endif
_FN0(_I(SEXP_OBJECT), "thread-parameters", 0, sexp_thread_parameters),
_FN1(_I(SEXP_OBJECT), _I(SEXP_OBJECT), "thread-parameters-set!", 0, sexp_thread_parameters_set),
#if SEXP_USE_PROFILE_VM
_FN0(SEXP_VOID, "reset-vm-profile", 0, sexp_reset_vm_profile),
_FN0(SEXP_VOID, "print-vm-profile", 0, sexp_print_vm_profile),
#endif
#if SEXP_USE_AUTO_FORCE
_OP(SEXP_OPC_GENERIC, SEXP_OP_FORCE, 1, 0, _I(SEXP_OBJECT), _I(SEXP_OBJECT), SEXP_FALSE, SEXP_FALSE, 0, "force", 0, NULL),
_FN2(_I(SEXP_PROMISE), _I(SEXP_BOOLEAN), _I(SEXP_OBJECT), "promise", 0, sexp_make_promise),
#endif
_OP(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
};

struct sexp_opcode_struct* sexp_primitive_opcodes = opcodes;
