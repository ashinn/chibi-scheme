
typedef sexp (*sexp_proc8) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc9) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc10) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc11) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc12) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc13) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc14) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc15) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc16) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc17) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc18) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);
typedef sexp (*sexp_proc19) (sexp, sexp, sexp_sint_t, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp, sexp);

#define _A(i) stack[top-i]

sexp sexp_fcall (sexp ctx, sexp self, sexp_sint_t n, sexp f) {
  sexp *stack = sexp_stack_data(sexp_context_stack(ctx));
  sexp_sint_t top = sexp_context_top(ctx);
  switch (n) {
  case 5: return ((sexp_proc6)sexp_opcode_func(f))(ctx, f, 5, _A(1), _A(2), _A(3), _A(4), _A(5));
  case 6: return ((sexp_proc7)sexp_opcode_func(f))(ctx, f, 6, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6));
  case 7: return ((sexp_proc8)sexp_opcode_func(f))(ctx, f, 7, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7));
  case 8: return ((sexp_proc9)sexp_opcode_func(f))(ctx, f, 8, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8));
  case 9: return ((sexp_proc10)sexp_opcode_func(f))(ctx, f, 9, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9));
  case 10: return ((sexp_proc11)sexp_opcode_func(f))(ctx, f, 10, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9), _A(10));
  case 11: return ((sexp_proc12)sexp_opcode_func(f))(ctx, f, 11, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9), _A(10), _A(11));
  case 12: return ((sexp_proc13)sexp_opcode_func(f))(ctx, f, 12, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9), _A(10), _A(11), _A(12));
  case 13: return ((sexp_proc14)sexp_opcode_func(f))(ctx, f, 13, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9), _A(10), _A(11), _A(12), _A(13));
  case 14: return ((sexp_proc15)sexp_opcode_func(f))(ctx, f, 14, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9), _A(10), _A(11), _A(12), _A(13), _A(14));
  case 15: return ((sexp_proc16)sexp_opcode_func(f))(ctx, f, 15, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9), _A(10), _A(11), _A(12), _A(13), _A(14), _A(15));
  case 16: return ((sexp_proc17)sexp_opcode_func(f))(ctx, f, 16, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9), _A(10), _A(11), _A(12), _A(13), _A(14), _A(15), _A(16));
  case 17: return ((sexp_proc18)sexp_opcode_func(f))(ctx, f, 17, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9), _A(10), _A(11), _A(12), _A(13), _A(14), _A(15), _A(16), _A(17));
  case 18: return ((sexp_proc19)sexp_opcode_func(f))(ctx, f, 18, _A(1), _A(2), _A(3), _A(4), _A(5), _A(6), _A(7), _A(8), _A(9), _A(10), _A(11), _A(12), _A(13), _A(14), _A(15), _A(16), _A(17), _A(18));
  default: return sexp_user_exception(ctx, self, "too many FFI arguments", f);
  }
}
