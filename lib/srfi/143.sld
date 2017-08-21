(define-library (srfi 143)
  (import (scheme base)
          (srfi 141)
          (rename (srfi 151)
                  (bitwise-not fxnot)
                  (bitwise-and fxand)
                  (bitwise-ior fxior)
                  (bitwise-xor fxxor)
                  (arithmetic-shift fxarithmetic-shift)
                  (arithmetic-shift-left fxarithmetic-shift)
                  (bit-count fxbit-count)
                  (integer-length fxlength)
                  (bitwise-if fxif)
                  (copy-bit fxcopy-bit)
                  (first-set-bit fxfirst-set-bit)
                  (bit-field fxbit-field)
                  (bit-field-rotate fxbit-field-rotate)
                  (bit-field-reverse fxbit-field-reverse))
          (only (chibi) fixnum?))
  (export
   fx-width fx-greatest fx-least fixnum?
   fx=? fx<? fx>? fx<=? fx>=?
   fxzero? fxpositive? fxnegative? fxodd? fxeven? fxmax fxmin
   fx+ fx- fxneg fx* fxabs fxsquare fxsqrt
   fxquotient fxremainder
   fx+/carry fx-/carry fx+*/carry
   fxnot fxand fxior fxxor
   fxarithmetic-shift fxarithmetic-shift-left
   fxarithmetic-shift-right
   fxbit-count fxlength
   fxbit-field fxbit-field-rotate fxbit-field-reverse
   fxif fxbit-set? fxcopy-bit fxfirst-set-bit)
  (include "143/fixnum.scm"))
