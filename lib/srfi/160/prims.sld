
(define-library (srfi 160 prims)
  (export
   uvector-length
   SEXP_U1 SEXP_S8 SEXP_U8 SEXP_S16 SEXP_U16
   SEXP_S32 SEXP_U32 SEXP_S64 SEXP_U64
   SEXP_F32 SEXP_F64 SEXP_F8 SEXP_F16
   SEXP_C64 SEXP_C128
   u1vector? u1vector-ref u1vector-set!
   ;; u8vector? u8vector-ref u8vector-set!
   s8vector? s8vector-ref s8vector-set!
   u16vector? u16vector-ref u16vector-set!
   s16vector? s16vector-ref s16vector-set!
   u32vector? u32vector-ref u32vector-set!
   s32vector? s32vector-ref s32vector-set!
   u64vector? u64vector-ref u64vector-set!
   s64vector? s64vector-ref s64vector-set!
   f8vector? f8vector-ref f8vector-set!
   f16vector? f16vector-ref f16vector-set!
   f32vector? f32vector-ref f32vector-set!
   f64vector? f64vector-ref f64vector-set!
   c64vector? c64vector-ref c64vector-set!
   c128vector? c128vector-ref c128vector-set!
   )
  (include-shared "uvprims"))
