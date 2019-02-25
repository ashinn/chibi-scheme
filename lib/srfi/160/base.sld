
(define-library (srfi 160 base)
  (import (scheme base)
          (only (chibi) list->uvector make-uvector))
  (export
   ;;
   make-u1vector u1vector u1? u1vector?
   u1vector-ref u1vector-set! u1vector-length
   u1vector->list list->u1vector
   ;;
   make-u8vector u8vector u8? u8vector?
   u8vector-ref u8vector-set! u8vector-length
   u8vector->list list->u8vector
   ;;
   make-s8vector s8vector s8? s8vector?
   s8vector-ref s8vector-set! s8vector-length
   s8vector->list list->s8vector
   ;;
   make-u16vector u16vector u16? u16vector?
   u16vector-ref u16vector-set! u16vector-length
   u16vector->list list->u16vector
   ;;
   make-s16vector s16vector s16? s16vector?
   s16vector-ref s16vector-set! s16vector-length
   s16vector->list list->s16vector
   ;;
   make-u32vector u32vector u32? u32vector?
   u32vector-ref u32vector-set! u32vector-length
   u32vector->list list->u32vector
   ;;
   make-s32vector s32vector s32? s32vector?
   s32vector-ref s32vector-set! s32vector-length
   s32vector->list list->s32vector
   ;;
   make-u64vector u64vector u64? u64vector?
   u64vector-ref u64vector-set! u64vector-length
   u64vector->list list->u64vector
   ;;
   make-s64vector s64vector s64? s64vector?
   s64vector-ref s64vector-set! s64vector-length
   s64vector->list list->s64vector
   ;;
   make-f32vector f32vector f32? f32vector?
   f32vector-ref f32vector-set! f32vector-length
   f32vector->list list->f32vector
   ;;
   make-f64vector f64vector f64? f64vector?
   f64vector-ref f64vector-set! f64vector-length
   f64vector->list list->f64vector
   ;;
   make-c64vector c64vector c64? c64vector?
   c64vector-ref c64vector-set! c64vector-length
   c64vector->list list->c64vector
   ;;
   make-c128vector c128vector c128? c128vector?
   c128vector-ref c128vector-set! c128vector-length
   c128vector->list list->c128vector
   )
  (cond-expand
   (uvector
    (include-shared "uvprims"))
   (else
    (begin
      )))
  (begin
    (define u8vector? bytevector?)
    (define u8vector-ref bytevector-u8-ref)
    (define u8vector-set! bytevector-u8-set!)
    (define (u1? x) (memq x '(0 1)))
    (define (u8? x) (and (exact-integer? x) (<= 0 x 255)))
    (define (s8? x) (and (exact-integer? x) (<= -128 x 127)))
    (define (u16? x) (and (exact-integer? x) (<= 0 x 65536)))
    (define (s16? x) (and (exact-integer? x) (<= -32768 x 32767)))
    (define (u32? x) (and (exact-integer? x) (<= 0 x 4294967296)))
    (define (s32? x) (and (exact-integer? x) (<= -2147483648 x 2147483647)))
    (define (u64? x) (and (exact-integer? x) (<= 0 x 18446744073709551616)))
    (define (s64? x)
      (and (exact-integer? x) (<= -9223372036854775808 x 9223372036854775807)))
    (define (f32? x) (and (real? x) (inexact? x)))
    (define (f64? x) (and (real? x) (inexact? x)))
    (define (c64? x) (and (complex? x) (inexact? x)))
    (define (c128? x) (and (complex? x) (inexact? x)))
    (define u1vector-length uvector-length)
    (define u8vector-length bytevector-length)
    (define s8vector-length uvector-length)
    (define u16vector-length uvector-length)
    (define s16vector-length uvector-length)
    (define u32vector-length uvector-length)
    (define s32vector-length uvector-length)
    (define u64vector-length uvector-length)
    (define s64vector-length uvector-length)
    (define f32vector-length uvector-length)
    (define f64vector-length uvector-length)
    (define c64vector-length uvector-length)
    (define c128vector-length uvector-length)
    (define (list->u1vector ls) (list->uvector SEXP_U1 ls))
    (define (list->u8vector ls) (apply u8vector ls))
    (define (list->s8vector ls) (list->uvector SEXP_S8 ls))
    (define (list->u16vector ls) (list->uvector SEXP_U16 ls))
    (define (list->s16vector ls) (list->uvector SEXP_S16 ls))
    (define (list->u32vector ls) (list->uvector SEXP_U32 ls))
    (define (list->s32vector ls) (list->uvector SEXP_S32 ls))
    (define (list->u64vector ls) (list->uvector SEXP_U64 ls))
    (define (list->s64vector ls) (list->uvector SEXP_S64 ls))
    (define (list->f32vector ls) (list->uvector SEXP_F32 ls))
    (define (list->f64vector ls) (list->uvector SEXP_F64 ls))
    (define (list->c64vector ls) (list->uvector SEXP_C64 ls))
    (define (list->c128vector ls) (list->uvector SEXP_C128 ls))
    (define (u1vector . ls) (list->u1vector ls))
    (define (u8vector . ls) (list->u8vector ls))
    (define (s8vector . ls) (list->s8vector ls))
    (define (u16vector . ls) (list->u16vector ls))
    (define (s16vector . ls) (list->s16vector ls))
    (define (u32vector . ls) (list->u32vector ls))
    (define (s32vector . ls) (list->s32vector ls))
    (define (u64vector . ls) (list->u64vector ls))
    (define (s64vector . ls) (list->s64vector ls))
    (define (f32vector . ls) (list->f32vector ls))
    (define (f64vector . ls) (list->f64vector ls))
    (define (c64vector . ls) (list->c64vector ls))
    (define (c128vector . ls) (list->c128vector ls))
    (define (make-u1vector len) (make-uvector SEXP_U1 len))
    (define make-u8vector make-bytevector)
    (define (make-s8vector len) (make-uvector SEXP_S8 len))
    (define (make-u16vector len) (make-uvector SEXP_U16 len))
    (define (make-s16vector len) (make-uvector SEXP_S16 len))
    (define (make-u32vector len) (make-uvector SEXP_U32 len))
    (define (make-s32vector len) (make-uvector SEXP_S32 len))
    (define (make-u64vector len) (make-uvector SEXP_U64 len))
    (define (make-s64vector len) (make-uvector SEXP_S64 len))
    (define (make-f32vector len) (make-uvector SEXP_F32 len))
    (define (make-f64vector len) (make-uvector SEXP_F64 len))
    (define (make-c64vector len) (make-uvector SEXP_C64 len))
    (define (make-c128vector len) (make-uvector SEXP_C128 len))
    (define-syntax define-uvector->list
      (syntax-rules ()
        ((define-uvector->list uv->list len ref)
         (define (uv->list uv)
           (do ((i (- (len uv) 1) (- i 1))
                (res '() (cons (ref uv i) res)))
               ((< i 0) res))))))
    (define-uvector->list u1vector->list u1vector-length u1vector-ref)
    (define-uvector->list u8vector->list bytevector-length bytevector-u8-ref)
    (define-uvector->list s8vector->list s8vector-length s8vector-ref)
    (define-uvector->list u16vector->list u16vector-length u16vector-ref)
    (define-uvector->list s16vector->list s16vector-length s16vector-ref)
    (define-uvector->list u32vector->list u32vector-length u32vector-ref)
    (define-uvector->list s32vector->list s32vector-length s32vector-ref)
    (define-uvector->list u64vector->list u64vector-length u64vector-ref)
    (define-uvector->list s64vector->list s64vector-length s64vector-ref)
    (define-uvector->list f32vector->list f32vector-length f32vector-ref)
    (define-uvector->list f64vector->list f64vector-length f64vector-ref)
    (define-uvector->list c64vector->list c64vector-length c64vector-ref)
    (define-uvector->list c128vector->list c128vector-length c128vector-ref)
    ))
