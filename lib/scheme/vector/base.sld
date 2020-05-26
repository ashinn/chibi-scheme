
(define-library (scheme vector base)
  (import (srfi 160 base))
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
   c128vector->list list->c128vector))
