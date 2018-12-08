
(define-library (chibi bytevector-test)
  (export run-tests)
  (import (scheme base) (chibi bytevector) (chibi test))
  (begin

    (define floats
      `(0.0 -1.0 #i1/3 1.192092896E-07 ,(+ 1 1.192092896E-07)
        1e-23 -1e-23
        3.40282346638528860e+38 -3.40282346638528860e+38
        1.40129846432481707e-45 -1.40129846432481707e-45
        3.14159265358979323846))

    (define f32-le
      '#u8(#x00 #x00 #x00 #x00 #x00 #x00 #x80 #xbf
           #xab #xaa #xaa #x3e #x00 #x00 #x00 #x34
           #x01 #x00 #x80 #x3f #x9a #x6d #x41 #x19
           #x9a #x6d #x41 #x99 #xff #xff #x7f #x7f
           #xff #xff #x7f #xff #x01 #x00 #x00 #x00
           #x01 #x00 #x00 #x80 #xdb #x0f #x49 #x40))

    (define f64-le
      '#u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
           #x00 #x00 #x00 #x00 #x00 #x00 #xf0 #xbf
           #x55 #x55 #x55 #x55 #x55 #x55 #xd5 #x3f
           #x68 #x5f #x1c #x00 #x00 #x00 #x80 #x3e
           #x00 #x00 #x00 #x20 #x00 #x00 #xf0 #x3f
           #x51 #xb2 #x12 #x40 #xb3 #x2d #x28 #x3b
           #x51 #xb2 #x12 #x40 #xb3 #x2d #x28 #xbb
           #x00 #x00 #x00 #xe0 #xff #xff #xef #x47
           #x00 #x00 #x00 #xe0 #xff #xff #xef #xc7
           #x00 #x00 #x00 #x00 #x00 #x00 #xa0 #x36
           #x00 #x00 #x00 #x00 #x00 #x00 #xa0 #xb6
           #x18 #x2d #x44 #x54 #xfb #x21 #x09 #x40))

    (define (run-tests)
      (test-begin "bytevector")

      (test-group "reading ieee"

        (do ((ls floats (cdr ls))
            (i 0 (+ i 4)))
           ((null? ls))
         (test (car ls) (bytevector-ieee-single-native-ref f32-le i)))

       (do ((ls floats (cdr ls))
            (i 0 (+ i 8)))
           ((null? ls))
         (test (car ls) (bytevector-ieee-double-native-ref f64-le i))))

      (test-group "writing ieee"

        (do ((ls floats (cdr ls))
            (i 0 (+ i 4)))
           ((null? ls))
         (let ((bv (make-bytevector 4 0)))
           (bytevector-ieee-single-native-set! bv 0 (car ls))
           (test (bytevector-copy f32-le i (+ i 4)) (values bv))))

        (do ((ls floats (cdr ls))
            (i 0 (+ i 8)))
           ((null? ls))
         (let ((bv (make-bytevector 8 0)))
           (bytevector-ieee-double-native-set! bv 0 (car ls))
           (test (bytevector-copy f64-le i (+ i 8)) (values bv)))))

      (test-group "ber integers"
        (do ((ls '(0 1 128 16383 32767
                   18446744073709551615
                   340282366920938463463374607431768211456)
                 (cdr ls)))
            ((null? ls))
          (let ((bv (make-bytevector 256)))
            (do ((offsets '(0 1 27) (cdr offsets)))
                ((null? offsets))
              (bytevector-ber-set! bv (car ls) (car offsets))
              (test (car ls) (bytevector-ber-ref bv (car offsets)))))))

      (test-end))))
