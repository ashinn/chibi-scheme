
;; adapted from guile bytevectors.test by Ludovic Courtès

(define-library (scheme bytevector-test)
  (import (except (scheme base) bytevector-copy!)
          (scheme bytevector)
          (scheme list)
          (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "scheme bytevector")
      ;; (test 258 (bytevector-uint-ref #u8(0 1 2 0) 1 (endianness big) 2))
      ;; (test 513 (bytevector-uint-ref #u8(0 1 2 0) 1 (endianness little) 2))
      ;; (test -65281
      ;;     (bytevector-sint-ref #u8(0 #xFF 1 0) 1 (endianness big) 2))
      ;; (test -65281
      ;;     (bytevector-sint-ref #u8(0 1 #xFF 0) 1 (endianness little) 2))
      (test-begin "2.2 General Operations")
      (test-assert "native-endianness"
        (not (not (memq (native-endianness) '(big little)))))

      (test-assert "make-bytevector"
        (and (bytevector? (make-bytevector 20))
             (bytevector? (make-bytevector 20 3))))

      (test-assert "bytevector-length"
        (= (bytevector-length (make-bytevector 20)) 20))

      (test-assert "bytevector=?"
        (and (bytevector=? (make-bytevector 20 7)
                           (make-bytevector 20 7))
             (not (bytevector=? (make-bytevector 20 7)
                                (make-bytevector 20 0)))))

      (test "bytevector-fill! with fill 255"
          #u8(255 255 255 255)
          (let ((bv (make-bytevector 4)))
            (bytevector-fill! bv 255)
            bv))

      (test "bytevector-copy! overlapping"
          #u8(1 2 3 1 2 3 4 8)
          (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
            (bytevector-copy! b 0 b 3 4)
            b))
      (test-end)

      (test-begin "2.3 Operations on Bytes and Octets")

      (test "bytevector-{u8,s8}-ref"
          '(-127 129 -1 255)
        (let ((b1 (make-bytevector 16 -127))
              (b2 (make-bytevector 16 255)))
          (list (bytevector-s8-ref b1 0)
                (bytevector-u8-ref b1 0)
                (bytevector-s8-ref b2 0)
                (bytevector-u8-ref b2 0))))

      (test "bytevector-{u8,s8}-set!"
          '(-126 130 -10 246)
        (let ((b (make-bytevector 16 -127)))
          (bytevector-s8-set! b 0 -126)
          (bytevector-u8-set! b 1 246)
          (list (bytevector-s8-ref b 0)
                (bytevector-u8-ref b 0)
                (bytevector-s8-ref b 1)
                (bytevector-u8-ref b 1))))

      (test-assert "bytevector->u8-list"
        (let ((lst '(1 2 3 128 150 255)))
          (equal? lst
                  (bytevector->u8-list
                   (let ((b (make-bytevector 6)))
                     (for-each (lambda (i v)
                                 (bytevector-u8-set! b i v))
                               (iota 6)
                               lst)
                     b)))))

      (test-assert "u8-list->bytevector"
        (let ((lst '(1 2 3 128 150 255)))
          (equal? lst
                  (bytevector->u8-list (u8-list->bytevector lst)))))

      (test-error "u8-list->bytevector [invalid argument type]"
                  (u8-list->bytevector 'not-a-list))

      (test-error "u8-list->bytevector [circular list]"
                  (u8-list->bytevector (circular-list 1 2 3)))

      (test "bytevector-uint-{ref,set!} [small]"
          #x3412
        (let ((b (make-bytevector 15)))
          (bytevector-uint-set! b 0 #x1234 (endianness little) 2)
          (bytevector-uint-ref b 0 (endianness big) 2)))

      (test "bytevector-uint-set! [large]"
          '(253 255 255 255 255 255 255 255
                255 255 255 255 255 255 255 255)
        (let ((b (make-bytevector 16)))
          (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness little) 16)
          (bytevector->u8-list b)))

      (test "bytevector-uint-{ref,set!} [large]"
          #xfffffffffffffffffffffffffffffffd
        (let ((b (make-bytevector 120)))
          (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                                (endianness little) 16)
          (bytevector-uint-ref b 0 (endianness little) 16)))

      (test "bytevector-sint-ref big [small]"
          -16
        (let ((b (u8-list->bytevector '(#xff #xf0 #xff))))
          (bytevector-sint-ref b 0 (endianness big) 2)))

      (test "bytevector-sint-ref little [small]"
          -16
        (let ((b (u8-list->bytevector '(#xff #xf0 #xff))))
          (bytevector-sint-ref b 1 (endianness little) 2)))

      (test "bytevector-sint-ref [large]"
          -3
        (let ((b (make-bytevector 50)))
          (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness little) 16)
          (bytevector-sint-ref b 0 (endianness little) 16)))

      (test "bytevector-sint-set! [small]"
          '(#xff #xf0 #xff)
        (let ((b (make-bytevector 3)))
          (bytevector-sint-set! b 0 -16 (endianness big) 2)
          (bytevector-sint-set! b 1 -16 (endianness little) 2)
          (bytevector->u8-list b)))

      (test-assert "equal?"
        (let ((bv1 (u8-list->bytevector (iota 123)))
              (bv2 (u8-list->bytevector (iota 123))))
          (equal? bv1 bv2)))
      (test-end)

      (test-begin "2.4 Operations on Integers of Arbitrary Size")

      (test '(513 -253 513 513)
          (bytevector->sint-list #u8(1 2 3 255 1 2 1 2) (endianness little) 2))

      (test "bytevector->uint-list"
          '(513 65283 513 513)
        (let ((b (u8-list->bytevector '(2 1 255 3 2 1 2 1))))
          (bytevector->uint-list b (endianness big) 2)))

      (test "bytevector->uint-list [empty]"
          '()
        (let ((b (make-bytevector 0)))
          (bytevector->uint-list b (endianness big) 2)))

      (test-error "bytevector->sint-list [out-of-range]"
                  (bytevector->sint-list (make-bytevector 6) (endianness little) -1))

      (test-error "bytevector->uint-list [out-of-range]"
                  (bytevector->uint-list (make-bytevector 6) (endianness little) 0))

      (test-error "bytevector->uint-list [word size doesn't divide length]"
                  (bytevector->uint-list (make-bytevector 6) (endianness little) 4))

      (test-assert "{sint,uint}-list->bytevector"
        (let ((b1 (sint-list->bytevector '(513 -253 513 513)
                                         (endianness little) 2))
              (b2 (uint-list->bytevector '(513 65283 513 513)
                                         (endianness little) 2))
              (b3 (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
          (and (bytevector=? b1 b2)
               (bytevector=? b2 b3))))

      (test-assert "sint-list->bytevector [limits]"
        (bytevector=? (sint-list->bytevector '(-32768 32767)
                                             (endianness big) 2)
                      (let ((bv (make-bytevector 4)))
                        (bytevector-u8-set! bv 0 #x80)
                        (bytevector-u8-set! bv 1 #x00)
                        (bytevector-u8-set! bv 2 #x7f)
                        (bytevector-u8-set! bv 3 #xff)
                        bv)))

      (test-error "sint-list->bytevector [invalid argument type]"
                  (sint-list->bytevector 'not-a-list (endianness big) 2))

      (test-error "uint-list->bytevector [invalid argument type]"
                  (uint-list->bytevector 'not-a-list (endianness big) 2))

      (test-error "sint-list->bytevector [circular list]"
                  (sint-list->bytevector (circular-list 1 2 3) (endianness big)
                                         2))

      (test-error "uint-list->bytevector [circular list]"
                  (uint-list->bytevector (circular-list 1 2 3) (endianness big)
                                         2))

      (test-error "sint-list->bytevector [out-of-range]"
                  (sint-list->bytevector (list 0 0 (expt 2 16)) (endianness big)
                                         2))

      (test-error "uint-list->bytevector [out-of-range]"
                  (uint-list->bytevector '(0 -1) (endianness big) 2))
      (test-end)

      (test-begin "2.5 Operations on 16-Bit Integers")

      (let ((b #u8(255 255 255 255 255 255 255 255
                       255 255 255 255 255 255 255 253)))
        (test #xfdff (bytevector-u16-ref b 14 (endianness little)))
        (test #xfffd (bytevector-u16-ref b 14 (endianness big))))

      (let ((b #u8(255 255 255 255 255 255 255 255
                       255 255 255 255 255 255 255 253)))
        (test -513 (bytevector-s16-ref b 14 (endianness little)))
        (test -3 (bytevector-s16-ref b 14 (endianness big))))

      (let ((b (u8-list->bytevector '(#xff #xf0 #xff))))
        (test -16 (bytevector-s16-ref b 1 (endianness little))))

      (test-assert "bytevector-{u16,s16}-ref"
        (let ((b (make-bytevector 2)))
          (bytevector-u16-set! b 0 44444 (endianness little))
          (and (equal? (bytevector-u16-ref b 0 (endianness little))
                       44444)
               (equal? (bytevector-s16-ref b 0 (endianness little))
                       (- 44444 65536)))))

      (test-assert "bytevector-native-{u16,s16}-{ref,set!}"
        (let ((b (make-bytevector 2)))
          (bytevector-u16-native-set! b 0 44444)
          (and (equal? (bytevector-u16-native-ref b 0)
                       44444)
               (equal? (bytevector-s16-native-ref b 0)
                       (- 44444 65536)))))

      (test-assert "bytevector-{u16,s16}-{ref,set!} [unaligned]"
        (let ((b (make-bytevector 5)))
          (bytevector-s16-set! b 1 -77 (endianness little))
          (bytevector-s16-set! b 3 -77 (endianness big))
          (and (equal? (bytevector-s16-ref b 1 (endianness little))
                       -77)
               (equal? (bytevector-u16-ref b 1 (endianness little))
                       (- 65536 77))
               (equal? (bytevector-s16-ref b 3 (endianness big))
                       -77)
               (equal? (bytevector-u16-ref b 3 (endianness big))
                       (- 65536 77)))))
      (test-end)

      (test-begin "2.6 Operations on 32-bit Integers")

      (test-assert "bytevector-u32-ref"
        (let ((b (u8-list->bytevector
                  '(255 255 255 255 255 255 255 255
                        255 255 255 255 255 255 255 253))))
          (and (equal? (bytevector-u32-ref b 12 (endianness little))
                       #xfdffffff)
               (equal? (bytevector-u32-ref b 12 (endianness big))
                       #xfffffffd))))

      (test-assert "bytevector-s32-ref"
        (let ((b (u8-list->bytevector
                  '(255 255 255 255 255 255 255 255
                        255 255 255 255 255 255 255 253))))
          (and (equal? (bytevector-s32-ref b 12 (endianness little))
                       -33554433)
               (equal? (bytevector-s32-ref b 12 (endianness big))
                       -3))))

      (test-assert "bytevector-{u32,s32}-ref"
        (let ((b (make-bytevector 4)))
          (bytevector-u32-set! b 0 2222222222 (endianness little))
          (and (equal? (bytevector-u32-ref b 0 (endianness little))
                       2222222222)
               (equal? (bytevector-s32-ref b 0 (endianness little))
                       (- 2222222222 (expt 2 32))))))

      (test-assert "bytevector-{u32,s32}-native-{ref,set!}"
        (let ((b (make-bytevector 4)))
          (bytevector-u32-native-set! b 0 2222222222)
          (and (equal? (bytevector-u32-native-ref b 0)
                       2222222222)
               (equal? (bytevector-s32-native-ref b 0)
                       (- 2222222222 (expt 2 32))))))

      (test-assert "bytevector-{u32,s32}-{ref,set!} [unaligned]"
        (let ((b (make-bytevector 9)))
          (bytevector-s32-set! b 1 -77777 (endianness little))
          (bytevector-s32-set! b 5 -77777 (endianness big))
          (and (equal? (bytevector-s32-ref b 1 (endianness little))
                       -77777)
               (equal? (bytevector-u32-ref b 1 (endianness little))
                       (- (expt 2 32) 77777))
               (equal? (bytevector-s32-ref b 5 (endianness big))
                       -77777)
               (equal? (bytevector-u32-ref b 5 (endianness big))
                       (- (expt 2 32) 77777)))))
      (test-end)

      (test-begin "2.7 Operations on 64-bit Integers")

      (let ((b (u8-list->bytevector
                '(255 255 255 255 255 255 255 255
                      255 255 255 255 255 255 255 253))))
        (test #xfdffffffffffffff
            (bytevector-u64-ref b 8 (endianness little)))
        (test #xfffffffffffffffd
            (bytevector-u64-ref b 8 (endianness big)))
        (test -144115188075855873
            (bytevector-s64-ref b 8 (endianness little)))
        (test -3
            (bytevector-s64-ref b 8 (endianness big))))

      (let ((b (make-bytevector 8))
            (big 9333333333333333333))
        (bytevector-u64-set! b 0 big (endianness little))
        (test big
            (bytevector-u64-ref b 0 (endianness little)))
        (test (- big (expt 2 64))
            (bytevector-s64-ref b 0 (endianness little))))

      (let ((b (make-bytevector 8))
            (big 9333333333333333333))
        (bytevector-u64-native-set! b 0 big)
        (test big
            (bytevector-u64-native-ref b 0))
        (test (- big (expt 2 64))
            (bytevector-s64-native-ref b 0)))

      (test-assert "ref/set! with zero"
        (let ((b (make-bytevector 8)))
          (bytevector-s64-set! b 0 -1 (endianness big))
          (bytevector-u64-set! b 0  0 (endianness big))
          (= 0 (bytevector-u64-ref b 0 (endianness big)))))

      (test-assert "bytevector-{u64,s64}-{ref,set!} [unaligned]"
        (let ((b (make-bytevector 17)))
          (bytevector-s64-set! b 1 -7777777777 (endianness little))
          (bytevector-s64-set! b 9 -7777777777 (endianness big))
          (and (equal? (bytevector-s64-ref b 1 (endianness little))
                       -7777777777)
               (equal? (bytevector-u64-ref b 1 (endianness little))
                       (- (expt 2 64) 7777777777))
               (equal? (bytevector-s64-ref b 9 (endianness big))
                       -7777777777)
               (equal? (bytevector-u64-ref b 9 (endianness big))
                       (- (expt 2 64) 7777777777)))))
      (test-end)

      (test-begin "2.8 Operations on IEEE-754 Representations")

      (test-assert "single, little endian"
        (let ((b (make-bytevector 4)))
          (bytevector-ieee-single-set! b 0 1.0 (endianness little))
          (equal? #u8(0 0 128 63) b)))

      (test-assert "single, big endian"
        (let ((b (make-bytevector 4)))
          (bytevector-ieee-single-set! b 0 1.0 (endianness big))
          (equal? #u8(63 128 0 0) b)))

      (test-assert "bytevector-ieee-single-native-{ref,set!}"
        (let ((b (make-bytevector 4))
              (number 3.00))
          (bytevector-ieee-single-native-set! b 0 number)
          (equal? (bytevector-ieee-single-native-ref b 0)
                  number)))

      (test-assert "bytevector-ieee-single-{ref,set!}"
        (let ((b (make-bytevector 8))
              (number 3.14))
          (bytevector-ieee-single-set! b 0 number (endianness little))
          (bytevector-ieee-single-set! b 4 number (endianness big))
          (equal? (bytevector-ieee-single-ref b 0 (endianness little))
                  (bytevector-ieee-single-ref b 4 (endianness big)))))

      (test-assert "bytevector-ieee-single-{ref,set!} [unaligned]"
        (let ((b (make-bytevector 9))
              (number 3.14))
          (bytevector-ieee-single-set! b 1 number (endianness little))
          (bytevector-ieee-single-set! b 5 number (endianness big))
          (equal? (bytevector-ieee-single-ref b 1 (endianness little))
                  (bytevector-ieee-single-ref b 5 (endianness big)))))

      (test-assert "double, little endian"
        (let ((b (make-bytevector 8)))
          (bytevector-ieee-double-set! b 0 1.0 (endianness little))
          (equal? #u8(0 0 0 0 0 0 240 63) b)))

      (test-assert "double, big endian"
        (let ((b (make-bytevector 8)))
          (bytevector-ieee-double-set! b 0 1.0 (endianness big))
          (equal? #u8(63 240 0 0 0 0 0 0) b)))

      (test-assert "bytevector-ieee-double-native-{ref,set!}"
        (let ((b (make-bytevector 8))
              (number 3.14))
          (bytevector-ieee-double-native-set! b 0 number)
          (equal? (bytevector-ieee-double-native-ref b 0)
                  number)))

      (test-assert "bytevector-ieee-double-{ref,set!}"
        (let ((b (make-bytevector 16))
              (number 3.14))
          (bytevector-ieee-double-set! b 0 number (endianness little))
          (bytevector-ieee-double-set! b 8 number (endianness big))
          (equal? (bytevector-ieee-double-ref b 0 (endianness little))
                  (bytevector-ieee-double-ref b 8 (endianness big)))))

      (test-assert "bytevector-ieee-double-{ref,set!} [unaligned]"
        (let ((b (make-bytevector 17))
              (number 3.14))
          (bytevector-ieee-double-set! b 1 number (endianness little))
          (bytevector-ieee-double-set! b 9 number (endianness big))
          (equal? (bytevector-ieee-double-ref b 1 (endianness little))
                  (bytevector-ieee-double-ref b 9 (endianness big)))))
      (test-end)


      (test-begin "2.9 Operations on Strings")

      (test-assert "string->utf8"
        (let* ((str  "hello, world")
               (utf8 (string->utf8 str)))
          (and (bytevector? utf8)
               (= (bytevector-length utf8)
                  (string-length str))
               (equal? (string->list str)
                       (map integer->char (bytevector->u8-list utf8))))))

      (test-assert "string->utf8 [latin-1]"
        (let* ((str  "hé, ça va bien ?")
               (utf8 (string->utf8 str)))
          (and (bytevector? utf8)
               (= (bytevector-length utf8)
                  (+ 2 (string-length str))))))

      (test-assert "string->utf16"
        (let* ((str   "hello, world")
               (utf16 (string->utf16 str)))
          (and (bytevector? utf16)
               (= (bytevector-length utf16)
                  (* 2 (string-length str)))
               (equal? (string->list str)
                       (map integer->char
                            (bytevector->uint-list utf16
                                                   (endianness big) 2))))))

      (test-assert "string->utf16 [little]"
        (let* ((str   "hello, world")
               (utf16 (string->utf16 str (endianness little))))
          (and (bytevector? utf16)
               (= (bytevector-length utf16)
                  (* 2 (string-length str)))
               (equal? (string->list str)
                       (map integer->char
                            (bytevector->uint-list utf16
                                                   (endianness little) 2))))))


      (test-assert "string->utf32"
        (let* ((str   "hello, world")
               (utf32 (string->utf32 str)))
          (and (bytevector? utf32)
               (= (bytevector-length utf32)
                  (* 4 (string-length str)))
               (equal? (string->list str)
                       (map integer->char
                            (bytevector->uint-list utf32
                                                   (endianness big) 4))))))

      (test-assert "string->utf32 [Greek]"
        (let* ((str   "Ἄνεμοι")
               (utf32 (string->utf32 str)))
          (and (bytevector? utf32)
               (equal? (bytevector->uint-list utf32 (endianness big) 4)
                       '(#x1f0c #x3bd #x3b5 #x3bc #x3bf #x3b9)))))

      (test-assert "string->utf32 [little]"
        (let* ((str   "hello, world")
               (utf32 (string->utf32 str (endianness little))))
          (and (bytevector? utf32)
               (= (bytevector-length utf32)
                  (* 4 (string-length str)))
               (equal? (string->list str)
                       (map integer->char
                            (bytevector->uint-list utf32
                                                   (endianness little) 4))))))

      (test-assert "utf8->string"
        (let* ((utf8  (u8-list->bytevector (map char->integer
                                                (string->list "hello, world"))))
               (str   (utf8->string utf8)))
          (and (string? str)
               (= (string-length str)
                  (bytevector-length utf8))
               (equal? (string->list str)
                       (map integer->char (bytevector->u8-list utf8))))))

      (test-assert "utf8->string [latin-1]"
        (let* ((utf8  (string->utf8 "hé, ça va bien ?"))
               (str   (utf8->string utf8)))
          (and (string? str)
               (= (string-length str)
                  (- (bytevector-length utf8) 2)))))

      (test "utf8->string [replacement character]"
          '(104 105 65533)
        (map char->integer
             (string->list (utf8->string #u8(104 105 239 191 189)))))

      (test-assert "utf16->string"
        (let* ((utf16  (uint-list->bytevector (map char->integer
                                                   (string->list "hello, world"))
                                              (endianness big) 2))
               (str   (utf16->string utf16)))
          (and (string? str)
               (= (* 2 (string-length str))
                  (bytevector-length utf16))
               (equal? (string->list str)
                       (map integer->char
                            (bytevector->uint-list utf16 (endianness big)
                                                   2))))))

      (test-assert "utf16->string [little]"
        (let* ((utf16  (uint-list->bytevector (map char->integer
                                                   (string->list "hello, world"))
                                              (endianness little) 2))
               (str   (utf16->string utf16 (endianness little))))
          (and (string? str)
               (= (* 2 (string-length str))
                  (bytevector-length utf16))
               (equal? (string->list str)
                       (map integer->char
                            (bytevector->uint-list utf16 (endianness little)
                                                   2))))))
      (test-assert "utf32->string"
        (let* ((utf32  (uint-list->bytevector (map char->integer
                                                   (string->list "hello, world"))
                                              (endianness big) 4))
               (str   (utf32->string utf32)))
          (and (string? str)
               (= (* 4 (string-length str))
                  (bytevector-length utf32))
               (equal? (string->list str)
                       (map integer->char
                            (bytevector->uint-list utf32 (endianness big)
                                                   4))))))

      (test-assert "utf32->string [little]"
        (let* ((utf32  (uint-list->bytevector (map char->integer
                                                   (string->list "hello, world"))
                                              (endianness little) 4))
               (str   (utf32->string utf32 (endianness little))))
          (and (string? str)
               (= (* 4 (string-length str))
                  (bytevector-length utf32))
               (equal? (string->list str)
                       (map integer->char
                            (bytevector->uint-list utf32 (endianness little)
                                                   4))))))
      (test-end)

      (test-end))))
