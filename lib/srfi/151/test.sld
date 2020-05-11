(define-library (srfi 151 test)
  (export run-tests)
  (import (scheme base) (srfi 151) (chibi test))
  (begin
    (define (run-tests)
      (test-begin "srfi-151: bitwise operations")

      (test 0 (integer-length #b0))
      (test 1 (integer-length #b1))
      (test 2 (integer-length #b10))
      (test 3 (integer-length #b100))
      (test 4 (integer-length #b1000))
      (test 5 (integer-length #b10000))
      (test 6 (integer-length #b110000))
      (test 0 (bitwise-and #b0 #b1))
      (test 1 (bitwise-and #b1 #b1))
      (test 0 (bitwise-and #b1 #b10))
      (test #b10 (bitwise-and #b11 #b10))
      (test #b101 (bitwise-and #b101 #b111))
      (test #b111 (bitwise-and -1 #b111))
      (test #b110 (bitwise-and -2 #b111))
      (test 3769478 (bitwise-and -4290775858 1694076839))
      (test 1680869008 (bitwise-and -193073517 1689392892))
      (test -4294967295 (bitwise-ior 1 (- -1 #xffffffff)))
      (test -18446744073709551615 (bitwise-ior 1 (- -1 #xffffffffffffffff)))
      (test -4294967126 (bitwise-xor #b10101010 (- -1 #xffffffff)))
      (test -18446744073709551446 (bitwise-xor #b10101010 (- -1 #xffffffffffffffff)))
      (test -2600468497 (bitwise-ior 1694076839 -4290775858))
      (test -184549633 (bitwise-ior -193073517 1689392892))
      (test -167776621 (bitwise-ior -193073517 1689392892000000000000))
      (test -18446744073709551616
          (bitwise-ior -18446744073709551616
                       340282366920938463463374607431768211456))
      (test 340282366920938463481821351505477763072
          (bitwise-ior 18446744073709551616
                       340282366920938463463374607431768211456))
      (test -7090566332214939648
          (bitwise-ior -193073517000000000000 1689392892000000000000))
      (test -351599414102633810746018680881203758247936
          (bitwise-ior -1930735170000000000001689392892000000000000
                       1689392892000000000000193073517000000000000))
      (test -2604237975 (bitwise-xor 1694076839 -4290775858))
      (test -1865418641 (bitwise-xor -193073517 1689392892))
      (test -1689392892000142479725
          (bitwise-xor -193073517 1689392892000000000000))
      (test -1510500507664429879296
          (bitwise-xor -193073517000000000000 1689392892000000000000))
      (test -1510500507664429879296
          (bitwise-xor -193073517000000000000 1689392892000000000000))
      (test -461856550205267621490541042387407516495872
          (bitwise-xor -1930735170000000000001689392892000000000000
                       1689392892000000000000193073517000000000000))
      (test -461856550205267621490541042387407516495872
          (bitwise-xor 1689392892000000000000193073517000000000000
                       -1930735170000000000001689392892000000000000))
      (test 461856550205267621490541042387407516495872
          (bitwise-xor -1930735170000000000001689392892000000000000
                       -1689392892000000000000193073517000000000000))
      (test 3769478 (bitwise-and 1694076839 -4290775858))
      (test 1680869008 (bitwise-and -193073517 1689392892))
      (test 340282366920938463463374607431768211456
          (bitwise-and 340282366920938463463374607431768211456
                       (bitwise-not 18446744073709551616)))

      (test 1 (arithmetic-shift 1 0))
      (test 2 (arithmetic-shift 1 1))
      (test 4 (arithmetic-shift 1 2))
      (test 8 (arithmetic-shift 1 3))
      (test 16 (arithmetic-shift 1 4))
      (test (expt 2 31) (arithmetic-shift 1 31))
      (test (expt 2 32) (arithmetic-shift 1 32))
      (test (expt 2 33) (arithmetic-shift 1 33))
      (test (expt 2 63) (arithmetic-shift 1 63))
      (test (expt 2 64) (arithmetic-shift 1 64))
      (test (expt 2 65) (arithmetic-shift 1 65))
      (test (expt 2 127) (arithmetic-shift 1 127))
      (test (expt 2 128) (arithmetic-shift 1 128))
      (test (expt 2 129) (arithmetic-shift 1 129))
      (test 3028397001194014464 (arithmetic-shift 11829675785914119 8))

      (test -1 (arithmetic-shift -1 0))
      (test -2 (arithmetic-shift -1 1))
      (test -4 (arithmetic-shift -1 2))
      (test -8 (arithmetic-shift -1 3))
      (test -16 (arithmetic-shift -1 4))
      (test (- (expt 2 31)) (arithmetic-shift -1 31))
      (test (- (expt 2 32)) (arithmetic-shift -1 32))
      (test (- (expt 2 33)) (arithmetic-shift -1 33))
      (test (- (expt 2 63)) (arithmetic-shift -1 63))
      (test (- (expt 2 64)) (arithmetic-shift -1 64))
      (test (- (expt 2 65)) (arithmetic-shift -1 65))
      (test (- (expt 2 127)) (arithmetic-shift -1 127))
      (test (- (expt 2 128)) (arithmetic-shift -1 128))
      (test (- (expt 2 129)) (arithmetic-shift -1 129))

      (test 0 (arithmetic-shift 1 -63))
      (test 0 (arithmetic-shift 1 -64))
      (test 0 (arithmetic-shift 1 -65))

      (test #x1000000000000000100000000000000000000000000000000
          (arithmetic-shift #x100000000000000010000000000000000 64))
      (test #x8e73b0f7da0e6452c810f32b809079e5
          (arithmetic-shift #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b -64))
      (test -79 (arithmetic-shift -100000000000000000000000000000000 -100))
      (test 78 (arithmetic-shift 100000000000000000000000000000000 -100))
      (test -200000000000000000000000000000000
          (arithmetic-shift -100000000000000000000000000000000 1))

      (test-not (bit-set? 64 1))
      (test-assert (bit-set? 64 #x10000000000000000))
      (test-assert (bit-set? 1000000 -1))
      (test-assert (bit-set? 1000 -1))

      (test #b1010 (bit-field #b1101101010 0 4))
      (test #b101101 (bit-field #b1101101010 3 9))
      (test #b10110 (bit-field #b1101101010 4 9))
      (test #b110110 (bit-field #b1101101010 4 10))

      (test 0 (bitwise-if 1 2 1))
      (test 3 (bitwise-if 1 1 2))
      (test 9 (bitwise-if 3 1 8))
      (test 0 (bitwise-if 3 8 1))
      (test #b00110011 (bitwise-if #b00111100 #b11110000 #b00001111))

      (test #b1 (copy-bit 0 0 #t))
      (test #b100 (copy-bit 2 0 #t))
      (test #b1011 (copy-bit 2 #b1111 #f))

      (test #b1110 (bit-swap 0 1 #b1101))
      (test #b1011 (bit-swap 1 2 #b1101))
      (test #b1011 (bit-swap 2 1 #b1101))
      (test #b10000000101 (bit-swap 3 10 #b1101))

      (test '(#t #t #t #f #t #f #t) (bits->list #b1010111))
      (test '(#t #t #t #f #t) (bits->list #b1010111 5))
      (test '(#t #t #t #f #t #f #t #f #f) (bits->list #b1010111 9))
      (test '#(#t #t #t #f #t #f #t) (bits->vector #b1010111))
      (test '#(#t #t #t #f #t #f #t #f #f) (bits->vector #b1010111 9))

      (test #b1010111 (list->bits '(#t #t #t #f #t #f #t)))
      (test #b1010111 (list->bits '(#t #t #t #f #t #f #t #f #f)))
      (test #b1010111 (vector->bits '#(#t #t #t #f #t #f #t)))
      (test #b1010111 (vector->bits '#(#t #t #t #f #t #f #t #f #f)))
      (test #b1010111 (bits #t #t #t #f #t #f #t))
      (test #b1010111 (bits #t #t #t #f #t #f #t #f #f))

      (test 0 (first-set-bit 1))
      (test 1 (first-set-bit 2))
      (test -1 (first-set-bit 0))
      (test 3 (first-set-bit 40))
      (test 2 (first-set-bit -28))
      (test 99 (first-set-bit (expt  2 99)))
      (test 99 (first-set-bit (expt -2 99)))

      (test '(#t #f #t #f #t #t #t) (bitwise-fold cons '() #b1010111))

      (test 5
          (let ((count 0))
            (bitwise-for-each (lambda (b) (if b (set! count (+ count 1))))
                              #b1010111)
            count))

      (test #b101010101
          (bitwise-unfold (lambda (i) (= i 10)) even? (lambda (i) (+ i 1)) 0))

      (test #b110  (bit-field-rotate #b110 1 1 2))
      (test #b1010 (bit-field-rotate #b110 1 2 4))
      (test #b1011 (bit-field-rotate #b0111 -1 1 4))
      (test #b0  (bit-field-rotate #b0 128 0 256))
      (test #b1  (bit-field-rotate #b1 128 1 256))
      (test #x100000000000000000000000000000000 
	    (bit-field-rotate #x100000000000000000000000000000000 128 0 64))
      (test #x100000000000000000000000000000008 
	    (bit-field-rotate #x100000000000000000000000000000001 3 0 64))
      (test #x100000000000000002000000000000000 
	    (bit-field-rotate #x100000000000000000000000000000001 -3 0 64))
      (test #b110 (bit-field-rotate #b110 0 0 10))
      (test #b110 (bit-field-rotate #b110 0 0 256))
      (test 1 (bit-field-rotate #x100000000000000000000000000000000 1 0 129))

      (test 6 (bit-field-reverse 6 1 3))
      (test 12 (bit-field-reverse 6 1 4))
      (test #x80000000 (bit-field-reverse 1 0 32))
      (test #x40000000 (bit-field-reverse 1 0 31))
      (test #x20000000 (bit-field-reverse 1 0 30))
      (test (bitwise-ior (arithmetic-shift -1 32) #xFBFFFFFF) 
        (bit-field-reverse -2 0 27))
      (test (bitwise-ior (arithmetic-shift -1 32) #xF7FFFFFF) 
        (bit-field-reverse -2 0 28))
      (test (bitwise-ior (arithmetic-shift -1 32) #xEFFFFFFF) 
        (bit-field-reverse -2 0 29))
      (test (bitwise-ior (arithmetic-shift -1 32) #xDFFFFFFF) 
        (bit-field-reverse -2 0 30))
      (test (bitwise-ior (arithmetic-shift -1 32) #xBFFFFFFF) 
        (bit-field-reverse -2 0 31))
      (test (bitwise-ior (arithmetic-shift -1 32) #x7FFFFFFF) 
        (bit-field-reverse -2 0 32))
      (test 5 (bit-field-reverse #x140000000000000000000000000000000 0 129))

      (test-assert (bit-field-any? #b1001001 1 6))
      (test-not    (bit-field-any? #b1000001 1 6))
      (test-assert (bit-field-every? 45 2 4))
      (test-assert (bit-field-every? 45 0 1))
      (test-assert (bit-field-every? #b1011110 1 5))
      (test-not    (bit-field-every? #b1011010 1 5))

      (test-end))))
