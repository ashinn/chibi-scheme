
(define-library (srfi 14 test)
  (import (scheme base) (scheme char) (srfi 14) (chibi test))
  (export run-tests)
  (begin
    (define-syntax test-cs
      (syntax-rules ()
        ((test-cs . o)
         (test-equal char-set= . o))))
    (define (vowel? c) (member c '(#\a #\e #\i #\o #\u)))
    (define (run-tests)
      (test-begin "srfi-14: char-sets")
      (test-not (char-set? 5))

      (test-assert (char-set? (char-set #\a #\e #\i #\o #\u)))

      (test-assert (char-set=))
      (test-assert (char-set= (char-set)))

      (test-cs (char-set #\a #\e #\i #\o #\u)
               (string->char-set "ioeauaiii"))

      (test-not (char-set= (char-set #\e #\i #\o #\u)
                           (string->char-set "ioeauaiii")))

      (test-assert (char-set<=))
      (test-assert (char-set<= (char-set)))

      (test-assert (char-set<= (char-set #\a #\e #\i #\o #\u)
                               (string->char-set "ioeauaiii")))

      (test-assert (char-set<= (char-set #\e #\i #\o #\u)
                               (string->char-set "ioeauaiii")))

      (test-assert (<= 0 (char-set-hash char-set:graphic 100) 99))

      (test 4 (char-set-fold (lambda (c i) (+ i 1)) 0
                             (char-set #\e #\i #\o #\u #\e #\e)))

      (test-cs (string->char-set "eiaou2468013579999")
               (char-set-unfold null? car cdr
                                '(#\a #\e #\i #\o #\u #\u #\u)
                                (char-set-intersection char-set:ascii
                                                       char-set:digit)))

      (test-cs (string->char-set "eiaou246801357999")
               (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
                                 (string->char-set "0123456789")))

      (test-not (char-set= (string->char-set "eiaou246801357")
                           (char-set-unfold! null? car cdr
                                             '(#\a #\e #\i #\o #\u)
                                             (string->char-set "0123456789"))))

      (let ((cs (string->char-set "0123456789")))
        (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
                           (string->char-set "02468000"))
        (test-cs cs (string->char-set "97531")))

      (test-not (let ((cs (string->char-set "0123456789")))
                  (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
                                     (string->char-set "02468"))
                  (char-set= cs (string->char-set "7531"))))

      (test-cs (string->char-set "IOUAEEEE")
               (char-set-map char-upcase (string->char-set "aeiou")))

      (test-not (char-set= (char-set-map char-upcase (string->char-set "aeiou"))
                           (string->char-set "OUAEEEE")))

      (test-cs (string->char-set "aeiou")
               (char-set-copy (string->char-set "aeiou")))

      (test-cs (string->char-set "xy") (char-set #\x #\y))
      (test-not (char-set= (char-set #\x #\y #\z) (string->char-set "xy")))

      (test-cs (string->char-set "xy") (list->char-set '(#\x #\y)))
      (test-not (char-set= (string->char-set "axy")
                           (list->char-set '(#\x #\y))))

      (test-cs (string->char-set "xy12345")
               (list->char-set '(#\x #\y) (string->char-set "12345")))
      (test-not (char-set= (string->char-set "y12345")
                           (list->char-set '(#\x #\y)
                                           (string->char-set "12345"))))

      (test-cs (string->char-set "xy12345")
               (list->char-set! '(#\x #\y) (string->char-set "12345")))
      (test-not (char-set= (string->char-set "y12345")
                           (list->char-set! '(#\x #\y)
                                            (string->char-set "12345"))))

      (test-cs (string->char-set "aeiou12345")
               (char-set-filter vowel?
                                char-set:ascii
                                (string->char-set "12345")))
      (test-not (char-set= (string->char-set "aeou12345")
                           (char-set-filter vowel?
                                            char-set:ascii
                                            (string->char-set "12345"))))

      (test-cs (string->char-set "aeiou12345")
               (char-set-filter! vowel?
                                 char-set:ascii
                                 (string->char-set "12345")))
      (test-not (char-set= (string->char-set "aeou12345")
                           (char-set-filter! vowel?
                                             char-set:ascii
                                             (string->char-set "12345"))))

      (test-cs (string->char-set "abcdef12345")
               (ucs-range->char-set 97 103 #t (string->char-set "12345")))
      (test-not (char-set=
                 (string->char-set "abcef12345")
                 (ucs-range->char-set 97 103 #t (string->char-set "12345"))))

      (test-cs (string->char-set "abcdef12345")
               (ucs-range->char-set! 97 103 #t (string->char-set "12345")))
      (test-not (char-set=
                 (string->char-set "abcef12345")
                 (ucs-range->char-set! 97 103 #t (string->char-set "12345"))))

      (test-assert (char-set= (->char-set #\x)
                              (->char-set "x")
                              (->char-set (char-set #\x))))

      (test-not (char-set= (->char-set #\x)
                           (->char-set "y")
                           (->char-set (char-set #\x))))

      (test 10 (char-set-size
                (char-set-intersection char-set:ascii char-set:digit)))
      (test 10 (char-set-size
                (char-set-intersection char-set:digit char-set:ascii)))

      (test 5 (char-set-count vowel? char-set:ascii))

      (test '(#\x) (char-set->list (char-set #\x)))
      (test-not (equal? '(#\X) (char-set->list (char-set #\x))))

      (test "x" (char-set->string (char-set #\x)))
      (test-not (equal? "X" (char-set->string (char-set #\x))))

      (test-assert (char-set-contains? (->char-set "xyz") #\x))
      (test-not (char-set-contains? (->char-set "xyz") #\a))

      (test-assert (char-set-every char-lower-case? (->char-set "abcd")))
      (test-not (char-set-every char-lower-case? (->char-set "abcD")))
      (test-assert (char-set-any char-lower-case? (->char-set "abcd")))
      (test-not (char-set-any char-lower-case? (->char-set "ABCD")))

      (test-cs (->char-set "ABCD")
               (let ((cs (->char-set "abcd")))
                 (let lp ((cur (char-set-cursor cs)) (ans '()))
                   (if (end-of-char-set? cur) (list->char-set ans)
                       (lp (char-set-cursor-next cs cur)
                           (cons (char-upcase (char-set-ref cs cur)) ans))))))


      (test-cs (->char-set "123xa")
               (char-set-adjoin (->char-set "123") #\x #\a))
      (test-not (char-set= (char-set-adjoin (->char-set "123") #\x #\a)
                           (->char-set "123x")))
      (test-cs (->char-set "123xa")
               (char-set-adjoin! (->char-set "123") #\x #\a))
      (test-not (char-set= (char-set-adjoin! (->char-set "123") #\x #\a)
                           (->char-set "123x")))

      (test-cs (->char-set "13")
               (char-set-delete (->char-set "123") #\2 #\a #\2))
      (test-not (char-set= (char-set-delete (->char-set "123") #\2 #\a #\2)
                           (->char-set "13a")))
      (test-cs (->char-set "13")
               (char-set-delete! (->char-set "123") #\2 #\a #\2))
      (test-not (char-set= (char-set-delete! (->char-set "123") #\2 #\a #\2)
                           (->char-set "13a")))

      (test-cs (->char-set "abcdefABCDEF")
               (char-set-intersection char-set:hex-digit
                                      (char-set-complement char-set:digit)))
      (test-cs (->char-set "abcdefABCDEF")
               (char-set-intersection!
                (char-set-complement! (->char-set "0123456789"))
                char-set:hex-digit))

      (test-cs (->char-set "abcdefABCDEFghijkl0123456789")
               (char-set-union char-set:hex-digit
                               (->char-set "abcdefghijkl")))
      (test-cs (->char-set "abcdefABCDEFghijkl0123456789")
               (char-set-union! (->char-set "abcdefghijkl")
                                char-set:hex-digit))

      (test-cs (->char-set "ghijklmn")
               (char-set-difference (->char-set "abcdefghijklmn")
                                    char-set:hex-digit))
      (test-cs (->char-set "ghijklmn")
               (char-set-difference! (->char-set "abcdefghijklmn")
                                     char-set:hex-digit))

      (test-cs (->char-set "abcdefABCDEF")
               (char-set-xor (->char-set "0123456789")
                             char-set:hex-digit))
      (test-cs (->char-set "abcdefABCDEF")
               (char-set-xor! (->char-set "0123456789")
                              char-set:hex-digit))

      (call-with-values
          (lambda ()
            (char-set-diff+intersection char-set:hex-digit
                                        char-set:letter))
        (lambda (d i)
          (test-cs d (->char-set "0123456789"))
          (test-cs i (->char-set "abcdefABCDEF"))))

      (call-with-values
          (lambda ()
            (char-set-diff+intersection! (char-set-copy char-set:hex-digit)
                                         (char-set-copy char-set:letter)))
        (lambda (d i)
          (test-cs d (->char-set "0123456789"))
          (test-cs i (->char-set "abcdefABCDEF"))))

      (test-end))))
