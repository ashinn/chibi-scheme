(define-library (srfi 125 test)
  (export run-tests)
  (import (scheme base) (scheme char) (scheme write)
          (srfi 125) (srfi 128) (srfi 132)
          (chibi test))
  (begin
    (define (run-tests)
      (define number-comparator
        (make-comparator real? = < (lambda (x . o) (exact (abs (round x))))))
      (define ht-default (make-hash-table default-comparator))
      (define ht-eq (make-hash-table eq-comparator 'random-argument "another"))
      (define ht-eqv (make-hash-table eqv-comparator))
      (define ht-eq2 (make-hash-table eq?))
      (define ht-eqv2 (make-hash-table eqv?))
      (define ht-equal (make-hash-table equal?))
      (define ht-string (make-hash-table string=?))
      (define ht-string-ci (make-hash-table string-ci=?))
      (define ht-symbol (make-hash-table symbol=?))    ; FIXME: glass-box
      (define ht-fixnum (make-hash-table = (lambda (x . o) (abs x))))
      (define ht-default2
        (hash-table default-comparator 'foo 'bar 101.3 "fever" '(x y z) '#()))
      (define ht-fixnum2
        (let ((ht (make-hash-table number-comparator)))
          (do ((i 0 (+ i 1)))
              ((= i 10) (hash-table-copy ht))
            (hash-table-set! ht (* i i) i))))
      (define ht-string2
        (hash-table-unfold (lambda (s) (= 0 (string-length s)))
                           (lambda (s) (values s (string-length s)))
                           (lambda (s) (substring s 0 (- (string-length s) 1)))
                           "prefixes"
                           string-comparator
                           'ignored1 'ignored2 "ignored3" '#(ignored 4 5)))
      (define ht-string-ci2
        (alist->hash-table '(("" . 0) ("Mary" . 4) ("Paul" . 4) ("Peter" . 5))
                           string-ci-comparator
                           "ignored1" 'ignored2))
      (define ht-symbol2
        (alist->hash-table '((mary . travers) (noel . stookey) (peter .yarrow))
                           eq?))
      (define ht-equal2
        (alist->hash-table '(((edward) . abbey)
                             ((dashiell) . hammett)
                             ((edward) . teach)
                             ((mark) . twain))
                           equal?
                           (comparator-hash-function default-comparator)))
      (define test-tables
        (list ht-default   ht-default2   ; initial keys: foo, 101.3, (x y z)
              ht-eq        ht-eq2        ; initially empty
              ht-eqv       ht-eqv2       ; initially empty
              ht-equal     ht-equal2     ; initial keys: (edward), (dashiell), (mark)
              ht-string    ht-string2    ; initial keys: "p, "pr", ..., "prefixes"
              ht-string-ci ht-string-ci2 ; initial keys: "", "Mary", "Paul", "Peter"
              ht-symbol    ht-symbol2    ; initial keys: mary, noel, peter
              ht-fixnum    ht-fixnum2))  ; initial keys: 0, 1, 4, 9, ..., 81

      (test-begin "srfi 125: intermediate hash tables")

      ;; Predicates

      (test (append '(#f #f) (map (lambda (x) #t) test-tables))
          (map hash-table?
               (cons '#()
                     (cons default-comparator
                           test-tables))))

      (test '(#f #t #f #f #f #f #f #t #f #t #f #t #f #t #f #t)
          (map hash-table-contains?
               test-tables
               '(foo 101.3
                     x "y"
                     (14 15) #\newline
                     (edward) (mark)
                     "p" "pref"
                     "mike" "PAUL"
                     jane noel
                     0 4)))

      (test (map (lambda (x) #f) test-tables)
          (map hash-table-contains?
               test-tables
               '(#u8() 47.9
                    '#() '()
                    foo bar
                    19 (henry)
                    "p" "perp"
                    "mike" "Noel"
                    jane paul
                    0 5)))

      (test '(#t #f #t #t #t #t #t #f #t #f #t #f #t #f #t #f)
          (map hash-table-empty? test-tables))

      ;; (test (map (lambda (x) #t) test-tables)
      ;;     (map (lambda (ht1 ht2) (hash-table=? default-comparator ht1 ht2))
      ;;          test-tables
      ;;          test-tables))

      ;; (test '(#f #f #t #t #t #t #f #f #f #f #f #f #f #f #f #f)
      ;;     (map (lambda (ht1 ht2) (hash-table=? default-comparator ht1 ht2))
      ;;          test-tables
      ;;          (do ((tables (reverse test-tables) (cddr tables))
      ;;               (rev '() (cons (car tables) (cons (cadr tables) rev))))
      ;;              ((null? tables)
      ;;               rev))))

      (test '(#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f)
          (map hash-table-mutable? test-tables))

      ;; FIXME: glass-box
      ;; (test (map hash-table-mutable? (map hash-table-copy test-tables))
      ;;     (map (lambda (x) #f) test-tables))

      (test #t
          (hash-table-mutable? (hash-table-copy ht-fixnum2 #t)))

      ;; Accessors.

      ;; FIXME: glass-box (implementations not required to raise an exception here)
      ;; (test (map (lambda (ht)
      ;;              (guard (exn
      ;;                      (else 'err))
      ;;                (hash-table-ref ht 'not-a-key)))
      ;;            test-tables)
      ;;     (map (lambda (ht) 'err) test-tables))

      ;; FIXME: glass-box (implementations not required to raise an exception here)
      ;; (test (map (lambda (ht)
      ;;              (guard (exn
      ;;                      (else 'err))
      ;;                (hash-table-ref ht 'not-a-key (lambda () 'err))))
      ;;            test-tables)
      ;;     (map (lambda (ht) 'err) test-tables))

      ;; FIXME: glass-box (implementations not required to raise an exception here)
      ;; (test (map (lambda (ht)
      ;;              (guard (exn
      ;;                      (else 'err))
      ;;                (hash-table-ref ht 'not-a-key (lambda () 'err) values)))
      ;;            test-tables)
      ;;     (map (lambda (ht) 'err) test-tables))

      (test '(err "fever" err err err err err twain err 4 err 4 err stookey err 2)
          (map (lambda (ht key)
                 (guard (exn
                         (else 'err))
                   (hash-table-ref ht key)))
               test-tables
               '(foo 101.3
                     x "y"
                     (14 15) #\newline
                     (edward) (mark)
                     "p" "pref"
                     "mike" "PAUL"
                     jane noel
                     0 4)))

      (test '(eh "fever" eh eh eh eh eh twain eh 4 eh 4 eh stookey eh 2)
          (map (lambda (ht key)
                 (guard (exn
                         (else 'err))
                   (hash-table-ref ht key (lambda () 'eh))))
               test-tables
               '(foo 101.3
                     x "y"
                     (14 15) #\newline
                     (edward) (mark)
                     "p" "pref"
                     "mike" "PAUL"
                     jane noel
                     0 4)))

      (test '(eh ("fever") eh eh eh eh eh (twain) eh (4) eh (4) eh (stookey) eh (2))
          (map (lambda (ht key)
                 (guard (exn
                         (else 'err))
                   (hash-table-ref ht key (lambda () 'eh) list)))
               test-tables
               '(foo 101.3
                     x "y"
                     (14 15) #\newline
                     (edward) (mark)
                     "p" "pref"
                     "mike" "PAUL"
                     jane noel
                     0 4)))

      ;; FIXME: glass-box (implementations not required to raise an exception here)
      ;; (test (map (lambda (ht)
      ;;              (guard (exn
      ;;                      (else 'eh))
      ;;                (hash-table-ref/default ht 'not-a-key 'eh)))
      ;;            test-tables)
      ;;     (map (lambda (ht) 'eh) test-tables))

      (test '(eh "fever" eh eh eh eh eh twain eh 4 eh 4 eh stookey eh 2)
          (map (lambda (ht key)
                 (guard (exn
                         (else 'err))
                   (hash-table-ref/default ht key 'eh)))
               test-tables
               '(foo 101.3
                     x "y"
                     (14 15) #\newline
                     (edward) (mark)
                     "p" "pref"
                     "mike" "PAUL"
                     jane noel
                     0 4)))

      (test '()
          (begin (hash-table-set! ht-fixnum)
                 (list-sort < (hash-table-keys ht-fixnum))))

      (test '(121 144 169)
          (begin (hash-table-set! ht-fixnum 121 11 144 12 169 13)
                 (list-sort < (hash-table-keys ht-fixnum))))

      (test '(0 1 4 9 16 25 36 49 64 81 121 144 169)
          (begin (hash-table-set! ht-fixnum
                                  0 0 1 1 4 2 9 3 16 4 25 5 36 6 49 7 64 8 81 9)
                 (list-sort < (hash-table-keys ht-fixnum))))

      (test '(13 12 11 0 1 2 3 4 5 6 7 8 9)
          (map (lambda (i) (hash-table-ref/default ht-fixnum i 'error))
               '(169 144 121 0 1 4 9 16 25 36 49 64 81)))

      (test '(13 12 11 0 1 2 3 4 5 6 7 8 9)
          (begin (hash-table-delete! ht-fixnum)
                 (map (lambda (i) (hash-table-ref/default ht-fixnum i 'error))
                      '(169 144 121 0 1 4 9 16 25 36 49 64 81))))

      (test '(-1 12 -1 0 -1 2 -1 4 -1 6 -1 8 -1)
          (begin (hash-table-delete! ht-fixnum 1 9 25 49 81 200 121 169 81 1)
                 (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                      '(169 144 121 0 1 4 9 16 25 36 49 64 81))))

      (test '(-1 12 -1 -1 -1 2 -1 4 -1 -1 -1 8 -1)
          (begin (hash-table-delete! ht-fixnum 200 100 0 81 36)
                 (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                      '(169 144 121 0 1 4 9 16 25 36 49 64 81))))

      (test '(13 12 11 0 1 2 -1 4 -1 -1 -1 8 -1)
          (begin (hash-table-intern! ht-fixnum 169 (lambda () 13))
                 (hash-table-intern! ht-fixnum 121 (lambda () 11))
                 (hash-table-intern! ht-fixnum   0 (lambda ()  0))
                 (hash-table-intern! ht-fixnum   1 (lambda ()  1))
                 (hash-table-intern! ht-fixnum   1 (lambda () 99))
                 (hash-table-intern! ht-fixnum 121 (lambda () 66))
                 (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                      '(169 144 121 0 1 4 9 16 25 36 49 64 81))))

      (test '(#(0 0) #(1 1) #(4 2) #(16 4) #(64 8) #(121 11) #(144 12) #(169 13))
          (list-sort (lambda (v1 v2) (< (vector-ref v1 0) (vector-ref v2 0)))
                     (hash-table-map->list vector ht-fixnum)))

      (test (begin (hash-table-intern! ht-fixnum 169 (lambda () 13))
                   (hash-table-intern! ht-fixnum 144 (lambda () 9999))
                   (hash-table-intern! ht-fixnum 121 (lambda () 11))
                   (list-sort (lambda (l1 l2)
                                (< (car l1) (car l2)))
                              (hash-table-map->list list ht-fixnum)))
          '((0 0) (1 1) (4 2) (16 4) (64 8) (121 11) (144 12) (169 13)))

      (test (begin (hash-table-update! ht-fixnum 9 length (lambda () '(a b c)))
                   (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                        '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
          '(13 12 11 0 1 2 3 4 -1 -1 -1 8 -1))

      (test (begin (hash-table-update! ht-fixnum 16 -)
                   (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                        '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
          '(13 12 11 0 1 2 3 -4 -1 -1 -1 8 -1))

      (test (begin (hash-table-update! ht-fixnum 16 - abs)
                   (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                        '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
          '(13 12 11 0 1 2 3 4 -1 -1 -1 8 -1))

      (test (begin (hash-table-update!/default ht-fixnum 25 - 5)
                   (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                        '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
          '(13 12 11 0 1 2 3 4 -5 -1 -1 8 -1))

      (test (begin (hash-table-update!/default ht-fixnum 25 - 999)
                   (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                        '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
          '(13 12 11 0 1 2 3 4 5 -1 -1 8 -1))

      (test '(#t #t)
          (let* ((n0 (hash-table-size ht-fixnum))
                 (ht (hash-table-copy ht-fixnum #t)))
            (call-with-values
                (lambda () (hash-table-pop! ht))
              (lambda (key val)
                (list (= key (* val val))
                      (= (- n0 1) (hash-table-size ht)))))))

      (test '(13 12 11 0 1 2 3 4 5 -1 -1 8 -1 -1)
          (begin (hash-table-delete! ht-fixnum 75)
                 (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                      '(169 144 121 0 1 4 9 16 25 36 49 64 75 81))))

      (test '(13 12 11 0 1 2 3 4 5 -1 -1 8 -1)
          (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
               '(169 144 121 0 1 4 9 16 25 36 49 64 81)))

      (test '(13 12 11 0 1 2 3 4 5 6 -1 8 9)
          (begin (hash-table-set! ht-fixnum 36 6)
                 (hash-table-set! ht-fixnum 81 9)
                 (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                      '(169 144 121 0 1 4 9 16 25 36 49 64 81))))

      (test 0
          (begin (hash-table-clear! ht-eq)
                 (hash-table-size ht-eq)))

      ;; The whole hash table.

      (test 3
          (begin (hash-table-set! ht-eq 'foo 13 'bar 14 'baz 18)
                 (hash-table-size ht-eq)))

      (test '(0 3 #t)
          (let* ((ht (hash-table-empty-copy ht-eq))
                 (n0 (hash-table-size ht))
                 (ignored (hash-table-set! ht 'foo 13 'bar 14 'baz 18))
                 (n1 (hash-table-size ht)))
            (list n0 n1 (hash-table=? default-comparator ht ht-eq))))

      (test 0
          (begin (hash-table-clear! ht-eq)
                 (hash-table-size ht-eq)))

      (test '(144 12)
          (hash-table-find (lambda (key val)
                             (if (= 144 key (* val val))
                                 (list key val)
                                 #f))
                           ht-fixnum
                           (lambda () 99)))

      (test 99
          (hash-table-find (lambda (key val)
                             (if (= 144 key val)
                                 (list key val)
                                 #f))
                           ht-fixnum
                           (lambda () 99)))

      (test 2
          (hash-table-count <= ht-fixnum))

      ;; Mapping and folding.

      (test '(0 1 2 3 4 5 6 -1 8 9 -1 11 12 13 -1)
          (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
               '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196)))

      (test '(0 1 4 9 16 25 36 -1 64 81 -1 121 144 169 -1)
          (let ((ht (hash-table-map (lambda (val) (* val val))
                                    eqv-comparator
                                    ht-fixnum)))
            (map (lambda (i) (hash-table-ref/default ht i -1))
                 '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196))))

      (test '(#(0 1 4 9 16 25 36 -1 64 81 -1 121 144 169 -1)
              #(0 1 2 3  4  5  6 -1  8  9 -1  11  12  13 -1))
          (let ((keys (make-vector 15 -1))
                (vals (make-vector 15 -1)))
            (hash-table-for-each (lambda (key val)
                                   (vector-set! keys val key)
                                   (vector-set! vals val val))
                                 ht-fixnum)
            (list keys vals)))

      (test '(0 1 2 3 -4 -5 -6 -1 -8 -9 -1 -11 -12 -13 -1)
          (begin (hash-table-map! (lambda (key val)
                                    (if (<= 10 key)
                                        (- val)
                                        val))
                                  ht-fixnum)
                 (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                      '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196))))

      (test 13
          (hash-table-fold (lambda (key val acc)
                             (+ val acc))
                           0
                           ht-string-ci2))

      (test '(0 1 4 9 16 25 36 64 81 121 144 169)
          (list-sort < (hash-table-fold (lambda (key val acc)
                                          (cons key acc))
                                        '()
                                        ht-fixnum)))

      ;; Copying and conversion.

      (test #t
          (hash-table=? number-comparator ht-fixnum (hash-table-copy ht-fixnum)))

      (test #t
          (hash-table=? number-comparator ht-fixnum (hash-table-copy ht-fixnum #f)))

      (test #t
          (hash-table=? number-comparator ht-fixnum (hash-table-copy ht-fixnum #t)))

      (test #f
          (hash-table-mutable? (hash-table-copy ht-fixnum)))

      (test #f
          (hash-table-mutable? (hash-table-copy ht-fixnum #f)))

      (test #t
          (hash-table-mutable? (hash-table-copy ht-fixnum #t)))

      (test '()
          (hash-table->alist ht-eq))

      (test '((0 . 0)
              (1 . 1)
              (4 . 2)
              (9 . 3)
              (16 . -4)
              (25 . -5)
              (36 . -6)
              (64 . -8)
              (81 . -9)
              (121 . -11)
              (144 . -12)
              (169 . -13))
          (list-sort (lambda (x y) (< (car x) (car y)))
                     (hash-table->alist ht-fixnum)))

      ;; Hash tables as sets.

      (test '((0 . 0)
              (1 . 1)
              (4 . 2)
              (9 . 3)
              (16 . -4)
              (25 . -5)
              (36 . -6)
              (49 . 7)
              (64 . -8)
              (81 . -9)
              (121 . -11)
              (144 . -12)
              (169 . -13))
          (begin (hash-table-union! ht-fixnum ht-fixnum2)
                 (list-sort (lambda (x y) (< (car x) (car y)))
                            (hash-table->alist ht-fixnum))))

      (test '((0 . 0)
              (1 . 1)
              (4 . 2)
              (9 . 3)
              (16 . 4)
              (25 . 5)
              (36 . 6)
              (49 . 7)
              (64 . 8)
              (81 . 9)
              (121 . -11)
              (144 . -12)
              (169 . -13))
          (let ((ht (hash-table-copy ht-fixnum2 #t)))
            (hash-table-union! ht ht-fixnum)
            (list-sort (lambda (x y) (< (car x) (car y)))
                       (hash-table->alist ht))))

      (test #t
          (begin (hash-table-union! ht-eqv2 ht-fixnum)
                 (hash-table=? default-comparator ht-eqv2 ht-fixnum)))

      (test #t
          (begin (hash-table-intersection! ht-eqv2 ht-fixnum)
                 (hash-table=? default-comparator ht-eqv2 ht-fixnum)))

      (test #t
          (begin (hash-table-intersection! ht-eqv2 ht-eqv)
                 (hash-table-empty? ht-eqv2)))

      (test '((0 . 0)
              (1 . 1)
              (4 . 2)
              (9 . 3)
              (16 . -4)
              (25 . -5)
              (36 . -6)
              (49 . 7)
              (64 . -8)
              (81 . -9))
          (begin (hash-table-intersection! ht-fixnum ht-fixnum2)
                 (list-sort (lambda (x y) (< (car x) (car y)))
                            (hash-table->alist ht-fixnum))))

      (test '((4 . 2)
              (25 . -5))
          (begin (hash-table-intersection!
                  ht-fixnum
                  (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
                                     number-comparator))
                 (list-sort (lambda (x y) (< (car x) (car y)))
                            (hash-table->alist ht-fixnum))))

      (test '((0 . 0)
              (1 . 1)
              (9 . 3)
              (16 . 4)
              (36 . 6)
              (49 . 7)
              (64 . 8)
              (81 . 9))
          (let ((ht (hash-table-copy ht-fixnum2 #t)))
            (hash-table-difference!
             ht
             (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
                                number-comparator))
            (list-sort (lambda (x y) (< (car x) (car y)))
                       (hash-table->alist ht))))

      (test '((-1 . -1)
              (0 . 0)
              (1 . 1)
              (9 . 3)
              (16 . 4)
              (36 . 6)
              (49 . 7)
              (64 . 8)
              (81 . 9)
              (100 . 10))
          (let ((ht (hash-table-copy ht-fixnum2 #t)))
            (hash-table-xor!
             ht
             (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
                                number-comparator))
            (list-sort (lambda (x y) (< (car x) (car y)))
                       (hash-table->alist ht))))

      (test 'key-not-found
          (guard (exn
                  (else 'key-not-found))
            (hash-table-ref ht-default "this key won't be present")))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; Desultory tests of deprecated procedures and usages.
      ;; Deprecated usage of make-hash-table and alist->hash-table
      ;; has already been tested above.
      ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (test '(#t #t #t)
          (let* ((x (list 1 2 3))
                 (y (cons 1 (cdr x)))
                 (h1 (hash x))
                 (h2 (hash y)))
            (list (exact-integer? h1)
                  (exact-integer? h2)
                  (= h1 h2))))

      (test '(#t #t #t)
          (let* ((x "abcd")
                 (y (string-append "ab" "cd"))
                 (h1 (string-hash x))
                 (h2 (string-hash y)))
            (list (exact-integer? h1)
                  (exact-integer? h2)
                  (= h1 h2))))

      (test '(#t #t #t)
          (let* ((x "Hello There!")
                 (y "hello THERE!")
                 (h1 (string-ci-hash x))
                 (h2 (string-ci-hash y)))
            (list (exact-integer? h1)
                  (exact-integer? h2)
                  (= h1 h2))))

      (test '(#t #t #t)
          (let* ((x '#(a "bcD" #\c (d 2.718) -42 #u8() #() #u8(19 20)))
                 (y x)
                 (h1 (hash-by-identity x))
                 (h2 (hash-by-identity y)))
            (list (exact-integer? h1)
                  (exact-integer? h2)
                  (= h1 h2))))

      (test '(#t #t #t)
          (let* ((x (list 1 2 3))
                 (y (cons 1 (cdr x)))
                 (h1 (hash x 60))
                 (h2 (hash y 60)))
            (list (exact-integer? h1)
                  (exact-integer? h2)
                  (= h1 h2))))

      (test '(#t #t #t)
          (let* ((x "abcd")
                 (y (string-append "ab" "cd"))
                 (h1 (string-hash x 97))
                 (h2 (string-hash y 97)))
            (list (exact-integer? h1)
                  (exact-integer? h2)
                  (= h1 h2))))

      (test '(#t #t #t)
          (let* ((x "Hello There!")
                 (y "hello THERE!")
                 (h1 (string-ci-hash x 101))
                 (h2 (string-ci-hash y 101)))
            (list (exact-integer? h1)
                  (exact-integer? h2)
                  (= h1 h2))))

      (test '(#t #t #t)
          (let* ((x '#(a "bcD" #\c (d 2.718) -42 #u8() #() #u8(19 20)))
                 (y x)
                 (h1 (hash-by-identity x 102))
                 (h2 (hash-by-identity y 102)))
            (list (exact-integer? h1)
                  (exact-integer? h2)
                  (= h1 h2))))

      (test #t
          (let ((f (hash-table-equivalence-function ht-fixnum)))
            (if (procedure? f)
                (f 34 34)
                #t)))

      (test #t
          (let ((f (hash-table-hash-function ht-fixnum)))
            (if (procedure? f)
                (= (f 34) (f 34))
                #t)))

      (test '(#t #t #f #f #t #f #f #f #f #t #f)
          (map (lambda (key) (hash-table-exists? ht-fixnum2 key))
               '(0 1 2 3 4 5 6 7 8 9 10)))

      (test (apply +
                   (map (lambda (x) (* x x))
                        '(0 1 2 3 4 5 6 7 8 9)))
          (let ((n 0))
            (hash-table-walk ht-fixnum2
                             (lambda (key val) (set! n (+ n key))))
            n))

      (test '(0 1 4 9 16 25 36 49 64 81)
          (list-sort < (hash-table-fold ht-fixnum2
                                        (lambda (key val acc)
                                          (cons key acc))
                                        '())))

      (test '((0 . 0)
              (.25 . .5)
              (1 . 1)
              (4 . 2)
              (9 . 3)
              (16 . 4)
              (25 . 5)
              (36 . 6)
              (49 . 7)
              (64 . 8)
              (81 . 9)
              (121 . -11)
              (144 . -12))
          (let ((ht (hash-table-copy ht-fixnum2 #t))
                (ht2 (hash-table number-comparator
                                 .25 .5 64 9999 81 9998 121 -11 144 -12)))
            (hash-table-merge! ht ht2)
            (list-sort (lambda (x y) (< (car x) (car y)))
                       (hash-table->alist ht))))

      (test-end))))
