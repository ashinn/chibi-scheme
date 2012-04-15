
(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test name expect expr)
     (test expect expr))
    ((test expect expr)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((str (call-with-output-string
                    (lambda (out)
                      (write *tests-run*)
                      (display ". ")
                      (display 'expr out))))
             (res expr))
         (display str)
         (write-char #\space)
         (display (make-string (max 0 (- 72 (string-length str))) #\.))
         (flush-output)
         (cond
          ((equal? res expect)
           (set! *tests-passed* (+ *tests-passed* 1))
           (display " [PASS]\n"))
          (else
           (display " [FAIL]\n")
           (display "    expected ") (write expect)
           (display " but got ") (write res) (newline))))))))

(define-syntax test-assert
  (syntax-rules ()
    ((test-assert expr) (test #t expr))))

(define (test-begin . name)
  #f)

(define (test-end)
  (write *tests-passed*)
  (display " out of ")
  (write *tests-run*)
  (display " passed (")
  (write (* (/ *tests-passed* *tests-run*) 100))
  (display "%)")
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "r5rs")

(test 8 ((lambda (x) (+ x x)) 4))

(test '(3 4 5 6) ((lambda x x) 3 4 5 6))

(test '(5 6) ((lambda (x y . z) z) 3 4 5 6))

(test 'yes (if (> 3 2) 'yes 'no))

(test 'no (if (> 2 3) 'yes 'no))

(test 1 (if (> 3 2) (- 3 2) (+ 3 2)))

(test 'greater (cond ((> 3 2) 'greater) ((< 3 2) 'less)))

(test 'equal (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))

(test 'composite (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))

(test 'consonant
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else 'consonant)))

(test #t (and (= 2 2) (> 2 1)))

(test #f (and (= 2 2) (< 2 1)))

(test '(f g) (and 1 2 'c '(f g)))

(test #t (and))

(test #t (or (= 2 2) (> 2 1)))

(test #t (or (= 2 2) (< 2 1)))

(test '(b c) (or (memq 'b '(a b c)) (/ 3 0)))

(test 6 (let ((x 2) (y 3)) (* x y)))

(test 35 (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))

(test 70 (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))

(test -2 (let ()
           (define x 2)
           (define f (lambda () (- x)))
           (f)))

(define let*-def 1)
(let* () (define let*-def 2) #f)
(test 1 let*-def)

(test '#(0 1 2 3 4)
 (do ((vec (make-vector 5))
      (i 0 (+ i 1)))
     ((= i 5) vec)
   (vector-set! vec i i)))

(test 25
    (let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
          ((null? x)
           sum))))

(test '((6 1 3) (-5 -2))
    (let loop ((numbers '(3 -2 1 6 -5)) (nonneg '()) (neg '()))
      (cond
       ((null? numbers)
        (list nonneg neg))
       ((>= (car numbers) 0)
        (loop (cdr numbers) (cons (car numbers) nonneg) neg))
       ((< (car numbers) 0)
        (loop (cdr numbers) nonneg (cons (car numbers) neg))))))

(test '(list 3 4) `(list ,(+ 1 2) 4))

(test '(list a 'a) (let ((name 'a)) `(list ,name ',name)))

(test '(a 3 4 5 6 b)
    `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))

(test '(10 5 4 16 9 8)
    `(10 5 ,(expt 2 2) ,@(map (lambda (n) (expt n 2)) '(4 3)) 8))

(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
    `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))

(test '(a `(b ,x ,'y d) e)
    (let ((name1 'x)
          (name2 'y))
      `(a `(b ,,name1 ,',name2 d) e)))

(test '(list 3 4)
 (quasiquote (list (unquote (+ 1 2)) 4)))

(test #t (eqv? 'a 'a))

(test #f (eqv? 'a 'b))

(test #t (eqv? '() '()))

(test #f (eqv? (cons 1 2) (cons 1 2)))

(test #f (eqv? (lambda () 1) (lambda () 2)))

(test #t (let ((p (lambda (x) x))) (eqv? p p)))

(test #t (eq? 'a 'a))

(test #f (eq? (list 'a) (list 'a)))

(test #t (eq? '() '()))

(test #t (eq? car car))

(test #t (let ((x '(a))) (eq? x x)))

(test #t (let ((p (lambda (x) x))) (eq? p p)))

(test #t (equal? 'a 'a))

(test #t (equal? '(a) '(a)))

(test #t (equal? '(a (b) c) '(a (b) c)))

(test #t (equal? "abc" "abc"))

(test #f (equal? "abc" "abcd"))

(test #f (equal? "a" "b"))

(test #t (equal? 2 2))

;;(test #f (eqv? 2 2.0))

;;(test #f (equal? 2.0 2))

(test #t (equal? (make-vector 5 'a) (make-vector 5 'a)))

(test 4 (max 3 4))

;;(test 4 (max 3.9 4))

(test 7 (+ 3 4))

(test 3 (+ 3))

(test 0 (+))

(test 4 (* 4))

(test 1 (*))

(test -1 (- 3 4))

(test -6 (- 3 4 5))

(test -3 (- 3))

(test -1.0 (- 3.0 4))

(test 7 (abs -7))

(test 1 (modulo 13 4))

(test 1 (remainder 13 4))

(test 3 (modulo -13 4))

(test -1 (remainder -13 4))

(test -3 (modulo 13 -4))

(test 1 (remainder 13 -4))

(test -1 (modulo -13 -4))

(test -1 (remainder -13 -4))

(test 4 (gcd 32 -36))

(test 288 (lcm 32 -36))

(test 100 (string->number "100"))

(test 256 (string->number "100" 16))

(test 127 (string->number "177" 8))

(test 5 (string->number "101" 2))

(test 100.0 (string->number "1e2"))

(test "100" (number->string 100))

(test "100" (number->string 256 16))

(test "FF" (number->string 255 16))

(test "177" (number->string 127 8))

(test "101" (number->string 5 2))

(test #f (not 3))

(test #f (not (list 3)))

(test #f (not '()))

(test #f (not (list)))

(test #f (not '()))

(test #f (boolean? 0))

(test #f (boolean? '()))

(test #t (pair? '(a . b)))

(test #t (pair? '(a b c)))

(test '(a) (cons 'a '()))

(test '((a) b c d) (cons '(a) '(b c d)))

(test '("a" b c) (cons "a" '(b c)))

(test '(a . 3) (cons 'a 3))

(test '((a b) . c) (cons '(a b) 'c))

(test 'a (car '(a b c)))

(test '(a) (car '((a) b c d)))

(test 1 (car '(1 . 2)))

(test '(b c d) (cdr '((a) b c d)))

(test 2 (cdr '(1 . 2)))

(test #t (list? '(a b c)))

(test #t (list? '()))

(test #f (list? '(a . b)))

(test #f
    (let ((x (list 'a)))
      (set-cdr! x x)
      (list? x)))

(test '(a 7 c) (list 'a (+ 3 4) 'c))

(test '() (list))

(test 3 (length '(a b c)))

(test 3 (length '(a (b) (c d e))))

(test 0 (length '()))

(test '(x y) (append '(x) '(y)))

(test '(a b c d) (append '(a) '(b c d)))

(test '(a (b) (c)) (append '(a (b)) '((c))))

(test '(a b c . d) (append '(a b) '(c . d)))

(test 'a (append '() 'a))

(test '(c b a) (reverse '(a b c)))

(test '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

(test 'c (list-ref '(a b c d) 2))

(test '(a b c) (memq 'a '(a b c)))

(test '(b c) (memq 'b '(a b c)))

(test #f (memq 'a '(b c d)))

(test #f (memq (list 'a) '(b (a) c)))

(test '((a) c) (member (list 'a) '(b (a) c)))

(test '(101 102) (memv 101 '(100 101 102)))

(test #f (assq (list 'a) '(((a)) ((b)) ((c)))))

(test '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

(test '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

(test #t (symbol? 'foo))

(test #t (symbol? (car '(a b))))

(test #f (symbol? "bar"))

(test #t (symbol? 'nil))

(test #f (symbol? '()))

(test "flying-fish" (symbol->string 'flying-fish))

(test "Martin" (symbol->string 'Martin))

(test "Malvina" (symbol->string (string->symbol "Malvina")))

(test #t (string? "a"))

(test #f (string? 'a))

(test 0 (string-length ""))

(test 3 (string-length "abc"))

(test #\a (string-ref "abc" 0))

(test #\c (string-ref "abc" 2))

(test #t (string=? "a" (string #\a)))

(test #f (string=? "a" (string #\b)))

(test #t (string<? "a" "aa"))

(test #f (string<? "aa" "a"))

(test #f (string<? "a" "a"))

(test #t (string<=? "a" "aa"))

(test #t (string<=? "a" "a"))

(test #t (string=? "a" (make-string 1 #\a)))

(test #f (string=? "a" (make-string 1 #\b)))

(test "" (substring "abc" 0 0))

(test "a" (substring "abc" 0 1))

(test "bc" (substring "abc" 1 3))

(test "abc" (string-append "abc" ""))

(test "abc" (string-append "" "abc"))

(test "abc" (string-append "a" "bc"))

(test '#(0 ("Sue" "Sue") "Anna")
 (let ((vec (vector 0 '(2 2 2 2) "Anna")))
   (vector-set! vec 1 '("Sue" "Sue"))
   vec))

(test '(dah dah didah) (vector->list '#(dah dah didah)))

(test '#(dididit dah) (list->vector '(dididit dah)))

(test #t (procedure? car))

(test #f (procedure? 'car))

(test #t (procedure? (lambda (x) (* x x))))

(test #f (procedure? '(lambda (x) (* x x))))

(test #t (call-with-current-continuation procedure?))

(test 7 (call-with-current-continuation (lambda (k) (+ 2 5))))

(test 3 (call-with-current-continuation (lambda (k) (+ 2 5 (k 3)))))

(test 7 (apply + (list 3 4)))

(test '(b e h) (map cadr '((a b) (d e) (g h))))

(test '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))

(test '(5 7 9) (map + '(1 2 3) '(4 5 6)))

(test '#(0 1 4 9 16)
    (let ((v (make-vector 5)))
      (for-each
       (lambda (i) (vector-set! v i (* i i)))
       '(0 1 2 3 4))
      v))

(test 3 (force (delay (+ 1 2))))

(test '(3 3) (let ((p (delay (+ 1 2)))) (list (force p) (force p))))

(test 'ok (let ((else 1)) (cond (else 'ok) (#t 'bad))))

(test 'ok (let ((=> 1)) (cond (#t => 'ok))))

(test '(,foo) (let ((unquote 1)) `(,foo)))

(test '(,@foo) (let ((unquote-splicing 1)) `(,@foo)))

(test 'ok
    (let ((... 2))
      (let-syntax ((s (syntax-rules ()
                        ((_ x ...) 'bad)
                        ((_ . r) 'ok))))
        (s a b c))))

(test 'ok (let ()
            (let-syntax ()
              (define internal-def 'ok))
            internal-def))

(test 'ok (let ()
            (letrec-syntax ()
              (define internal-def 'ok))
            internal-def))

(test '(2 1)
    ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (list x y))))))

(test '(2 2)
    ((lambda () (let ((x 1)) (set! x 2) (let ((y x)) (list x y))))))

(test '(1 2)
    ((lambda () (let ((x 1)) (let ((y x)) (set! y 2) (list x y))))))

(test '(2 3)
    ((lambda () (let ((x 1)) (let ((y x)) (set! x 2) (set! y 3) (list x y))))))

(test '(a b c)
    (let* ((path '())
           (add (lambda (s) (set! path (cons s path)))))
      (dynamic-wind (lambda () (add 'a)) (lambda () (add 'b)) (lambda () (add 'c)))
      (reverse path)))

(test '(connect talk1 disconnect connect talk2 disconnect)
    (let ((path '())
          (c #f))
      (let ((add (lambda (s)
                   (set! path (cons s path)))))
        (dynamic-wind
            (lambda () (add 'connect))
            (lambda ()
              (add (call-with-current-continuation
                    (lambda (c0)
                      (set! c c0)
                      'talk1))))
            (lambda () (add 'disconnect)))
        (if (< (length path) 4)
            (c 'talk2)
            (reverse path)))))

(test 2 (let-syntax
            ((foo (syntax-rules ::: ()
                    ((foo ... args :::)
                     (args ::: ...)))))
          (foo 3 - 5)))

(test '(5 4 1 2 3)
    (let-syntax
        ((foo (syntax-rules ()
                ((foo args ... penultimate ultimate)
                 (list ultimate penultimate args ...)))))
      (foo 1 2 3 4 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-end)
