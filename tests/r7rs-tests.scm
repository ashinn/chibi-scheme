
(import (scheme base) (scheme char) (scheme division) (scheme lazy)
        (scheme inexact) (scheme complex) (scheme time) (scheme eval)
        (scheme file) (scheme read) (scheme write) (scheme case-lambda)
        (chibi test))

(test-begin "r7rs")

(define x 28)
(test 28 x)

(test 'a (quote a))
(test #(a b c) (quote #(a b c)))
(test '(+ 1 2) (quote (+ 1 2)))

(test 'a 'a)
(test #(a b c) '#(a b c))
(test '() '())
(test '(+ 1 2) '(+ 1 2))
(test '(quote a) '(quote a))
(test '(quote a) ''a)

(test "abc" '"abc")
(test "abc" "abc")
(test 145932 '145932)
(test 145932 145932)
(test #t '#t)
(test #t #t)

(test 7 (+ 3 4))
(test 12 ((if #f + *) 3 4))

;;(test {\em{}a procedure} (lambda (x) (+ x x)))
(test 8 ((lambda (x) (+ x x)) 4))
(define reverse-subtract
  (lambda (x y) (- y x)))
(test 3 (reverse-subtract 7 10))
(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(test 10 (add4 6))

(test '(3 4 5 6) ((lambda x x) 3 4 5 6))
(test '(5 6) ((lambda (x y . z) z)
 3 4 5 6))

(test 'yes (if (> 3 2) 'yes 'no))
(test 'no (if (> 2 3) 'yes 'no))
(test 1 (if (> 3 2)
    (- 3 2)
    (+ 3 2)))
(define x 2)
(test 3 (+ x 1))
;; (test \unspecified (set! x 4))
;; (test 5 (+ x 1))

(test 'greater (cond ((> 3 2) 'greater)
      ((< 3 2) 'less)))

(test 'equal (cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal)))

(test 2 (cond ((assv 'b '((a 1) (b 2))) => cadr)
      (else #f)))

(test 'composite (case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite)))
;; (test \unspecified (case (car '(c d))
;;   ((a) 'a)
;;   ((b) 'b)))
(test 'c (case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else => (lambda (x) x))))

(test #t (and (= 2 2) (> 2 1)))
(test #f (and (= 2 2) (< 2 1)))
(test '(f g) (and 1 2 'c '(f g)))
(test #t (and))

(test #t (or (= 2 2) (> 2 1)))
(test #t (or (= 2 2) (< 2 1)))
(test #f (or #f #f #f))
(test '(b c) (or (memq 'b '(a b c))
    (/ 3 0)))
;; (display "1")
;;   (test \unspecified (display "2"))
;; (display "1")
;;   (test \unspecified (display "2"))

(test 6 (let ((x 2) (y 3))
  (* x y)))

(test 35 (let ((x 2) (y 3))
  (let ((x 7)
        (z (+ x y)))
    (* z x))))

(test 70 (let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x))))

(test #t
(letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
  (even? 88)))

(test 5
(letrec* ((p
           (lambda (x)
             (+ 1 (q (- x 1)))))
          (q
           (lambda (y)
             (if (zero? y)
                 0
                 (+ 1 (p (- y 1))))))
          (x (p 5))
          (y x))
  y))
(let*-values (((root rem) (exact-integer-sqrt 32)))
  (test 35 (* root rem)))

(test '(x y x y) (let ((a 'a) (b 'b) (x 'x) (y 'y))
  (let*-values (((a b) (values x y))
                ((x y) (values a b)))
    (list a b x y))))
(set! x 5)
            (test 6 (+ x 1))

;; (test \unspecified (begin (display "4 plus 1 equals ")
;;        (display (+ 4 1))))

(test #(0 1 2 3 4) (do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
  (vector-set! vec i i)))

(test 25 (let ((x '(1 3 5 7 9)))
  (do ((x x (cdr x))
       (sum 0 (+ sum (car x))))
      ((null? x) sum))))
(test '((6 1 3) (-5 -2))
    (let loop ((numbers '(3 -2 1 6 -5))
               (nonneg '())
               (neg '()))
      (cond ((null? numbers) (list nonneg neg))
            ((>= (car numbers) 0)
             (loop (cdr numbers)
                   (cons (car numbers) nonneg)
                   neg))
            ((< (car numbers) 0)
             (loop (cdr numbers)
                   nonneg
                   (cons (car numbers) neg))))))

(test 3 (force (delay (+ 1 2))))
(test '(3 3)  
(let ((p (delay (+ 1 2))))
  (list (force p) (force p))))

(define integers
  (letrec ((next
            (lambda (n)
              (delay (cons n (next (+ n 1)))))))
    (next 0)))
(define head
  (lambda (stream) (car (force stream))))
(define tail
  (lambda (stream) (cdr (force stream))))

(test 2  
(head (tail (tail integers))))

(define (stream-filter p? s)
  (lazy
   (if (null? (force s)) 
       (delay '())
       (let ((h (car (force s)))
             (t (cdr (force s))))
         (if (p? h)
             (delay (cons h (stream-filter p? t)))
             (stream-filter p? t))))))

(test 5
(head (tail (tail (stream-filter odd? integers)))))

(define count 0)
(define p
  (delay (begin (set! count (+ count 1))
                (if (> count x)
                    count
                    (force p)))))
(define x 5)
;; (test {\it{}a promise} p)
(test 6 (force p))
;; (test {\it{}a promise, still} p)
(test 6 (begin (set! x 10)
       (force p)))

;; (test \unspecified (eqv? (delay 1) 1))
;; (test \unspecified (pair? (delay (cons 1 2))))

;; (test 34 (+ (delay (* 3 7)) 13))

(define radix
  (make-parameter
   10
   (lambda (x)
     (if (and (integer? x) (<= 2 x 16))
         x
         (error "invalid radix")))))
(define (f n) (number->string n (radix)))
(test "12" (f 12))
(test "1100" (parameterize ((radix 2))
  (f 12)))
(test "12" (f 12))

;; (test \unspecified (radix 16))

;; (test \scherror (parameterize ((radix 0))
;;   (f 12)))

(test '(list 3 4) `(list ,(+ 1 2) 4))
(let ((name 'a)) (test '(list a (quote a)) `(list ,name ',name)))
(test '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
;; `(({\cf foo} ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))) (test '((foo 7) . cons)
;; )
(test #(10 5 2 4 3 8) `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
    `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) )
(let ((name1 'x)
      (name2 'y))
   (test '(a `(b ,x ,'y d) e) `(a `(b ,,name1 ,',name2 d) e)))
(test '(list 3 4) (quasiquote (list (unquote (+ 1 2)) 4)) )
(test `(list ,(+ 1 2) 4) (quasiquote (list (unquote (+ 1 2)) 4)))

(test 'now (let-syntax ((when (syntax-rules ()
                     ((when test stmt1 stmt2 ...)
                      (if test
                          (begin stmt1
                                 stmt2 ...))))))
  (let ((if #t))
    (when if (set! if 'now))
    if)))

(test 'outer (let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (m)))))

(test 7 (letrec-syntax
  ((my-or (syntax-rules ()
            ((my-or) #f)
            ((my-or e) e)
            ((my-or e1 e2 ...)
             (let ((temp e1))
               (if temp
                   temp
                   (my-or e2 ...)))))))
  (let ((x #f)
        (y 7)
        (temp 8)
        (let odd?)
        (if even?))
    (my-or x
           (let temp)
           (if y)
           y))))
(define-syntax be-like-begin
  (syntax-rules ()
    ((be-like-begin name)
     (define-syntax name
       (syntax-rules ()
         ((name expr (... ...))
          (begin expr (... ...))))))))
(be-like-begin sequence)
(test 4 (sequence 1 2 3 4))

(test 'ok (let ((=> #f)) (cond (#t => 'ok))))

;; (test #0= (let ((x (list 'a 'b 'c)))
;;   (set-cdr! (cddr x) x)
;;   x))
;; (a b c . #0#)

;; (test #1=\scherror
;; (begin (display #\backwhack{}x) . #1#))

;; (test #t
;; (string=? (symbol->string obj1)
;;           (symbol->string obj2)))

;; (test #f
;; (string=? (symbol->string \vari{obj})
;;           (symbol->string \varii{obj})))

(test #t (eqv? 'a 'a))
(test #f (eqv? 'a 'b))
(test #t (eqv? 2 2))
(test #t (eqv? '() '()))
(test #t (eqv? 100000000 100000000))
(test #f (eqv? (cons 1 2) (cons 1 2)))
(test #f (eqv? (lambda () 1)
      (lambda () 2)))
(test #f (eqv? #f 'nil))

;; (test \unspecified (eqv? "" ""))
;; (test \unspecified (eqv? '#() '#()))
;; (test \unspecified (eqv? (lambda (x) x)
;;       (lambda (x) x)))
;; (test \unspecified (let ((p (lambda (x) x)))
;;   (eqv? p p)))
;; (test \unspecified (eqv? (lambda (x) x)
;;       (lambda (y) y)))
;; (test \unspecified (eqv? +nan.0 +nan.0))
(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(test #t (let ((g (gen-counter)))
  (eqv? g g)))
(test #f
(eqv? (gen-counter) (gen-counter)))
(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))
(test #t (let ((g (gen-loser)))
  (eqv? g g)))
;; (test \unspecified
;; (eqv? (gen-loser) (gen-loser)))

;; (test \unspecified
;; (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
;;          (g (lambda () (if (eqv? f g) 'both 'g))))
;;   (eqv? f g)))

(test #f
(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
         (g (lambda () (if (eqv? f g) 'g 'both))))
  (eqv? f g)))

;; (test \unspecified (eqv? '(a) '(a)))
;; (test \unspecified (eqv? "a" "a"))
;; (test \unspecified (eqv? '(b) (cdr '(a b))))
(test #t (let ((x '(a)))
  (eqv? x x)))

(test #t (eq? 'a 'a))
;; (test \unspecified (eq? '(a) '(a)))
(test #f (eq? (list 'a) (list 'a)))
;; (test \unspecified (eq? "a" "a"))
;; (test \unspecified (eq? "" ""))
(test #t (eq? '() '()))
;; (test \unspecified (eq? 2 2))
;; (test \unspecified (eq? #\backwhack{}A #\backwhack{}A))
;; (test \unspecified (eq? car car))
;; (test \unspecified (let ((n (+ 2 3)))
;;   (eq? n n)))
(test #t (let ((x '(a)))
  (eq? x x)))
(test #t (let ((x '#()))
  (eq? x x)))
(test #t (let ((p (lambda (x) x)))
  (eq? p p)))

(test #t (equal? 'a 'a))
(test #t (equal? '(a) '(a)))
(test #t (equal? '(a (b) c)
        '(a (b) c)))
(test #t (equal? "abc" "abc"))
(test #t (equal? 2 2))
(test #t (equal? (make-vector 5 'a)
        (make-vector 5 'a)))
;; (test \unspecified (equal? (lambda (x) x)
;;         (lambda (y) y)))

(test #t (complex? 3+4i))
(test #t (complex? 3))
(test #t (real? 3))
(test #t (real? -2.5+0i))
(test #f (real? -2.5+0.0i))
(test #t (real? #e1e10))
(test #t (real? +inf.0))
(test #f (rational? -inf.0))
(test #t (rational? 6/10))
(test #t (rational? 6/3))
(test #t (integer? 3+0i))
(test #t (integer? 3.0))
(test #t (integer? 8/4))

(test #f (exact? 3.0))
(test #t (exact? #e3.0))
(test #t (inexact? 3.))

(test #t (exact-integer? 32))
(test #f (exact-integer? 32.0))
(test #f (exact-integer? 32/5))

(test #t (finite? 3))
(test #f (finite? +inf.0))
;; (test #f (finite? 3.0+inf.0i))

(test #t (nan? +nan.0))
(test #f (nan? 32))
;; (test #t (nan? +nan.0+5.0i))
(test #f (nan? 1+2i))

(test 4 (max 3 4))    ; exact
(test 4.0 (max 3.9 4))  ; inexact%

(test 7 (+ 3 4))
(test 3 (+ 3))
(test 0 (+))
(test 4 (* 4))
(test 1 (*))

(test -1 (- 3 4))
(test -6 (- 3 4 5))
(test -3 (- 3))
(test 3/20 (/ 3 4 5))
(test 1/3 (/ 3))

(test 7 (abs -7))

;; (test \vr (\hyper{operator}/ \vri{n} \vrii{n}))
;; {n_r}
;; (test \vr (\hyper{operator}-quotient \vri{n} \vrii{n}))
;; {n_q}
;; (test \vr (\hyper{operator}-remainder \vri{n} \vrii{n}))
;; {n_r}

;;      (test #t
;; (= \vri{n} (+ (* \vrii{n} (\hyper{operator}-quotient \vri{n} \vrii{n}))
;;            (\hyper{operator}-remainder \vri{n} \vrii{n}))))

(test 1 (modulo 13 4))
(test 1 (remainder 13 4))

(test 3 (modulo -13 4))
(test -1 (remainder -13 4))

(test -3 (modulo 13 -4))
(test 1 (remainder 13 -4))

(test -1 (modulo -13 -4))
(test -1 (remainder -13 -4))

(test -1.0 (remainder -13 -4.0))  ; inexact%

(test 4 (gcd 32 -36))
(test 0 (gcd))
(test 288 (lcm 32 -36))
(test 288.0 (lcm 32.0 -36))  ; inexact
(test 1 (lcm))

(test 3 (numerator (/ 6 4)))
(test 2 (denominator (/ 6 4)))
(test 2.0 (denominator
  (exact->inexact (/ 6 4))))

(test -5.0 (floor -4.3))
(test -4.0 (ceiling -4.3))
(test -4.0 (truncate -4.3))
(test -4.0 (round -4.3))

(test 3.0 (floor 3.5))
(test 4.0 (ceiling 3.5))
(test 3.0 (truncate 3.5))
(test 4.0 (round 3.5))  ; inexact

(test 4 (round 7/2))    ; exact
(test 7 (round 7))

(test 1/3 (rationalize
  (inexact->exact .3) 1/10))    ; exact
;; (test #i1/3 (rationalize .3 1/10))  ; inexact%

(test '(2 0) (call-with-values (lambda () (exact-integer-sqrt 4)) list))
(test '(2 1) (call-with-values (lambda () (exact-integer-sqrt 5)) list))

;; (test \vr (make-rectangular \vri{x} \vrii{x}))
;; {z}
;; (test \vr (make-polar \vriii{x} \vriv{x}))
;; {z}
;; (test \vri (real-part \vr{z}))
;; {x}
;; (test \vrii (imag-part \vr{z}))
;; {x}
;; (test $ (magnitude \vr{z}))
;; $
;; (test $x_ (angle \vr{z}))
;; (let ((number \vr{number})
;;       (radix \vr{radix}))
;;   (eqv? number
;;         (string->number (number->string number
;;                                         radix)
;;                         radix)))

(test 100 (string->number "100"))
(test 256 (string->number "100" 16))
(test 100.0 (string->number "1e2"))

(test #t #t)
(test #f #f)
(test #f '#f)

(test #f (not #t))
(test #f (not 3))
(test #f (not (list 3)))
(test #t (not #f))
(test #f (not '()))
(test #f (not (list)))
(test #f (not 'nil))

(test #t (boolean? #f))
(test #f (boolean? 0))
(test #f (boolean? '()))
;; (define y x)
;; (test '(a b c) y)
;; (test #t (list? y))
;; (test \unspecified (set-cdr! x 4))
;; (test '(a . 4) x)
;; (test #t (eqv? x y))
;; (test '(a . 4) y)
;; (test #f (list? y))
;; (test \unspecified (set-cdr! x x))
;; (test #f (list? x))

(test #t (pair? '(a . b)))
(test #t (pair? '(a b c)))
(test #f (pair? '()))
(test #f (pair? '#(a b)))

(test '(a) (cons 'a '()))
(test '((a) b c d) (cons '(a) '(b c d)))
(test '("a" b c) (cons "a" '(b c)))
(test '(a . 3) (cons 'a 3))
(test '((a b) . c) (cons '(a b) 'c))

(test 'a (car '(a b c)))
(test '(a) (car '((a) b c d)))
(test 1 (car '(1 . 2)))
;; (test \scherror (car '()))

(test '(b c d) (cdr '((a) b c d)))
(test 2 (cdr '(1 . 2)))
;; (test \scherror (cdr '()))
(define (g) '(constant-list))
;; (test \unspecified (set-car! (f) 3))
;; (test \scherror (set-car! (g) 3))

(test #t (list? '(a b c)))
(test #t (list? '()))
(test #f (list? '(a . b)))
(test #f (let ((x (list 'a))) (set-cdr! x x) (list? x)))

(test '(3 3) (make-list 2 3))

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
(test 'c (list-ref '(a b c d)
          (inexact->exact (round 1.8))))

(test '(0 ("Sue" "Sue") "Anna")
    (let ((lst (list 0 '(2 2 2 2) "Anna")))
      (list-set! lst 1 '("Sue" "Sue"))
      lst))

;; (test \scherror (list-set! '(0 1 2) 1 "doe"))  ; constant list%

(test '(a b c) (memq 'a '(a b c)))
(test '(b c) (memq 'b '(a b c)))
(test #f (memq 'a '(b c d)))
(test #f (memq (list 'a) '(b (a) c)))
(test '((a) c) (member (list 'a)
        '(b (a) c)))
(test '("b" "c") (member "B"
        '("a" "b" "c")
        string-ci=?))
;; (test \unspecified (memq 101 '(100 101 102)))
(test '(101 102) (memv 101 '(100 101 102)))
(define e '((a 1) (b 2) (c 3)))
(test '(a 1) (assq 'a e))
(test '(b 2) (assq 'b e))
(test #f (assq 'd e))
(test #f
(assq (list 'a) '(((a)) ((b)) ((c)))))
(test '((a))   
(assoc (list 'a) '(((a)) ((b)) ((c)))))
(test '(2 4)
(assoc 2.0 '((1 1) (2 4) (3 9)) =))
;; (test \unspecified    
;; (assq 5 '((2 3) (5 7) (11 13))))
(test '(5 7)    
(assv 5 '((2 3) (5 7) (11 13))))

(test #t (symbol? 'foo))
(test #t (symbol? (car '(a b))))
(test #f (symbol? "bar"))
(test #t (symbol? 'nil))
(test #f (symbol? '()))
(test #f (symbol? #f))

(test "flying-fish"     
(symbol->string 'flying-fish))
(test "Martin" (symbol->string 'Martin))
(test "Malvina" (symbol->string (string->symbol "Malvina")))

(test 'mISSISSIppi (string->symbol "mISSISSIppi"))
(test #t (eq? 'bitBlt (string->symbol "bitBlt")))
(test #t (eq? 'JollyWog (string->symbol (symbol->string 'JollyWog))))
(test #t (string=? "K. Harper, M.D."
                   (symbol->string (string->symbol "K. Harper, M.D."))))

;; (test 3 (digit-value #\backwhack{}3))
;; (test 4 (digit-value #\backwhack{}x0664))
;; (test 0 (digit-value #\backwhack{}x0EA6))
;; (define (g) "***")
;; (test \unspecified (string-set! (f) 0 #\backwhack{}?))
;; (test \scherror (string-set! (g) 0 #\backwhack{}?))
;; (test \scherror (string-set! (symbol->string 'immutable)
;;              0
;;              #\backwhack{}?))

(test #(0 (2 2 2 2) "Anna") '#(0 (2 2 2 2) "Anna"))

(test #(a b c) (vector 'a 'b 'c))

(test 8 (vector-ref '#(1 1 2 3 5 8 13 21)
            5))
(test 13 (vector-ref '#(1 1 2 3 5 8 13 21)
            (let ((i (round (* 2 (acos -1)))))
              (if (inexact? i)
                  (inexact->exact i)
                  i))))

(test #(0 ("Sue" "Sue") "Anna") (let ((vec (vector 0 '(2 2 2 2) "Anna")))
  (vector-set! vec 1 '("Sue" "Sue"))
  vec))

;; (test \scherror (vector-set! '#(0 1 2) 1 "doe"))  ; constant vector%

(test '(dah dah didah) (vector->list '#(dah dah didah)))
(test #(dididit dah) (list->vector '(dididit dah)))

;; (test #(#\backwhack{}A #\backwhack{}B #\backwhack{}C) (string->vector "ABC"))
;; vector->string
;;   (test "123" #(#\backwhack{}1 #\backwhack{}2 #\backwhack{}3))

(test "A" (utf8->string #u8(#x41)))
(test #u8(#xCE #xBB) (string->utf8 "λ"))

(test #t (procedure? car))
(test #f (procedure? 'car))
(test #t (procedure? (lambda (x) (* x x))))
(test #f (procedure? '(lambda (x) (* x x))))
(test #t (call-with-current-continuation procedure?))

(test 7 (apply + (list 3 4)))
(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

(test 30 ((compose sqrt *) 12 75))

(test '(b e h) (map cadr '((a b) (d e) (g h))))

(test '(1 4 27 256 3125) (map (lambda (n) (expt n n))
     '(1 2 3 4 5)))

(test '(5 7 9) (map + '(1 2 3) '(4 5 6 7)))

(test '(1 2) (let ((count 0))
  (map (lambda (ignored)
         (set! count (+ count 1))
         count)
       '(a b))))

(test "abdegh" (string-map char-foldcase "AbdEgH"))

(test "IBM" (string-map
 (lambda (c)
   (integer->char (+ 1 (char->integer c))))
 "HAL"))

(test "StUdLyCaPs"
    (string-map
     (lambda (c k) (if (eqv? k #\u) (char-upcase c) (char-downcase c)))
     "studlycaps xxx"
     "ululululul"))

(test #(b e h) (vector-map cadr '#((a b) (d e) (g h))))

(test #(1 4 27 256 3125) (vector-map (lambda (n) (expt n n))
            '#(1 2 3 4 5)))

(test #(5 7 9) (vector-map + '#(1 2 3) '#(4 5 6 7)))

;; (test #(1 2) or #(2 1) (let ((count 0))
;;   (vector-map
;;    (lambda (ignored)
;;      (set! count (+ count 1))
;;      count)
;;    '#(a b))))

(test #(0 1 4 9 16) (let ((v (make-vector 5)))
  (for-each (lambda (i)
              (vector-set! v i (* i i)))
            '(0 1 2 3 4))
  v))

(test '(101 100 99 98 97)
    (let ((v '()))
      (string-for-each
       (lambda (c) (set! v (cons (char->integer c) v)))
       "abcde")
      v))

(test '(0 1 4 9 16) (let ((v (make-list 5)))
  (vector-for-each
   (lambda (i) (list-set! v i (* i i)))
   '#(0 1 2 3 4))
  v))

(test -3 (call-with-current-continuation
  (lambda (exit)
    (for-each (lambda (x)
                (if (negative? x)
                    (exit x)))
              '(54 0 37 -3 245 19))
    #t)))
(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r
                  (lambda (obj)
                    (cond ((null? obj) 0)
                          ((pair? obj)
                           (+ (r (cdr obj)) 1))
                          (else (return #f))))))
          (r obj))))))

(test 4 (list-length '(1 2 3 4)))

(test #f (list-length '(a b . c)))
;; (define (values . things)
;;   (call-with-current-continuation 
;;     (lambda (cont) (apply cont things))))

(test 5
(call-with-values (lambda () (values 4 5))
                  (lambda (a b) b)))

(test -1 (call-with-values * -))

(test '(connect talk1 disconnect
               connect talk2 disconnect)
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

(test 21 (eval '(* 7 3) (scheme-report-environment 7)))

(test 20
(let ((f (eval '(lambda (f x) (f x x))
               (null-environment 7))))
  (f + 10)))

;; (test "/usr/local/bin:/usr/bin:/bin" (get-environment-variable "PATH"))

;; (test '(("USER" . "root") ("HOME" . "/")) (get-environment-variables))
(define add3
  (lambda (x) (+ x 3)))
(test 6 (add3 3))
(define first car)
(test 1 (first '(1 2)))

(test 45 (let ((x 5))
  (define foo (lambda (y) (bar x y)))
  (define bar (lambda (a b) (+ (* a b) a)))
  (foo (+ x 3))))

(test 3 (let ()
  (define-values (x y) (values 1 2))
  (+ x y)))

(test '(2 1) (let ((x 1) (y 2))
  (define-syntax swap!
    (syntax-rules ()
      ((swap! a b)
       (let ((tmp a))
         (set! a b)
         (set! b tmp)))))
  (swap! x y)
  (list x y)))
(define-record-type <pare>
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(test #t (pare? (kons 1 2)))
(test #f (pare? (cons 1 2)))
(test 1 (kar (kons 1 2)))
(test 2 (kdr (kons 1 2)))
(test 3 (let ((k (kons 1 2)))
          (set-kar! k 3)
          (kar k)))

(test 40 (* 5 8))

(test-end)
