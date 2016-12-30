;; -*- coding: utf-8 -*-

(import (scheme base) (scheme char) (scheme lazy)
        (scheme inexact) (scheme complex) (scheme time)
        (scheme file) (scheme read) (scheme write)
        (scheme eval) (scheme process-context) (scheme case-lambda)
        (scheme r5rs)
        (chibi test)  ; or (srfi 64)
        )

;; R7RS test suite.  Covers all procedures and syntax in the small
;; language except `delete-file'.  Currently assumes full-unicode
;; support, the full numeric tower and all standard libraries
;; provided.
;;
;; Uses the (chibi test) library which is written in portable R7RS.
;; This is mostly a subset of SRFI-64, providing test-begin, test-end
;; and test, which could be defined as something like:
;;
;;   (define (test-begin . o) #f)
;;
;;   (define (test-end . o) #f)
;;
;;   (define-syntax test
;;     (syntax-rules ()
;;       ((test expected expr)
;;        (let ((res expr))
;;          (cond
;;           ((not (equal? expr expected))
;;            (display "FAIL: ")
;;            (write 'expr)
;;            (display ": expected ")
;;            (write expected)
;;            (display " but got ")
;;            (write res)
;;            (newline)))))))
;;
;; however (chibi test) provides nicer output, timings, and
;; approximate equivalence for floating point numbers.

(test-begin "R7RS")

(test-begin "4.1 Primitive expression types")

(let ()
  (define x 28)
  (test 28 x))

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
(let ()
  (define x 2)
  (test 3 (+ x 1)))

(test-end)

(test-begin "4.2 Derived expression types")

(test 'greater
    (cond ((> 3 2) 'greater)
          ((< 3 2) 'less)))

(test 'equal
    (cond ((> 3 3) 'greater)
          ((< 3 3) 'less)
          (else 'equal)))

(test 2
    (cond ((assv 'b '((a 1) (b 2))) => cadr)
          (else #f)))

(test 'composite
    (case (* 2 3)
      ((2 3 5 7) 'prime)
      ((1 4 6 8 9) 'composite)))

(test 'c
    (case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else => (lambda (x) x))))

(test '((other . z) (semivowel . y) (other . x)
        (semivowel . w) (vowel . u))
    (map (lambda (x)
           (case x
             ((a e i o u) => (lambda (w) (cons 'vowel w)))
             ((w y) (cons 'semivowel x))
             (else => (lambda (w) (cons 'other w)))))
         '(z y x w u)))

(test #t (and (= 2 2) (> 2 1)))
(test #f (and (= 2 2) (< 2 1)))
(test '(f g) (and 1 2 'c '(f g)))
(test #t (and))

(test #t (or (= 2 2) (> 2 1)))
(test #t (or (= 2 2) (< 2 1)))
(test #f (or #f #f #f))
(test '(b c) (or (memq 'b '(a b c))
    (/ 3 0)))

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

;; By Jussi Piitulainen <jpiitula@ling.helsinki.fi>
;; and John Cowan <cowan@mercury.ccil.org>:
;; http://lists.scheme-reports.org/pipermail/scheme-reports/2013-December/003876.html
(define (means ton)
  (letrec*
     ((mean
        (lambda (f g)
          (f (/ (sum g ton) n))))
      (sum
        (lambda (g ton)
          (if (null? ton)
            (+)
            (if (number? ton)
                (g ton)
                (+ (sum g (car ton))
                   (sum g (cdr ton)))))))
      (n (sum (lambda (x) 1) ton)))
    (values (mean values values)
            (mean exp log)
            (mean / /))))
(let*-values (((a b c) (means '(8 5 99 1 22))))
  (test 27 a)
  (test 9.728 b)
  (test 1800/497 c))

(let*-values (((root rem) (exact-integer-sqrt 32)))
  (test 35 (* root rem)))

(test '(1073741824 0)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 60))))
      (list root rem)))

(test '(1518500249 3000631951)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 61))))
      (list root rem)))

(test '(815238614083298888 443242361398135744)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 119))))
      (list root rem)))

(test '(1152921504606846976 0)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 120))))
      (list root rem)))

(test '(1630477228166597776 1772969445592542976)
    (let*-values (((root rem) (exact-integer-sqrt (expt 2 121))))
      (list root rem)))

(test '(31622776601683793319 62545769258890964239)
    (let*-values (((root rem) (exact-integer-sqrt (expt 10 39))))
      (list root rem)))

(let*-values (((root rem) (exact-integer-sqrt (expt 2 140))))
  (test 0 rem)
  (test (expt 2 140) (square root)))

(test '(x y x y) (let ((a 'a) (b 'b) (x 'x) (y 'y))
  (let*-values (((a b) (values x y))
                ((x y) (values a b)))
    (list a b x y))))

(let ()
  (define x 0)
  (set! x 5)
  (test 6 (+ x 1)))

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

(test 2 (head (tail (tail integers))))

(define (stream-filter p? s)
  (delay-force
   (if (null? (force s)) 
       (delay '())
       (let ((h (car (force s)))
             (t (cdr (force s))))
         (if (p? h)
             (delay (cons h (stream-filter p? t)))
             (stream-filter p? t))))))

(test 5 (head (tail (tail (stream-filter odd? integers)))))

(let ()
  (define x 5)
  (define count 0)
  (define p
    (delay (begin (set! count (+ count 1))
                  (if (> count x)
                      count
                      (force p)))))
  (test 6 (force p))
  (test 6 (begin (set! x 10) (force p))))

(test #t (promise? (delay (+ 2 2))))
(test #t (promise? (make-promise (+ 2 2))))
(test #t
    (let ((x (delay (+ 2 2))))
      (force x)
      (promise? x)))
(test #t
    (let ((x (make-promise (+ 2 2))))
      (force x)
      (promise? x)))

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

(test '(list 3 4) `(list ,(+ 1 2) 4))
(let ((name 'a)) (test '(list a (quote a)) `(list ,name ',name)))
(test '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(test #(10 5 4 16 9 8)
    `#(10 5 ,(square 2) ,@(map square '(4 3)) 8))
(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
    `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) )
(let ((name1 'x)
      (name2 'y))
   (test '(a `(b ,x ,'y d) e) `(a `(b ,,name1 ,',name2 d) e)))
(test '(list 3 4) (quasiquote (list (unquote (+ 1 2)) 4)) )
(test `(list ,(+ 1 2) 4) (quasiquote (list (unquote (+ 1 2)) 4)))

(define plus
  (case-lambda 
   (() 0)
   ((x) x)
   ((x y) (+ x y))
   ((x y z) (+ (+ x y) z))
   (args (apply + args))))

(test 0 (plus))
(test 1 (plus 1))
(test 3 (plus 1 2))
(test 6 (plus 1 2 3))
(test 10 (plus 1 2 3 4))

(define mult
  (case-lambda 
   (() 1)
   ((x) x)
   ((x y) (* x y))
   ((x y . z) (apply mult (* x y) z))))

(test 1 (mult))
(test 1 (mult 1))
(test 2 (mult 1 2))
(test 6 (mult 1 2 3))
(test 24 (mult 1 2 3 4))

(test-end)

(test-begin "4.3 Macros")

(test 'now (let-syntax
               ((when (syntax-rules ()
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

(define-syntax be-like-begin1
  (syntax-rules ()
    ((be-like-begin1 name)
     (define-syntax name
       (syntax-rules ()
         ((name expr (... ...))
          (begin expr (... ...))))))))
(be-like-begin1 sequence1)
(test 3 (sequence1 0 1 2 3))

(define-syntax be-like-begin2
  (syntax-rules ()
    ((be-like-begin2 name)
     (define-syntax name
       (... (syntax-rules ()
              ((name expr ...)
               (begin expr ...))))))))
(be-like-begin2 sequence2)
(test 4 (sequence2 1 2 3 4))

(define-syntax be-like-begin3
  (syntax-rules ()
    ((be-like-begin3 name)
     (define-syntax name
       (syntax-rules dots ()
         ((name expr dots)
          (begin expr dots)))))))
(be-like-begin3 sequence3)
(test 5 (sequence3 2 3 4 5))

;; Syntax pattern with ellipsis in middle of proper list.
(define-syntax part-2
  (syntax-rules ()
    ((_ a b (m n) ... x y)
     (vector (list a b) (list m ...) (list n ...) (list x y)))
    ((_ . rest) 'error)))
(test '#((10 43) (31 41 51) (32 42 52) (63 77))
    (part-2 10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77))
;; Syntax pattern with ellipsis in middle of improper list.
(define-syntax part-2x
  (syntax-rules ()
    ((_ (a b (m n) ... x y . rest))
     (vector (list a b) (list m ...) (list n ...) (list x y)
             (cons "rest:" 'rest)))
    ((_ . rest) 'error)))
(test '#((10 43) (31 41 51) (32 42 52) (63 77) ("rest:"))
    (part-2x (10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77)))
(test '#((10 43) (31 41 51) (32 42 52) (63 77) ("rest:" . "tail"))
    (part-2x (10 (+ 21 22) (31 32) (41 42) (51 52) (+ 61 2) 77 . "tail")))

;; underscore
(define-syntax count-to-2
  (syntax-rules ()
    ((_) 0)
    ((_ _) 1)
    ((_ _ _) 2)
    ((_ . _) 'many)))
(test '(2 0 many)
    (list (count-to-2 a b) (count-to-2) (count-to-2 a b c d)))

(define-syntax count-to-2_
  (syntax-rules (_)
    ((_) 0)
    ((_ _) 1)
    ((_ _ _) 2)
    ((x . y) 'fail)))
(test '(2 0 fail fail)
    (list (count-to-2_ _ _) (count-to-2_)
          (count-to-2_ a b) (count-to-2_ a b c d)))

(define-syntax jabberwocky
  (syntax-rules ()
    ((_ hatter)
     (begin
       (define march-hare 42)
       (define-syntax hatter
         (syntax-rules ()
           ((_) march-hare)))))))
(jabberwocky mad-hatter)
(test 42 (mad-hatter))

(test 'ok (let ((=> #f)) (cond (#t => 'ok))))

(let ()
  (define x 1)
  (let-syntax ()
    (define x 2)
    #f)
  (test 1 x))

(let ()
 (define-syntax foo
   (syntax-rules ()
     ((foo bar y)
      (define-syntax bar
        (syntax-rules ()
          ((bar x) 'y))))))
 (foo bar x)
 (test 'x (bar 1)))

(begin
  (define-syntax ffoo
    (syntax-rules ()
      ((ffoo ff)
       (begin
         (define (ff x)
           (gg x))
         (define (gg x)
           (* x x))))))
  (ffoo ff)
  (test 100 (ff 10)))

(test-end)

(test-begin "5 Program structure")

(define add3
  (lambda (x) (+ x 3)))
(test 6 (add3 3))
(define first car)
(test 1 (first '(1 2)))

(test 45 (let ((x 5))
  (define foo (lambda (y) (bar x y)))
  (define bar (lambda (a b) (+ (* a b) a)))
  (foo (+ x 3))))

(test 'ok
    (let ()
      (define-values () (values))
      'ok))
(test 1
    (let ()
      (define-values (x) (values 1))
      x))
(test 3
    (let ()
      (define-values x (values 1 2))
      (apply + x)))
(test 3
    (let ()
      (define-values (x y) (values 1 2))
      (+ x y)))
(test 6
    (let ()
      (define-values (x y z) (values 1 2 3))
      (+ x y z)))
(test 10
    (let ()
      (define-values (x y . z) (values 1 2 3 4))
      (+ x y (car z) (cadr z))))

(test '(2 1) (let ((x 1) (y 2))
  (define-syntax swap!
    (syntax-rules ()
      ((swap! a b)
       (let ((tmp a))
         (set! a b)
         (set! b tmp)))))
  (swap! x y)
  (list x y)))

;; Records

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

(test-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6 Standard Procedures

(test-begin "6.1 Equivalence Predicates")

(test #t (eqv? 'a 'a))
(test #f (eqv? 'a 'b))
(test #t (eqv? 2 2))
(test #t (eqv? '() '()))
(test #t (eqv? 100000000 100000000))
(test #f (eqv? (cons 1 2) (cons 1 2)))
(test #f (eqv? (lambda () 1)
               (lambda () 2)))
(test #f (eqv? #f 'nil))

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(test #t
    (let ((g (gen-counter)))
      (eqv? g g)))
(test #f (eqv? (gen-counter) (gen-counter)))
(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) 27))))
(test #t (let ((g (gen-loser)))
  (eqv? g g)))

(test #f
(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
         (g (lambda () (if (eqv? f g) 'g 'both))))
   (eqv? f g)))

(test #t
    (let ((x '(a)))
      (eqv? x x)))

(test #t (eq? 'a 'a))
(test #f (eq? (list 'a) (list 'a)))
(test #t (eq? '() '()))
(test #t
    (let ((x '(a)))
      (eq? x x)))
(test #t
    (let ((x '#()))
      (eq? x x)))
(test #t
    (let ((p (lambda (x) x)))
      (eq? p p)))

(test #t (equal? 'a 'a))
(test #t (equal? '(a) '(a)))
(test #t (equal? '(a (b) c)
                 '(a (b) c)))
(test #t (equal? "abc" "abc"))
(test #t (equal? 2 2))
(test #t (equal? (make-vector 5 'a)
                 (make-vector 5 'a)))

(test-end)

(test-begin "6.2 Numbers")

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
(test #f (finite? 3.0+inf.0i))

(test #f (infinite? 3))
(test #t (infinite? +inf.0))
(test #f (infinite? +nan.0))
(test #t (infinite? 3.0+inf.0i))

(test #t (nan? +nan.0))
(test #f (nan? 32))
;; (test #t (nan? +nan.0+5.0i))
(test #f (nan? 1+2i))

(test #t (= 1 1.0 1.0+0.0i))
(test #f (= 1.0 1.0+1.0i))
(test #t (< 1 2 3))
(test #f (< 1 1 2))
(test #t (> 3.0 2.0 1.0))
(test #f (> -3.0 2.0 1.0))
(test #t (<= 1 1 2))
(test #f (<= 1 2 1))
(test #t (>= 2 1 1))
(test #f (>= 1 2 1))
(test '(#t #f) (list (<= 1 1 2) (<= 2 1 3)))

;; From R7RS 6.2.6 Numerical operations:
;;
;; These predicates are required to be transitive.
;;
;; _Note:_ The traditional implementations of these predicates in
;; Lisp-like languages, which involve converting all arguments to inexact
;; numbers if any argument is inexact, are not transitive.

;; Example from Alan Bawden
(let ((a (- (expt 2 1000) 1))
      (b (inexact (expt 2 1000))) ; assuming > single-float-epsilon
      (c (+ (expt 2 1000) 1)))
  (test #t (if (and (= a b) (= b c))
               (= a c)
               #t)))

;; From CLtL 12.3. Comparisons on Numbers:
;;
;;  Let _a_ be the result of (/ 10.0 single-float-epsilon), and let
;;  _j_ be the result of (floor a). ..., all of (<= a j), (< j (+ j
;;  1)), and (<= (+ j 1) a) would be true; transitivity would then
;;  imply that (< a a) ought to be true ...

;; Transliteration from Jussi Piitulainen
(define single-float-epsilon
  (do ((eps 1.0 (* eps 2.0)))
      ((= eps (+ eps 1.0)) eps)))

(let* ((a (/ 10.0 single-float-epsilon))
       (j (exact a)))
  (test #t (if (and (<= a j) (< j (+ j 1)))
               (not (<= (+ j 1) a))
               #t)))

(test #t (zero? 0))
(test #t (zero? 0.0))
(test #t (zero? 0.0+0.0i))
(test #f (zero? 1))
(test #f (zero? -1))

(test #f (positive? 0))
(test #f (positive? 0.0))
(test #t (positive? 1))
(test #t (positive? 1.0))
(test #f (positive? -1))
(test #f (positive? -1.0))
(test #t (positive? +inf.0))
(test #f (positive? -inf.0))

(test #f (negative? 0))
(test #f (negative? 0.0))
(test #f (negative? 1))
(test #f (negative? 1.0))
(test #t (negative? -1))
(test #t (negative? -1.0))
(test #f (negative? +inf.0))
(test #t (negative? -inf.0))

(test #f (odd? 0))
(test #t (odd? 1))
(test #t (odd? -1))
(test #f (odd? 102))

(test #t (even? 0))
(test #f (even? 1))
(test #t (even? -2))
(test #t (even? 102))

(test 3 (max 3))
(test 4 (max 3 4))
(test 4.0 (max 3.9 4))
(test 5.0 (max 5 3.9 4))
(test +inf.0 (max 100 +inf.0))
(test 3 (min 3))
(test 3 (min 3 4))
(test 3.0 (min 3 3.1))
(test -inf.0 (min -inf.0 -100))

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
(test 7 (abs 7))

(test-values (values 2 1) (floor/ 5 2))
(test-values (values -3 1) (floor/ -5 2))
(test-values (values -3 -1) (floor/ 5 -2))
(test-values (values 2 -1) (floor/ -5 -2))
(test-values (values 2 1) (truncate/ 5 2))
(test-values (values -2 -1) (truncate/ -5 2))
(test-values (values -2 1) (truncate/ 5 -2))
(test-values (values 2 -1) (truncate/ -5 -2))
(test-values (values 2.0 -1.0) (truncate/ -5.0 -2))

(test 1 (modulo 13 4))
(test 1 (remainder 13 4))

(test 3 (modulo -13 4))
(test -1 (remainder -13 4))

(test -3 (modulo 13 -4))
(test 1 (remainder 13 -4))

(test -1 (modulo -13 -4))
(test -1 (remainder -13 -4))

(test -1.0 (remainder -13 -4.0))

(test 4 (gcd 32 -36))
(test 0 (gcd))
(test 288 (lcm 32 -36))
(test 288.0 (lcm 32.0 -36))
(test 1 (lcm))

(test 3 (numerator (/ 6 4)))
(test 2 (denominator (/ 6 4)))
(test 2.0 (denominator (inexact (/ 6 4))))
(test 11.0 (numerator 5.5))
(test 2.0 (denominator 5.5))
(test 5.0 (numerator 5.0))
(test 1.0 (denominator 5.0))

(test -5.0 (floor -4.3))
(test -4.0 (ceiling -4.3))
(test -4.0 (truncate -4.3))
(test -4.0 (round -4.3))

(test 3.0 (floor 3.5))
(test 4.0 (ceiling 3.5))
(test 3.0 (truncate 3.5))
(test 4.0 (round 3.5))

(test 4 (round 7/2))
(test 7 (round 7))

(test 1/3 (rationalize (exact .3) 1/10))
(test #i1/3 (rationalize .3 1/10))

(test 1.0 (inexact (exp 0))) ;; may return exact number
(test 20.0855369231877 (exp 3))

(test 0.0 (inexact (log 1))) ;; may return exact number
(test 1.0 (log (exp 1)))
(test 42.0 (log (exp 42)))
(test 2.0 (log 100 10))
(test 12.0 (log 4096 2))

(test 0.0 (inexact (sin 0))) ;; may return exact number
(test 1.0 (sin 1.5707963267949))
(test 1.0 (inexact (cos 0))) ;; may return exact number
(test -1.0 (cos 3.14159265358979))
(test 0.0 (inexact (tan 0))) ;; may return exact number
(test 1.5574077246549 (tan 1))

(test 0.0 (inexact (asin 0))) ;; may return exact number
(test 1.5707963267949 (asin 1))
(test 0.0 (inexact (acos 1))) ;; may return exact number
(test 3.14159265358979 (acos -1))

;; (test 0.0-0.0i (asin 0+0.0i))
;; (test 1.5707963267948966+0.0i (acos 0+0.0i))

(test 0.0 (atan 0.0 1.0))
(test -0.0 (atan -0.0 1.0))
(test 0.785398163397448 (atan 1.0 1.0))
(test 1.5707963267949 (atan 1.0 0.0))
(test 2.35619449019234 (atan 1.0 -1.0))
(test 3.14159265358979 (atan 0.0 -1.0))
(test -3.14159265358979 (atan -0.0 -1.0)) ;
(test -2.35619449019234 (atan -1.0 -1.0))
(test -1.5707963267949 (atan -1.0 0.0))
(test -0.785398163397448 (atan -1.0 1.0))
;; (test undefined (atan 0.0 0.0))

(test 1764 (square 42))
(test 4 (square 2))

(test 3.0 (inexact (sqrt 9)))
(test 1.4142135623731 (sqrt 2))
(test 0.0+1.0i (inexact (sqrt -1)))

(test '(2 0) (call-with-values (lambda () (exact-integer-sqrt 4)) list))
(test '(2 1) (call-with-values (lambda () (exact-integer-sqrt 5)) list))

(test 27 (expt 3 3))
(test 1 (expt 0 0))
(test 0 (expt 0 1))
(test 1.0 (expt 0.0 0))
(test 0.0 (expt 0 1.0))

(test 1+2i (make-rectangular 1 2))

(test 0.54030230586814+0.841470984807897i (make-polar 1 1))

(test 1 (real-part 1+2i))

(test 2 (imag-part 1+2i))

(test 2.23606797749979 (magnitude 1+2i))

(test 1.10714871779409 (angle 1+2i))

(test 1.0 (inexact 1))
(test #t (inexact? (inexact 1)))
(test 1 (exact 1.0))
(test #t (exact? (exact 1.0)))

(test 100 (string->number "100"))
(test 256 (string->number "100" 16))
(test 100.0 (string->number "1e2"))

(test-end)

(test-begin "6.3 Booleans")

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

(test #t (boolean=? #t #t))
(test #t (boolean=? #f #f))
(test #f (boolean=? #t #f))
(test #t (boolean=? #f #f #f))
(test #f (boolean=? #t #t #f))

(test-end)

(test-begin "6.4 Lists")

(let* ((x (list 'a 'b 'c))
       (y x))
  (test '(a b c) (values y))
  (test #t (list? y))
  (set-cdr! x 4)
  (test '(a . 4) (values x))
  (test #t (eqv? x y))
  (test #f (list? y))
  (set-cdr! x x)
  (test #f (list? x)))

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

(test '(b c d) (cdr '((a) b c d)))
(test 2 (cdr '(1 . 2)))
(define (g) '(constant-list))

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

(test '(d e) (list-tail '(a b c d e) 3))

(test 'c (list-ref '(a b c d) 2))
(test 'c (list-ref '(a b c d)
          (exact (round 1.8))))

(test '(0 ("Sue" "Sue") "Anna")
    (let ((lst (list 0 '(2 2 2 2) "Anna")))
      (list-set! lst 1 '("Sue" "Sue"))
      lst))

(test '(a b c) (memq 'a '(a b c)))
(test '(b c) (memq 'b '(a b c)))
(test #f (memq 'a '(b c d)))
(test #f (memq (list 'a) '(b (a) c)))
(test '((a) c) (member (list 'a) '(b (a) c)))
(test '("b" "c") (member "B" '("a" "b" "c") string-ci=?))
(test '(101 102) (memv 101 '(100 101 102)))

(let ()
  (define e '((a 1) (b 2) (c 3)))
  (test '(a 1) (assq 'a e))
  (test '(b 2) (assq 'b e))
  (test #f (assq 'd e)))

(test #f (assq (list 'a) '(((a)) ((b)) ((c)))))
(test '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
(test '(2 4) (assoc 2.0 '((1 1) (2 4) (3 9)) =))
(test '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

(test '(1 2 3) (list-copy '(1 2 3)))
(test "foo" (list-copy "foo"))
(test '() (list-copy '()))
(test '(3 . 4) (list-copy '(3 . 4)))
(test '(6 7 8 . 9) (list-copy '(6 7 8 . 9)))
(let* ((l1 '((a b) (c d) e))
       (l2 (list-copy l1)))
  (test l2 '((a b) (c d) e))
  (test #t (eq? (car l1) (car l2)))
  (test #t (eq? (cadr l1) (cadr l2)))
  (test #f (eq? (cdr l1) (cdr l2)))
  (test #f (eq? (cddr l1) (cddr l2))))

(test-end)

(test-begin "6.5 Symbols")

(test #t (symbol? 'foo))
(test #t (symbol? (car '(a b))))
(test #f (symbol? "bar"))
(test #t (symbol? 'nil))
(test #f (symbol? '()))
(test #f (symbol? #f))

(test #t (symbol=? 'a 'a))
(test #f (symbol=? 'a 'A))
(test #t (symbol=? 'a 'a 'a))
(test #f (symbol=? 'a 'a 'A))

(test "flying-fish"     
(symbol->string 'flying-fish))
(test "Martin" (symbol->string 'Martin))
(test "Malvina" (symbol->string (string->symbol "Malvina")))

(test 'mISSISSIppi (string->symbol "mISSISSIppi"))
(test #t (eq? 'bitBlt (string->symbol "bitBlt")))
(test #t (eq? 'LollyPop (string->symbol (symbol->string 'LollyPop))))
(test #t (string=? "K. Harper, M.D."
                   (symbol->string (string->symbol "K. Harper, M.D."))))

(test-end)

(test-begin "6.6 Characters")

(test #t (char? #\a))
(test #f (char? "a"))
(test #f (char? 'a))
(test #f (char? 0))

(test #t (char=? #\a #\a #\a))
(test #f (char=? #\a #\A))
(test #t (char<? #\a #\b #\c))
(test #f (char<? #\a #\a))
(test #f (char<? #\b #\a))
(test #f (char>? #\a #\b))
(test #f (char>? #\a #\a))
(test #t (char>? #\c #\b #\a))
(test #t (char<=? #\a #\b #\b))
(test #t (char<=? #\a #\a))
(test #f (char<=? #\b #\a))
(test #f (char>=? #\a #\b))
(test #t (char>=? #\a #\a))
(test #t (char>=? #\b #\b #\a))

(test #t (char-ci=? #\a #\a))
(test #t (char-ci=? #\a #\A #\a))
(test #f (char-ci=? #\a #\b))
(test #t (char-ci<? #\a #\B #\c))
(test #f (char-ci<? #\A #\a))
(test #f (char-ci<? #\b #\A))
(test #f (char-ci>? #\A #\b))
(test #f (char-ci>? #\a #\A))
(test #t (char-ci>? #\c #\B #\a))
(test #t (char-ci<=? #\a #\B #\b))
(test #t (char-ci<=? #\A #\a))
(test #f (char-ci<=? #\b #\A))
(test #f (char-ci>=? #\A #\b))
(test #t (char-ci>=? #\a #\A))
(test #t (char-ci>=? #\b #\B #\a))

(test #t (char-alphabetic? #\a))
(test #f (char-alphabetic? #\space))
(test #t (char-numeric? #\0))
(test #f (char-numeric? #\.))
(test #f (char-numeric? #\a))
(test #t (char-whitespace? #\space))
(test #t (char-whitespace? #\tab))
(test #t (char-whitespace? #\newline))
(test #f (char-whitespace? #\_))
(test #f (char-whitespace? #\a))
(test #t (char-upper-case? #\A))
(test #f (char-upper-case? #\a))
(test #f (char-upper-case? #\3))
(test #t (char-lower-case? #\a))
(test #f (char-lower-case? #\A))
(test #f (char-lower-case? #\3))

(test #t (char-alphabetic? #\Λ))
(test #f (char-alphabetic? #\x0E50))
(test #t (char-upper-case? #\Λ))
(test #f (char-upper-case? #\λ))
(test #f (char-lower-case? #\Λ))
(test #t (char-lower-case? #\λ))
(test #f (char-numeric? #\Λ))
(test #t (char-numeric? #\x0E50))
(test #t (char-whitespace? #\x1680))

(test 0 (digit-value #\0))
(test 3 (digit-value #\3))
(test 9 (digit-value #\9))
(test 4 (digit-value #\x0664))
(test 0 (digit-value #\x0AE6))
(test #f (digit-value #\.))
(test #f (digit-value #\-))

(test 97 (char->integer #\a))
(test #\a (integer->char 97))

(test #\A (char-upcase #\a))
(test #\A (char-upcase #\A))
(test #\a (char-downcase #\a))
(test #\a (char-downcase #\A))
(test #\a (char-foldcase #\a))
(test #\a (char-foldcase #\A))

(test #\Λ (char-upcase #\λ))
(test #\Λ (char-upcase #\Λ))
(test #\λ (char-downcase #\λ))
(test #\λ (char-downcase #\Λ))
(test #\λ (char-foldcase #\λ))
(test #\λ (char-foldcase #\Λ))

(test-end)

(test-begin "6.7 Strings")

(test #t (string? ""))
(test #t (string? " "))
(test #f (string? 'a))
(test #f (string? #\a))

(test 3 (string-length (make-string 3)))
(test "---" (make-string 3 #\-))

(test "" (string))
(test "---" (string #\- #\- #\-))
(test "kitten" (string #\k #\i #\t #\t #\e #\n))

(test 0 (string-length ""))
(test 1 (string-length "a"))
(test 3 (string-length "abc"))

(test #\a (string-ref "abc" 0))
(test #\b (string-ref "abc" 1))
(test #\c (string-ref "abc" 2))

(test "a-c" (let ((str (string #\a #\b #\c))) (string-set! str 1 #\-) str))

(test (string #\a #\x1F700 #\c)
    (let ((s (string #\a #\b #\c)))
      (string-set! s 1 #\x1F700)
      s))

(test #t (string=? "" ""))
(test #t (string=? "abc" "abc" "abc"))
(test #f (string=? "" "abc"))
(test #f (string=? "abc" "aBc"))

(test #f (string<? "" ""))
(test #f (string<? "abc" "abc"))
(test #t (string<? "abc" "abcd" "acd"))
(test #f (string<? "abcd" "abc"))
(test #t (string<? "abc" "bbc"))

(test #f (string>? "" ""))
(test #f (string>? "abc" "abc"))
(test #f (string>? "abc" "abcd"))
(test #t (string>? "acd" "abcd" "abc"))
(test #f (string>? "abc" "bbc"))

(test #t (string<=? "" ""))
(test #t (string<=? "abc" "abc"))
(test #t (string<=? "abc" "abcd" "abcd"))
(test #f (string<=? "abcd" "abc"))
(test #t (string<=? "abc" "bbc"))

(test #t (string>=? "" ""))
(test #t (string>=? "abc" "abc"))
(test #f (string>=? "abc" "abcd"))
(test #t (string>=? "abcd" "abcd" "abc"))
(test #f (string>=? "abc" "bbc"))

(test #t (string-ci=? "" ""))
(test #t (string-ci=? "abc" "abc"))
(test #f (string-ci=? "" "abc"))
(test #t (string-ci=? "abc" "aBc"))
(test #f (string-ci=? "abc" "aBcD"))

(test #f (string-ci<? "abc" "aBc"))
(test #t (string-ci<? "abc" "aBcD"))
(test #f (string-ci<? "ABCd" "aBc"))

(test #f (string-ci>? "abc" "aBc"))
(test #f (string-ci>? "abc" "aBcD"))
(test #t (string-ci>? "ABCd" "aBc"))

(test #t (string-ci<=? "abc" "aBc"))
(test #t (string-ci<=? "abc" "aBcD"))
(test #f (string-ci<=? "ABCd" "aBc"))

(test #t (string-ci>=? "abc" "aBc"))
(test #f (string-ci>=? "abc" "aBcD"))
(test #t (string-ci>=? "ABCd" "aBc"))

(test #t (string-ci=? "ΑΒΓ" "αβγ" "αβγ"))
(test #f (string-ci<? "ΑΒΓ" "αβγ"))
(test #f (string-ci>? "ΑΒΓ" "αβγ"))
(test #t (string-ci<=? "ΑΒΓ" "αβγ"))
(test #t (string-ci>=? "ΑΒΓ" "αβγ"))

;; latin
(test "ABC" (string-upcase "abc"))
(test "ABC" (string-upcase "ABC"))
(test "abc" (string-downcase "abc"))
(test "abc" (string-downcase "ABC"))
(test "abc" (string-foldcase "abc"))
(test "abc" (string-foldcase "ABC"))

;; cyrillic
(test "ΑΒΓ" (string-upcase "αβγ"))
(test "ΑΒΓ" (string-upcase "ΑΒΓ"))
(test "αβγ" (string-downcase "αβγ"))
(test "αβγ" (string-downcase "ΑΒΓ"))
(test "αβγ" (string-foldcase "αβγ"))
(test "αβγ" (string-foldcase "ΑΒΓ"))

;; special cases
(test "SSA" (string-upcase "ßa"))
(test "ßa" (string-downcase "ßa"))
(test "ssa" (string-downcase "SSA"))
(test "İ" (string-upcase "İ"))
(test "i\x0307;" (string-downcase "İ"))
(test "i\x0307;" (string-foldcase "İ"))
(test "J̌" (string-upcase "ǰ"))

;; context-sensitive (final sigma)
(test "ΓΛΏΣΣΑ" (string-upcase "γλώσσα"))
(test "γλώσσα" (string-downcase "ΓΛΏΣΣΑ"))
(test "γλώσσα" (string-foldcase "ΓΛΏΣΣΑ"))
(test "ΜΈΛΟΣ" (string-upcase "μέλος"))
(test #t (and (member (string-downcase "ΜΈΛΟΣ") '("μέλος" "μέλοσ")) #t))
(test "μέλοσ" (string-foldcase "ΜΈΛΟΣ"))
(test #t (and (member (string-downcase "ΜΈΛΟΣ ΕΝΌΣ")
                      '("μέλος ενός" "μέλοσ ενόσ"))
              #t))

(test "" (substring "" 0 0))
(test "" (substring "a" 0 0))
(test "" (substring "abc" 1 1))
(test "ab" (substring "abc" 0 2))
(test "bc" (substring "abc" 1 3))

(test "" (string-append ""))
(test "" (string-append "" ""))
(test "abc" (string-append "" "abc"))
(test "abc" (string-append "abc" ""))
(test "abcde" (string-append "abc" "de"))
(test "abcdef" (string-append "abc" "de" "f"))

(test '() (string->list ""))
(test '(#\a) (string->list "a"))
(test '(#\a #\b #\c) (string->list "abc"))
(test '(#\a #\b #\c) (string->list "abc" 0))
(test '(#\b #\c) (string->list "abc" 1))
(test '(#\b #\c) (string->list "abc" 1 3))

(test "" (list->string '()))
(test "abc" (list->string '(#\a #\b #\c)))

(test "" (string-copy ""))
(test "" (string-copy "" 0))
(test "" (string-copy "" 0 0))
(test "abc" (string-copy "abc"))
(test "abc" (string-copy "abc" 0))
(test "bc" (string-copy "abc" 1))
(test "b" (string-copy "abc" 1 2))
(test "bc" (string-copy "abc" 1 3))

(test "-----"
    (let ((str (make-string 5 #\x))) (string-fill! str #\-) str))
(test "xx---"
    (let ((str (make-string 5 #\x))) (string-fill! str #\- 2) str))
(test "xx-xx"
    (let ((str (make-string 5 #\x))) (string-fill! str #\- 2 3) str))

(test "a12de"
    (let ((str (string-copy "abcde"))) (string-copy! str 1 "12345" 0 2) str))
(test "-----"
    (let ((str (make-string 5 #\x))) (string-copy! str 0 "-----") str))
(test "---xx"
    (let ((str (make-string 5 #\x))) (string-copy! str 0 "-----" 2) str))
(test "xx---"
    (let ((str (make-string 5 #\x))) (string-copy! str 2 "-----" 0 3) str))
(test "xx-xx"
    (let ((str (make-string 5 #\x))) (string-copy! str 2 "-----" 2 3) str))

;; same source and dest
(test "aabde"
    (let ((str (string-copy "abcde"))) (string-copy! str 1 str 0 2) str))
(test "abcab"
    (let ((str (string-copy "abcde"))) (string-copy! str 3 str 0 2) str))

(test-end)

(test-begin "6.8 Vectors")

(test #t (vector? #()))
(test #t (vector? #(1 2 3)))
(test #t (vector? '#(1 2 3)))

(test 0 (vector-length (make-vector 0)))
(test 1000 (vector-length (make-vector 1000)))

(test #(0 (2 2 2 2) "Anna") '#(0 (2 2 2 2) "Anna"))

(test #(a b c) (vector 'a 'b 'c))

(test 8 (vector-ref '#(1 1 2 3 5 8 13 21) 5))
(test 13 (vector-ref '#(1 1 2 3 5 8 13 21)
            (let ((i (round (* 2 (acos -1)))))
              (if (inexact? i)
                  (exact i)
                  i))))

(test #(0 ("Sue" "Sue") "Anna") (let ((vec (vector 0 '(2 2 2 2) "Anna")))
  (vector-set! vec 1 '("Sue" "Sue"))
  vec))

(test '(dah dah didah) (vector->list '#(dah dah didah)))
(test '(dah didah) (vector->list '#(dah dah didah) 1))
(test '(dah) (vector->list '#(dah dah didah) 1 2))
(test #(dididit dah) (list->vector '(dididit dah)))

(test #() (string->vector ""))
(test #(#\A #\B #\C) (string->vector "ABC"))
(test #(#\B #\C) (string->vector "ABC" 1))
(test #(#\B) (string->vector "ABC" 1 2))

(test "" (vector->string #()))
(test "123" (vector->string #(#\1 #\2 #\3)))
(test "23" (vector->string #(#\1 #\2 #\3) 1))
(test "2" (vector->string #(#\1 #\2 #\3) 1 2))

(test #() (vector-copy #()))
(test #(a b c) (vector-copy #(a b c)))
(test #(b c) (vector-copy #(a b c) 1))
(test #(b) (vector-copy #(a b c) 1 2))

(test #() (vector-append #()))
(test #() (vector-append #() #()))
(test #(a b c) (vector-append #() #(a b c)))
(test #(a b c) (vector-append #(a b c) #()))
(test #(a b c d e) (vector-append #(a b c) #(d e)))
(test #(a b c d e f) (vector-append #(a b c) #(d e) #(f)))

(test #(1 2 smash smash 5)
    (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'smash 2 4) vec))
(test #(x x x x x)
    (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'x) vec))
(test #(1 2 x x x)
    (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'x 2) vec))
(test #(1 2 x 4 5)
    (let ((vec (vector 1 2 3 4 5))) (vector-fill! vec 'x 2 3) vec))

(test #(1 a b 4 5)
    (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 1 #(a b c d e) 0 2) vec))
(test #(a b c d e)
    (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 0 #(a b c d e)) vec))
(test #(c d e 4 5)
    (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 0 #(a b c d e) 2) vec))
(test #(1 2 a b c)
    (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 2 #(a b c d e) 0 3) vec))
(test #(1 2 c 4 5)
    (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 2 #(a b c d e) 2 3) vec))

;; same source and dest
(test #(1 1 2 4 5)
    (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 1 vec 0 2) vec))
(test #(1 2 3 1 2)
    (let ((vec (vector 1 2 3 4 5))) (vector-copy! vec 3 vec 0 2) vec))

(test-end)

(test-begin "6.9 Bytevectors")

(test #t (bytevector? #u8()))
(test #t (bytevector? #u8(0 1 2)))
(test #f (bytevector? #()))
(test #f (bytevector? #(0 1 2)))
(test #f (bytevector? '()))
(test #t (bytevector? (make-bytevector 0)))

(test 0 (bytevector-length (make-bytevector 0)))
(test 1024 (bytevector-length (make-bytevector 1024)))
(test 1024 (bytevector-length (make-bytevector 1024 255)))

(test 3 (bytevector-length (bytevector 0 1 2)))

(test 0 (bytevector-u8-ref (bytevector 0 1 2) 0))
(test 1 (bytevector-u8-ref (bytevector 0 1 2) 1))
(test 2 (bytevector-u8-ref (bytevector 0 1 2) 2))

(test #u8(0 255 2)
    (let ((bv (bytevector 0 1 2))) (bytevector-u8-set! bv 1 255) bv))

(test #u8() (bytevector-copy #u8()))
(test #u8(0 1 2) (bytevector-copy #u8(0 1 2)))
(test #u8(1 2) (bytevector-copy #u8(0 1 2) 1))
(test #u8(1) (bytevector-copy #u8(0 1 2) 1 2))

(test #u8(1 6 7 4 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 1 #u8(6 7 8 9 10) 0 2)
      bv))
(test #u8(6 7 8 9 10)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 0 #u8(6 7 8 9 10))
      bv))
(test #u8(8 9 10 4 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 0 #u8(6 7 8 9 10) 2)
      bv))
(test #u8(1 2 6 7 8)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 2 #u8(6 7 8 9 10) 0 3)
      bv))
(test #u8(1 2 8 4 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 2 #u8(6 7 8 9 10) 2 3)
      bv))

;; same source and dest
(test #u8(1 1 2 4 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 1 bv 0 2)
      bv))
(test #u8(1 2 3 1 2)
    (let ((bv (bytevector 1 2 3 4 5)))
      (bytevector-copy! bv 3 bv 0 2)
      bv))

(test #u8() (bytevector-append #u8()))
(test #u8() (bytevector-append #u8() #u8()))
(test #u8(0 1 2) (bytevector-append #u8() #u8(0 1 2)))
(test #u8(0 1 2) (bytevector-append #u8(0 1 2) #u8()))
(test #u8(0 1 2 3 4) (bytevector-append #u8(0 1 2) #u8(3 4)))
(test #u8(0 1 2 3 4 5) (bytevector-append #u8(0 1 2) #u8(3 4) #u8(5)))

(test "ABC" (utf8->string #u8(#x41 #x42 #x43)))
(test "ABC" (utf8->string #u8(0 #x41 #x42 #x43) 1))
(test "ABC" (utf8->string #u8(0 #x41  #x42 #x43 0) 1 4))
(test "λ" (utf8->string #u8(0 #xCE #xBB 0) 1 3))
(test #u8(#x41 #x42 #x43) (string->utf8 "ABC"))
(test #u8(#x42 #x43) (string->utf8 "ABC" 1))
(test #u8(#x42) (string->utf8 "ABC" 1 2))
(test #u8(#xCE #xBB) (string->utf8 "λ"))

(test-end)

(test-begin "6.10 Control Features")

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
(test '(30 0)
    (call-with-values (lambda () ((compose exact-integer-sqrt *) 12 75))
      list))

(test '(b e h) (map cadr '((a b) (d e) (g h))))

(test '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))

(test '(5 7 9) (map + '(1 2 3) '(4 5 6 7)))

(test #t
    (let ((res (let ((count 0))
                 (map (lambda (ignored)
                        (set! count (+ count 1))
                        count)
                      '(a b)))))
      (or (equal? res '(1 2))
          (equal? res '(2 1)))))

(test '(10 200 3000 40 500 6000)
    (let ((ls1 (list 10 100 1000))
          (ls2 (list 1 2 3 4 5 6)))
      (set-cdr! (cddr ls1) ls1)
      (map * ls1 ls2)))

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

(test #(1 4 27 256 3125)
    (vector-map (lambda (n) (expt n n))
                '#(1 2 3 4 5)))

(test #(5 7 9) (vector-map + '#(1 2 3) '#(4 5 6 7)))

(test #t
    (let ((res (let ((count 0))
                 (vector-map
                  (lambda (ignored)
                    (set! count (+ count 1))
                    count)
                  '#(a b)))))
      (or (equal? res #(1 2))
          (equal? res #(2 1)))))

(test #(0 1 4 9 16)
    (let ((v (make-vector 5)))
      (for-each (lambda (i)
                  (vector-set! v i (* i i)))
                '(0 1 2 3 4))
      v))

(test 9750
    (let ((ls1 (list 10 100 1000))
          (ls2 (list 1 2 3 4 5 6))
          (count 0))
      (set-cdr! (cddr ls1) ls1)
      (for-each (lambda (x y) (set! count (+ count (* x y)))) ls2 ls1)
      count))

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

(test-end)

(test-begin "6.11 Exceptions")

(test 65
    (with-exception-handler
     (lambda (con) 42)
     (lambda ()
       (+ (raise-continuable "should be a number")
          23))))

(test #t
    (error-object? (guard (exn (else exn)) (error "BOOM!" 1 2 3))))
(test "BOOM!"
    (error-object-message (guard (exn (else exn)) (error "BOOM!" 1 2 3))))
(test '(1 2 3)
    (error-object-irritants (guard (exn (else exn)) (error "BOOM!" 1 2 3))))

(test #f
    (file-error? (guard (exn (else exn)) (error "BOOM!"))))
(test #t
    (file-error? (guard (exn (else exn)) (open-input-file " no such file "))))

(test #f
    (read-error? (guard (exn (else exn)) (error "BOOM!"))))
(test #t
    (read-error? (guard (exn (else exn)) (read (open-input-string ")")))))

(define something-went-wrong #f)
(define (test-exception-handler-1 v)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (x)
        (set! something-went-wrong (list "condition: " x))
        (k 'exception))
      (lambda ()
        (+ 1 (if (> v 0) (+ v 100) (raise 'an-error))))))))
(test 106 (test-exception-handler-1 5))
(test #f something-went-wrong)
(test 'exception (test-exception-handler-1 -1))
(test '("condition: " an-error) something-went-wrong)

(set! something-went-wrong #f)
(define (test-exception-handler-2 v)
  (guard (ex (else 'caught-another-exception))
    (with-exception-handler
     (lambda (x)
       (set! something-went-wrong #t)
       (list "exception:" x))
     (lambda ()
       (+ 1 (if (> v 0) (+ v 100) (raise 'an-error)))))))
(test 106 (test-exception-handler-2 5))
(test #f something-went-wrong)
(test 'caught-another-exception (test-exception-handler-2 -1))
(test #t something-went-wrong)

;; Based on an example from R6RS-lib section 7.1 Exceptions.
;; R7RS section 6.11 Exceptions has a simplified version.
(let* ((out (open-output-string))
       (value (with-exception-handler
               (lambda (con)
                 (cond
                  ((not (list? con))
                   (raise con))
                  ((list? con)
                   (display (car con) out))
                  (else
                   (display "a warning has been issued" out)))
                 42)
               (lambda ()
                 (+ (raise-continuable
                     (list "should be a number"))
                    23)))))
  (test "should be a number" (get-output-string out))
  (test 65 value))

;; From SRFI-34 "Examples" section - #3
(define (test-exception-handler-3 v out)
  (guard (condition
          (else
           (display "condition: " out)
           (write condition out)
           (display #\! out)
           'exception))
         (+ 1 (if (= v 0) (raise 'an-error) (/ 10 v)))))
(let* ((out (open-output-string))
       (value (test-exception-handler-3 0 out)))
  (test 'exception value)
  (test "condition: an-error!" (get-output-string out)))

(define (test-exception-handler-4 v out)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (x)
        (display "reraised " out)
        (write x out) (display #\! out)
        (k 'zero))
      (lambda ()
        (guard (condition
                ((positive? condition)
                 'positive)
                ((negative? condition)
                 'negative))
          (raise v)))))))

;; From SRFI-34 "Examples" section - #5
(let* ((out (open-output-string))
       (value (test-exception-handler-4 1 out)))
  (test "" (get-output-string out))
  (test 'positive value))
;; From SRFI-34 "Examples" section - #6
(let* ((out (open-output-string))
       (value (test-exception-handler-4 -1 out)))
  (test "" (get-output-string out))
  (test 'negative value))
;; From SRFI-34 "Examples" section - #7
(let* ((out (open-output-string))
       (value (test-exception-handler-4 0 out)))
  (test "reraised 0!" (get-output-string out))
  (test 'zero value))

;; From SRFI-34 "Examples" section - #8
(test 42
    (guard (condition
            ((assq 'a condition) => cdr)
            ((assq 'b condition)))
      (raise (list (cons 'a 42)))))

;; From SRFI-34 "Examples" section - #9
(test '(b . 23)
    (guard (condition
            ((assq 'a condition) => cdr)
            ((assq 'b condition)))
      (raise (list (cons 'b 23)))))

(test 'caught-d
    (guard (condition
            ((assq 'c condition) 'caught-c)
            ((assq 'd condition) 'caught-d))
      (list
       (sqrt 8)
       (guard (condition
               ((assq 'a condition) => cdr)
               ((assq 'b condition)))
         (raise (list (cons 'd 24)))))))

(test-end)

(test-begin "6.12 Environments and evaluation")

;; (test 21 (eval '(* 7 3) (scheme-report-environment 5)))

(test 20
    (let ((f (eval '(lambda (f x) (f x x)) (null-environment 5))))
      (f + 10)))

(test 1024 (eval '(expt 2 10) (environment '(scheme base))))
;; (sin 0) may return exact number
(test 0.0 (inexact (eval '(sin 0) (environment '(scheme inexact)))))
;; ditto
(test 1024.0 (eval '(+ (expt 2 10) (inexact (sin 0)))
                   (environment '(scheme base) '(scheme inexact))))

(test-end)

(test-begin "6.13 Input and output")

(test #t (port? (current-input-port)))
(test #t (input-port? (current-input-port)))
(test #t (output-port? (current-output-port)))
(test #t (output-port? (current-error-port)))
(test #t (input-port? (open-input-string "abc")))
(test #t (output-port? (open-output-string)))

(test #t (textual-port? (open-input-string "abc")))
(test #t (textual-port? (open-output-string)))
(test #t (binary-port? (open-input-bytevector #u8(0 1 2))))
(test #t (binary-port? (open-output-bytevector)))

(test #t (input-port-open? (open-input-string "abc")))
(test #t (output-port-open? (open-output-string)))

(test #f
    (let ((in (open-input-string "abc")))
      (close-input-port in)
      (input-port-open? in)))

(test #f
    (let ((out (open-output-string)))
      (close-output-port out)
      (output-port-open? out)))

(test #f
    (let ((out (open-output-string)))
      (close-port out)
      (output-port-open? out)))

(test 'error
    (let ((in (open-input-string "abc")))
      (close-input-port in)
      (guard (exn (else 'error)) (read-char in))))

(test 'error
    (let ((out (open-output-string)))
      (close-output-port out)
      (guard (exn (else 'error)) (write-char #\c out))))

(test #t (eof-object? (eof-object)))
(test #t (eof-object? (read (open-input-string ""))))
(test #t (char-ready? (open-input-string "42")))
(test 42 (read (open-input-string " 42 ")))

(test #t (eof-object? (read-char (open-input-string ""))))
(test #\a (read-char (open-input-string "abc")))

(test #t (eof-object? (read-line (open-input-string ""))))
(test "abc" (read-line (open-input-string "abc")))
(test "abc" (read-line (open-input-string "abc\ndef\n")))

(test #t (eof-object? (read-string 3 (open-input-string ""))))
(test "abc" (read-string 3 (open-input-string "abcd")))
(test "abc" (read-string 3 (open-input-string "abc\ndef\n")))

(let ((in (open-input-string (string #\x10F700 #\x10F701 #\x10F702))))
  (let* ((c1 (read-char in))
         (c2 (read-char in))
         (c3 (read-char in)))
    (test #\x10F700 c1)
    (test #\x10F701 c2)
    (test #\x10F702 c3)))

(test (string #\x10F700)
    (let ((out (open-output-string)))
      (write-char #\x10F700 out)
      (get-output-string out)))

(test "abc"
    (let ((out (open-output-string)))
      (write 'abc out)
      (get-output-string out)))

(test "abc def"
    (let ((out (open-output-string)))
      (display "abc def" out)
      (get-output-string out)))

(test "abc"
    (let ((out (open-output-string)))
      (display #\a out)
      (display "b" out)
      (display #\c out)
      (get-output-string out)))

(test #t
      (let* ((out (open-output-string))
             (r (begin (newline out) (get-output-string out))))
        (or (equal? r "\n") (equal? r "\r\n"))))

(test "abc def"
    (let ((out (open-output-string)))
      (write-string "abc def" out)
      (get-output-string out)))

(test "def"
    (let ((out (open-output-string)))
      (write-string "abc def" out 4)
      (get-output-string out)))

(test "c d"
    (let ((out (open-output-string)))
      (write-string "abc def" out 2 5)
      (get-output-string out)))

(test ""
  (let ((out (open-output-string)))
    (flush-output-port out)
    (get-output-string out)))

(test #t (eof-object? (read-u8 (open-input-bytevector #u8()))))
(test 1 (read-u8 (open-input-bytevector #u8(1 2 3))))

(test #t (eof-object? (read-bytevector 3 (open-input-bytevector #u8()))))
(test #t (u8-ready? (open-input-bytevector #u8(1))))
(test #u8(1) (read-bytevector 3 (open-input-bytevector #u8(1))))
(test #u8(1 2) (read-bytevector 3 (open-input-bytevector #u8(1 2))))
(test #u8(1 2 3) (read-bytevector 3 (open-input-bytevector #u8(1 2 3))))
(test #u8(1 2 3) (read-bytevector 3 (open-input-bytevector #u8(1 2 3 4))))

(test #t
    (let ((bv (bytevector 1 2 3 4 5)))
      (eof-object? (read-bytevector! bv (open-input-bytevector #u8())))))

(test #u8(6 7 8 9 10)
  (let ((bv (bytevector 1 2 3 4 5)))
    (read-bytevector! bv (open-input-bytevector #u8(6 7 8 9 10)) 0 5)
    bv))

(test #u8(6 7 8 4 5)
  (let ((bv (bytevector 1 2 3 4 5)))
    (read-bytevector! bv (open-input-bytevector #u8(6 7 8 9 10)) 0 3)
    bv))

(test #u8(1 2 3 6 5)
  (let ((bv (bytevector 1 2 3 4 5)))
    (read-bytevector! bv (open-input-bytevector #u8(6 7 8 9 10)) 3 4)
    bv))

(test #u8(1 2 3)
  (let ((out (open-output-bytevector)))
    (write-u8 1 out)
    (write-u8 2 out)
    (write-u8 3 out)
    (get-output-bytevector out)))

(test #u8(1 2 3 4 5)
  (let ((out (open-output-bytevector)))
    (write-bytevector #u8(1 2 3 4 5) out)
    (get-output-bytevector out)))

(test #u8(3 4 5)
  (let ((out (open-output-bytevector)))
    (write-bytevector #u8(1 2 3 4 5) out 2)
    (get-output-bytevector out)))

(test #u8(3 4)
  (let ((out (open-output-bytevector)))
    (write-bytevector #u8(1 2 3 4 5) out 2 4)
    (get-output-bytevector out)))

(test #u8()
  (let ((out (open-output-bytevector)))
    (flush-output-port out)
    (get-output-bytevector out)))

(test #t
    (and (member
          (let ((out (open-output-string))
                (x (list 1)))
            (set-cdr! x x)
            (write x out)
            (get-output-string out))
          ;; labels not guaranteed to be 0 indexed, spacing may differ
          '("#0=(1 . #0#)" "#1=(1 . #1#)"))
         #t))

(test "((1 2 3) (1 2 3))"
    (let ((out (open-output-string))
          (x (list 1 2 3)))
      (write (list x x) out)
      (get-output-string out)))

(test "((1 2 3) (1 2 3))"
    (let ((out (open-output-string))
          (x (list 1 2 3)))
      (write-simple (list x x) out)
      (get-output-string out)))

(test #t
    (and (member (let ((out (open-output-string))
                       (x (list 1 2 3)))
                   (write-shared (list x x) out)
                   (get-output-string out))
                 '("(#0=(1 2 3) #0#)" "(#1=(1 2 3) #1#)"))
         #t))

(test-begin "Read syntax")

;; check reading boolean followed by eof
(test #t (read (open-input-string "#t")))
(test #t (read (open-input-string "#true")))
(test #f (read (open-input-string "#f")))
(test #f (read (open-input-string "#false")))
(define (read2 port)
  (let* ((o1 (read port)) (o2 (read port)))
    (cons o1 o2)))
;; check reading boolean followed by delimiter
(test '(#t . (5)) (read2 (open-input-string "#t(5)")))
(test '(#t . 6) (read2 (open-input-string "#true 6 ")))
(test '(#f . 7) (read2 (open-input-string "#f 7")))
(test '(#f . "8") (read2 (open-input-string "#false\"8\"")))

(test '() (read (open-input-string "()")))
(test '(1 2) (read (open-input-string "(1 2)")))
(test '(1 . 2) (read (open-input-string "(1 . 2)")))
(test '(1 2) (read (open-input-string "(1 . (2))")))
(test '(1 2 3 4 5) (read (open-input-string "(1 . (2 3 4 . (5)))")))
(test '1 (cadr (read (open-input-string "#0=(1 . #0#)"))))
(test '(1 2 3) (cadr (read (open-input-string "(#0=(1 2 3) #0#)"))))

(test '(quote (1 2)) (read (open-input-string "'(1 2)")))
(test '(quote (1 (unquote 2))) (read (open-input-string "'(1 ,2)")))
(test '(quote (1 (unquote-splicing 2))) (read (open-input-string "'(1 ,@2)")))
(test '(quasiquote (1 (unquote 2))) (read (open-input-string "`(1 ,2)")))

(test #() (read (open-input-string "#()")))
(test #(a b) (read (open-input-string "#(a b)")))

(test #u8() (read (open-input-string "#u8()")))
(test #u8(0 1) (read (open-input-string "#u8(0 1)")))

(test 'abc (read (open-input-string "abc")))
(test 'abc (read (open-input-string "abc def")))
(test 'ABC (read (open-input-string "ABC")))
(test 'Hello (read (open-input-string "|H\\x65;llo|")))

(test 'abc (read (open-input-string "#!fold-case ABC")))
(test 'ABC (read (open-input-string "#!fold-case #!no-fold-case ABC")))

(test 'def (read (open-input-string "#; abc def")))
(test 'def (read (open-input-string "; abc \ndef")))
(test 'def (read (open-input-string "#| abc |# def")))
(test 'ghi (read (open-input-string "#| abc #| def |# |# ghi")))
(test 'ghi (read (open-input-string "#; ; abc\n def ghi")))
(test '(abs -16) (read (open-input-string "(#;sqrt abs -16)")))
(test '(a d) (read (open-input-string "(a #; #;b c d)")))
(test '(a e) (read (open-input-string "(a #;(b #;c d) e)")))
(test '(a . c) (read (open-input-string "(a . #;b c)")))
(test '(a . b) (read (open-input-string "(a . b #;c)")))

(define (test-read-error str)
  (test-assert str
      (guard (exn (else #t))
        (read (open-input-string str))
        #f)))

(test-read-error "(#;a . b)")
(test-read-error "(a . #;b)")
(test-read-error "(a #;. b)")
(test-read-error "(#;x #;y . z)")
(test-read-error "(#; #;x #;y . z)")
(test-read-error "(#; #;x . z)")

(test #\a (read (open-input-string "#\\a")))
(test #\space (read (open-input-string "#\\space")))
(test 0 (char->integer (read (open-input-string "#\\null"))))
(test 7 (char->integer (read (open-input-string "#\\alarm"))))
(test 8 (char->integer (read (open-input-string "#\\backspace"))))
(test 9 (char->integer (read (open-input-string "#\\tab"))))
(test 10 (char->integer (read (open-input-string "#\\newline"))))
(test 13 (char->integer (read (open-input-string "#\\return"))))
(test #x7F (char->integer (read (open-input-string "#\\delete"))))
(test #x1B (char->integer (read (open-input-string "#\\escape"))))
(test #x03BB (char->integer (read (open-input-string "#\\λ"))))
(test #x03BB (char->integer (read (open-input-string "#\\x03BB"))))

(test "abc" (read (open-input-string "\"abc\"")))
(test "abc" (read (open-input-string "\"abc\" \"def\"")))
(test "ABC" (read (open-input-string "\"ABC\"")))
(test "Hello" (read (open-input-string "\"H\\x65;llo\"")))
(test 7 (char->integer (string-ref (read (open-input-string "\"\\a\"")) 0)))
(test 8 (char->integer (string-ref (read (open-input-string "\"\\b\"")) 0)))
(test 9 (char->integer (string-ref (read (open-input-string "\"\\t\"")) 0)))
(test 10 (char->integer (string-ref (read (open-input-string "\"\\n\"")) 0)))
(test 13 (char->integer (string-ref (read (open-input-string "\"\\r\"")) 0)))
(test #x22 (char->integer (string-ref (read (open-input-string "\"\\\"\"")) 0)))
(test #x7C (char->integer (string-ref (read (open-input-string "\"\\|\"")) 0)))
(test "line 1\nline 2\n" (read (open-input-string "\"line 1\nline 2\n\"")))
(test "line 1continued\n" (read (open-input-string "\"line 1\\\ncontinued\n\"")))
(test "line 1continued\n" (read (open-input-string "\"line 1\\ \ncontinued\n\"")))
(test "line 1continued\n" (read (open-input-string "\"line 1\\\n continued\n\"")))
(test "line 1continued\n" (read (open-input-string "\"line 1\\ \t \n \t continued\n\"")))
(test "line 1\n\nline 3\n" (read (open-input-string "\"line 1\\ \t \n \t \n\nline 3\n\"")))
(test #x03BB (char->integer (string-ref (read (open-input-string "\"\\x03BB;\"")) 0)))

(define-syntax test-write-syntax
  (syntax-rules ()
    ((test-write-syntax expect-str obj-expr)
     (let ((out (open-output-string)))
       (write obj-expr out)
       (test expect-str (get-output-string out))))))

(test-write-syntax "|.|" '|.|)
(test-write-syntax "|a b|" '|a b|)
(test-write-syntax "|,a|" '|,a|)
(test-write-syntax "|\"|" '|\"|)
(test-write-syntax "a" '|a|)
;; (test-write-syntax "a.b" '|a.b|)
(test-write-syntax "|2|" '|2|)
(test-write-syntax "|+3|" '|+3|)
(test-write-syntax "|-.4|" '|-.4|)
(test-write-syntax "|+i|" '|+i|)
(test-write-syntax "|-i|" '|-i|)
(test-write-syntax "|+inf.0|" '|+inf.0|)
(test-write-syntax "|-inf.0|" '|-inf.0|)
(test-write-syntax "|+nan.0|" '|+nan.0|)
(test-write-syntax "|+NaN.0|" '|+NaN.0|)
(test-write-syntax "|+NaN.0abc|" '|+NaN.0abc|)

(test-end)

(test-begin "Numeric syntax")

;; Numeric syntax adapted from Peter Bex's tests.
;;
;; These are updated to R7RS, using string ports instead of
;; string->number, and "error" tests removed because implementations
;; are free to provide their own numeric extensions.  Currently all
;; tests are run by default - need to cond-expand and test for
;; infinities and -0.0.

(define-syntax test-numeric-syntax
  (syntax-rules ()
    ((test-numeric-syntax str expect strs ...)
     (let* ((z (read (open-input-string str)))
            (out (open-output-string))
            (z-str (begin (write z out) (get-output-string out))))
       (test expect (values z))
       (test #t (and (member z-str '(str strs ...)) #t))))))

;; Each test is of the form:
;;
;;   (test-numeric-syntax input-str expected-value expected-write-values ...)
;;
;; where the input should be eqv? to the expected-value, and the
;; written output the same as any of the expected-write-values.  The
;; form
;;
;;   (test-numeric-syntax input-str expected-value)
;;
;; is a shorthand for
;;
;;   (test-numeric-syntax input-str expected-value (input-str))

;; Simple
(test-numeric-syntax "1" 1)
(test-numeric-syntax "+1" 1 "1")
(test-numeric-syntax "-1" -1)
(test-numeric-syntax "#i1" 1.0 "1.0" "1.")
(test-numeric-syntax "#I1" 1.0 "1.0" "1.")
(test-numeric-syntax "#i-1" -1.0 "-1.0" "-1.")
;; Decimal
(test-numeric-syntax "1.0" 1.0 "1.0" "1.")
(test-numeric-syntax "1." 1.0 "1.0" "1.")
(test-numeric-syntax ".1" 0.1 "0.1" "100.0e-3")
(test-numeric-syntax "-.1" -0.1 "-0.1" "-100.0e-3")
;; Some Schemes don't allow negative zero. This is okay with the standard
(test-numeric-syntax "-.0" -0.0 "-0." "-0.0" "0.0" "0." ".0")
(test-numeric-syntax "-0." -0.0 "-.0" "-0.0" "0.0" "0." ".0")
(test-numeric-syntax "#i1.0" 1.0 "1.0" "1.")
(test-numeric-syntax "#e1.0" 1 "1")
(test-numeric-syntax "#e-.0" 0 "0")
(test-numeric-syntax "#e-0." 0 "0")
;; Decimal notation with suffix
(test-numeric-syntax "1e2" 100.0 "100.0" "100.")
(test-numeric-syntax "1E2" 100.0 "100.0" "100.")
(test-numeric-syntax "1s2" 100.0 "100.0" "100.")
(test-numeric-syntax "1S2" 100.0 "100.0" "100.")
(test-numeric-syntax "1f2" 100.0 "100.0" "100.")
(test-numeric-syntax "1F2" 100.0 "100.0" "100.")
(test-numeric-syntax "1d2" 100.0 "100.0" "100.")
(test-numeric-syntax "1D2" 100.0 "100.0" "100.")
(test-numeric-syntax "1l2" 100.0 "100.0" "100.")
(test-numeric-syntax "1L2" 100.0 "100.0" "100.")
;; NaN, Inf
(test-numeric-syntax "+nan.0" +nan.0 "+nan.0" "+NaN.0")
(test-numeric-syntax "+NAN.0" +nan.0 "+nan.0" "+NaN.0")
(test-numeric-syntax "+inf.0" +inf.0 "+inf.0" "+Inf.0")
(test-numeric-syntax "+InF.0" +inf.0 "+inf.0" "+Inf.0")
(test-numeric-syntax "-inf.0" -inf.0 "-inf.0" "-Inf.0")
(test-numeric-syntax "-iNF.0" -inf.0 "-inf.0" "-Inf.0")
(test-numeric-syntax "#i+nan.0" +nan.0 "+nan.0" "+NaN.0")
(test-numeric-syntax "#i+inf.0" +inf.0 "+inf.0" "+Inf.0")
(test-numeric-syntax "#i-inf.0" -inf.0 "-inf.0" "-Inf.0")
;; Exact ratios
(test-numeric-syntax "1/2" (/ 1 2))
(test-numeric-syntax "#e1/2" (/ 1 2) "1/2")
(test-numeric-syntax "10/2" 5 "5")
(test-numeric-syntax "-1/2" (- (/ 1 2)))
(test-numeric-syntax "0/10" 0 "0")
(test-numeric-syntax "#e0/10" 0 "0")
(test-numeric-syntax "#i3/2" (/ 3.0 2.0) "1.5")
;; Exact complex
(test-numeric-syntax "1+2i" (make-rectangular 1 2))
(test-numeric-syntax "1+2I" (make-rectangular 1 2) "1+2i")
(test-numeric-syntax "1-2i" (make-rectangular 1 -2))
(test-numeric-syntax "-1+2i" (make-rectangular -1 2))
(test-numeric-syntax "-1-2i" (make-rectangular -1 -2))
(test-numeric-syntax "+i" (make-rectangular 0 1) "+i" "+1i" "0+i" "0+1i")
(test-numeric-syntax "0+i" (make-rectangular 0 1) "+i" "+1i" "0+i" "0+1i")
(test-numeric-syntax "0+1i" (make-rectangular 0 1) "+i" "+1i" "0+i" "0+1i")
(test-numeric-syntax "-i" (make-rectangular 0 -1) "-i" "-1i" "0-i" "0-1i")
(test-numeric-syntax "0-i" (make-rectangular 0 -1) "-i" "-1i" "0-i" "0-1i")
(test-numeric-syntax "0-1i" (make-rectangular 0 -1) "-i" "-1i" "0-i" "0-1i")
(test-numeric-syntax "+2i" (make-rectangular 0 2) "2i" "+2i" "0+2i")
(test-numeric-syntax "-2i" (make-rectangular 0 -2) "-2i" "0-2i")
;; Decimal-notation complex numbers (rectangular notation)
(test-numeric-syntax "1.0+2i" (make-rectangular 1.0 2) "1.0+2.0i" "1.0+2i" "1.+2i" "1.+2.i")
(test-numeric-syntax "1+2.0i" (make-rectangular 1 2.0) "1.0+2.0i" "1+2.0i" "1.+2.i" "1+2.i")
(test-numeric-syntax "1e2+1.0i" (make-rectangular 100.0 1.0) "100.0+1.0i" "100.+1.i")
(test-numeric-syntax "1s2+1.0i" (make-rectangular 100.0 1.0) "100.0+1.0i" "100.+1.i")
(test-numeric-syntax "1.0+1e2i" (make-rectangular 1.0 100.0) "1.0+100.0i" "1.+100.i")
(test-numeric-syntax "1.0+1s2i" (make-rectangular 1.0 100.0) "1.0+100.0i" "1.+100.i")
;; Fractional complex numbers (rectangular notation)
(test-numeric-syntax "1/2+3/4i" (make-rectangular (/ 1 2) (/ 3 4)))
;; Mixed fractional/decimal notation complex numbers (rectangular notation)
(test-numeric-syntax "0.5+3/4i" (make-rectangular 0.5 (/ 3 4))
  "0.5+0.75i" ".5+.75i" "0.5+3/4i" ".5+3/4i" "500.0e-3+750.0e-3i")
;; Complex NaN, Inf (rectangular notation)
;;(test-numeric-syntax "+nan.0+nan.0i" (make-rectangular the-nan the-nan) "+NaN.0+NaN.0i") 
(test-numeric-syntax "+inf.0+inf.0i" (make-rectangular +inf.0 +inf.0) "+Inf.0+Inf.0i")
(test-numeric-syntax "-inf.0+inf.0i" (make-rectangular -inf.0 +inf.0) "-Inf.0+Inf.0i")
(test-numeric-syntax "-inf.0-inf.0i" (make-rectangular -inf.0 -inf.0) "-Inf.0-Inf.0i")
(test-numeric-syntax "+inf.0-inf.0i" (make-rectangular +inf.0 -inf.0) "+Inf.0-Inf.0i")
;; Complex numbers (polar notation)
;; Need to account for imprecision in write output.
;;(test-numeric-syntax "1@2" -0.416146836547142+0.909297426825682i "-0.416146836547142+0.909297426825682i")
;; Base prefixes
(test-numeric-syntax "#x11" 17 "17")
(test-numeric-syntax "#X11" 17 "17")
(test-numeric-syntax "#d11" 11 "11")
(test-numeric-syntax "#D11" 11 "11")
(test-numeric-syntax "#o11" 9 "9")
(test-numeric-syntax "#O11" 9 "9")
(test-numeric-syntax "#b11" 3 "3")
(test-numeric-syntax "#B11" 3 "3")
(test-numeric-syntax "#o7" 7 "7")
(test-numeric-syntax "#xa" 10 "10")
(test-numeric-syntax "#xA" 10 "10")
(test-numeric-syntax "#xf" 15 "15")
(test-numeric-syntax "#x-10" -16 "-16")
(test-numeric-syntax "#d-10" -10 "-10")
(test-numeric-syntax "#o-10" -8 "-8")
(test-numeric-syntax "#b-10" -2 "-2")
;; Combination of prefixes
(test-numeric-syntax "#e#x10" 16 "16")
(test-numeric-syntax "#i#x10" 16.0 "16.0" "16.")
;; (Attempted) decimal notation with base prefixes
(test-numeric-syntax "#d1." 1.0 "1.0" "1.")
(test-numeric-syntax "#d.1" 0.1 "0.1" ".1" "100.0e-3")
(test-numeric-syntax "#x1e2" 482 "482")
(test-numeric-syntax "#d1e2" 100.0 "100.0" "100.")
;; Fractions with prefixes
(test-numeric-syntax "#x10/2" 8 "8")
(test-numeric-syntax "#x11/2" (/ 17 2) "17/2")
(test-numeric-syntax "#d11/2" (/ 11 2) "11/2")
(test-numeric-syntax "#o11/2" (/ 9 2) "9/2")
(test-numeric-syntax "#b11/10" (/ 3 2) "3/2")
;; Complex numbers with prefixes
;;(test-numeric-syntax "#x10+11i" (make-rectangular 16 17) "16+17i")
(test-numeric-syntax "#d1.0+1.0i" (make-rectangular 1.0 1.0) "1.0+1.0i" "1.+1.i")
(test-numeric-syntax "#d10+11i" (make-rectangular 10 11) "10+11i")
;;(test-numeric-syntax "#o10+11i" (make-rectangular 8 9) "8+9i")
;;(test-numeric-syntax "#b10+11i" (make-rectangular 2 3) "2+3i")
;;(test-numeric-syntax "#e1.0+1.0i" (make-rectangular 1 1) "1+1i" "1+i")
;;(test-numeric-syntax "#i1.0+1.0i" (make-rectangular 1.0 1.0) "1.0+1.0i" "1.+1.i")

(test-end)

(test-end)

(test-begin "6.14 System interface")

;; 6.14 System interface

;; (test "/usr/local/bin:/usr/bin:/bin" (get-environment-variable "PATH"))

(test #t (string? (get-environment-variable "PATH")))

;; (test '(("USER" . "root") ("HOME" . "/")) (get-environment-variables))

(let ((env (get-environment-variables)))
  (define (env-pair? x)
    (and (pair? x) (string? (car x)) (string? (cdr x))))
  (define (all? pred ls)
    (or (null? ls) (and (pred (car ls)) (all? pred (cdr ls)))))
  (test #t (list? env))
  (test #t (all? env-pair? env)))

(test #t (list? (command-line)))

(test #t (real? (current-second)))
(test #t (inexact? (current-second)))
(test #t (exact? (current-jiffy)))
(test #t (exact? (jiffies-per-second)))

(test #t (list? (features)))
(test #t (and (memq 'r7rs (features)) #t))

(test #t (file-exists? "."))
(test #f (file-exists? " no such file "))

(test #t (file-error?
          (guard (exn (else exn))
            (delete-file " no such file "))))

(test-end)

(test-end)
