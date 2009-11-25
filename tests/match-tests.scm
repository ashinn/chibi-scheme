
(import (chibi match))

(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test name expr expect)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((str (call-with-output-string (lambda (out) (display name out))))
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

(define (test-report)
  (write *tests-passed*)
  (display " out of ")
  (write *tests-run*)
  (display " passed (")
  (write (* (/ *tests-passed* *tests-run*) 100))
  (display "%)")
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run tests

(test "any" (match 'any (_ 'ok)) 'ok)
(test "symbol" (match 'ok (x x)) 'ok)
(test "number" (match 28 (28 'ok)) 'ok)
(test "string" (match "good" ("bad" 'fail) ("good" 'ok)) 'ok)
(test "literal symbol" (match 'good ('bad 'fail) ('good 'ok)) 'ok)
(test "null" (match '() (() 'ok)) 'ok)
(test "pair" (match '(ok) ((x) x)) 'ok)
(test "vector" (match '#(ok) (#(x) x)) 'ok)
(test "any doubled" (match '(1 2) ((_ _) 'ok)) 'ok)
(test "and empty" (match '(o k) ((and) 'ok)) 'ok)
(test "and single" (match 'ok ((and x) x)) 'ok)
(test "and double" (match 'ok ((and (? symbol?) y) 'ok)) 'ok)
(test "or empty" (match '(o k) ((or) 'fail) (else 'ok)) 'ok)
(test "or single" (match 'ok ((or x) 'ok)) 'ok)
(test "or double" (match 'ok ((or (? symbol? y) y) y)) 'ok)
(test "not" (match 28 ((not (a . b)) 'ok)) 'ok)
(test "pred" (match 28 ((? number?) 'ok)) 'ok)
(test "named pred" (match 28 ((? number? x) (+ x 1))) 29)

(test "duplicate symbols pass" (match '(ok . ok) ((x . x) x)) 'ok)
(test "duplicate symbols fail" (match '(ok . bad) ((x . x) 'bad) (else 'ok)) 'ok)
(test "duplicate symbols samth" (match '(ok . ok) ((x . 'bad) x) (('ok . x) x)) 'ok)

(test "ellipses"
            (match '((a . 1) (b . 2) (c . 3))
              (((x . y) ___) (list x y)))
            '((a b c) (1 2 3)))

(test "real ellipses"
            (match '((a . 1) (b . 2) (c . 3))
              (((x . y) ...) (list x y)))
            '((a b c) (1 2 3)))

(test "vector ellipses"
            (match '#(1 2 3 (a . 1) (b . 2) (c . 3))
              (#(a b c (hd . tl) ...) (list a b c hd tl)))
            '(1 2 3 (a b c) (1 2 3)))

(test "pred ellipses"
            (match '(1 2 3)
              (((? odd? n) ___) n)
              (((? number? n) ___) n))
            '(1 2 3))

(test "failure continuation"
            (match '(1 2)
              ((a . b) (=> next) (if (even? a) 'fail (next)))
              ((a . b) 'ok))
            'ok)

(test "let"
            (match-let ((x 'ok) (y '(o k)))
              y)
            '(o k))

(test "let*"
            (match-let* ((x 'f) (y 'o) ((z w) (list y x)))
              (list x y z w))
            '(f o o f))

(test "getter car"
            (match '(1 . 2) (((get! a) . b) (list (a) b)))
            '(1 2))

(test "getter cdr"
            (match '(1 . 2) ((a . (get! b)) (list a (b))))
            '(1 2))

(test "getter vector"
            (match '#(1 2 3) (#((get! a) b c) (list (a) b c)))
            '(1 2 3))

(test "setter car"
            (let ((x (cons 1 2)))
              (match x (((set! a) . b) (a 3)))
              x)
            '(3 . 2))

(test "setter cdr"
            (let ((x (cons 1 2)))
              (match x ((a . (set! b)) (b 3)))
              x)
            '(1 . 3))

(test "setter vector"
            (let ((x (vector 1 2 3)))
              (match x (#(a (set! b) c) (b 0)))
              x)
            '#(1 0 3))

(test "single tail"
            (match '((a . 1) (b . 2) (c . 3))
              (((x . y) ... last) (list x y last)))
            '((a b) (1 2) (c . 3)))

(test "single tail 2"
            (match '((a . 1) (b . 2) 3)
              (((x . y) ... last) (list x y last)))
            '((a b) (1 2) 3))

(test "multiple tail"
            (match '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))
              (((x . y) ... u v w) (list x y u v w)))
            '((a b) (1 2) (c . 3) (d . 4) (e . 5)))

(test "Riastradh quasiquote"
            (match '(1 2 3) (`(1 ,b ,c) (list b c)))
            '(2 3))

(test "trivial tree search"
            (match '(1 2 3) ((_ *** (a b c)) (list a b c)))
            '(1 2 3))

(test "simple tree search"
            (match '(x (1 2 3)) ((_ *** (a b c)) (list a b c)))
            '(1 2 3))

(test "deep tree search"
            (match '(x (x (x (1 2 3)))) ((_ *** (a b c)) (list a b c)))
            '(1 2 3))

(test "non-tail tree search"
            (match '(x (x (x a b c (1 2 3) d e f))) ((_ *** (a b c)) (list a b c)))
            '(1 2 3))

(test "restricted tree search"
            (match '(x (x (x a b c (1 2 3) d e f))) (('x *** (a b c)) (list a b c)))
            '(1 2 3))

(test "fail restricted tree search"
            (match '(x (y (x a b c (1 2 3) d e f)))
              (('x *** (a b c)) (list a b c))
              (else #f))
            #f)

(test "sxml tree search"
    (match '(p (ul (li a (b c) (a (^ (href . "http://synthcode.com/")) "synthcode") d e f)))
      (((or 'p 'ul 'li 'b) *** ('a ('^ attrs ...) text ...))
       (list attrs text))
      (else #f))
  '(((href . "http://synthcode.com/")) ("synthcode")))

(test "failed sxml tree search"
    (match '(p (ol (li a (b c) (a (^ (href . "http://synthcode.com/")) "synthcode") d e f)))
      (((or 'p 'ul 'li 'b) *** ('a ('^ attrs ...) text ...))
       (list attrs text))
      (else #f))
  #f)

(test "collect tree search"
    (match '(p (ul (li a (b c) (a (^ (href . "http://synthcode.com/")) "synthcode") d e f)))
      (((and tag (or 'p 'ul 'li 'b)) *** ('a ('^ attrs ...) text ...))
       (list tag attrs text))
      (else #f))
  '((p ul li) ((href . "http://synthcode.com/")) ("synthcode")))

(test-report)

