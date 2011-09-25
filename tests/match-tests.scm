
(cond-expand
 (modules (import (chibi match) (only (chibi test) test-begin test test-end)))
 (else (load "lib/chibi/match/match.scm")))

(test-begin "match")

(test "any" 'ok (match 'any (_ 'ok)))
(test "symbol" 'ok (match 'ok (x x)))
(test "number" 'ok (match 28 (28 'ok)))
(test "string" 'ok (match "good" ("bad" 'fail) ("good" 'ok)))
(test "literal symbol" 'ok (match 'good ('bad 'fail) ('good 'ok)))
(test "null" 'ok (match '() (() 'ok)))
(test "pair" 'ok (match '(ok) ((x) x)))
(test "vector" 'ok (match '#(ok) (#(x) x)))
(test "any doubled" 'ok (match '(1 2) ((_ _) 'ok)))
(test "and empty" 'ok (match '(o k) ((and) 'ok)))
(test "and single" 'ok (match 'ok ((and x) x)))
(test "and double" 'ok (match 'ok ((and (? symbol?) y) 'ok)))
(test "or empty" 'ok (match '(o k) ((or) 'fail) (else 'ok)))
(test "or single" 'ok (match 'ok ((or x) 'ok)))
(test "or double" 'ok (match 'ok ((or (? symbol? y) y) y)))
(test "not" 'ok (match 28 ((not (a . b)) 'ok)))
(test "pred" 'ok (match 28 ((? number?) 'ok)))
(test "named pred" 29 (match 28 ((? number? x) (+ x 1))))

(test "duplicate symbols pass" 'ok (match '(ok . ok) ((x . x) x)))
(test "duplicate symbols fail" 'ok (match '(ok . bad) ((x . x) 'bad) (else 'ok)))
(test "duplicate symbols samth" 'ok (match '(ok . ok) ((x . 'bad) x) (('ok . x) x)))
(test "duplicate symbols bound" 3 (let ((a '(1 2))) (match a ((and (a 2) (1 b)) (+ a b)) (_ #f))))

(test "ellipses" '((a b c) (1 2 3))
  (match '((a . 1) (b . 2) (c . 3))
    (((x . y) ___) (list x y))))

(test "real ellipses" '((a b c) (1 2 3))
  (match '((a . 1) (b . 2) (c . 3))
    (((x . y) ...) (list x y))))

(test "vector ellipses" '(1 2 3 (a b c) (1 2 3))
  (match '#(1 2 3 (a . 1) (b . 2) (c . 3))
    (#(a b c (hd . tl) ...) (list a b c hd tl))))

(test "pred ellipses" '(1 2 3)
  (match '(1 2 3)
    (((? odd? n) ___) n)
    (((? number? n) ___) n)))

(test "failure continuation" 'ok
  (match '(1 2)
    ((a . b) (=> next) (if (even? a) 'fail (next)))
    ((a . b) 'ok)))

(test "let" '(o k)
  (match-let ((x 'ok) (y '(o k))) y))

(test "let*" '(f o o f)
  (match-let* ((x 'f) (y 'o) ((z w) (list y x))) (list x y z w)))

(test "getter car" '(1 2)
  (match '(1 . 2) (((get! a) . b) (list (a) b))))

(test "getter cdr" '(1 2)
  (match '(1 . 2) ((a . (get! b)) (list a (b)))))

(test "getter vector" '(1 2 3)
  (match '#(1 2 3) (#((get! a) b c) (list (a) b c))))

(test "setter car" '(3 . 2)
  (let ((x (cons 1 2)))
    (match x (((set! a) . b) (a 3)))
    x))

(test "setter cdr" '(1 . 3)
  (let ((x (cons 1 2)))
    (match x ((a . (set! b)) (b 3)))
    x))

(test "setter vector" '#(1 0 3)
  (let ((x (vector 1 2 3)))
    (match x (#(a (set! b) c) (b 0)))
    x))

(test "single tail" '((a b) (1 2) (c . 3))
  (match '((a . 1) (b . 2) (c . 3))
    (((x . y) ... last) (list x y last))))

(test "single tail 2" '((a b) (1 2) 3)
  (match '((a . 1) (b . 2) 3)
    (((x . y) ... last) (list x y last))))

(test "multiple tail" '((a b) (1 2) (c . 3) (d . 4) (e . 5))
  (match '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))
    (((x . y) ... u v w) (list x y u v w))))

(test "tail against improper list" #f
  (match '(a b c d e f . g)
    ((x ... y u v w) (list x y u v w))
    (else #f)))

(test "Riastradh quasiquote" '(2 3)
  (match '(1 2 3) (`(1 ,b ,c) (list b c))))

(test "trivial tree search" '(1 2 3)
  (match '(1 2 3) ((_ *** (a b c)) (list a b c))))

(test "simple tree search" '(1 2 3)
  (match '(x (1 2 3)) ((_ *** (a b c)) (list a b c))))

(test "deep tree search" '(1 2 3)
  (match '(x (x (x (1 2 3)))) ((_ *** (a b c)) (list a b c))))

(test "non-tail tree search" '(1 2 3)
  (match '(x (x (x a b c (1 2 3) d e f))) ((_ *** (a b c)) (list a b c))))

(test "restricted tree search" '(1 2 3)
  (match '(x (x (x a b c (1 2 3) d e f))) (('x *** (a b c)) (list a b c))))

(test "fail restricted tree search" #f
  (match '(x (y (x a b c (1 2 3) d e f)))
    (('x *** (a b c)) (list a b c))
    (else #f)))

(test "sxml tree search" '(((href . "http://synthcode.com/")) ("synthcode"))
  (match '(p (ul (li a (b c) (a (^ (href . "http://synthcode.com/")) "synthcode") d e f)))
    (((or 'p 'ul 'li 'b) *** ('a ('^ attrs ...) text ...))
     (list attrs text))
    (else #f)))

(test "failed sxml tree search" #f
  (match '(p (ol (li a (b c) (a (^ (href . "http://synthcode.com/")) "synthcode") d e f)))
    (((or 'p 'ul 'li 'b) *** ('a ('^ attrs ...) text ...))
     (list attrs text))
    (else #f)))

(test "collect tree search"
    '((p ul li) ((href . "http://synthcode.com/")) ("synthcode"))
  (match '(p (ul (li a (b c) (a (^ (href . "http://synthcode.com/")) "synthcode") d e f)))
    (((and tag (or 'p 'ul 'li 'b)) *** ('a ('^ attrs ...) text ...))
     (list tag attrs text))
    (else #f)))

(test "anded tail pattern" '(1 2)
      (match '(1 2 3) ((and (a ... b) x) a)))

(test "anded search pattern" '(a b c)
      (match '(a (b (c d))) ((and (p *** 'd) x) p)))

(test "joined tail" '(1 2)
      (match '(1 2 3) ((and (a ... b) x) a)))

(test "list ..1" '(a b c)
    (match '(a b c) ((x ..1) x)))

(test "list ..1 failed" #f
    (match '()
      ((x ..1) x)
      (else #f)))

(test "list ..1 with predicate" '(a b c)
    (match '(a b c)
      (((and x (? symbol?)) ..1) x)))

(test "list ..1 with failed predicate" #f
    (match '(a b 3)
      (((and x (? symbol?)) ..1) x)
      (else #f)))

(test-end)
