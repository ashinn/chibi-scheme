
;; cond case delay do
;; quasiquote let-syntax
;; letrec-syntax syntax-rules not boolean? number?
;; complex? real? rational? integer? exact? inexact?
;; positive? negative? odd? even? max min quotient remainder
;; modulo numerator denominator floor ceiling truncate round
;; rationalize expt
;; make-rectangular make-polar real-part imag-part magnitude angle
;; exact->inexact inexact->exact number->string string->number
;; symbol->string string->symbol
;; char-alphabetic? char-numeric? char-whitespace?
;; char-upper-case? char-lower-case? char->integer integer->char
;; char-upcase char-downcase make-string string string-length
;; string=? string-ci=? string<? string>?
;; string<=? string>=? string-ci<? string-ci>? string-ci<=? string-ci>=?
;; substring string-append string->list list->string string-copy
;; string-fill! vector vector-length
;; vector->list list->vector vector-fill! procedure? apply
;; map for-each force call-with-current-continuation values
;; call-with-values dynamic-wind scheme-report-environment
;; null-environment call-with-input-file call-with-output-file
;; current-input-port current-output-port
;; with-input-from-file with-output-to-file open-input-file
;; open-output-file close-input-port close-output-port
;; peek-char char-ready?

;; provide c[ad]{2,4}r

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

;; (define (caaaar x) (car (car (car (car x)))))
;; (define (caaadr x) (car (car (car (cdr x)))))
;; (define (caadar x) (car (car (cdr (car x)))))
;; (define (caaddr x) (car (car (cdr (cdr x)))))
;; (define (cadaar x) (car (cdr (car (car x)))))
;; (define (cadadr x) (car (cdr (car (cdr x)))))
;; (define (caddar x) (car (cdr (cdr (car x)))))
;; (define (cadddr x) (car (cdr (cdr (cdr x)))))
;; (define (cdaaar x) (cdr (car (car (car x)))))
;; (define (cdaadr x) (cdr (car (car (cdr x)))))
;; (define (cdadar x) (cdr (car (cdr (car x)))))
;; (define (cdaddr x) (cdr (car (cdr (cdr x)))))
;; (define (cddaar x) (cdr (cdr (car (car x)))))
;; (define (cddadr x) (cdr (cdr (car (cdr x)))))
;; (define (cdddar x) (cdr (cdr (cdr (car x)))))
;; (define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (list . args) args)

(define (list-tail ls k)
  (if (zero? k)
      ls
      (list-tail (cdr ls) (- k 1))))

(define (list-ref ls k) (car (list-tail ls k)))

(define eqv? equal?)

(define (member obj ls)
  (if (null? ls)
      #f
      (if (equal? obj (car ls))
          ls
          (member obj (cdr ls)))))

(define memv member)

(define (assoc obj ls)
  (if (null? ls)
      #f
      (if (equal? obj (caar ls))
          ls
          (member obj (cdr ls)))))

(define assv assoc)

(define (append-reverse a b)
  (if (pair? a)
      (append-reverse (cdr a) (cons (car a) b))
      b))

(define (append a b)
  (append-reverse (reverse a) b))

(define (apply proc . args)
  (if (null? args)
      (proc)
      ((lambda (lol)
         (apply1 proc (append (reverse (cdr lol)) (car lol))))
       (reverse args))))

;; map with a fast-path for single lists

(define (map proc ls . lol)
  (if (null? lol)
      (map1 proc ls '())
      (mapn proc (cons ls lol) '())))

(define (map1 proc ls res)
  (if (pair? ls)
      (map1 proc (cdr ls) (cons (proc (car ls)) res))
      (reverse res)))

(define (mapn proc lol res)
  (if (null? (car lol))
      (reverse res)
      (mapn proc
            (map1 cdr lol '())
            (cons (apply1 proc (map1 car lol '())) res))))

;; math utilities

(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

;; syntax

(define sc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure mac-env '() (f expr use-env)))))

(define rsc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure use-env '() (f expr mac-env)))))

(define er-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      ((lambda (rename compare) (f expr rename compare))
       ((lambda (renames)
          (lambda (identifier)
            ((lambda (cell)
               (if cell
                   (cdr cell)
                   ((lambda (name)
                      (set! renames (cons (cons identifier name) renames))
                      name)
                    (make-syntactic-closure mac-env '() identifier))))
             (assq identifier renames))))
        '())
       (lambda (x y) (identifier=? use-env x use-env y))))))

(define-syntax letrec
  (er-macro-transformer
   (lambda (expr rename compare)
     (list
      (cons (rename 'lambda)
            (cons '()
                  (append (map (lambda (x) (cons (rename 'define) x)) (cadr expr))
                          (cddr expr))))))))

(define-syntax let
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (identifier? (cadr expr))
         (list (rename 'letrec)
               (list (list (cadr expr)
                           (cons (rename 'lambda)
                                 (cons (map car (caddr expr))
                                       (cdddr expr)))))
               (cons (cadr expr) (map cadr (caddr expr))))
         (cons (cons (rename 'lambda) (cons (map car (cadr expr)) (cddr expr)))
               (map cadr (cadr expr)))))))

(define-syntax let*
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cadr expr))
         (cons (rename 'begin) (cddr expr))
         (list (rename 'let)
               (list (caadr expr))
               (cons (rename 'let*) (cons (cdadr expr) (cddr expr))))))))

(define-syntax or
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr))
         #f
         (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
               (list (rename 'if) (rename 'tmp)
                     (rename 'tmp)
                     (cons (rename 'or) (cddr expr))))))))

(define-syntax and
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr))
         #t
         (if (null? (cddr expr))
             (cadr expr)
             (list (rename 'if) (cadr expr)
                   (cons (rename 'and) (cddr expr))
                   #f))))))

(define-syntax cond
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr))
         #f
         (let ((cl (cadr expr)))
           (if (eq? 'else (car cl))
               (cons (rename 'begin) (cdr cl))
               (if (if (null? (cdr cl)) #t (eq? '=> (cadr cl)))
                   (list (rename 'let)
                         (list (list (rename 'tmp) (car cl)))
                         (list (rename 'if) (rename 'tmp)
                               (if (null? (cdr cl))
                                   (rename 'tmp)
                                   (list (caddr cl) (rename 'tmp)))))
                   (list (rename 'if)
                         (car cl)
                         (cons (rename 'begin) (cdr cl))
                         (cons (rename 'cond) (cddr expr))))))))))

(define-syntax quasiquote
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (qq x d)
       (if (pair? x)
           (if (eq? 'unquote (car x))
               (if (<= d 0)
                   (cadr x)
                   (list (rename 'unquote) (qq (cadr x) (- d 1))))
               (if (eq? 'unquote-splicing (car x))
                   (if (<= d 0)
                       (list (rename 'cons) (qq (car x) d) (qq (cdr x) d))
                       (list (rename 'unquote-splicing) (qq (cadr x) (- d 1))))
                   (if (eq? 'quasiquote (car x))
                       (list (rename 'quasiquote) (qq (cadr x) (+ d 1)))
                       (if (and (<= d 0)
                                (pair? (car x))
                                (eq? 'unquote-splicing (caar x)))
                           (list (rename 'append)
                                 (cadar x)
                                 (qq (cdr x) d))
                           (list (rename 'cons)
                                 (qq (car x) d)
                                 (qq (cdr x) d))))))
           (if (vector? x)
               (list (rename 'list->vector) (qq (vector->list x) d))
               (if (symbol? x)
                   (list (rename 'quote) x)
                   x))))
     (qq (cadr expr) 0))))

;; char utils

;; (define (char=? a b) (= (char->integer a) (char->integer b)))
;; (define (char<? a b) (< (char->integer a) (char->integer b)))
;; (define (char>? a b) (> (char->integer a) (char->integer b)))
;; (define (char<=? a b) (<= (char->integer a) (char->integer b)))
;; (define (char>=? a b) (>= (char->integer a) (char->integer b)))

;; (define (char-ci=? a b)
;;   (= (char->integer (char-downcase a)) (char->integer (char-downcase b))))
;; (define (char-ci<? a b)
;;   (< (char->integer (char-downcase a)) (char->integer (char-downcase b))))
;; (define (char-ci>? a b)
;;   (> (char->integer (char-downcase a)) (char->integer (char-downcase b))))
;; (define (char-ci<=? a b)
;;   (<= (char->integer (char-downcase a)) (char->integer (char-downcase b))))
;; (define (char-ci>=? a b)
;;   (>= (char->integer (char-downcase a)) (char->integer (char-downcase b))))

;; vector utils

(define (list->vector ls)
  (let ((vec (make-vector (length ls))))
    (let lp ((ls ls) (i 0))
      (if (pair? ls)
          (begin
            (vector-set! vec i (car ls))
            (lp (cdr ls) (+ i 1)))))
    vec))

(define (vector->list vec)
  (let lp ((i (- (vector-length vec) 1)) (res '()))
    (if (< i 0)
        res
        (lp (- i 1) (cons (vector-ref vec i) res)))))

;; math

;; (define (abs x) (if (< x 0) (- x) x))

;; (define (gcd a b)
;;   (if (= b 0)
;;       a
;;       (gcd b (modulo a b))))

;; (define (lcm a b)
;;   (quotient (* a b) (gcd a b)))

(define (load file) (%load file (interaction-environment)))

