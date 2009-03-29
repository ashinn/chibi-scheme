
;; define set! let let* letrec lambda if cond case delay and or begin do
;; quote quasiquote unquote unquote-splicing define-syntax let-syntax
;; letrec-syntax syntax-rules eqv? eq? equal? not boolean? number?
;; complex? real? rational? integer? exact? inexact? = < > <= >= zero?
;; positive? negative? odd? even? max min + * - / abs quotient remainder
;; modulo gcd lcm numerator denominator floor ceiling truncate round
;; rationalize exp log sin cos tan asin acos atan sqrt expt
;; make-rectangular make-polar real-part imag-part magnitude angle
;; exact->inexact inexact->exact number->string string->number pair? cons
;; car cdr set-car! set-cdr! caar cadr cdar cddr caaar caadr cadar caddr
;; cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr
;; caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
;; null? list? list length append reverse list-tail list-ref memq memv
;; member assq assv assoc symbol? symbol->string string->symbol char?
;; char=? char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>?
;; char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
;; char-upper-case? char-lower-case? char->integer integer->char
;; char-upcase char-downcase string? make-string string string-length
;; string-ref string-set! string=? string-ci=? string<? string>?
;; string<=? string>=? string-ci<? string-ci>? string-ci<=? string-ci>=?
;; substring string-append string->list list->string string-copy
;; string-fill! vector? make-vector vector vector-length vector-ref
;; vector-set! vector->list list->vector vector-fill! procedure? apply
;; map for-each force call-with-current-continuation values
;; call-with-values dynamic-wind scheme-report-environment
;; null-environment call-with-input-file call-with-output-file
;; input-port? output-port? current-input-port current-output-port
;; with-input-from-file with-output-to-file open-input-file
;; open-output-file close-input-port close-output-port read read-char
;; peek-char eof-object? char-ready? write display newline write-char
;; load eval

;; provide c[ad]{2,4}r

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; (define (caaar x) (car (car (car x))))
;; (define (caadr x) (car (car (cdr x))))
;; (define (cadar x) (car (cdr (car x))))
;; (define (caddr x) (car (cdr (cdr x))))
;; (define (cdaar x) (cdr (car (car x))))
;; (define (cdadr x) (cdr (car (cdr x))))
;; (define (cddar x) (cdr (cdr (car x))))
;; (define (cdddr x) (cdr (cdr (cdr x))))

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

(define-syntax let
  (lambda (expr use-env mac-env)
    (cons (cons 'lambda (cons (map car (cadr expr)) (cddr expr)))
          (map cadr (cadr expr)))))

(define-syntax letrec
  (lambda (expr use-env mac-env)
    (list
     (cons 'lambda
           (cons '()
                 (append (map (lambda (x) (cons 'define x)) (cadr expr))
                         (cddr expr)))))))

(define-syntax or
  (sc-macro-transformer
   (lambda (expr use-env)
     (if (null? (cdr expr))
         #f
         (if (null? (cddr expr))
             (make-syntactic-closure use-env '() (cadr expr))
             (list 'let (list (list 'tmp (make-syntactic-closure use-env '() (cadr expr))))
                   (list 'if 'tmp
                         'tmp
                         (make-syntactic-closure use-env '() (cons 'or (cddr expr))))))))))

;; math

;; (define (abs x) (if (< x 0) (- x) x))

;; (define (gcd a b)
;;   (if (= b 0)
;;       a
;;       (gcd b (modulo a b))))

;; (define (lcm a b)
;;   (quotient (* a b) (gcd a b)))

