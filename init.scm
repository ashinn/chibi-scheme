
;; syntax-rules
;; number->string string->number
;; symbol->string string->symbol

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
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (list . args) args)

(define (list-tail ls k)
  (if (eq? k 0)
      ls
      (list-tail (cdr ls) (- k 1))))

(define (list-ref ls k) (car (list-tail ls k)))

(define (eqv? a b) (if (eq? a b) #t (and (flonum? a) (flonum? b) (= a b))))

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
          (car ls)
          (assoc obj (cdr ls)))))

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
  (if (null? lol)
      (map1 proc ls '())
      (mapn proc (cons ls lol) '())))

(define for-each map)

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
         ((lambda (cl)
            (if (compare 'else (car cl))
                (cons (rename 'begin) (cdr cl))
                (if (if (null? (cdr cl)) #t (compare '=> (cadr cl)))
                    (list (rename 'let)
                          (list (list (rename 'tmp) (car cl)))
                          (list (rename 'if) (rename 'tmp)
                                (if (null? (cdr cl))
                                    (rename 'tmp)
                                    (list (caddr cl) (rename 'tmp)))))
                    (list (rename 'if)
                          (car cl)
                          (cons (rename 'begin) (cdr cl))
                          (cons (rename 'cond) (cddr expr))))))
          (cadr expr))))))

(define-syntax quasiquote
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (qq x d)
       (cond
        ((pair? x)
         (cond
          ((eq? 'unquote (car x))
           (if (<= d 0)
               (cadr x)
               (list (rename 'unquote) (qq (cadr x) (- d 1)))))
          ((eq? 'unquote-splicing (car x))
           (if (<= d 0)
               (list (rename 'cons) (qq (car x) d) (qq (cdr x) d))
               (list (rename 'unquote-splicing) (qq (cadr x) (- d 1)))))
          ((eq? 'quasiquote (car x))
           (list (rename 'quasiquote) (qq (cadr x) (+ d 1))))
          ((and (<= d 0) (pair? (car x)) (eq? 'unquote-splicing (caar x)))
           (if (null? (cdr x))
               (cadar x)
               (list (rename 'append) (cadar x) (qq (cdr x) d))))
          (else
           (list (rename 'cons) (qq (car x) d) (qq (cdr x) d)))))
        ((vector? x) (list (rename 'list->vector) (qq (vector->list x) d)))
        ((symbol? x) (list (rename 'quote) x))
        (else x)))
     (qq (cadr expr) 0))))

(define-syntax letrec
  (er-macro-transformer
   (lambda (expr rename compare)
     ((lambda (defs)
        `((,(rename 'lambda) () ,@defs ,@(cddr expr))))
      (map (lambda (x) (cons (rename 'define) x)) (cadr expr))))))

(define-syntax let
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (identifier? (cadr expr))
         `(,(rename 'letrec) ((,(cadr expr)
                               (,(rename 'lambda) ,(map car (caddr expr))
                                ,@(cdddr expr))))
           ,(cons (cadr expr) (map cadr (caddr expr))))
         `((,(rename 'lambda) ,(map car (cadr expr)) ,@(cddr expr))
           ,@(map cadr (cadr expr)))))))

(define-syntax let*
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cadr expr))
         `(,(rename 'begin) ,@(cddr expr))
         `(,(rename 'let) (,(caadr expr))
           (,(rename 'let*) ,(cdadr expr) ,@(cddr expr)))))))

(define-syntax case
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (clause ls)
       (cond
        ((null? ls) #f)
        ((compare 'else (caar ls))
         `(,(rename 'begin) ,@(cdar ls)))
        (else
         (if (and (pair? (caar ls)) (null? (cdaar ls)))
             `(,(rename 'if) (,(rename 'eqv?) ,(rename 'tmp) ',(caaar ls))
               (,(rename 'begin) ,@(cdar ls))
               ,(clause (cdr ls)))
             `(,(rename 'if) (,(rename 'memv) ,(rename 'tmp) ',(caar ls))
               (,(rename 'begin) ,@(cdar ls))
               ,(clause (cdr ls)))))))
     `(let ((,(rename 'tmp) ,(cadr expr)))
        ,(clause (cddr expr))))))

(define-syntax do
  (er-macro-transformer
   (lambda (expr rename compare)
     (let* ((body
             `(,(rename 'begin)
               ,@(cdddr expr)
               (,(rename 'lp)
                ,@(map (lambda (x) (if (pair? (cddr x)) (caddr x) (car x)))
                       (cadr expr)))))
            (check (caddr expr))
            (wrap
             (if (null? (cdr check))
                 `(,(rename 'let) ((,(rename 'tmp) ,(car check)))
                   (,(rename 'if) ,(rename 'tmp)
                    ,(rename 'tmp)
                    ,body))
                 `(,(rename 'if) ,(car check)
                   (,(rename 'begin) ,@(cdr check))
                   ,body))))
       `(,(rename 'let) ,(rename 'lp)
         ,(map (lambda (x) (list (car x) (cadr x))) (cadr expr))
         ,wrap)))))

(define-syntax delay
  (er-macro-transformer
   (lambda (expr rename compare)
     `(,(rename 'make-promise) (,(rename 'lambda) () ,(cadr epr))))))

(define (make-promise thunk)
  (lambda ()
    (let ((computed? #f) (result #f))
      (if (not computed?)
          (begin
            (set! result (thunk))
            (set! computed? #t)))
      result)))

(define (force x) (if (procedure? x) (x) x))

;; booleans

(define (not x) (if x #f #t))
(define (boolean? x) (if (eq? x #t) #t (eq? x #f)))

;; char utils

(define (char-alphabetic? ch) (<= 65 (char->integer (char-upcase ch)) 90))
(define (char-numeric? ch) (<= 48 (char->integer ch) 57))
(define (char-whitespace? ch)
  (if (eq? ch #\space)
      #t
      (if (eq? ch #\tab) #t (if (eq? ch #\newline) #t (eq? ch #\return)))))
(define (char-upper-case? ch) (<= 65 (char->integer ch) 90))
(define (char-lower-case? ch) (<= 97 (char->integer ch) 122))

(define (char=? a b) (= (char->integer a) (char->integer b)))
(define (char<? a b) (< (char->integer a) (char->integer b)))
(define (char>? a b) (> (char->integer a) (char->integer b)))
(define (char<=? a b) (<= (char->integer a) (char->integer b)))
(define (char>=? a b) (>= (char->integer a) (char->integer b)))

(define (char-ci=? a b)
  (= (char->integer (char-downcase a)) (char->integer (char-downcase b))))
(define (char-ci<? a b)
  (< (char->integer (char-downcase a)) (char->integer (char-downcase b))))
(define (char-ci>? a b)
  (> (char->integer (char-downcase a)) (char->integer (char-downcase b))))
(define (char-ci<=? a b)
  (<= (char->integer (char-downcase a)) (char->integer (char-downcase b))))
(define (char-ci>=? a b)
  (>= (char->integer (char-downcase a)) (char->integer (char-downcase b))))

;; string utils

(define (list->string ls)
  (let ((str (make-string (length ls) #\space)))
    (let lp ((ls ls) (i 0))
      (if (pair? ls)
          (begin
            (string-set! str i (car ls))
            (lp (cdr ls) (+ i 1)))))
    str))

(define (string->list str)
  (let lp ((i (- (string-length str) 1)) (res '()))
    (if (< i 0) res (lp (- i 1) (cons (string-ref str i) res)))))

(define (string-fill! str ch)
  (let lp ((i (- (string-length str) 1)))
    (if (>= i 0) (begin (string-set! str i ch) (lp (- i 1))))))

(define (string . args) (list->string args))
(define (string-append . args) (string-concatenate args))
(define (string-copy s) (substring s 0 (string-length s)))

(define (string=? s1 s2) (eq? (string-cmp s1 s2) 0))
(define (string<? s1 s2) (< (string-cmp s1 s2) 0))
(define (string<=? s1 s2) (<= (string-cmp s1 s2) 0))
(define (string>? s1 s2) (> (string-cmp s1 s2) 0))
(define (string>=? s1 s2) (>= (string-cmp s1 s2) 0))

(define (string-ci=? s1 s2) (eq? (string-cmp-ci s1 s2) 0))
(define (string-ci<? s1 s2) (< (string-cmp-ci s1 s2) 0))
(define (string-ci<=? s1 s2) (<= (string-cmp-ci s1 s2) 0))
(define (string-ci>? s1 s2) (> (string-cmp-ci s1 s2) 0))
(define (string-ci>=? s1 s2) (>= (string-cmp-ci s1 s2) 0))

;; math utils

(define (number? x) (if (fixnum? x) #t (flonum? x)))
(define complex? number?)
(define rational? number?)
(define real? number?)
(define exact? fixnum?)
(define inexact? flonum?)
(define (integer? x) (if (fixnum? x) #t (and (flonum? x) (= x (truncate x)))))

(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (= (remainder n 2) 1))

(define (abs x) (if (< x 0) (- x) x))

(define (modulo a b)
  (let ((res (remainder a b)))
    (if (< b 0)
        (if (< res 0) res (- res b))
        (if (> res 0) res (+ res b)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (lcm a b)
  (quotient (* a b) (gcd a b)))

(define (max x . rest)
  (let lp ((hi x) (ls rest))
    (if (null? ls)
        hi
        (lp (if (> (car ls) hi) (car ls) hi) (cdr ls)))))

(define (min x . rest)
  (let lp ((lo x) (ls rest))
    (if (null? ls)
        lo
        (lp (if (< (car ls) lo) (car ls) lo) (cdr ls)))))

(define (real-part z) z)
(define (imag-part z) 0.0)
(define magnitude abs)
(define (angle z) (if (< z 0) 3.141592653589793 0))

;; vector utils

(define (list->vector ls)
  (let ((vec (make-vector (length ls) #f)))
    (let lp ((ls ls) (i 0))
      (if (pair? ls)
          (begin
            (vector-set! vec i (car ls))
            (lp (cdr ls) (+ i 1)))))
    vec))

(define (vector->list vec)
  (let lp ((i (- (vector-length vec) 1)) (res '()))
    (if (< i 0) res (lp (- i 1) (cons (vector-ref vec i) res)))))

(define (vector-fill! str ch)
  (let lp ((i (- (vector-length str) 1)))
    (if (>= i 0) (begin (vector-set! str i ch) (lp (- i 1))))))

(define (vector . args) (list->vector args))

;; I/O utils

(define (char-ready? . o)
  (not (eof-object? (peek-char (if (pair? o) (car o) (current-input-port))))))

(define (load file) (%load file (interaction-environment)))

(define (call-with-input-string str proc)
  (proc (open-input-string str)))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (call-with-input-file file proc)
  (let* ((in (open-input-file file))
         (res (proc in)))
    (close-input-port in)
    res))

(define (call-with-output-file file proc)
  (let* ((out (open-output-file file))
         (res (proc in)))
    (close-output-port in)
    res))

(define (with-input-from-file file thunk)
  (let ((old-in (current-input-port))
        (tmp-in (open-input-file file)))
    (current-input-port tmp-in)
    (let ((res (thunk)))
      (current-input-port old-in)
      res)))

(define (with-output-to-file file thunk)
  (let ((old-out (current-input-port))
        (tmp-out (open-output-file file)))
    (current-input-port tmp-out)
    (let ((res (thunk)))
      (current-output-port old-out)
      res)))

;; values

(define *values-tag* (list 'values))

(define (values . ls)
  (if (and (pair? ls) (null? (cdr ls)))
      (car ls)
      (cons *values-tag* ls)))

(define (call-with-values producer consumer)
  (let ((res (producer)))
    (if (and (pair? res) (eq? *values-tag* (car res)))
        (apply consumer (cdr res))
        (consumer res))))
