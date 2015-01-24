;; init-7.scm -- core library procedures for R7RS
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (cons-source kar kdr source)
  ((lambda (pair)
     (if (pair? source)
         (pair-source-set! pair (pair-source source)))
     pair)
   (cons kar kdr)))

;; basic utils

(define (procedure? x) (if (closure? x) #t (opcode? x)))

(define (length ls)
  (if (list? ls) (length* ls) (error "length: not a list" ls)))

(define (list . args) args)

(define (list-tail ls k)
  (if (eq? k 0)
      ls
      (list-tail (cdr ls) (- k 1))))

(define (list-ref ls k) (car (list-tail ls k)))

(define (append-helper ls res)
  (if (null? ls)
      res
      (append-helper (cdr ls) (append2 (car ls) res))))

(define (append . o)
  (if (null? o)
      '()
      ((lambda (lol)
         (append-helper (cdr lol) (car lol)))
       (reverse o))))

(define (apply proc . args)
  (if (null? args)
      (proc)
      ((lambda (lol)
         (apply1 proc (append2 (reverse (cdr lol)) (car lol))))
       (reverse args))))

;; map with a fast-path for single lists

(define (map proc ls . lol)
  (define (map1 proc ls res)
    (if (pair? ls)
        (map1 proc (cdr ls) (cons (proc (car ls)) res))
        (reverse res)))
  (define (mapn proc lol res)
    (if (every pair? lol)
        (mapn proc
              (map1 cdr lol '())
              (cons (apply proc (map1 car lol '())) res))
        (reverse res)))
  (if (null? lol)
      (map1 proc ls '())
      (mapn proc (cons ls lol) '())))

(define (for-each f ls . lol)
  (define (for1 f ls) (if (pair? ls) (begin (f (car ls)) (for1 f (cdr ls)))))
  (if (null? lol) (for1 f ls) (begin (apply map f ls lol) (if #f #f))))

(define (any pred ls . lol)
  (define (any1 pred ls)
    (if (pair? (cdr ls))
        ((lambda (x) (if x x (any1 pred (cdr ls)))) (pred (car ls)))
        (pred (car ls))))
  (define (anyn pred lol)
    (if (every pair? lol)
        ((lambda (x) (if x x (anyn pred (map cdr lol))))
         (apply pred (map car lol)))
        #f))
  (if (null? lol) (if (pair? ls) (any1 pred ls) #f) (anyn pred (cons ls lol))))

(define (every pred ls . lol)
  (define (every1 pred ls)
    (if (null? (cdr ls))
        (pred (car ls))
        (if (pred (car ls)) (every1 pred (cdr ls)) #f)))
  (if (null? lol)
      (if (pair? ls) (every1 pred ls) #t)
      (not (apply any (lambda xs (not (apply pred xs))) ls lol))))

(define (error msg . args)
  (raise (make-exception 'user msg args #f #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax

(define sc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (make-syntactic-closure mac-env '() (f expr use-env)))))

(define rsc-macro-transformer
  (lambda (f)
    (lambda (expr use-env mac-env)
      (f expr mac-env))))

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

(define-syntax cond
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr))
         (if #f #f)
         ((lambda (cl)
            (if (compare (rename 'else) (car cl))
                (if (pair? (cddr expr))
                    (error "non-final else in cond" expr)
                    (cons (rename 'begin) (cdr cl)))
                (if (if (null? (cdr cl)) #t (compare (rename '=>) (cadr cl)))
                    (list (list (rename 'lambda) (list (rename 'tmp))
                                (list (rename 'if) (rename 'tmp)
                                      (if (null? (cdr cl))
                                          (rename 'tmp)
                                          (list (car (cddr cl)) (rename 'tmp)))
                                      (cons (rename 'cond) (cddr expr))))
                          (car cl))
                    (list (rename 'if)
                          (car cl)
                          (cons (rename 'begin) (cdr cl))
                          (cons (rename 'cond) (cddr expr))))))
          (cadr expr))))))

(define-syntax or
  (er-macro-transformer
   (lambda (expr rename compare)
     (cond ((null? (cdr expr)) #f)
           ((null? (cddr expr)) (cadr expr))
           (else
            (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                  (list (rename 'if) (rename 'tmp)
                        (rename 'tmp)
                        (cons (rename 'or) (cddr expr)))))))))

(define-syntax and
  (er-macro-transformer
   (lambda (expr rename compare)
     (cond ((null? (cdr expr)))
           ((null? (cddr expr)) (cadr expr))
           (else (list (rename 'if) (cadr expr)
                       (cons (rename 'and) (cddr expr))
                       #f))))))

(define-syntax quasiquote
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (qq x d)
       (cond
        ((pair? x)
         (cond
          ((compare (rename 'unquote) (car x))
           (if (<= d 0)
               (cadr x)
               (list (rename 'list) (list (rename 'quote) 'unquote)
                     (qq (cadr x) (- d 1)))))
          ((compare (rename 'unquote-splicing) (car x))
           (if (<= d 0)
               (list (rename 'cons) (qq (car x) d) (qq (cdr x) d))
               (list (rename 'list) (list (rename 'quote) 'unquote-splicing)
                     (qq (cadr x) (- d 1)))))
          ((compare (rename 'quasiquote) (car x))
           (list (rename 'list) (list (rename 'quote) 'quasiquote)
                 (qq (cadr x) (+ d 1))))
          ((and (<= d 0) (pair? (car x))
                (compare (rename 'unquote-splicing) (caar x)))
           (if (null? (cdr x))
               (cadr (car x))
               (list (rename 'append) (cadr (car x)) (qq (cdr x) d))))
          (else
           (list (rename 'cons) (qq (car x) d) (qq (cdr x) d)))))
        ((vector? x) (list (rename 'list->vector) (qq (vector->list x) d)))
        ((if (identifier? x) #t (null? x)) (list (rename 'quote) x))
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
     (if (null? (cdr expr)) (error "empty let" expr))
     (if (null? (cddr expr)) (error "no let body" expr))
     ((lambda (bindings)
        (if (list? bindings) #f (error "bad let bindings"))
        (if (every (lambda (x)
                     (if (pair? x) (if (pair? (cdr x)) (null? (cddr x)) #f) #f))
                   bindings)
            ((lambda (vars vals)
               (if (identifier? (cadr expr))
                   `((,(rename 'lambda) ,vars
                      (,(rename 'letrec) ((,(cadr expr)
                                           (,(rename 'lambda) ,vars
                                            ,@(cdr (cddr expr)))))
                       (,(cadr expr) ,@vars)))
                     ,@vals)
                   `((,(rename 'lambda) ,vars ,@(cddr expr)) ,@vals)))
             (map car bindings)
             (map cadr bindings))
            (error "bad let syntax" expr)))
      (if (identifier? (cadr expr)) (car (cddr expr)) (cadr expr))))))

(define-syntax let*
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr)) (error "empty let*" expr))
     (if (null? (cddr expr)) (error "no let* body" expr))
     (if (null? (cadr expr))
         `(,(rename 'let) () ,@(cddr expr))
         (if (if (list? (cadr expr))
                 (every
                  (lambda (x)
                    (if (pair? x) (if (pair? (cdr x)) (null? (cddr x)) #f) #f))
                  (cadr expr))
                 #f)
             `(,(rename 'let) (,(caar (cdr expr)))
               (,(rename 'let*) ,(cdar (cdr expr)) ,@(cddr expr)))
             (error "bad let* syntax"))))))

(define-syntax case
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (body exprs)
       (cond
        ((null? exprs)
         (rename 'tmp))
        ((compare (rename '=>) (car exprs))
         `(,(cadr exprs) ,(rename 'tmp)))
        (else
         `(,(rename 'begin) ,@exprs))))
     (define (clause ls)
       (cond
        ((null? ls) #f)
        ((compare (rename 'else) (caar ls))
         (body (cdar ls)))
        ((and (pair? (car (car ls))) (null? (cdr (car (car ls)))))
         `(,(rename 'if) (,(rename 'eqv?) ,(rename 'tmp)
                          (,(rename 'quote) ,(car (caar ls))))
           ,(body (cdar ls))
           ,(clause (cdr ls))))
        (else
         `(,(rename 'if) (,(rename 'memv) ,(rename 'tmp)
                          (,(rename 'quote) ,(caar ls)))
           ,(body (cdar ls))
           ,(clause (cdr ls))))))
     `(let ((,(rename 'tmp) ,(cadr expr)))
        ,(clause (cddr expr))))))

(define-syntax do
  (er-macro-transformer
   (lambda (expr rename compare)
     (let* ((body
             `(,(rename 'begin)
               ,@(cdr (cddr expr))
               (,(rename 'lp)
                ,@(map (lambda (x)
                         (if (pair? (cddr x))
                             (if (pair? (cdr (cddr x)))
                                 (error "too many forms in do iterator" x)
                                 (car (cddr x)))
                             (car x)))
                       (cadr expr)))))
            (check (car (cddr expr)))
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

(define-syntax delay-force
  (er-macro-transformer
   (lambda (expr rename compare)
     `(,(rename 'promise) #f (,(rename 'lambda) () ,(cadr expr))))))

(define-syntax delay
  (er-macro-transformer
   (lambda (expr rename compare)
     `(,(rename 'delay-force) (,(rename 'promise) #t ,(cadr expr))))))

(define-syntax define-auxiliary-syntax
  (er-macro-transformer
   (lambda (expr rename compare)
     `(,(rename 'define-syntax) ,(cadr expr)
       (,(rename 'er-macro-transformer)
        (,(rename 'lambda) (expr rename compare)
         (,(rename 'error) "invalid use of auxiliary syntax" ',(cadr expr))))))))

(define-auxiliary-syntax _)
(define-auxiliary-syntax =>)
(define-auxiliary-syntax ...)
(define-auxiliary-syntax else)
(define-auxiliary-syntax unquote)
(define-auxiliary-syntax unquote-splicing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library functions

;; booleans

(define (not x) (if x #f #t))
(define (boolean? x) (if (eq? x #t) #t (eq? x #f)))

;; char utils

(define (char-alphabetic? ch) (<= 65 (char->integer (char-upcase ch)) 90))
(define (char-numeric? ch) (<= 48 (char->integer ch) 57))
(define (char-whitespace? ch)
  (if (eq? ch #\space)
      #t
      (if (eq? ch #\tab) #t (if (eq? ch #\newline)
                                #t
                                (if (eq? ch #\xC0) #f (eq? ch #\return))))))
(define (char-upper-case? ch) (<= 65 (char->integer ch) 90))
(define (char-lower-case? ch) (<= 97 (char->integer ch) 122))

(define (char-cmp op a ls)
  (let lp ((op op) (a (char->integer a)) (ls ls))
    (if (null? ls)
        #t
        (let ((b (char->integer (car ls))))
          (and (op a b) (lp op b (cdr ls)))))))

(define (char=? a . ls) (char-cmp = a ls))
(define (char<? a . ls) (char-cmp < a ls))
(define (char>? a . ls) (char-cmp > a ls))
(define (char<=? a . ls) (char-cmp <= a ls))
(define (char>=? a . ls) (char-cmp >= a ls))

(define (char-cmp-ci op a ls)
  (let lp ((op op) (a (char->integer (char-downcase a))) (ls ls))
    (if (null? ls)
        #t
        (let ((b (char->integer (char-downcase (car ls)))))
          (and (op a b) (lp op b (cdr ls)))))))

(define (char-ci=? a . ls) (char-cmp-ci = a ls))
(define (char-ci<? a . ls) (char-cmp-ci < a ls))
(define (char-ci>? a . ls) (char-cmp-ci > a ls))
(define (char-ci<=? a . ls) (char-cmp-ci <= a ls))
(define (char-ci>=? a . ls) (char-cmp-ci >= a ls))

;; string utils

(define (digit-char n)
  (if (<= n 9)
      (integer->char (+ n (char->integer #\0)))
      (integer->char (+ (- n 10) (char->integer #\a)))))

(define (%number->string num)
  (call-with-output-string (lambda (out) (write num out))))

(define (number->string num . o)
  (cond
   ((not (number? num))
    (error "not a number" num))
   ((if (null? o) #t (eq? 10 (car o)))
    (%number->string num))
   (else
    (let lp ((n (abs num)) (d (car o)) (res '()))
      (if (> n 0)
          (lp (quotient n d) d (cons (digit-char (remainder n d)) res))
          (if (null? res)
              "0"
              (list->string (if (negative? num) (cons #\- res) res))))))))

(define (list->string ls)
  (call-with-output-string
    (lambda (out) (for-each (lambda (ch) (write-char ch out)) ls))))

(define (string->list str . o)
  (cond
   ((null? o)
    (let lp ((i (string-cursor-prev str (string-cursor-end str))) (res '()))
      (if (< i 0)
          res
          (lp (string-cursor-prev str i) (cons (string-cursor-ref str i) res)))))
   (else
    (string->list (apply substring str o)))))

(define (string-fill! str ch . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (let lp ((i (- end 1)))
      (if (>= i start) (begin (string-set! str i ch) (lp (- i 1)))))))

(define (string . args) (list->string args))
(define (string-append . args) (string-concatenate args))

(define (string-cmp-ls op ci? s ls)
  (if (null? ls)
      #t
      (and (op (string-cmp s (car ls) ci?) 0)
           (string-cmp-ls op ci? (car ls) (cdr ls)))))

(define (string=? s . ls) (string-cmp-ls eq? #f s ls))
(define (string<? s . ls) (string-cmp-ls < #f s ls))
(define (string>? s . ls) (string-cmp-ls > #f s ls))
(define (string<=? s . ls) (string-cmp-ls <= #f s ls))
(define (string>=? s . ls) (string-cmp-ls >= #f s ls))

(define (string-ci=? s . ls) (string-cmp-ls eq? #t s ls))
(define (string-ci<? s . ls) (string-cmp-ls < #t s ls))
(define (string-ci>? s . ls) (string-cmp-ls > #t s ls))
(define (string-ci<=? s . ls) (string-cmp-ls <= #t s ls))
(define (string-ci>=? s . ls) (string-cmp-ls >= #t s ls))

;; list utils

(define (make-list n . o)
  (let ((default (if (pair? o) (car o))))
    (let lp ((n n) (res '()))
      (if (<= n 0) res (lp (- n 1) (cons default res))))))

(define (list-copy ls)
  (let lp ((ls ls) (res '()))
    (if (pair? ls)
        (lp (cdr ls) (cons (car ls) res))
        (append (reverse res) ls))))

(define (member obj ls . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (let lp ((ls ls))
      (and (pair? ls) (if (eq obj (car ls)) ls (lp (cdr ls)))))))

(define memv member)

(define (assoc obj ls . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (let assoc ((ls ls))
      (cond ((null? ls) #f)
            ((eq obj (caar ls)) (car ls))
            (else (assoc (cdr ls)))))))

(define (assv obj ls) (assoc obj ls eqv?))

(define (find-tail pred ls)
  (and (pair? ls) (if (pred (car ls)) ls (find-tail pred (cdr ls)))))

(define (find pred ls)
  (cond ((find-tail pred ls) => car) (else #f)))

;; vector utils

(define (vector-copy vec . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec)))
         (res (make-vector (- end start))))
    (do ((i 0 (+ i 1)) (j start (+ j 1))) ((>= j end) res)
      (vector-set! res i (vector-ref vec j)))))

(define (list->vector ls)
  (let ((vec (make-vector (length ls) #f)))
    (let lp ((ls ls) (i 0))
      (if (pair? ls)
          (begin
            (vector-set! vec i (car ls))
            (lp (cdr ls) (+ i 1)))))
    vec))

(define (vector->list vec . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec))))
    (let lp ((i (- end 1)) (res '()))
      (if (< i start) res (lp (- i 1) (cons (vector-ref vec i) res))))))

(define (vector-fill! vec ch . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec))))
    (let lp ((i (- end 1)))
      (if (>= i start) (begin (vector-set! vec i ch) (lp (- i 1)))))))

(define (vector . args) (list->vector args))

;; I/O utils

(define (display x . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (cond ((char? x) (write-char x out))
          ((string? x) (%write-string x #t out))
          (else (write x out)))))

(define (newline . o)
  (write-char #\newline (if (pair? o) (car o) (current-output-port))))

(define (port? x) (or (input-port? x) (output-port? x)))

(define textual-port? port?)

(define (call-with-input-string str proc)
  (let* ((in (open-input-string str))
         (res (proc in)))
    (close-input-port in)
    res))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (let ((res (get-output-string out)))
      (close-output-port out)
      res)))

(define (call-with-input-file file proc)
  (let* ((in (open-input-file file))
         (res (proc in)))
    (close-input-port in)
    res))

(define (call-with-output-file file proc)
  (let* ((out (open-output-file file))
         (res (proc out)))
    (close-output-port out)
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; values

(define *values-tag* (list 'values))

(define (%values ls)
  (if (and (pair? ls) (null? (cdr ls)))
      (car ls)
      (cons *values-tag* ls)))

(define (values . ls) (%values ls))

(define (call-with-values producer consumer)
  (let ((res (producer)))
    (if (and (pair? res) (eq? *values-tag* (car res)))
        (apply consumer (cdr res))
        (consumer res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SRFI-0

(define-syntax cond-expand
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (check x)
       (if (pair? x)
           (case (car x)
             ((and) (every check (cdr x)))
             ((or) (any check (cdr x)))
             ((not) (not (check (cadr x))))
             ((library) (eval `(find-module ',(cadr x)) (%meta-env)))
             (else (error "cond-expand: bad feature" x)))
           (memq (identifier->symbol x) *features*)))
     (let expand ((ls (cdr expr)))
       (cond ((null? ls))  ; (error "cond-expand: no expansions" expr)
             ((not (pair? (car ls))) (error "cond-expand: bad clause" (car ls)))
             ((eq? 'else (identifier->symbol (caar ls)))
              (if (pair? (cdr ls))
                  (error "cond-expand: else in non-final position")
                  `(,(rename 'begin) ,@(cdar ls))))
             ((check (caar ls)) `(,(rename 'begin) ,@(cdar ls)))
             (else (expand (cdr ls))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dynamic-wind

(define %make-point vector)
(define (%point-depth point) (vector-ref point 0))
(define (%point-in point) (vector-ref point 1))
(define (%point-out point) (vector-ref point 2))
(define (%point-parent point) (vector-ref point 3))

(define root-point			; Shared among all state spaces
  (%make-point 0
	      (lambda () (error "winding in to root!"))
	      (lambda () (error "winding out of root!"))
	      #f))

(cond-expand
 (threads)
 (else
  (define %dk
    (let ((dk root-point))
      (lambda o (if (pair? o) (set! dk (car o)) dk))))))

(%dk root-point)

(define (dynamic-wind in body out)
  (in)
  (let ((here (%dk)))
    (%dk (%make-point (+ (%point-depth here) 1)
                     in
                     out
                     here))
    (let ((res (body)))
      (%dk here)
      (out)
      res)))

(define (travel-to-point! here target)
  (cond
   ((eq? here target)
    'done)
   ((< (%point-depth here) (%point-depth target))
    (travel-to-point! here (%point-parent target))
    ((%point-in target)))
   (else
    ((%point-out here))
    (travel-to-point! (%point-parent here) target))))

(define (continuation->procedure cont point)
  (lambda res
    (travel-to-point! (%dk) point)
    (%dk point)
    (cont (%values res))))

(define (call-with-current-continuation proc)
  (%call/cc
   (lambda (cont)
     (proc (continuation->procedure cont (%dk))))))

(define (with-input-from-file file thunk)
  (let ((old-in (current-input-port))
        (tmp-in (open-input-file file)))
    (dynamic-wind
      (lambda () (current-input-port tmp-in))
      (lambda () (let ((res (thunk))) (close-input-port tmp-in) res))
      (lambda () (current-input-port old-in)))))

(define (with-output-to-file file thunk)
  (let ((old-out (current-output-port))
        (tmp-out (open-output-file file)))
    (dynamic-wind
      (lambda () (current-output-port tmp-out))
      (lambda () (let ((res (thunk))) (close-output-port tmp-out) res))
      (lambda () (current-output-port old-out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax-rules

(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((ellipsis-specified? (identifier? (cadr expr)))
           (count 0)
           (_er-macro-transformer (rename 'er-macro-transformer))
           (_lambda (rename 'lambda))      (_let (rename 'let))
           (_begin (rename 'begin))        (_if (rename 'if))
           (_and (rename 'and))            (_or (rename 'or))
           (_eq? (rename 'eq?))            (_equal? (rename 'equal?))
           (_car (rename 'car))            (_cdr (rename 'cdr))
           (_cons (rename 'cons))          (_pair? (rename 'pair?))
           (_null? (rename 'null?))        (_expr (rename 'expr))
           (_rename (rename 'rename))      (_compare (rename 'compare))
           (_quote (rename 'syntax-quote)) (_apply (rename 'apply))
           (_append (rename 'append))      (_map (rename 'map))
           (_vector? (rename 'vector?))    (_list? (rename 'list?))
           (_len (rename'len))             (_length (rename 'length*))
           (_- (rename '-))   (_>= (rename '>=))   (_error (rename 'error))
           (_ls (rename 'ls)) (_res (rename 'res)) (_i (rename 'i))
           (_reverse (rename 'reverse))
           (_vector->list (rename 'vector->list))
           (_list->vector (rename 'list->vector))
           (_cons3 (rename 'cons-source)))
       (define ellipsis (rename (if ellipsis-specified? (cadr expr) '...)))
       (define lits (if ellipsis-specified? (car (cddr expr)) (cadr expr)))
       (define forms (if ellipsis-specified? (cdr (cddr expr)) (cddr expr)))
       (define (next-symbol s)
         (set! count (+ count 1))
         (rename (string->symbol (string-append s (%number->string count)))))
       (define (expand-pattern pat tmpl)
         (let lp ((p (cdr pat))
                  (x (list _cdr _expr))
                  (dim 0)
                  (vars '())
                  (k (lambda (vars)
                       (list _cons (expand-template tmpl vars) #f))))
           (let ((v (next-symbol "v.")))
             (list
              _let (list (list v x))
              (cond
               ((identifier? p)
                (if (any (lambda (l) (compare p l)) lits)
                    (list _and
                          (list _compare v (list _rename (list _quote p)))
                          (k vars))
                    (list _let (list (list p v)) (k (cons (cons p dim) vars)))))
               ((ellipsis? p)
                (cond
                 ((not (null? (cdr (cdr p))))
                  (cond
                   ((any (lambda (x) (and (identifier? x) (compare x ellipsis)))
                         (cddr p))
                    (error "multiple ellipses" p))
                   (else
                    (let ((len (length* (cdr (cdr p))))
                          (_lp (next-symbol "lp.")))
                      `(,_let ((,_len (,_length ,v)))
                         (,_and (,_>= ,_len ,len)
                                (,_let ,_lp ((,_ls ,v)
                                             (,_i (,_- ,_len ,len))
                                             (,_res (,_quote ())))
                                  (,_if (,_>= 0 ,_i)
                                      ,(lp `(,(cddr p) 
                                             (,(car p) ,(car (cdr p))))
                                           `(,_cons ,_ls
                                                    (,_cons (,_reverse ,_res)
                                                            (,_quote ())))
                                           dim
                                           vars
                                           k)
                                      (,_lp (,_cdr ,_ls)
                                            (,_- ,_i 1)
                                            (,_cons3 (,_car ,_ls)
                                                     ,_res
                                                     ,_ls))))))))))
                 ((identifier? (car p))
                  (list _and (list _list? v)
                        (list _let (list (list (car p) v))
                              (k (cons (cons (car p) (+ 1 dim)) vars)))))
                 (else
                  (let* ((w (next-symbol "w."))
                         (_lp (next-symbol "lp."))
                         (new-vars (all-vars (car p) (+ dim 1)))
                         (ls-vars (map (lambda (x)
                                         (next-symbol
                                          (string-append
                                           (symbol->string
                                            (identifier->symbol (car x)))
                                           "-ls")))
                                       new-vars))
                         (once
                          (lp (car p) (list _car w) (+ dim 1) '()
                              (lambda (_)
                                (cons
                                 _lp
                                 (cons
                                  (list _cdr w)
                                  (map (lambda (x l)
                                         (list _cons (car x) l))
                                       new-vars
                                       ls-vars)))))))
                    (list
                     _let
                     _lp (cons (list w v)
                               (map (lambda (x) (list x (list _quote '()))) ls-vars))
                     (list _if (list _null? w)
                           (list _let (map (lambda (x l)
                                             (list (car x) (list _reverse l)))
                                           new-vars
                                           ls-vars)
                                 (k (append new-vars vars)))
                           (list _and (list _pair? w) once)))))))
               ((pair? p)
                (list _and (list _pair? v)
                      (lp (car p)
                          (list _car v)
                          dim
                          vars
                          (lambda (vars)
                            (lp (cdr p) (list _cdr v) dim vars k)))))
               ((vector? p)
                (list _and
                      (list _vector? v)
                      (lp (vector->list p) (list _vector->list v) dim vars k)))
               ((null? p) (list _and (list _null? v) (k vars)))
               (else (list _and (list _equal? v p) (k vars))))))))
       (define (ellipsis-escape? x) (and (pair? x) (compare ellipsis (car x))))
       (define (ellipsis? x)
         (and (pair? x) (pair? (cdr x)) (compare ellipsis (cadr x))))
       (define (ellipsis-depth x)
         (if (ellipsis? x)
             (+ 1 (ellipsis-depth (cdr x)))
             0))
       (define (ellipsis-tail x)
         (if (ellipsis? x)
             (ellipsis-tail (cdr x))
             (cdr x)))
       (define (all-vars x dim)
         (let lp ((x x) (dim dim) (vars '()))
           (cond ((identifier? x)
                  (if (any (lambda (lit) (compare x lit)) lits)
                      vars
                      (cons (cons x dim) vars)))
                 ((ellipsis? x) (lp (car x) (+ dim 1) (lp (cddr x) dim vars)))
                 ((pair? x) (lp (car x) dim (lp (cdr x) dim vars)))
                 ((vector? x) (lp (vector->list x) dim vars))
                 (else vars))))
       (define (free-vars x vars dim)
         (let lp ((x x) (free '()))
           (cond
            ((identifier? x)
             (if (and (not (memq x free))
                      (cond ((assq x vars) => (lambda (cell) (>= (cdr cell) dim)))
                            (else #f)))
                 (cons x free)
                 free))
            ((pair? x) (lp (car x) (lp (cdr x) free)))
            ((vector? x) (lp (vector->list x) free))
            (else free))))
       (define (expand-template tmpl vars)
         (let lp ((t tmpl) (dim 0))
           (cond
            ((identifier? t)
             (cond
              ((find (lambda (v) (compare t (car v))) vars)
               => (lambda (cell)
                    (if (<= (cdr cell) dim)
                        t
                        (error "too few ...'s"))))
              (else
               (list _rename (list _quote t)))))
            ((pair? t)
             (cond
              ((ellipsis-escape? t)
               (list _quote
                     (if (pair? (cdr t))
                         (if (pair? (cddr t)) (cddr t) (cadr t))
                         (cdr t))))
              ((ellipsis? t)
               (let* ((depth (ellipsis-depth t))
                      (ell-dim (+ dim depth))
                      (ell-vars (free-vars (car t) vars ell-dim)))
                 (cond
                  ((null? ell-vars)
                   (error "too many ...'s"))
                  ((and (null? (cdr (cdr t))) (identifier? (car t)))
                   ;; shortcut for (var ...)
                   (lp (car t) ell-dim))
                  (else
                   (let* ((once (lp (car t) ell-dim))
                          (nest (if (and (null? (cdr ell-vars))
                                         (identifier? once)
                                         (eq? once (car vars)))
                                    once ;; shortcut
                                    (cons _map
                                          (cons (list _lambda ell-vars once)
                                                ell-vars))))
                          (many (do ((d depth (- d 1))
                                     (many nest
                                           (list _apply _append many)))
                                    ((= d 1) many))))
                     (if (null? (ellipsis-tail t))
                         many ;; shortcut
                         (list _append many (lp (ellipsis-tail t) dim))))))))
              (else (list _cons3 (lp (car t) dim) (lp (cdr t) dim) (list _quote t)))))
            ((vector? t) (list _list->vector (lp (vector->list t) dim)))
            ((null? t) (list _quote '()))
            (else t))))
       (list
        _er-macro-transformer
        (list _lambda (list _expr _rename _compare)
              (list
               _car
               (cons
                _or
                (append
                 (map
                  (lambda (clause) (expand-pattern (car clause) (cadr clause)))
                  forms)
                 (list
                  (list _cons
                        (list _error "no expansion for"
                              (list (rename 'strip-syntactic-closures) _expr))
                        #f)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional syntax

(define-syntax syntax-error
  (er-macro-transformer
   (lambda (expr rename compare)
     (apply error (cdr expr)))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var val) ...) . body)
     (let () (define var val) ... . body))))

(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* opt-ls () . body)
     (begin . body))
    ((let-optionals* (op . args) vars . body)
     (let ((tmp (op . args)))
       (let-optionals* tmp vars . body)))
    ((let-optionals* tmp ((var default) . rest) . body)
     (let ((var (if (pair? tmp) (car tmp) default))
           (tmp2 (if (pair? tmp) (cdr tmp) '())))
       (let-optionals* tmp2 rest . body)))
    ((let-optionals* tmp tail . body)
     (let ((tail tmp)) . body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exceptions

;; We wrap continuable objects in specially marked exceptions.
(define (raise-continuable exn)
  (raise (make-exception 'continuable "" exn #f #f)))

(cond-expand
 (threads
  (define (%with-exception-handler handler thunk)
    (let* ((old (thread-parameters))
           (new (cons (cons current-exception-handler handler) old)))
      (dynamic-wind
        (lambda () (thread-parameters-set! new))
        thunk
        (lambda () (thread-parameters-set! old))))))
 (else
  (define (%with-exception-handler handler thunk)
    (let ((old (current-exception-handler)))
      (dynamic-wind
        (lambda () (current-exception-handler handler))
        thunk
        (lambda () (current-exception-handler old)))))))

(define (with-exception-handler handler thunk)
  (letrec ((orig-handler (current-exception-handler))
           (self (lambda (exn)
                   (%with-exception-handler orig-handler
                     (lambda ()
                       (cond
                        ((and (exception? exn)
                              (eq? 'continuable (exception-kind exn)))
                         (handler (exception-irritants exn)))
                        (else
                         (handler exn)
                         (error "exception handler returned"))))))))
    (%with-exception-handler self thunk)))

(define-syntax protect
  (syntax-rules ()
    ((protect (var clause ...) e1 e2 ...)
     (let ((orig-handler (current-exception-handler)))
       (call-with-current-continuation
        (lambda (protect-k)
          (with-exception-handler
           (lambda (condition)
             (let ((var condition))  ; clauses may set! var
               (protect-k
                (protect-aux (raise-continuable condition) clause ...))))
           (lambda () e1 e2 ...))))))))

(define-syntax protect-aux
  (syntax-rules (else =>)
    ((protect-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((protect-aux reraise (test => result))
     (let ((temp test))
       (if temp (result temp) reraise)))
    ((protect-aux reraise (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp (result temp) (protect-aux reraise clause1 clause2 ...))))
    ((protect-aux reraise (test))
     (or test reraise))
    ((protect-aux reraise (test) clause1 clause2 ...)
     (or test (protect-aux reraise clause1 clause2 ...)))
    ((protect-aux reraise (test result1 result2 ...))
     (if test (begin result1 result2 ...) reraise))
    ((protect-aux reraise (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (protect-aux reraise clause1 clause2 ...)))))

;; Similar to unwind-protect, but allows escape via continuations
;; assuming they'll return, whereas exceptions are assumed to not return.
(define (with-exception-protect thunk final)
  (let* ((finalized? #f)
         (run-finalize
          (lambda ()
            (cond ((not finalized?)
                   (set! finalized? #t)
                   (final))))))
    (protect (exn (else (run-finalize) (raise exn)))
      (let ((res (thunk)))
        (run-finalize)
        res))))

(define-syntax exception-protect
  (syntax-rules ()
    ((exception-protect expr final)
     (with-exception-protect (lambda () expr)  (lambda () final)))))

(define (eval x . o)
  (let ((thunk (compile x (if (pair? o) (car o) (interaction-environment)))))
    (if (procedure? thunk) (thunk) (raise thunk))))

(define (load file . o)
  (let* ((env (if (pair? o) (car o) (interaction-environment)))
         (len (string-length file))
         (ext *shared-object-extension*)
         (ext-len (string-length ext)))
    (cond
     ((and (> len ext-len 0) (equal? ext (substring file (- len ext-len))))
      (%load file env))
     (else
      (let ((old-env (current-environment)))
        (dynamic-wind
          (lambda () (set-current-environment! env))
          (lambda ()
            (call-with-input-file file
              (lambda (in)
                (set-port-line! in 1)
                (let lp ()
                  (let ((x (read in)))
                    (cond
                     ((not (eof-object? x))
                      (eval x env)
                      (lp))))))))
          (lambda () (set-current-environment! old-env))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; promises

(cond-expand
 (auto-force
  )
 (else
  (define (promise done? proc)
    (list (cons done? proc)))
  (define (promise-done? x) (car (car x)))
  (define (promise-value x) (cdr (car x)))
  (define (promise-update! new old)
    (set-car! (car old) (promise-done? new))
    (set-cdr! (car old) (promise-value new))
    (set-car! new (car old)))
  (define (force promise)
    (if (promise-done? promise)
        (promise-value promise)
        (let ((promise* ((promise-value promise))))
          (if (not (promise-done? promise))
            (promise-update! promise* promise))
          (force promise))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math utils

(cond-expand
 (complex
  (define (exact-complex? x)
    (and (%complex? x) (exact? (complex-real x)) (exact? (complex-imag x)))))
 (else
  (define (exact-complex? x) #f)))

(cond-expand
 (ratios
  (cond-expand
   (complex
    (define (exact? x)
      (if (fixnum? x)
          #t
          (if (bignum? x) #t (if (ratio? x) #t (exact-complex? x))))))
   (else
    (define (exact? x) (if (fixnum? x) #t (if (bignum? x) #t (ratio? x))))))
  (define (numerator x)
    (if (ratio? x)
        (ratio-numerator x)
        (if (inexact? x)
            (exact->inexact (numerator (inexact->exact x)))
            x)))
  (define (denominator x)
    (if (exact? x)
        (if (ratio? x) (ratio-denominator x) 1)
        (if (integer? x)
            1.0
            (exact->inexact (denominator (inexact->exact x)))))))
 (else
  (cond-expand
   (complex
    (define (exact? x)
      (if (fixnum? x) #t (if (bignum? x) #t (exact-complex? x)))))
   (else
    (define (exact? x) (if (fixnum? x) #t (bignum? x)))))
  (define (numerator x)
    (if (integer? x) x (numerator (* x 10))))
  (define (denominator x)
    (if (exact? x)
        1
        (let lp ((x x) (r 1.0)) (if (integer? x) r (lp (* x 10) (* r 10))))))))

(cond-expand
 (complex
  (define (inexact? x)
    (if (flonum? x) #t (and (%complex? x) (not (exact-complex? x))))))
 (else (define inexact? flonum?)))
(define (exact-integer? x) (if (fixnum? x) #t (bignum? x)))
(define (integer? x)
  (if (exact-integer? x) #t (and (flonum? x) (= x (truncate x)))))
(define (number? x) (if (inexact? x) #t (exact? x)))
(define complex? number?)
(cond-expand
 (complex (define (real? x) (and (number? x) (not (%complex? x)))))
 (else (define real? number?)))
(define (rational? x)
  (and (real? x) (= x x) (not (= x (+ x (if (positive? x) 1 -1))))))

(define (eqv? a b) (if (eq? a b) #t (and (number? a) (equal? a b))))

(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (not (= (remainder n 2) 0)))

(define (abs x) (if (< x 0) (- x) x))

(define (modulo a b)
  (let ((res (remainder a b)))
    (if (< b 0)
        (if (<= res 0) res (+ res b))
        (if (>= res 0) res (+ res b)))))

(define (gcd2 a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

(define (gcd . args)
  (if (null? args)
      0
      (let lp ((x (car args)) (ls (cdr args)))
        (if (null? ls) x (lp (gcd2 x (car ls)) (cdr ls))))))

(define (lcm2 a b)
  (abs (quotient (* a b) (gcd a b))))

(define (lcm . args)
  (if (null? args)
      1
      (let lp ((x (car args)) (ls (cdr args)))
        (if (null? ls) x (lp (lcm2 x (car ls)) (cdr ls))))))

(define (max x . rest)
  (define (~max hi ls)
    (if (null? ls)
        (exact->inexact hi)
        (~max (if (> (car ls) hi) (car ls) hi) (cdr ls))))
  (if (inexact? x)
      (~max x rest)
      (let lp ((hi x) (ls rest))
        (cond ((null? ls) hi)
              ((inexact? (car ls)) (~max hi ls))
              (else (lp (if (> (car ls) hi) (car ls) hi) (cdr ls)))))))

(define (min x . rest)
  (define (~min lo ls)
    (if (null? ls)
        (exact->inexact lo)
        (~min (if (< (car ls) lo) (car ls) lo) (cdr ls))))
  (if (inexact? x)
      (~min x rest)
      (let lp ((lo x) (ls rest))
        (cond ((null? ls) lo)
              ((inexact? (car ls)) (~min lo ls))
              (else (lp (if (< (car ls) lo) (car ls) lo) (cdr ls)))))))

(cond-expand
 (complex
  (define (real-part z) (if (%complex? z) (complex-real z) z))
  (define (imag-part z) (if (%complex? z) (complex-imag z) 0))
  (define (magnitude z)
    (sqrt (+ (* (real-part z) (real-part z))
             (* (imag-part z) (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-rectangular x y)
    (+ x (* y (sqrt -1))))
  (define (make-polar r phi)
    (make-rectangular (* r (cos phi)) (* r (sin phi)))))
 (else
  (define (real-part z) z)
  (define (imag-part z) 0.0)
  (define magnitude abs)
  (define (angle z) (if (< z 0) 3.141592653589793 0))))

(define (log x . o)
  (if (pair? o) (/ (ln x) (ln (car o))) (ln x)))

(define (atan y . o)
  (define (inf? z) (if (= +inf.0 z) #t (= -inf.0 z)))
  (if (null? o)
      (atan1 y)
      (let ((x (exact->inexact (car o))))
        (if (and (inf? x) (inf? y))
            (* (if (< y 0) -1 1) (if (= x -inf.0) 3 1) 0.7853981633974483)
            (if (negative? x)
                (if (or (negative? y) (eqv? y -0.0))
                    (- (atan1 (/ y x)) 3.141592653589793)
                    (- 3.141592653589793 (atan1 (/ y (- x)))))
                (if (and (zero? x) (zero? y))
                    (* (if (eqv? y -0.0) -1 1)
                       (if (eqv? x -0.0) 3.141592653589793 x))
                    (atan1 (/ y x))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string cursors

(define string-cursor<? <)
(define string-cursor<=? <=)
(define string-cursor>? >)
(define string-cursor>=? >=)
(define string-cursor=? =)

(define (string-cursor-start s) 0)

(define (string-copy str . o)
  (apply substring str (if (pair? o) o '(0))))

(cond-expand
 (full-unicode
  (define string-cursor-end string-size))
 (else
  (define (string-index->offset str i) i)
  (define (string-offset->index str off) off)
  (define string-size string-length)
  (define substring-cursor substring)
  (define string-cursor-end string-length)
  (define string-cursor-ref string-ref)
  (define (string-cursor-next s i) (+ i 1))
  (define (string-cursor-prev s i) (- i 1))))
