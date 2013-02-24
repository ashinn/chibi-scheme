;; parse.scm -- Parser Combinators
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse stream type
;;
;; Abstraction to treat ports as proper streams so that we can
;; backtrack from previous states.  A single Parse-Stream record
;; represents a single buffered chunk of text.

(define-record-type Parse-Stream
  (%make-parse-stream filename port buffer cache offset prev-char tail)
  parse-stream?
  ;; The file the data came from, for debugging and error reporting.
  (filename parse-stream-filename)
  ;; The underlying port.
  (port parse-stream-port)
  ;; A vector of characters read from the port.  We use a vector
  ;; rather than a string for guaranteed O(1) access.
  (buffer parse-stream-buffer)
  ;; A vector of caches corresponding to parser successes or failures
  ;; starting from the corresponding char.  Currently each cache is
  ;; just an alist, optimized under the assumption that the number of
  ;; possible memoized parsers is relatively small.  Note that
  ;; memoization is only enabled explicitly.
  (cache parse-stream-cache)
  ;; The current offset of filled characters in the buffer.
  ;; If offset is non-zero, (vector-ref buffer (- offset 1)) is
  ;; valid.
  (offset parse-stream-offset parse-stream-offset-set!)
  ;; The previous char before the beginning of this Parse-Stream.
  ;; Used for line/word-boundary checks.
  (prev-char parse-stream-prev-char)
  ;; The successor Parse-Stream chunk, created on demand and filled
  ;; from the same port.
  (tail %parse-stream-tail %parse-stream-tail-set!))

;; We want to balance avoiding reallocating buffers with avoiding
;; holding many memoized values in memory.
(define default-buffer-size 256)

(define (make-parse-stream filename . o)
  (let ((port (if (pair? o) (car o) (open-input-file filename)))
        (len (if (and (pair? o) (pair? (cdr o))) (cadr o) default-buffer-size)))
    (%make-parse-stream
     filename port (make-vector len #f) (make-vector len '()) 0 #f #f)))

(define (file->parse-stream filename)
  (make-parse-stream filename (open-input-file filename)))

(define (string->parse-stream str)
  (make-parse-stream #f (open-input-string str)))

(define (parse-stream-tail source)
  (or (%parse-stream-tail source)
      (let* ((len (vector-length (parse-stream-buffer source)))
             (tail (%make-parse-stream (parse-stream-filename source)
                                       (parse-stream-port source)
                                       (make-vector len #f)
                                       (make-vector len '())
                                       0
                                       (parse-stream-last-char source)
                                       #f)))
        (%parse-stream-tail-set! source tail)
        tail)))

(define (parse-stream-fill! source i)
  (let ((off (parse-stream-offset source))
        (buf (parse-stream-buffer source)))
    (if (<= off i)
        (do ((off off (+ off 1)))
            ((> off i) (parse-stream-offset-set! source off))
          (vector-set! buf off (read-char (parse-stream-port source))))
        #f)))

(define (parse-stream-start? source i)
  (and (zero? i) (not (parse-stream-prev-char source))))

(define (parse-stream-end? source i)
  (eof-object? (parse-stream-ref source i)))

(define (parse-stream-ref source i)
  (parse-stream-fill! source i)
  (vector-ref (parse-stream-buffer source) i))

(define (parse-stream-last-char source)
  (let ((buf (parse-stream-buffer source)))
    (let lp ((i (parse-stream-offset source)))
      (if (negative? i)
          (parse-stream-prev-char source)
          (let ((ch (vector-ref buf i)))
            (if (eof-object? ch)
                (lp (- i 1))
                ch))))))

(define (parse-stream-char-before source i)
  (if (> i (parse-stream-offset source))
      (parse-stream-ref source (- i 1))
      (parse-stream-prev-char source)))

(define (parse-stream-max-char source)
  (let ((buf (parse-stream-buffer source)))
    (let lp ((i (parse-stream-offset source)))
      (if (or (negative? i)
              (char? (vector-ref buf i)))
          i
          (lp (- i 1))))))

(define (parse-stream-next-source source i)
  (if (>= (+ i 1) (vector-length (parse-stream-buffer source)))
      (parse-stream-tail source)
      source))

(define (parse-stream-next-index source i)
  (if (>= (+ i 1) (vector-length (parse-stream-buffer source)))
      0
      (+ i 1)))

(define (parse-stream-close source)
  (close-input-port (parse-stream-port source)))

(define (vector-substring vec start end)
  (let ((res (make-string (- end start))))
    (do ((i start (+ i 1)))
        ((= i end) res)
      (string-set! res (- i start) (vector-ref vec i)))))

(define (parse-stream-substring s0 i0 s1 i1)
  (cond
   ((eq? s0 s1)
    (parse-stream-fill! s0 i1)
    (vector-substring (parse-stream-buffer s0) i0 i1))
   (else
    (let lp ((s (parse-stream-next-source s0))
             (res (list (vector-substring (parse-stream-buffer s0) i0 i1))))
      (let ((buf (parse-stream-buffer s)))
        (cond
         ((eq? s s1)
          (apply string-append
                 (reverse (cons (vector-substring buf 0 i1) res))))
         (else
          (lp (parse-stream-next-source s)
              (cons (vector-substring buf 0 (vector-length buf)) res)))))))))

(define (parse-stream-cache-cell s i f)
  (assv f (vector-ref (parse-stream-cache s) i)))

(define (parse-stream-cache-set! s i f x)
  (let ((cache (vector-ref (parse-stream-cache s) i)))
    (cond
     ((assv f cache)
      => (lambda (cell)
           ;; prefer longer matches
           (if (and (pair? (cdr cell)) (< (cadr (cddr cell)) i))
               (set-cdr! cell x))))
     (else
      (vector-set! (parse-stream-cache s) i (cons (cons f x) cache))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the parser interface

(define (call-with-parse f source index sk . o)
  (let ((p (if (string? source) (string->parse-stream source) source))
        (fk (if (pair? o) (car o) (lambda () #f))))
    (f p index sk fk)))

(define (parse f source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (call-with-parse f source index (lambda (r s i fk) r))))

(define (parse-fully f source . o)
  (let ((p (if (string? source) (string->parse-stream source) source))
        (index (if (pair? o) (car o) 0)))
    (call-with-parse
     f p index
     (lambda (r s i fk) (if (parse-stream-end? s i) r (fk)))
     (lambda ()
       (let ((i (parse-stream-max-char p)))
         (error "incomplete parse, max char" i (parse-stream-ref p i)))))))

(define (parse-fold f kons knil source . o)
  (let lp ((p (if (string? source) (string->parse-stream source) source))
           (index (if (pair? o) (car o) 0))
           (acc knil))
    (f p index (lambda (r s i fk) (lp s i (kons r acc))) (lambda () acc))))

(define (parse->list f source . o)
  (reverse (apply parse-fold cons '() f source o)))

(define (parse-fully->list f source . o)
  (let lp ((p (if (string? source) (string->parse-stream source) source))
           (index (if (pair? o) (car o) 0))
           (acc '()))
    (f p index
       (lambda (r s i fk)
         (if (eof-object? r) (reverse acc) (lp s i (cons r acc))))
       (lambda () (error "incomplete parse")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic parsing combinators

(define parse-epsilon
  (lambda (source index sk fk)
    (sk #t source index fk)))

(define parse-anything
  (lambda (source index sk fk)
    (if (parse-stream-end? source index)
        (fk)
        (sk (parse-stream-ref source index)
            (parse-stream-next-source source index)
            (parse-stream-next-index source index)
            fk))))

(define parse-nothing
  (lambda (source index sk fk)
    (fk)))

(define (parse-or f . o)
  (if (null? o)
      f
      (let ((g (apply parse-or o)))
        (lambda (source index sk fk)
          (let ((fk2 (lambda () (g source index sk fk))))
            (f source index sk fk2))))))

(define (parse-and f g)
  (lambda (source index sk fk)
    (f source index (lambda (r s i fk) (g source index sk fk)) fk)))

(define (parse-not f)
  (lambda (source index sk fk)
    (f source index (lambda (r s i fk) (fk)) (lambda () (sk #t source index fk)))))

(define (parse-seq . o)
  (cond
   ((null? o)
    parse-epsilon)
   ((null? (cdr o))
    (let ((f (car o)))
      (lambda (s i sk fk)
        (f s i (lambda (r s i fk) (sk (list r) s i fk)) fk))))
   (else
    (let* ((f (car o))
           (o (cdr o))
           (g (car o))
           (o (cdr o))
           (g (if (pair? o)
                  (apply parse-seq g o)
                  (lambda (s i sk fk)
                    (g s i (lambda (r s i fk) (sk (list r) s i fk)) fk)))))
      (lambda (source index sk fk)
        (f source
           index
           (lambda (r s i fk)
             (g s i (lambda (r2 s i fk)
                      (let ((r2 (if (eq? r ignored-value) r2 (cons r r2))))
                        (sk r2 s i fk)))
                fk))
           fk))))))

(define (maybe-parse-seq f . o)
  (if (null? o) f (apply parse-seq f o)))

(define (parse-optional f . o)
  (if (pair? o)
      (parse-optional (apply parse-seq f o))
      (lambda (source index sk fk)
        (f source index sk (lambda () (sk #f source index fk))))))

(define ignored-value (list 'ignore))

(define (parse-repeat f . o)
  (let ((lo (if (pair? o) (car o) 0))
        (hi (and (pair? o) (pair? (cdr o)) (cadr o))))
    (lambda (source0 index0 sk fk)
      (let repeat ((source source0) (index index0) (fk fk) (j 0) (res '()))
        (let ((fk (if (>= j lo)
                      (lambda () (sk (reverse res) source index fk))
                      fk)))
          (if (and hi (= j hi))
              (sk (reverse res) source index fk)
              (f source
                 index
                 (lambda (r s i fk) (repeat s i fk (+ j 1) (cons r res)))
                 fk)))))))

(define (parse-repeat+ f)
  (parse-repeat f 1))

(define (parse-map f proc)
  (lambda (source index sk fk)
    (f source index (lambda (res s i fk) (sk (proc res) s i fk)) fk)))

(define (parse-map-substring f . o)
  (let ((proc (if (pair? o) (car o) (lambda (res) res))))
    (lambda (source index sk fk)
      (f source
         index
         (lambda (res s i fk)
           (sk (proc (parse-stream-substring source index s i)) s i fk))
         fk))))

(define (parse-ignore f)
  (parse-map f (lambda (res) ignored-value)))

(define (parse-assert f check?)
  (lambda (source index sk fk)
    (f source
       index
       (lambda (res s i fk)
         (if (check? res) (sk res s i fk) (fk)))
       fk)))

(define (parse-atomic f)
  (lambda (source index sk fk)
    (f source index (lambda (res s i fk2) (sk res s i fk)) fk)))

(define (parse-commit f)
  (lambda (source index sk fk)
    (f source index (lambda (res s i fk) (sk res s i (lambda () #f))) fk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boundary checks

(define parse-beginning
  (lambda (source index sk fk)
    (if (parse-stream-start? source index)
        (sk #t source index fk)
        (fk))))

(define parse-end
  (lambda (source index sk fk)
    (if (parse-stream-end? source index)
        (sk #t source index fk)
      (fk))))

(define parse-beginning-of-line
  (lambda (source index sk fk)
    (let ((before (parse-stream-char-before source index)))
      (if (or (not before) (eqv? #\newline before))
          (sk #t source index fk)
          (fk)))))

(define parse-end-of-line
  (lambda (source index sk fk)
    (if (or (parse-stream-end? source index)
            (eqv? #\newline (parse-stream-ref source index)))
        (sk #t source index fk)
        (fk))))

(define (char-word? ch)
  (or (char-alphabetic? ch) (eqv? ch #\_)))

(define parse-beginning-of-word
  (lambda (source index sk fk)
    (let ((before (parse-stream-char-before source index)))
      (if (and (or (not before) (not (char-word? before)))
               (not (parse-stream-end? source index))
               (char-word? (parse-stream-ref source index)))
          (sk #t source index fk)
          (fk)))))

(define parse-end-of-word
  (lambda (source index sk fk)
    (let ((before (parse-stream-char-before source index)))
      (if (and before
               (char-word? before)
               (or (parse-stream-end? source index)
                   (not (char-word? (parse-stream-ref source index)))))
          (sk #t source index fk)
          (fk)))))

(define (parse-word . o)
  (let ((word (if (pair? o) (car o) (parse-token char-word?))))
    (lambda (source index sk fk)
      (parse-seq parse-beginning-of-word
                 word
                 parse-end-of-word))))

(define (parse-word+ . o)
  (let ((pred (if (pair? o)
                  (lambda (ch) (and (char-word? ch) ((car o) ch)))
                  char-word?)))
    (parse-word (parse-token pred))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constant parsers

(define (parse-char-pred pred)
  (lambda (source index sk fk)
    (let ((ch (parse-stream-ref source index)))
      (if (and (char? ch) (pred ch))
          (sk ch
              (parse-stream-next-source source index)
              (parse-stream-next-index source index)
              fk)
          (fk)))))

(define (x->char-predicate x)
  (cond
   ((char? x)
    (lambda (ch) (eqv? ch x)))
   ((char-set? x)
    (lambda (ch) (and (char? ch) (char-set-contains? x ch))))
   ((procedure? x)
    (lambda (ch) (and (char? ch) (x ch))))
   (else
    (error "don't know how to handle char predicate" x))))

(define (parse-char x)
  (parse-char-pred (x->char-predicate x)))

(define (parse-not-char x)
  (let ((pred (x->char-predicate x)))
    (parse-char-pred (lambda (ch) (not (pred ch))))))

(define (parse-string x)
  (parse-map (apply parse-seq (map parse-char (string->list x)))
             list->string))

(define (parse-token x)
  ;; (parse-map (parse-repeat+ (parse-char x)) list->string)
  ;; Tokens are atomic - we don't want to split them at any point in
  ;; the middle - so the implementation is slightly more complex than
  ;; the above.  With a sane grammar the result would be the same
  ;; either way, but this provides a useful optimization.
  (let ((f (parse-char x)))
    (lambda (source0 index0 sk fk)
      (let lp ((source source0) (index index0))
        (f source
           index
           (lambda (r s i fk) (lp s i))
           (lambda ()
             (if (and (eq? source source0) (eqv? index index0))
                 (fk)
                 (sk (parse-stream-substring source0 index0 source index)
                     source index fk))))))))

;; We provide a subset of SRE syntax, optionally interspersed with
;; existing parsers.  These are just translated directly into parser
;; combinators.  A future version may translate pieces into a
;; non-backtracking engine where possible.
(define (parse-sre x)
  (cond
   ((procedure? x)  ; an embedded parser
    x)
   ((or (char? x) (char-set? x))
    (parse-char x))
   ((string? x)
    (parse-string x))
   ((null? x)
    parse-epsilon)
   ((list? x)
    (case (car x)
      ((: seq) (apply parse-seq (map parse-sre (cdr x))))
      ((or) (apply parse-or (map parse-sre (cdr x))))
      ((and) (apply parse-and (map parse-sre (cdr x))))
      ((not) (apply parse-not (map parse-sre (cdr x))))
      ((*) (parse-repeat (apply maybe-parse-seq (map parse-sre (cdr x)))))
      ((+) (parse-repeat+ (apply maybe-parse-seq (map parse-sre (cdr x)))))
      ((?) (apply parse-optional (map parse-sre (cdr x))))
      ((=> ->) (apply maybe-parse-seq (map parse-sre (cddr x))))
      ((word) (apply parse-word (cdr x)))
      ((word+) (apply parse-word+ (cdr x)))
      (else (error "unknown sre list parser" x))))
   (else
    (case x
      ((any) parse-anything)
      ((nonl) (parse-char (lambda (ch) (not (eqv? ch #\newline)))))
      ((space whitespace) (parse-char char-whitespace?))
      ((digit numeric) (parse-char char-numeric?))
      ((alpha alphabetic) (parse-char char-alphabetic?))
      ((alnum alphanumeric)
       (parse-char-pred (lambda (ch) (or (char-alphabetic? ch) (char-numeric? ch)))))
      ((lower lower-case) (parse-char char-lower-case?))
      ((upper upper-case) (parse-char char-upper-case?))
      ((word) (parse-word))
      ((bow) parse-beginning-of-word)
      ((eow) parse-end-of-word)
      ((bol) parse-beginning-of-line)
      ((eol) parse-end-of-line)
      ((bos) parse-beginning)
      ((eos) parse-end)
      (else (error "unknown sre parser" x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delayed combinators for self-referentiality

(define-syntax parse-lazy
  (syntax-rules ()
    ((parse-lazy f)
     (let ((g (delay f)))
       (lambda (source index sk fk)
         ((force g) source index sk fk))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memoization wrapper for packrat-like parsing

;; debugging
(define *procedures* '())
(define (procedure-name f)
  (cond ((assq f *procedures*) => cdr) (else #f)))
(define (procedure-name-set! f name)
  (set! *procedures* (cons (cons f name) *procedures*)))

(define (parse-memoize name f)
  (if (not (procedure-name f)) (procedure-name-set! f name))
  (lambda (source index sk fk)
    (cond
     ((parse-stream-cache-cell source index f)
      => (lambda (cell)
           (if (cdr cell)
               (apply sk (append (cdr cell) (list fk)))
               (fk))))
     (else
      (f source
         index
         (lambda (res s i fk)
           (parse-stream-cache-set! source index f (list res s i))
           (sk res s i fk))
         (lambda ()
           (if (not (pair? (parse-stream-cache-cell source index f)))
               (parse-stream-cache-set! source index f #f))
           (fk)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntactic sugar

;; The four basic interfaces are grammar, define-grammar, and their
;; unmemoized variants grammar/unmemoized and
;; define-grammar/unmemoized.  This is optimized for the common case -
;; generally you want to memoize grammars, and may or may not want to
;; memoize the smaller lexical components.

(define-syntax grammar/unmemoized
  (syntax-rules ()
    ((grammar/unmemoized init (rule (clause . action) ...) ...)
     (letrec ((rule (parse-or (grammar-clause clause . action) ...))
              ...)
       init))))

(define-syntax grammar
  (syntax-rules ()
    ((grammar init (rule (clause . action) ...) ...)
     (letrec ((rule
               (parse-memoize
                'rule
                (parse-or (grammar-clause clause . action) ...)))
              ...)
       init))))

(define-syntax define-grammar/unmemoized
  (syntax-rules ()
    ((define-grammar/unmemoized name (rule (clause . action) ...) ...)
     (begin
       (define rule (parse-or (grammar-clause clause . action) ...))
       ...
       (define name (list (cons 'rule rule) ...))))))

(define-syntax define-grammar
  (syntax-rules ()
    ((define-grammar name (rule (clause . action) ...) ...)
     (begin
       (define rule
         (parse-memoize 'rule (parse-or (grammar-clause clause . action) ...)))
       ...
       (define name (list (cons 'rule rule) ...))))))

;; Most of the implementation goes into how we parse a single grammar
;; clause.  This is hard to read if you're not used to CPS macros.

(define-syntax grammar-clause
  (syntax-rules ()
    ((grammar-clause clause . action)
     (grammar-extract clause () (grammar-action action)))))

(define-syntax grammar-extract
  (syntax-rules (unquote -> => : seq * + ? or and)
    ;; Named patterns
    ((grammar-extract (-> name pattern) bindings k)
     (grammar-extract pattern bindings (grammar-bind name k)))
    ((grammar-extract (-> name pattern ...) bindings k)
     (grammar-extract (: pattern ...) bindings (grammar-bind name k)))
    ;; Allow => as an alias for -> for SRE compatibility.
    ((grammar-extract (=> name pattern) bindings k)
     (grammar-extract pattern bindings (grammar-bind name k)))
    ((grammar-extract (=> name pattern ...) bindings k)
     (grammar-extract (: pattern ...) bindings (grammar-bind name k)))
    ((grammar-extract ,name bindings k)
     (grammar-bind name k (parse-sre name) bindings))
    ;; Walk container patterns.
    ((grammar-extract (: x y ...) bindings k)
     (grammar-extract x bindings (grammar-map parse-seq (y ...) () k)))
    ((grammar-extract (* x) bindings k)
     (grammar-extract x bindings (grammar-map parse-repeat () () k)))
    ((grammar-extract (* x y ...) bindings k)
     (grammar-extract (: x y ...) bindings (grammar-map parse-repeat () () k)))
    ((grammar-extract (+ x) bindings k)
     (grammar-extract x bindings (grammar-map parse-repeat+ () () k)))
    ((grammar-extract (+ x y ...) bindings k)
     (grammar-extract (: x y ...) bindings (grammar-map parse-repeat+ () () k)))
    ((grammar-extract (? x y ...) bindings k)
     (grammar-extract x bindings (grammar-map parse-optional (y ...) () k)))
    ((grammar-extract (or x y ...) bindings k)
     (grammar-extract x bindings (grammar-map parse-or (y ...) () k)))
    ((grammar-extract (and x y ...) bindings k)
     (grammar-extract x bindings (grammar-map parse-and (y ...) () k)))
    ;; Anything else is an implicitly quasiquoted SRE
    ((grammar-extract pattern bindings (k ...))
     (k ... (parse-sre `pattern) bindings))))

(define-syntax grammar-map
  (syntax-rules ()
    ((grammar-map f () (args ...) (k ...) x bindings)
     (k ... (f args ... x) bindings))
    ((grammar-map f (y . rest) (args ...) k x bindings)
     (grammar-extract y bindings (grammar-map f rest (args ... x) k)))))

(define-syntax grammar-action
  (syntax-rules (=>)
    ((grammar-action () parser bindings)
     ;; By default just return the result.
     (grammar-action (=> (lambda (r s i fk) r)) parser bindings))
    ((grammar-action (=> receiver) parser ((var tmp) ...))
     ;; Explicit => handler.
     (lambda (source index sk fk)
       (let ((tmp #f) ...)
         (parser source
                 index
                 (lambda (r s i fk)
                   (sk (receiver r s i fk) s i fk))
                 fk))))
    ((grammar-action (action-expr) parser ())
     ;; Fast path - no named variables.
     (let ((f parser))
       (lambda (source index sk fk)
         (f source index (lambda (r s i fk) (sk action-expr s i fk)) fk))))
    ((grammar-action (action-expr) parser ((var tmp) ...))
     (lambda (source index sk fk)
       (let ((tmp #f) ...)
         ;; TODO: Precompute static components of the parser.
         ;; We need to bind fresh variables on each parse, so some
         ;; components must be reified in this scope.
         (parser source
                 index
                 (lambda (r s i fk)
                   (sk (let ((var tmp) ...) action-expr) s i fk))
                 fk))))))
