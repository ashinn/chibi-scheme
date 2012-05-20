;; highlight.scm -- source code highlighting library
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Library for highlighting source code in different
;;> languages.  Currently supports Scheme, C and Assembly.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-concatenate-reverse ls)
  (string-concatenate (reverse ls)))

(define (reverse-list->string ls)
  (list->string (reverse ls)))

;;> Returns an sxml structure representing the code from source
;;> with various language constructs wrapped in highlighting
;;> forms.  @var{source} should be a string or port.  The
;;> language to highlight for is auto-detected.

(define (highlight source)
  (let ((str (if (string? source) source (port->string source))))
    ((highlighter-for (highlight-detect-language str)) str)))

;;> Attempst to auto-detect which language @var{str} is code
;;> for, and returns a symbol representing that language.

(define (highlight-detect-language str)
  (cond
   ((guard (exn (else #f))
      (call-with-input-string str
        (lambda (in) (do ((x #f (read in))) ((eof-object? x)))))
      #t)
    'scheme)
   (else
    'c)))

;;> Return a procedure for highlighting the given language.

(define (highlighter-for language)
  (case language
    ((scheme) highlight-scheme)
    ((asm) highlight-assembly)
    ((none) (lambda (x) x))
    (else highlight-c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define highlight-themes
  '((light
     (keyword      . "#800080")
     (type         . "#008000")
     (function     . "#0000FF")
     (variable     . "#B8860B")
     (comment      . "#FF0000")
     (string       . "#BC8F8F")
     (attribute    . "#FF5000")
     (preprocessor . "#FF00FF")
     (builtin      . "#FF00FF")
     (character    . "#0055AA")
     (syntaxerror  . "#FF0000")
     (diff-deleted . "#5F2121")
     (diff-added   . "#215F21")
     )))

(define highlight-paren-styles
  ;;'("#BAFFFF" "#FFCACA" "#FFFFBA" "#CACAFF" "#CAFFCA" "FFBAFF")
  '("#AAAAAA" "#888888" "#666666" "#444444" "#222222" "#000000"))

;;> Returns a string representing the CSS needed for the output
;;> of @var{highlight}.  This should be included in a referenced
;;> CSS file, or in a @var{<script>} section in the generated in
;;> the generated HTML output.

(define (highlight-style . theme)
  (string-concatenate
   (append
    (map
     (lambda (x)
       (if (and (list? x) (= 3 (length x)))
           (string-append
            "." (symbol->string (car x)) " { color: " (cadr x)
            "; background-color: " (car (cddr x)) "; }\n")
           (string-append
            "." (symbol->string (car x)) " { color: "
            (if (pair? (cdr x)) (cadr x) (cdr x))
            "; background-color: inherit; }\n")))
     (cond ((assq (and (pair? theme) (car theme)) highlight-themes) => cdr)
           (else (cdar highlight-themes))))
    (map
     (lambda (s i)
       (string-append
        ;;"span.paren" (number->string i)
        ;;":hover { color: inherit; background-color: " s "; }\n"
        "span.paren" (number->string i)
        " { color: " s "; background-color: inherit; }\n"))
     highlight-paren-styles
     (cdr (iota (+ 1 (length highlight-paren-styles))))))))

(define (highlight-class class x)
  `(span (^ (class . ,class)) ,@(if (list? x) x (list x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-whitespace in)
  (let lp ((res '()))
    (if (char-whitespace? (peek-char in))
        (lp (cons (read-char in) res))
        (reverse-list->string res))))

(define (read-to-whitespace in res)
  (let ((c (peek-char in)))
    (cond
     ((or (eof-object? c) (char-whitespace? c))
      (reverse-list->string res))
     (else
      (read-to-whitespace in (cons (read-char in) res))))))

(define (read-escaped in term ls)
  (let ((c (read-char in)))
    (cond
     ((eof-object? c) (reverse-list->string ls))
     ((eqv? c term) (reverse-list->string (cons c ls)))
     ((eqv? c #\<) (read-escaped in term `(#\; #\t #\l #\& ,@ls)))
     ;;((eqv? c #\>) (read-escaped in term `(#\; #\t #\g #\& ,@ls)))
     ((eqv? c #\&) (read-escaped in term `(#\; #\p #\m #\a #\& ,@ls)))
     ;;((eqv? c #\\) (read-escaped in term (cons (read-char in) (cons c ls))))
     (else (read-escaped in term (cons c ls))))))

(define (read-to-eol in ls)
  (let ((c (read-char in)))
    (cond
     ((eof-object? c) (reverse-list->string ls))
     ((eqv? c #\newline) (reverse-list->string (cons c ls)))
     (else (read-to-eol in (cons c ls))))))

(define (html-escape str)
  (call-with-input-string str (lambda (in) (read-escaped in #f '()))))

(define (collect str res)
  (if (pair? str) (cons (reverse-list->string str) res) res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (highlight-scheme-delimiter? ch)
  (or (eof-object? ch)
      (char-whitespace? ch)
      (memq ch '(#\; #\# #\( #\) #\[ #\] #\{ #\} #\' #\` #\, #\"))))

(define (highlight-scheme-definition? id)
  (memq id '(define define-syntax define-module define-class
             define-record define-record-type)))

(define (highlight-scheme-syntax? id)
  (memq id '(if lambda define set! cond case let let* letrec letrec*
             let-values let-values* let-optionals let-optionals*
             let-keywords let-keywords* and-let* rec receive do
             loop rxmatch-cond rxmatch-case begin when unless
             match match-lambda match-let match-let* dotimes dolist
             quote quasiquote unquote unquote-splicing error errorf
             define-syntax let-syntax letrec-syntax syntax-rules
             syntax-case parameterize module library require
             require-extension use use-modules import import-immutable
             define-module select-module provide autoload export
             only except rename prefix include include-shared
             condition-case guard cond-expand for with to by
             in-list in-lists in-string in-string-reverse
             in-vector in-vector-reverse in-file listing appending
             summing multpliying up-from down-from else
             )))

;;> Highlighter for Scheme source code.

(define (highlight-scheme source)
  (let ((in (if (string? source) (open-input-string source) source)))
    (define (read-identifier ls)
      (let ((c (peek-char in)))
        (cond
         ((highlight-scheme-delimiter? c)
          (reverse-list->string ls))
         (else
          (read-char in)
          (read-identifier (cons c ls))))))
    (define (hash-mark)
      (let ((c (read-char in)))
        (case c
          ((#\;)
           (highlight-class "comment" (highlight 0 '(#\; #\#) '())))
          ((#\\)
           (highlight-class "string"
                            (read-identifier (list (read-char in) #\\ #\#))))
          (else
           (string-append "#" (if (char? c) (string c) ""))))))
    (define (highlight n str res)
      (let ((c (read-char in)))
        (if (eof-object? c)
            (reverse (collect str res))
            (case c
              ((#\;)
               (let lp ((ls '()))
                 (let ((ls (cons (read-to-eol in (list #\;)) ls)))
                   (cond
                    ((eqv? #\; (peek-char in))
                     (lp ls))
                    (else
                     (highlight n
                                '()
                                (cons (highlight-class
                                       "comment"
                                       (string-concatenate-reverse ls))
                                      (collect str res))))))))
              ((#\")
               (let ((s (read-escaped in #\" (list #\"))))
                 (highlight n
                            '()
                            (cons (highlight-class "string" s)
                                  (collect str res)))))
              ((#\()
               ;;(highlight-start
               ;; (string->symbol
               ;;  (string-append
               ;;   "paren"
               ;;   (number->string
               ;;    (+ 1 (modulo n (length highlight-paren-styles))))))
               ;;out)
               (let ((res (collect (cons #\( str) res)))
                 (if (highlight-scheme-delimiter? (peek-char in))
                     (highlight (+ n 1) '() res)
                     (let* ((id (read-identifier '()))
                            (sym (string->symbol id)))
                       (cond
                        ((highlight-scheme-definition? sym)
                         (let* ((res (cons (highlight-class "keyword" id) res))
                                (res (cons (read-whitespace in) res))
                                (res (if (eqv? #\( (peek-char in))
                                         (cons (string (read-char in)) res)
                                         res)))
                           (highlight
                            (+ n 1)
                            '()
                            (cons
                             (highlight-class "function" (read-identifier '()))
                             res))))
                        ((highlight-scheme-syntax? sym)
                         (highlight (+ n 1)
                                    '()
                                    (cons (highlight-class "keyword" id) res)))
                        (else
                         (highlight (+ n 1) '() (cons id res))))))))
              ((#\))
               (cond
                ((zero? n)
                 (highlight n
                            '()
                            (cons (highlight-class "syntaxerror" ")")
                                  (collect str res))))
                (else
                 ;;(highlight-end 'paren
                 (highlight (- n 1) (cons c str) res))))
              ((#\#)
               (highlight n '() (cons (hash-mark) (collect str res))))
              (else
               (cond
                ((highlight-scheme-delimiter? c)
                 (highlight n (cons c str) res))
                (else
                 (let ((id (read-identifier (list c))))
                   (highlight n '() (cons `(span ,id) (collect str res)))))))))))
    (highlight 0 '() '())))

(define (highlight-c-keyword? id)
  (memq id '(asm break case catch const_cast continue default delete
             do dynamic_cast else explicit export false for friend goto
             if mutable namespace new operator private protected public
             register reinterpret_cast return sizeof static_cast switch
             template this throw true try typedef typeid typename using
             virtual while)))

(define (highlight-c-type? id)
  (memq id '(auto bool char class const double enum extern float inline int long
             short signed static struct union unsigned void volatile wchar_t
             sexp sexp_uint_t sexp_sint_t)))

;;> Highlighter for C source code.

(define (highlight-c source)
  (let ((in (if (string? source) (open-input-string source) source)))
    (define (char-c-initial? c)
      (and (char? c) (or (char-alphabetic? c) (eqv? c #\_) (eqv? c #\$))))
    (define (char-c-identifier? c)
      (and (char? c) (or (char-c-initial? c) (char-numeric? c))))
    (define (read-identifier in ls)
      (let ((c (peek-char in)))
        (if (char-c-identifier? c)
            (read-identifier in (cons (read-char in) ls))
            (reverse-list->string ls))))
    (define (highlight-identifier id)
      (let ((sym (string->symbol id)))
        (cond
         ((highlight-c-keyword? sym)
          (highlight-class "keyword" id))
         ((highlight-c-type? sym)
          (highlight-class "type" id))
         (else
          id))))
    (define (highlight-line res)
      (highlight
       '()
       (cond
        ((eqv? #\# (peek-char in))
         (read-char in)
         (let* ((res (cons (read-whitespace in) (cons "#" res)))
                (id (read-identifier in '()))
                (res (cons (read-whitespace in)
                           (cons (highlight-class "preprocessor" id) res))))
           (case (string->symbol id)
            ((define)
             (cons (highlight-class "function" (read-to-whitespace in '())) res))
            ((include import)
             (cons (highlight-class "string" (read-to-whitespace in '())) res))
            (else
             res))))
        ((char-c-initial? (peek-char in))
         ;; line beginning w/ an identifier is probably a
         ;; function declaration
         (let ((id1 (read-identifier in '())))
           (cond
            ((eqv? #\: (peek-char in))
             (cons (highlight-class "function" id1) res))
            (else
             (let lp ((decls '())
                      (id id1))
               (let ((space (read-whitespace in)))
                 (cond
                  ((char-c-initial? (peek-char in))
                   (lp (cons space (cons id decls))
                       (read-identifier in '())))
                  ((eqv? #\( (peek-char in))
                   `(,space
                     ,(highlight-class "function" id)
                     ,(highlight-class "type" (reverse decls))
                     ,@res))
                  (else
                   `(,space ,id ,@decls ,@res)))))))))
        (else
         res))))
    (define (highlight str res)
      (let ((c (read-char in)))
        (if (eof-object? c)
            (reverse (collect str res))
            (case c
              ((#\/)
               (case (peek-char in)
                 ((#\/)
                  (highlight
                   '()
                   (cons (highlight-class "comment" (read-to-eol in '(#\/ #\/)))
                         (collect str res))))
                 ((#\*)
                  (let lp ((ls (cons (read-char in) '(#\/))))
                    (let ((c (read-char in)))
                      (if (not (and (eqv? (car ls) #\*) (eqv? c #\/)))
                          (lp (cons c ls))
                          (highlight
                           '()
                           (cons (highlight-class "comment"
                                                  (reverse-list->string ls))
                                 (collect str res)))))))
                 (else
                  (highlight (cons c str) res))))
              ((#\" #\')
               (let ((res (collect str res))
                     (s (read-escaped in c (list c))))
                 (highlight '() (cons (highlight-class "string" s) res))))
              ((#\newline)
               (highlight-line (collect (cons #\newline str) res)))
              (else
               (cond
                ((char-c-initial? c)
                 (highlight
                  '()
                  (cons
                   (let ((id (read-identifier in (list c))))
                     (if (eqv? #\: (peek-char in))
                         (highlight-class "function" id)
                         (highlight-identifier id)))
                   (collect str res))))
                (else
                 (highlight (cons c str) res))))))))
    (highlight-line '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> Highlighter for Assembly source code.

(define (highlight-assembly source)
  (let ((in (if (string? source) (open-input-string source) source)))
    (define (char-asm-initial? c)
      (and (char? c) (or (char-alphabetic? c) (memv c '(#\_ #\$ #\.)))))
    (define (char-asm-identifier? c)
      (and (char? c) (or (char-asm-initial? c) (char-numeric? c))))
    (define (read-identifier in ls)
      (let ((c (peek-char in)))
        (if (char-asm-identifier? c)
            (read-identifier (cons (read-char in) ls))
            (reverse-list->string ls))))
    (define (highlight str res)
      (let ((c (read-char in)))
        (cond
         ((eof-object? c)
          (reverse (collect str res)))
         (else
          (case c
            ((#\newline)
             (highlight-line (collect str res)))
            ((#\")
             (let ((s (read-escaped in c (list c))))
               (highlight
                '()
                (cons (highlight-class "string" s) (collect str res)))))
            ((#\%)
             (highlight
              '()
              (cons (highlight-class "variable" (read-identifier in (list c)))
                    (collect str res))))
            ((#\;)
             (highlight
              '()
              (cons (highlight-class "comment" (read-to-eol in (list c)))
                    (collect str res))))
            (else
             (highlight (cons c str) res)))))))
    (define (highlight-line res) 
      (cond
       ((eof-object? (peek-char in))
        (highlight '() res))
       ((char-asm-initial? (peek-char in))
        (let ((id (read-identifier in '())))
          (highlight
           '()
           (cons
            (if (eqv? #\: (peek-char in))
                (highlight-class "function" id)
                (highlight-class "keyword" id))
            res))))
       ((eqv? #\tab (peek-char in))
        (highlight
         '()
         (cons (highlight-class "keyword" (read-identifier in '()))
               (cons "\t" res))))
       (else
        (highlight '() res))))
    (highlight-line '())))
