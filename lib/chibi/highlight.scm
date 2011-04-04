;; highlight.scm -- source code highlighting library
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-concatenate-reverse ls)
  (string-concatenate (reverse ls)))

(define (reverse-list->string ls)
  (list->string (reverse ls)))

(define (highlight-detect-language str)
  (cond
   ((or (string-contains str "(define")
        (string-contains str "(eval")
        (string-contains str "(set")
        (string-contains str "(string")
        (string-contains str "(let")
        (string-contains str "(lambda (")
        )
    'scheme)
   ((string-contains str "\tmovl\t")
    'asm)
   (else
    'c)))

(define (highlight source)
  (let ((str (if (string? source) source (port->string source))))
    ((case (highlight-detect-language str)
       ((scheme) highlight-scheme)
       ((asm) highlight-assembly)
       ((c) highlight-c))
     str)))

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
     (diff-added   . "#215F21"))))

(define highlight-paren-styles
  ;;'("#BAFFFF" "#FFCACA" "#FFFFBA" "#CACAFF" "#CAFFCA" "FFBAFF")
  '("#AAAAAA" "#888888" "#666666" "#444444" "#222222" "#000000"))

(define (highlight-style . theme)
  (string-concatenate
   (append
    (map
     (lambda (x)
       (string-append
        "." (symbol->string (car x)) " { color: " (cdr x)
        "; background-color: inherit; }\n"))
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
     (cdr (iota (+ 1 (length highlight-paren-styles)))))
    )))

(define (highlight-start class out)
  (display "<span class=\"" out)
  (display class out)
  (display "\">" out))

(define (highlight-end class out)
  (display "</span>" out))

(define (highlight-class class str out)
  (highlight-start class out)
  (display str out)
  (highlight-end class out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-whitespace in)
  (let lp ((res '()))
    (if (char-whitespace? (peek-char in))
        (lp (cons (read-char in) res))
        (reverse-list->string res))))

(define (read-to-whitespace in res)
  (cond
   ((char-whitespace? (peek-char in))
    (reverse-list->string res))
   (else
    (read-to-whitespace in (cons (read-char in) res)))))

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

(define (html-escape str)
  (call-with-input-string str (lambda (in) (read-escaped in #f '()))))

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
             require-extension use use-modules import export
             define-module select-module provide autoload
             condition-case guard cond-expand for with to by
             in-list in-lists in-string in-string-reverse
             in-vector in-vector-reverse in-file listing appending
             summing multpliying up-from down-from else
             )))

(define (highlight-scheme source)
  (let ((in (if (string? source) (open-input-string source) source)))
    (call-with-output-string
     (lambda (out)
       (define (read-to-eol ls)
         (let ((c (read-char in)))
           (cond
            ((eof-object? c) (reverse-list->string ls))
            ((eqv? c #\newline) (reverse-list->string (cons c ls)))
            (else (read-to-eol (cons c ls))))))
       (define (read-identifier ls)
         (let ((c (peek-char in)))
           (cond
            ((highlight-scheme-delimiter? c)
             (string->symbol (reverse-list->string ls)))
            (else
             (read-char in)
             (read-identifier (cons c ls))))))
       (define (hash-mark n)
         (let ((c (read-char in)))
           (case c
             ((#\;)
              (highlight-start 'comment out)
              (highlight n)
              (highlight-end 'comment out))
             ((#\\)
              (let ((id (read-identifier (list (read-char in) #\\ #\#))))
                (highlight-class 'string id out)
                (highlight n)))
             (else
              (write-char #\# out)
              (write-char c out)
              (highlight n)))))
       (define (highlight n)
         (let ((c (read-char in)))
           (if (eof-object? c)
               #f
               (case c
                 ((#\;)
                  (let lp ((ls '()))
                    (let ((ls (cons (read-to-eol (list #\;)) ls)))
                      (cond
                       ((eqv? #\; (peek-char in))
                        (lp ls))
                       (else
                        (highlight-class 'comment
                                         (string-concatenate-reverse ls)
                                         out)
                        (highlight n))))))
                 ((#\")
                  (let ((str (read-escaped in #\" (list #\"))))
                    (highlight-class 'string str out)
                    (highlight n)))
                 ((#\()
                  ;;(highlight-start
                  ;; (string->symbol
                  ;;  (string-append
                  ;;   "paren"
                  ;;   (number->string
                  ;;    (+ 1 (modulo n (length highlight-paren-styles))))))
                  ;;out)
                  (write-char #\( out)
                  (if (highlight-scheme-delimiter? (peek-char in))
                      (highlight (+ n 1))
                      (let ((id (read-identifier '())))
                        (cond
                         ((highlight-scheme-definition? id)
                          (highlight-class 'keyword id out)
                          (display (read-whitespace in) out)
                          (if (eqv? #\( (peek-char in))
                              (write-char (read-char in) out))
                          (highlight-class 'function (read-identifier '()) out)
                          (highlight (+ n 1)))
                         ((highlight-scheme-syntax? id)
                          (highlight-class 'keyword id out)
                          (highlight (+ n 1)))
                         (else
                          (display "<span>" out)
                          (display id out)
                          (display "</span>" out)
                          (highlight (+ n 1)))))))
                 ((#\))
                  (cond
                   ((zero? n)
                    (highlight-class 'syntaxerror c out)
                    (highlight n))
                   (else
                    (write-char c out)
                    ;;(highlight-end 'paren out)
                    (highlight (- n 1)))))
                 ((#\#)
                  (hash-mark n))
                 ((#\<)
                  (display "&lt;" out)
                  (highlight n))
                 ((#\&)
                  (display "&amp;" out)
                  (highlight n))
                 ;;((#\newline)
                 ;; (write-char c out)
                 ;; (highlight 0))
                 (else
                  (cond
                   ((highlight-scheme-delimiter? c)
                    (write-char c out)
                    (highlight n))
                   (else
                    (let ((id (read-identifier (list c))))
                      (display "<span>")
                      (display id out)
                      (display "</span>")
                      (highlight n)))))))))
       (highlight 0)))))

(define (highlight-c-keyword? id)
  (memq id '(asm break case catch const_cast continue default delete
             do dynamic_cast else explicit export false for friend goto
             if mutable namespace new operator private protected public
             register reinterpret_cast return sizeof static_cast switch
             template this throw true try typedef typeid typename using
             virtual while)))

(define (highlight-c-type? id)
  (memq id '(auto bool char class const double enum extern float inline int long
             short signed static struct union unsigned void volatile wchar_t)))

(define (highlight-c source)
  (let ((in (if (string? source) (open-input-string source) source)))
    (call-with-output-string
      (lambda (out)
        (define (read-to-eol ls)
          (let ((c (read-char in)))
            (cond
             ((eof-object? c) (reverse-list->string ls))
             ((eqv? c #\newline) (reverse-list->string (cons c ls)))
             (else (read-to-eol (cons c ls))))))
        (define (char-c-initial? c)
          (and (char? c) (or (char-alphabetic? c) (eqv? c #\_) (eqv? c #\$))))
        (define (char-c-identifier? c)
          (and (char? c) (or (char-c-initial? c) (char-numeric? c))))
        (define (read-identifier ls)
          (let ((c (peek-char in)))
            (if (char-c-identifier? c)
                (read-identifier (cons (read-char in) ls))
                (string->symbol (reverse-list->string ls)))))
        (define (write-identifier id)
          (cond
           ((highlight-c-keyword? id)
            (highlight-class 'keyword id out))
           ((highlight-c-type? id)
            (highlight-class 'type id out))
           (else
            (display id out))))
        (define (highlight-line)
          (cond
           ((eqv? #\# (peek-char in))
            (highlight-start 'preprocessor out)
            (write-char (read-char in) out)
            (display (read-whitespace in) out)
            (let ((id (read-identifier '())))
              (display id out)
              (highlight-end 'preprocessor out)
              (display (read-whitespace in) out)
              (cond
               ((eq? 'define id)
                (highlight-class
                 'function
                 (html-escape (read-to-whitespace in '()))
                 out))
               ((memq id '(include import))
                (highlight-class
                 'string
                 (html-escape (read-to-whitespace in '()))
                 out)))))
           ((char-c-initial? (peek-char in))
            ;; line beginning w/ an identifier is probably a
            ;; function declaration
            (let ((id1 (read-identifier '())))
              (cond
               ((eqv? #\: (peek-char in))
                (highlight-class 'function id1 out))
               (else
                (let lp ((decls '())
                         (id id1))
                  (let ((space (read-whitespace in)))
                    (cond
                     ((char-c-initial? (peek-char in))
                      (lp (cons space (cons id decls))
                          (read-identifier '())))
                     ((eqv? #\( (peek-char in))
                      (highlight-start 'type out)
                      (for-each (lambda (x) (display x out))
                                (reverse decls))
                      (highlight-end 'type out)
                      (highlight-start 'function out)
                      (display id out)
                      (highlight-end 'function out)
                      (display space out))
                     (else
                      (for-each write-identifier (reverse decls))
                      (display id out)
                      (display space out))))))))))
          (highlight))
        (define (highlight)
          (let ((c (read-char in)))
            (if (eof-object? c)
                #f
                (case c
                  ((#\/)
                   (case (peek-char in)
                     ((#\/)
                      (highlight-start 'comment out)
                      (write-char c out)
                      (display (read-to-eol '()) out)
                      (highlight-end 'comment out)
                      (newline out)
                      (highlight))
                     ((#\*)
                      (highlight-start 'comment out)
                      (write-char c out)
                      (write-char (read-char in) out)
                      (let ((prev (read-char in)))
                        (write-char prev out)
                        (let lp ((prev prev))
                          (let ((c (read-char in)))
                            (write-char c out)
                            (if (not (and (eqv? prev #\*) (eqv? c #\/)))
                                (lp c)
                                #f))))
                      (highlight-end 'comment out)
                      (highlight))
                     (else
                      (write-char c out)
                      (highlight))))
                  ((#\" #\')
                   (let ((str (read-escaped in c (list c))))
                     (highlight-class 'string str out)
                     (highlight)))
                  ((#\newline)
                   (newline out)
                   (highlight-line))
                  ((#\<)
                   (display "&lt;" out)
                   (highlight))
                  ((#\&)
                   (display "&amp;" out)
                   (highlight))
                  (else
                   (cond
                    ((char-c-initial? c)
                     (let ((id (read-identifier (list c))))
                       (if (eqv? #\: (peek-char in))
                           (highlight-class 'function id out)
                           (write-identifier id))))
                    (else
                     (write-char c out)))
                   (highlight))))))
        (highlight-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (highlight-assembly source)
  (let ((in (if (string? source) (open-input-string source) source)))
    (call-with-output-string
      (lambda (out)
        (define (read-to-eol ls)
          (let ((c (read-char in)))
            (cond
             ((eof-object? c) (reverse-list->string ls))
             ((eqv? c #\newline) (reverse-list->string (cons c ls)))
             (else (read-to-eol (cons c ls))))))
        (define (char-asm-initial? c)
          (and (char? c) (or (char-alphabetic? c) (memv c '(#\_ #\$ #\.)))))
        (define (char-asm-identifier? c)
          (and (char? c) (or (char-asm-initial? c) (char-numeric? c))))
        (define (read-identifier ls)
          (let ((c (peek-char in)))
            (if (char-asm-identifier? c)
                (read-identifier (cons (read-char in) ls))
                (string->symbol (reverse-list->string ls)))))
        (define (highlight)
          (let ((c (read-char in)))
            (cond
             ((eof-object? c))
             (else
              (case c
                ((#\newline)
                 (write-char c out)
                 (line))
                ((#\")
                 (let ((str (read-escaped in c (list c))))
                   (highlight-class 'string str out)
                   (highlight)))
                ((#\%)
                 (highlight-class 'variable (read-identifier (list c)) out)
                 (highlight))
                ((#\;)
                 (highlight-class 'comment (read-to-eol (list c)) out)
                 (highlight))
                ((#\<)
                 (display "&lt;" out)
                 (highlight))
                ((#\&)
                 (display "&amp;" out)
                 (highlight))
                (else
                 (write-char c out)
                 (highlight)))))))
        (define (line) 
          (cond
           ((eof-object? (peek-char in)))
           ((char-asm-initial? (peek-char in))
            (let ((id (read-identifier '())))
              (if (eqv? #\: (peek-char in))
                  (highlight-class 'function id out)
                  (highlight-class 'keyword id out))
              (highlight)))
           ((eqv? #\tab (peek-char in))
            (write-char (read-char in) out)
            (highlight-class 'keyword (read-identifier '()) out)
            (highlight))
           (else
            (highlight))))
        (line)))))
