
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional state information

(define (c-in-expr proc) (with ((expression? #t)) (c-expr proc)))
(define (c-in-stmt proc) (with ((expression? #f)) (c-expr proc)))
(define (c-in-test proc) (with ((in-cond? #t)) (c-in-expr proc)))
(define (c-with-op op proc) (with ((op op)) proc))

(define nl-str (call-with-output-string newline))
(define (make-nl-space n) (string-append nl-str (make-string n #\space)))
(define (make-space n) (make-string n #\space))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (let ((res (get-output-string out)))
      (close-output-port out)
      res)))

(define (display-to-string x)
  (if (string? x)
      x
      (call-with-output-string (lambda (out) (display x out)))))

(define (write-to-string x)
  (call-with-output-string (lambda (out) (write x out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; be smart about operator precedence

(define (c-op-precedence x)
  (if (string? x)
      (cond
        ((or (string=? x ".") (string=? x "->")) 10)
        ((or (string=? x "++") (string=? x "--")) 20)
        ((string=? x "|") 65)
        ((string=? x "||") 75)
        ((string=? x "|=") 85)
        ((or (string=? x "+=") (string=? x "-=")) 85)
        (else (c-op-precedence (string->symbol x))))
      (case x
        ((zero) 0)
        ;;((|::|) 5)                    ; C++
        ((paren bracket) 5)
        ((dot arrow post-decrement post-increment) 10)
        ((**) 15)                       ; Perl
        ((unary+ unary- ! ~ cast unary-* unary-& sizeof) 20) ; ++ --
        ((=~ !~) 25)                    ; Perl
        ((* / %) 30)
        ((+ -) 35)
        ((<< >>) 40)
        ((< > <= >=) 45)
        ((lt gt le ge) 45)              ; Perl
        ((== !=) 50)
        ((eq ne cmp) 50)                ; Perl
        ((&) 55)
        ((^) 60)
        ;;((|\||) 65)                   ; SCSH
        ((&&) 70)
        ;;((|\|\||) 75)                 ; SCSH
        ;;((.. ...) 77)                 ; Perl
        ((? if) 80)
        ((= *= /= %= &= ^= <<= >>=) 85) ; |\|=| ;  += -=
        ((comma) 90)
        ((=>) 90)                       ; Perl
        ((not) 92)                      ; Perl
        ((and) 93)                      ; Perl
        ((or xor) 94)                   ; Perl
        (else 95))))

(define (c-op< x y) (< (c-op-precedence x) (c-op-precedence y)))
(define (c-op<= x y) (<= (c-op-precedence x) (c-op-precedence y)))

(define (c-paren x)
  (each "(" x ")"))

(define (c-maybe-paren x-op x)
  (fn (op)
    (let ((x (with ((op x-op)) x)))
      (if (c-op<= op x-op)
          (c-paren x)
          x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default literals writer

(define (c-control-operator? x)
  (memq x '(if while switch repeat do for fun begin)))

(define (c-literal? x)
  (or (number? x) (string? x) (char? x) (boolean? x)))

(define (char->c-char c)
  (string-append "'" (c-escape-char c #\') "'"))

(define (c-escape-char c quote-char)
  (let ((n (char->integer c)))
    (if (<= 32 n 126)
        (if (or (eqv? c quote-char) (eqv? c #\\))
            (string #\\ c)
            (string c))
        (case n
          ((7) "\\a") ((8) "\\b") ((9) "\\t") ((10) "\\n")
          ((11) "\\v") ((12) "\\f") ((13) "\\r")
          (else (string-append "\\x" (number->string (char->integer c) 16)))))))

(define (c-format-number x)
  (if (and (integer? x) (exact? x))
      (fn (radix)
        (case radix
          ((16) (each "0x" (string-upcase-ascii (number->string x 16))))
          ((8) (each "0" (number->string x 8)))
          (else (each (number->string x)))))
      (each (number->string x))))

(define (c-format-string s)
  (each "\"" (each-in-list (c-string-escaped s)) "\""))

(define (c-string-escaped s)
  (let ((start (string-cursor-start s)))
    (let lp ((parts '()) (i (string-cursor-end s)))
      (let ((j (string-find-right s c-needs-string-escape? start i)))
        (cond
         ((string-cursor>? j start)
          (lp (cons (c-escape-char (string-cursor-ref s (string-cursor-prev s j)) #\")
                    (cons (substring-cursor s j i) parts))
              (string-cursor-prev s j)))
         (else
          (cons (substring-cursor s start i) parts)))))))

(define (c-needs-string-escape? c)
  (if (<= 32 (char->integer c) 127) (memv c '(#\" #\\)) #t))

(define (c-simple-literal x)
  (c-wrap-stmt
   (cond ((char? x) (each (char->c-char x)))
         ((boolean? x) (each (if x "1" "0")))
         ((number? x) (c-format-number x))
         ((string? x) (c-format-string x))
         ((null? x) (each "NULL"))
         ((eof-object? x) (each "EOF"))
         (else (each (write-to-string x))))))

(define (c-literal x)
  (fn (op in-macro? macro-vars)
    (if (and in-macro? (memq x macro-vars))
        (c-paren (c-simple-literal x))
        (c-simple-literal x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default expression generator

(define (c-expr/sexp x)
  (cond
   ((procedure? x)
    x)
   ((pair? x)
    (case (car x)
      ((if) (apply c-if (cdr x)))
      ((for) (apply c-for (cdr x)))
      ((while) (apply c-while (cdr x)))
      ((switch) (apply c-switch (cdr x)))
      ((case) (apply c-case (cdr x)))
      ((case/fallthrough) (apply c-case/fallthrough (cdr x)))
      ((default) (apply c-default (cdr x)))
      ((break) c-break)
      ((continue) c-continue)
      ((return) (apply c-return (cdr x)))
      ((goto) (apply c-goto (cdr x)))
      ((typedef) (apply c-typedef (cdr x)))
      ((struct union class) (apply c-struct/aux x))
      ((enum) (apply c-enum (cdr x)))
      ((inline auto restrict register volatile extern static)
       (each (car x) " " (apply c-begin (cdr x))))
      ;; non C-keywords must have some character invalid in a C
      ;; identifier to avoid conflicts - by default we prefix %
      ((vector-ref)
       (c-wrap-stmt
        (each (c-expr (cadr x)) "[" (c-expr (caddr x)) "]")))
      ((vector-set!)
       (c= (c-in-expr
            (each (c-expr (cadr x)) "[" (c-expr (caddr x)) "]"))
           (c-expr (cadddr x))))
      ((extern/C) (apply c-extern/C (cdr x)))
      ((%apply) (apply c-apply (cdr x)))
      ((%define) (apply cpp-define (cdr x)))
      ((%include) (apply cpp-include (cdr x)))
      ((%fun %lambda) (apply c-fun (cdr x)))
      ((%cond)
       (let lp ((ls (cdr x)) (res '()))
         (if (null? ls)
             (apply c-if (reverse res))
             (lp (cdr ls)
                 (cons (if (pair? (cddar ls))
                           (apply c-begin (cdar ls))
                           (cadar ls))
                       (cons (caar ls) res))))))
      ((%prototype) (apply c-prototype (cdr x)))
      ((%var) (apply c-var (cdr x)))
      ((%begin) (apply c-begin (cdr x)))
      ((%attribute) (apply c-attribute (cdr x)))
      ((%line) (apply cpp-line (cdr x)))
      ((%pragma %error %warning)
       (apply cpp-generic (substring (symbol->string (car x)) 1) (cdr x)))
      ((%if %ifdef %ifndef %elif)
       (apply cpp-if/aux (substring (symbol->string (car x)) 1) (cdr x)))
      ((%endif) (apply cpp-endif (cdr x)))
      ((%block) (apply c-braced-block (cdr x)))
      ((%comment) (apply c-comment (cdr x)))
      ((:) (apply c-label (cdr x)))
      ((%cast) (apply c-cast (cdr x)))
      ((+ - & * / % ! ~ ^ && < > <= >= == != << >>
          = *= /= %= &= ^= >>= <<=)     ; |\|| |\|\|| |\|=|
       (apply c-op x))
      ((bitwise-and bit-and) (apply c-op '& (cdr x)))
      ((bitwise-ior bit-or) (apply c-op "|" (cdr x)))
      ((bitwise-xor bit-xor) (apply c-op '^ (cdr x)))
      ((bitwise-not bit-not) (apply c-op '~ (cdr x)))
      ((arithmetic-shift) (apply c-op '<< (cdr x)))
      ((bitwise-ior= bit-or=) (apply c-op "|=" (cdr x)))
      ((%and) (apply c-op "&&" (cdr x)))
      ((%or) (apply c-op "||" (cdr x)))
      ((%. %field) (apply c-op "." (cdr x)))
      ((%->) (apply c-op "->" (cdr x)))
      (else
       (cond
        ((eq? (car x) (string->symbol "."))
         (apply c-op "." (cdr x)))
        ((eq? (car x) (string->symbol "->"))
         (apply c-op "->" (cdr x)))
        ((eq? (car x) (string->symbol "++"))
         (apply c-op "++" (cdr x)))
        ((eq? (car x) (string->symbol "--"))
         (apply c-op "--" (cdr x)))
        ((eq? (car x) (string->symbol "+="))
         (apply c-op "+=" (cdr x)))
        ((eq? (car x) (string->symbol "-="))
         (apply c-op "-=" (cdr x)))
        (else (c-apply x))))))
   ((vector? x)
    (c-wrap-stmt
     (each "{" (joined c-expr (vector->list x) ", ") "}")))
   (else
    (c-literal x))))

(define (c-apply ls)
  (c-wrap-stmt
   (with ((op 'comma))
     (each
      (c-expr (car ls))
      (let ((flat (with ((no-wrap? #t)) (joined c-expr (cdr ls) ", "))))
        (fn (no-wrap?)
          (if no-wrap?
              (c-paren flat)
              (c-paren
               (try-fitted
                flat
                (fn (col)
                  (let ((sep (string-append "," (make-nl-space col))))
                    (joined c-expr (cdr ls) sep))))))))))))

(define (c-expr x)
  (fn (gen) ((or gen c-expr/sexp) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comments, with Emacs-friendly escaping of nested comments

(define (make-comment-writer output)
  (lambda (str)
    (let ((start (string-cursor-start str))
          (end (string-cursor-prev str (string-cursor-end str))))
      (let lp ((i start))
        (let ((j (string-find str #\/ i)))
          (cond
           ((string-cursor>? j end)
            (output (substring-cursor str i j)))
           (else
            (each
             (cond
              ((and (string-cursor>? j start)
                    (eqv? #\* (string-cursor-ref str (string-cursor-prev str j))))
               (each (output (substring-cursor str i j))
                     (output "\\/")))
              (else
               (output (substring-cursor str i (string-cursor-next str j)))))
             (if (and (string-cursor<? j end)
                      (eqv? #\* (string-cursor-ref str (string-cursor-next str j))))
                 (output "\\")
                 nothing)
             (lp (string-cursor-next str j))))))))))

(define (c-comment . args)
  (each "/*" (fn (output)
               (with ((output (make-comment-writer output)))
                 (each-in-list args)))
        "*/"))

(define (make-block-comment-writer)
  (lambda (str)
    (fn (col output)
      (with ((output (make-comment-writer output)))
        (let ((end (string-cursor-end str))
              (indent (string-append (make-nl-space (+ col 1)) "* ")))
          (let lp ((i (string-cursor-start str)))
            (let ((j (string-find str #\newline i)))
              (output indent)
              (output (substring-cursor str i j))
              (if (string-cursor<? j end)
                  (lp (string-cursor-next str j))))))))))

(define (c-block-comment . args)
  (fn (col row)
    (let ((indent (c-indent-string col)))
      (each "/* "
            (with ((writer (make-block-comment-writer)))
              (each-in-list args))
            (fn ((row2 row))
              (cond
               ((= row row2) (displayed " */"))
               (else (each fl indent " */"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preprocessor

(define (make-cpp-writer output)
  (lambda (str)
    (let ((lim (string-length str)))
      (let lp ((i 0))
        (let ((j (string-find str #\newline i)))
          (output (substring str i j))
          (cond
           ((< j lim)
            (output "\n")
            (lp (+ j 1)))))))))

(define (cpp-include file)
  (if (string? file)
      (each fl "#include " (written file) fl)
      (each fl "#include <" file ">" fl)))

(define (list-dot x)
  (cond ((pair? x) (list-dot (cdr x)))
        ((null? x) #f)
        (else x)))

(define (replace-tree from to x)
  (let replace ((x x))
    (cond ((eq? x from) to)
          ((pair? x) (cons (replace (car x)) (replace (cdr x))))
          (else x))))

(define (cpp-define x . body)
  (define (name-of x) (c-expr (if (pair? x) (cadr x) x)))
  (fn ()
    (let* ((body (cond
                  ((and (pair? x) (list-dot x))
                   => (lambda (dot)
                        (if (eq? dot '...)
                            body
                            (replace-tree dot '__VA_ARGS__ body))))
                  (else body)))
           (tail
            (if (pair? body)
                (each " "
                      (fn (output)
                        (with ((writer (make-cpp-writer output))
                               (in-macro? (pair? x))
                               (macro-vars
                                (map (lambda (v) (if (pair? v) (cadr v) v))
                                     (if (pair? x) x (list x))))
                               (op 'zero))
                          (c-in-expr (apply c-begin body)))))
                "")))
      (c-in-expr
       (if (pair? x)
           (each fl "#define " (name-of (car x))
                 (c-paren
                  (joined/dot name-of
                              (lambda (dot) (displayed "..."))
                              (cdr x)
                              ", "))
                 tail fl)
           (each fl "#define " (c-expr x) tail fl))))))

(define (cpp-expr x)
  (if (or (symbol? x) (string? x)) (displayed x) (c-expr x)))

(define (cpp-if/aux name check . o)
  (let* ((pass (and (pair? o) (car o)))
         (comment
          (if (member name '("ifdef" "ifndef"))
              (each "  "
                    (c-comment
                     " " (if (equal? name "ifndef") "! " "")
                     check " "))
              ""))
         (endif (if pass (each fl "#endif" comment) ""))
         (tail (cond
                ((and (pair? o) (pair? (cdr o)))
                 (if (pair? (cddr o))
                     (apply cpp-elif (cdr o))
                     (each (cpp-else) (cadr o) endif)))
                (else endif))))
    (fn (col)
      (each fl "#" name " " (cpp-expr check) fl
            (or pass "")
            tail fl))))

(define (cpp-if check . o)
  (apply cpp-if/aux "if" check o))
(define (cpp-ifdef check . o)
  (apply cpp-if/aux "ifdef" check o))
(define (cpp-ifndef check . o)
  (apply cpp-if/aux "ifndef" check o))
(define (cpp-elif check . o)
  (apply cpp-if/aux "elif" check o))
(define (cpp-else . o)
  (each fl "#else " (if (pair? o) (c-comment (car o)) "") fl))
(define (cpp-endif . o)
  (each fl "#endif " (if (pair? o) (c-comment (car o)) "") fl))

(define (cpp-wrap-header name . body)
  (let ((name name)) ; consider auto-mangling
    (cpp-ifndef name (c-begin (cpp-define name) nl (apply c-begin body) nl))))

(define (cpp-line num . o)
  (each fl "#line " num (if (pair? o) (each " " (car o)) "") fl))

(define (cpp-generic name . ls)
  (each fl "#" name (each-in-list ls) fl))

(define (cpp-undef . args) (apply cpp-generic "undef" args))
(define (cpp-pragma . args) (apply cpp-generic "pragma" args))
(define (cpp-error . args) (apply cpp-generic "error" args))
(define (cpp-warning . args) (apply cpp-generic "warning" args))

(define (cpp-stringify x)
  (each "#" x))

(define (cpp-sym-cat . args)
  (joined displayed args " ## "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general indentation and brace rules

(define (c-indent-string col . o)
  (make-space (max 0 (+ col (if (pair? o) (car o) 0)))))

;; (c-indent [offset])
(define (c-indent . o)
  (fn (col indent-space)
    (displayed
     (make-space (max 0 (+ (or indent-space 4)
                           (if (pair? o) (car o) 0)))))))

(define (c-indent/switch)
  (fn (col switch-indent-space)
    (displayed (make-space (max 0 (- (or switch-indent-space 0) col))))))

(define (c-open-brace)
  (fn (col newline-before-brace?)
    (if newline-before-brace?
        (each nl (c-indent-string col) "{" nl)
        (each " {" nl))))

(define (c-close-brace)
  (displayed "}"))

(define (c-wrap-stmt x)
  (fn (expression? return?)
    (if expression?
        (c-expr x)
        (each (if return? "return " "")
              (c-in-expr (c-expr x)) ";" nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code blocks

(define (c-block . args)
  (apply c-block/aux 0 args))

(define (c-block/aux offset header body0 . body)
  (let ((inner (apply c-begin body0 body)))
    (if (or (pair? body)
            (not (or (c-literal? body0)
                     (and (pair? body0)
                          (not (c-control-operator? (car body0)))))))
        (c-braced-block/aux offset header inner)
        (fn (braceless-bodies?)
          (if braceless-bodies?
              (each header fl (c-indent offset) inner fl)
              (c-braced-block/aux offset header inner))))))

(define (c-braced-block . args)
  (fn (col) (apply c-braced-block/aux col args)))

(define (c-braced-block/aux offset header . body)
  (fn ()
    (each header (c-open-brace) (c-indent offset)
          (apply c-begin body) fl
          (c-indent-string offset)
          (c-close-brace))))

(define (c-begin . args)
  (apply c-begin/aux #f args))

(define (c-begin/aux ret? body0 . body)
  (if (null? body)
      (c-expr body0)
      (fn (col expression?)
        (if expression?
            (with ((no-wrap? #t))
               (joined c-expr (cons body0 body) ", "))
            (let ((sep (each fl (c-indent-string col))))
              (each
               (with ((return? #f))
                 (joined c-expr (cons body0 (drop-right body 1)) sep))
               sep
               (with ((return? ret?))
                 (c-expr (last body)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structures

(define (c-struct/aux type x . o)
  (let* ((name (if (null? o) (if (or (symbol? x) (string? x)) x #f) x))
         (body (if name (car o) x))
         (o (if (null? o) o (cdr o))))
    (c-wrap-stmt
     (each
      (c-braced-block
       (each type
             (if (and name (not (equal? name "")))
                 (each " " name)
                 nothing))
       (each
        (c-in-stmt
         (if (list? body)
             (apply c-begin (map c-wrap-stmt (map c-param body)))
             (c-wrap-stmt (c-expr body))))))
      (if (pair? o) (each " " (apply c-begin o)) nothing)))))

(define (c-struct . args) (apply c-struct/aux "struct" args))
(define (c-union . args) (apply c-struct/aux "union" args))
(define (c-class . args) (apply c-struct/aux "class" args))

(define (c-enum x . o)
  (define (c-enum-one x)
    (if (pair? x) (each (car x) " = " (c-expr (cadr x))) (displayed x)))
  (let* ((name (if (null? o) (if (or (symbol? x) (string? x)) x #f) x))
         (vals (if name (car o) x)))
    (fn (col indent-space)
      (let ((sep (each ",\n" (c-indent-string (+ col (or indent-space 4))))))
        (c-wrap-stmt
         (each
          (c-braced-block
           (if name (each "enum " name) (displayed "enum"))
           (joined c-enum-one vals sep))))))))

(define (c-attribute . args)
  (each "__attribute__ ((" (joined c-expr args ", ") "))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic control structures

(define (c-while check . body)
  (each (c-block (each "while (" (c-in-test check) ")")
                 (c-in-stmt (apply c-begin body)))
        fl))

(define (c-for init check update . body)
  (each
   (c-block
    (c-in-expr
     (each "for (" (c-expr init) "; " (c-in-test check) "; "
          (c-expr update ) ")"))
    (c-in-stmt (apply c-begin body)))
   fl))

(define (c-param x)
  (cond
    ((procedure? x) x)
    ((pair? x) (c-type (car x) (cadr x)))
    (else (fn (default-type) (c-type (or default-type 'int) x)))))

(define (c-param-list ls)
  (c-in-expr (joined/dot c-param (fn (dot) (displayed "...")) ls ", ")))

(define (c-fun type name params . body)
  (each (c-block (c-in-expr (c-prototype type name params))
                (with ((return? (not (eq? 'void type))))
                  (c-in-stmt (apply c-begin body))))
       fl))

(define (c-prototype type name params . o)
  (c-wrap-stmt
   (each (c-type type) " " (c-expr name) " (" (c-param-list params) ")"
         (joined/prefix c-expr o " "))))

(define (c-static x) (each "static " (c-expr x)))
(define (c-const x) (each "const " (c-expr x)))
(define (c-restrict x) (each "restrict " (c-expr x)))
(define (c-volatile x) (each "volatile " (c-expr x)))
(define (c-auto x) (each "auto " (c-expr x)))
(define (c-inline x) (each "inline " (c-expr x)))
(define (c-extern x) (each "extern " (c-expr x)))
(define (c-extern/C . body)
  (each "extern \"C\" {" nl (apply c-begin body) nl "}" nl))

(define (c-type type . o)
  (let ((name (and (pair? o) (car o))))
    (cond
     ((pair? type)
      (case (car type)
        ((%fun)
         (each (c-type (cadr type) #f)
              " (*" (or name "") ")("
              (joined (lambda (x) (c-type x #f)) (caddr type) ", ") ")"))
        ((%array)
         (let ((name (each name "[" (if (pair? (cddr type))
                                       (c-expr (caddr type))
                                       "")
                          "]")))
           (c-type (cadr type) name)))
        ((%pointer *)
         (let ((name (each "*" (if name (c-expr name) ""))))
           (c-type (cadr type)
                   (if (and (pair? (cadr type)) (eq? '%array (caadr type)))
                       (c-paren name)
                       name))))
        ((enum) (apply c-enum name (cdr type)))
        ((struct union class)
         (each (apply c-struct/aux (car type) (cdr type)) " " name))
        (else (joined/last c-expr (lambda (x) (c-type x name)) type " "))))
     ((not type)
      (fn (default-type) (c-type (or default-type 'int) name)))
     (else
      (each (if (eq? '%pointer type) '* type) (if name (each " " name) ""))))))

(define (c-var type name . init)
  (c-wrap-stmt
   (if (pair? init)
       (each (c-type type name) " = " (c-expr (car init)))
       (c-type type (if (pair? name)
                        (joined c-expr name ", ")
                        (c-expr name))))))

(define (c-cast type expr)
  (each "(" (c-type type) ")" (c-expr expr)))

(define (c-typedef type alias . o)
  (c-wrap-stmt
   (each "typedef " (c-type type alias) (joined/prefix c-expr o " "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized IF: allows multiple tail forms for if/else if/.../else
;; blocks.  A final ELSE can be signified with a test of #t or 'else,
;; or by simply using an odd number of expressions (by which the
;; normal 2 or 3 clause IF forms are special cases).

(define (c-if/stmt c p . rest)
  (fn (col)
    (let lp ((c c) (p p) (ls rest))
      (if (or (eq? c 'else) (eq? c #t))
          (if (not (null? ls))
              (error "forms after else clause in IF" c p ls)
              (each (c-block/aux col " else" p) fl))
          (let ((tail (if (pair? ls)
                          (if (pair? (cdr ls))
                              (lp (car ls) (cadr ls) (cddr ls))
                              (lp 'else (car ls) '()))
                          fl)))
            (each (c-block/aux
                   col
                   (each (if (eq? ls rest) nothing " else ")
                         "if (" (c-in-test (c-expr c)) ")")
                   p)
                  tail))))))

(define (c-if/expr c p . rest)
  (let lp ((c c) (p p) (ls rest))
    (cond
     ((or (eq? c 'else) (eq? c #t))
      (if (not (null? ls))
          (error "forms after else clause in IF" c p ls)
          (c-expr p)))
     ((pair? ls)
      (c-maybe-paren
       '?
       (with ((op '?))
         (c-in-test (c-expr c))
         " ? " (c-expr p) " : "
         (if (pair? (cdr ls))
             (lp (car ls) (cadr ls) (cddr ls))
             (lp 'else (car ls) '())))))
     (else
      (c-or (c-in-test (c-expr c)) (c-expr p))))))

(define (c-if . args)
  (fn (expression?)
    (if expression?
        (apply c-if/expr args)
        (apply c-if/stmt args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch statements, automatic break handling

(define (c-label name)
  (fn (col)
    (let ((indent (make-space (max 0 (- col 2)))))
      (each fl indent name ":" fl))))

(define c-break
  (c-wrap-stmt (displayed "break")))
(define c-continue
  (c-wrap-stmt (displayed "continue")))
(define (c-return . result)
  (if (pair? result)
      (c-wrap-stmt (each "return " (c-expr (car result))))
      (c-wrap-stmt (displayed "return"))))
(define (c-goto label)
  (c-wrap-stmt (each "goto " (c-expr label))))

(define (c-switch val . clauses)
  (fn (col)
    (let ((sep (each fl (c-indent-string col))))
      (each "switch (" (c-in-expr val) ")" (c-open-brace)
            (c-indent/switch)
            sep
            (joined c-in-stmt
                    (map (lambda (x) (c-switch-clause x col))
                         clauses)
                    sep)
            sep (c-close-brace) fl))))

(define (c-switch-clause/breaks x . o)
  (fn (col indent indent-space return?)
    (let* ((col (if (pair? o) (car o) col))
           (break?
            (and (car x)
                 (not (member (cadr x) '(case/fallthrough
                                         default/fallthrough
                                         else/fallthrough)))))
           (explicit-case? (member (cadr x) '(case case/fallthrough)))
           (indent-body (c-indent-string (+ col (or indent 4))))
           (indent (c-indent-string col))
           (sep (string-append ":" nl-str indent)))
      (each (c-in-expr
             (joined/suffix
              displayed
              (cond
               ((or explicit-case? (pair? (cadr x)))
                (map (lambda (y) (each "case " (c-expr y)))
                     (if explicit-case?
                         (if (list? (third x))
                             (third x)
                             (list (third x)))
                         (cadr x))))
               (else
                (list (each "default"))))
              sep))
            (make-space (or indent-space 4))
            (joined c-expr
                    (if explicit-case? (cdr (cddr x)) (cddr x))
                    indent-body)
            (if (and break? (not return?))
                (each fl indent-body c-break)
                "")))))

(define (c-switch-clause x . o)
  (if (procedure? x) x (apply c-switch-clause/breaks (cons #t x) o)))
(define (c-switch-clause/no-break x . o)
  (if (procedure? x) x (apply c-switch-clause/breaks (cons #f x) o)))

(define (c-case x . body)
  (c-switch-clause (cons (if (pair? x) x (list x)) body)))
(define (c-case/fallthrough x . body)
  (c-switch-clause/no-break (cons (if (pair? x) x (list x)) body)))
(define (c-default . body)
  (c-switch-clause/breaks (cons #t (cons 'else body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operators

(define (c-op op first . rest)
  (if (null? rest)
      (c-unary-op op first)
      (apply c-binary-op op first rest)))

(define (c-binary-op op . ls)
  (define (lit-op? x) (or (c-literal? x) (symbol? x)))
  (let ((str (display-to-string op)))
    (c-wrap-stmt
     (c-maybe-paren
      op
      (if (or (equal? str ".") (equal? str "->"))
          (joined c-expr ls str)
          (let ((flat
                 (with ((no-wrap? #t))
                   (fn (non-spaced-ops?)
                     (joined c-expr
                             ls
                             (if (and non-spaced-ops?
                                      (every lit-op? ls))
                                 str
                                 (string-append " " str " ")))))))
            (fn (no-wrap?)
              (if no-wrap?
                  flat
                  (try-fitted
                   flat
                   (fn (col)
                     (joined c-expr
                             ls
                             (each nl (make-space (+ 2 col)) str " ")
                             )))))))))))

(define (c-unary-op op x)
  (c-wrap-stmt
   (each (display-to-string op) (c-maybe-paren op (c-expr x)))))

;; some convenience definitions

(define (c++ . args) (apply c-op "++" args))
(define (c-- . args) (apply c-op "--" args))
(define (c+ . args) (apply c-op '+ args))
(define (c- . args) (apply c-op '- args))
(define (c* . args) (apply c-op '* args))
(define (c/ . args) (apply c-op '/ args))
(define (c% . args) (apply c-op '% args))
(define (c& . args) (apply c-op '& args))
;; (define (|c\|| . args) (apply c-op '|\|| args))
(define (c^ . args) (apply c-op '^ args))
(define (c~ . args) (apply c-op '~ args))
(define (c! . args) (apply c-op '! args))
(define (c&& . args) (apply c-op '&& args))
;; (define (|c\|\|| . args) (apply c-op '|\|\|| args))
(define (c<< . args) (apply c-op '<< args))
(define (c>> . args) (apply c-op '>> args))
(define (c== . args) (apply c-op '== args))
(define (c!= . args) (apply c-op '!= args))
(define (c< . args) (apply c-op '< args))
(define (c> . args) (apply c-op '> args))
(define (c<= . args) (apply c-op '<= args))
(define (c>= . args) (apply c-op '>= args))
(define (c= . args) (apply c-op '= args))
(define (c+= . args) (apply c-op "+=" args))
(define (c-= . args) (apply c-op "-=" args))
(define (c*= . args) (apply c-op '*= args))
(define (c/= . args) (apply c-op '/= args))
(define (c%= . args) (apply c-op '%= args))
(define (c&= . args) (apply c-op '&= args))
;; (define (|c\|=| . args) (apply c-op '|\|=| args))
(define (c^= . args) (apply c-op '^= args))
(define (c<<= . args) (apply c-op '<<= args))
(define (c>>= . args) (apply c-op '>>= args))

(define (c. . args) (apply c-op "." args))
(define (c-> . args) (apply c-op "->" args))

(define (c-bit-or . args) (apply c-op "|" args))
(define (c-or . args) (apply c-op "||" args))
(define (c-bit-or= . args) (apply c-op "|=" args))

(define (c++/post x)
  (each (c-maybe-paren 'post-increment (c-expr x)) "++"))
(define (c--/post x)
  (each (c-maybe-paren 'post-decrement (c-expr x)) "--"))
