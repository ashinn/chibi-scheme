
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils

(define (write-to-string x)
  (call-with-output-string (lambda (out) (write x out))))

(define (string-concatenate-reverse ls)
  (string-concatenate (reverse ls)))

(define (string-strip str . o)
  (let ((bad (if (pair? o) (car o) " \t\n")))
    (call-with-output-string
      (lambda (out)
        (call-with-input-string str
          (lambda (in)
            (let lp ()
              (let ((ch (read-char in)))
                (cond
                 ((not (eof-object? ch))
                  (if (not (string-find? bad ch))
                      (write-char ch out))
                  (lp)))))))))))

(define (string-first-token str sep)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (cond ((= i len) str)
            ((not (string-find? sep (string-ref str i))) (lp (+ i 1)))
            (else
             (let lp ((j (+ i 1)))
               (cond ((= j len) "")
                     ((string-find? sep (string-ref str j)) (lp (+ j 1)))
                     (else
                      (let lp ((k (+ j 1)))
                        (cond
                         ((or (= k len) (string-find? sep (string-ref str k)))
                          (substring str j k))
                         (else
                          (lp (+ k 1)))))))))))))

(define (intersperse ls x)
  (if (or (null? ls) (null? (cdr ls)))
      ls
      (let lp ((ls (cdr ls)) (res (list (car ls))))
        (let ((res (cons (car ls) (cons x res))))
          (if (null? (cdr ls))
              (reverse res)
              (lp (cdr ls) res))))))

(define (normalize-sxml x)
  (cond
   ((pair? x)
    (let lp ((ls x) (res '()))
      (cond ((null? ls)
             (string-concatenate-reverse res))
            ((string? (car ls))
             (lp (cdr ls) (cons (car ls) res)))
            ((pair? res)
             (cons (string-concatenate-reverse res)
                   (cons (car ls) (normalize-sxml (cdr ls)))))
            (else
             (cons (car ls) (normalize-sxml (cdr ls)))))))
   (else x)))

(define (map-sxml proc x)
  (if (pair? x)
      (cons (map-sxml proc (car x))  (map-sxml proc (cdr x)))
      (proc x)))

(define (sxml-body x)
  (cond ((not (and (pair? x) (pair? (cdr x)))) '())
        ((and (pair? (cadr x)) (eq? '@ (car (cadr x)))) (cddr x))
        (else (cdr x))))

(define (sxml->sexp-list x)
  (call-with-input-string (sxml-strip x) port->sexp-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doc environments

(define (env-ref env name . o)
  (cond ((assq name (car env)) => cdr)
        ((pair? o) (car o))
        (else #f)))

(define (env-set! env name value)
  (cond ((assq name (car env)) => (lambda (cell) (set-cdr! cell value)))
        (else (set-car! env (cons (cons name value) (car env))))))

(define (env-extend env vars vals)
  (list (append (map cons vars vals) (car env))))

(define (make-default-doc-env)
  `(((title . ,(expand-section 'h1))
     (section . ,(expand-section 'h2))
     (subsection . ,(expand-section 'h3))
     (subsubsection . ,(expand-section 'h4))
     (subsubsubsection . ,(expand-section 'h5))
     (procedure . ,expand-procedure)
     (macro . ,expand-macro)
     (centered . center)
     (smaller . small)
     (larger . large)
     (bold . b)
     (italic . i)
     (emph . em)
     (subscript . sub)
     (superscript . sup)
     (itemlist . ul)
     (item . li)
     (var . code)
     (cfun . code)
     (cmacro . code)
     (ctype . code)
     (url . ,expand-url)
     (hyperlink . ,expand-hyperlink)
     (rawcode . code)
     (code . ,expand-code)
     (codeblock . ,expand-codeblock)
     (ccode
      . ,(lambda (x env)
           (expand-code `(,(car x) language: c ,@(cdr x)) env)))
     (ccodeblock
      . ,(lambda (x env)
           (expand-codeblock `(,(car x) language: c ,@(cdr x)) env)))
     (scheme
      . ,(lambda (x env)
           (expand-code `(,(car x) language: scheme ,@(cdr x)) env)))
     (schemeblock
      . ,(lambda (x env)
           (expand-codeblock `(,(car x) language: scheme ,@(cdr x)) env)))
     (command . ,expand-command)
     (author . ,expand-author)
     (margin-note . ,expand-note)
     (example . ,expand-example)
     (example-import . ,expand-example-import)
     )))

(define (make-module-doc-env mod-name)
  (env-extend (make-default-doc-env)
              '(example-env)
              (list (environment '(scheme base)
                                 '(only (chibi) import)
                                 mod-name))))

(define (section-name tag name)
  (string-strip
   (call-with-output-string
     (lambda (out)
       (display tag out)
       (write-char #\_ out)
       (display name out)))))

(define (expand-section tag)
  (lambda (sxml env)
    (if (null? (cdr sxml))
        (error "section must not be empty" sxml)
        (let* ((name (and (eq? 'tag: (cadr sxml))
                          (pair? (cddr sxml))
                          (sxml-strip (car (cddr sxml)))))
               (body (map (lambda (x) (expand-docs x env))
                          (if name (cdr (cddr sxml)) (cdr sxml))))
               (name (or name (sxml-strip (cons tag body)))))
          `(div (a (@ (name . ,(section-name tag name)))) (,tag ,@body))))))

(define (expand-url sxml env)
  (if (not (= 2 (length sxml)))
      (error "url expects one argument" sxml)
      (let ((url (expand-docs (cadr sxml) env)))
        `(a (@ (href . ,url)) ,url))))

(define (expand-hyperlink sxml env)
  (if (not (>= (length sxml) 3))
      (error "hyperlink expects at least two arguments" sxml)
      (let ((url (expand-docs (cadr sxml) env)))
        `(a (@ (href . ,url))
            ,(map (lambda (x) (expand-docs x env)) (cddr sxml))))))

(define (expand-note sxml env)
  `(div (@ (id . "notes"))
        ,@(map (lambda (x) (expand-docs x env)) (cdr sxml))))

(define (expand-author sxml env)
  `(div (@ (id . "notes"))
        ,@(map (lambda (x) (expand-docs x env)) (cdr sxml))
        (br)
        ,(seconds->string (current-seconds))))

(define (expand-code sxml env)
  (let* ((hl (if (and (pair? (cdr sxml)) (eq? 'language: (cadr sxml)))
                 (highlighter-for (car (cddr sxml)))
                 highlight))
         (body (if (and (pair? (cdr sxml)) (eq? 'language: (cadr sxml)))
                   (cdr (cddr sxml))
                   (cdr sxml))))
    `(code ,@(map-sxml (lambda (x) (if (string? x) (hl x) x))
                       (normalize-sxml
                        (map (lambda (x) (expand-docs x env)) body))))))

(define (expand-codeblock sxml env)
  `(pre ,(expand-code sxml env)))

(define (expand-example x env)
  (let ((expr `(begin ,@(sxml->sexp-list x)))
        (example-env (or (env-ref env 'example-env) (current-environment))))
    `(div
      ,(expand-codeblock `(,(car x) language: scheme ,@(cdr x)) env)
      (code
       (div (@ (class . "result"))
            ,(call-with-output-string
               (lambda (out)
                 (protect (exn (#t (print-exception exn out)))
                   (let ((res (eval expr example-env)))
                     (display "=> " out)
                     (write res out))))))))))

(define (expand-example-import x env)
  (eval `(import ,@(cdr x))
        (or (env-ref env 'example-env) (current-environment)))
  "")

(define (expand-command sxml env)
  `(pre (@ (class . "command"))
        (code ,@(map (lambda (x) (expand-docs x env)) (cdr sxml)))))

(define (expand-tagged tag ls env)
  (cons tag (map (lambda (x) (expand-docs x env)) ls)))

(define (expand-docs sxml env)
  (cond
   ((pair? sxml)
    (cond
     ((symbol? (car sxml))
      (let ((op (env-ref env (car sxml))))
        (cond
         ((procedure? op)
          (op sxml env))
         ((symbol? op)
          (expand-tagged op (cdr sxml) env))
         (else
          (expand-tagged (car sxml) (cdr sxml) env)))))
     (else
      (map (lambda (x) (expand-docs x env)) sxml))))
   (else
    sxml)))

(define (expand-procedure sxml env)
  ((expand-section 'h3) `(,(car sxml) (rawcode ,@(cdr sxml))) env))

(define (expand-macro sxml env)
  (expand-procedure sxml env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adjustments for html

(define header-index
  (let* ((headers '(h1 h2 h3 h4 h5 h6))
         (len (length headers)))
    (lambda (h) (- len (length (memq h headers))))))

(define (extract-contents x)
  (match x
    (('div ('a ('@ ('name . name)) . _)
           ((and h (or 'h1 'h2 'h3 'h4 'h5 'h6)) . section))
     (let* ((raw-text (sxml-strip (cons h section)))
            (text (if (string-prefix? "(" raw-text)
                      (let ((end (string-find
                                  raw-text
                                  (lambda (ch)
                                    (or (char-whitespace? ch)
                                        (eqv? ch #\)))))))
                        (substring raw-text 1 end))
                      raw-text)))
       `((,(header-index h)
          (a (@ (href . ,(string-append "#" name)))
             ,text)))))
    ((a . b)
     (append (extract-contents a) (extract-contents b)))
    (else
     '())))

(define (get-contents x)
  (if (null? x)
      '()
      (let ((d (caar x)))
        (let lp ((ls (cdr x)) (parent (car (cdar x))) (kids '()) (res '()))
          (define (collect)
            (cons `(li ,parent ,(get-contents (reverse kids))) res))
          (cond
           ((null? ls)
            `(ol ,@(reverse (collect))))
           ((> (caar ls) d)
            (lp (cdr ls) parent (cons (car ls) kids) res))
           (else
            (lp (cdr ls) (car (cdar ls)) '() (collect))))))))

(define (fix-header x)
  `(html (head ,@(cond ((assq 'title x) => (lambda (x) (list x)))
                       (else '()))
               "\n"
               (style (@ (type . "text/css"))
                 "
body {color: #000; background-color: #FFF}
div#menu  {font-size: smaller; position: absolute; top: 50px; left: 0; width: 190px; height: 100%}
div#main  {position: absolute; top: 0; left: 200px; width: 540px; height: 100%}
div#notes {position: relative; top: 2em; left: 570px; max-width: 200px; height: 0px; font-size: smaller;}
div#footer {padding-bottom: 50px}
.result { color: #000; background-color: #FFEADF; width: 100%; padding: 3px}
.command { color: #000; background-color: #FFEADF; width: 100%; padding: 5px}
"
                 ,(highlight-style))
               "\n")
         (body
          (div (@ (id . "menu"))
               ,(let ((contents (get-contents (extract-contents x))))
                  (match contents
                    (('ol (li y sections ...))
                     sections)
                    (else contents))))
          (div (@ (id . "main"))
               ,@(map (lambda (x)
                        (if (and (pair? x) (eq? 'title (car x)))
                            (cons 'h1 (cdr x))
                            x))
                      x)
               (div (@ (id . "footer")))))))

(define (fix-paragraphs x)
  (let lp ((ls x) (p '()) (res '()))
    (define (collect)
      (if (pair? p) (cons `(p ,@(reverse p)) res) res))
    (define (inline? x)
      (or (string? x)
          (and (pair? x) (symbol? (car x))
               (memq (car x) '(a b i u span code small large sub sup em)))))
    (define (enclosing? x)
      (and (pair? x) (symbol? (car x))
           (memq (car x) '(div body))))
    (cond
     ((null? ls)
      (reverse (collect)))
     ((equal? "\n" (car ls))
      (if (and (pair? p) (equal? "\n" (car p)))
          (let lp2 ((ls (cdr ls)))
            (if (and (pair? ls) (equal? "\n" (car ls)))
                (lp2 (cdr ls))
                (lp ls '() (collect))))
          (lp (cdr ls) (cons (car ls) p) res)))
     ((inline? (car ls))
      (lp (cdr ls) (cons (car ls) p) res))
     ((enclosing? (car ls))
      (lp (cdr ls) '() (cons (car ls) (collect))))
     (else
      (lp (cdr ls) '() (cons (car ls) (collect)))))))

(define (fix-begins x)
  x)

(define (fixup-docs sxml)
  (fix-header (fix-paragraphs (fix-begins sxml))))

(define (generate-docs sxml . o)
  (fixup-docs
   (expand-docs sxml (if (pair? o) (car o) (make-default-doc-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extraction

(define (skip-horizontal-whitespace in)
  (cond ((memv (peek-char in) '(#\space #\tab))
         (read-char in)
         (skip-horizontal-whitespace in))))

(define (external-clause? x)
  (not (and (pair? (cdr x)) (pair? (cadr x)) (string? (car (cadr x))))))

(define (contains? tree x)
  (or (eq? tree x)
      (and (pair? tree)
           (or (contains? (car tree) x)
               (contains? (cdr tree) x)))))

;; Try to determine the names of optional parameters checking common
;; patterns.
(define (get-optionals ls body)
  (let lp ((ls ls) (pre '()))
    (cond
     ((pair? ls) (lp (cdr ls) (cons (car ls) pre)))
     ((null? ls) (reverse pre))
     (else
      (let* ((o ls)
             (o? (lambda (x) (eq? x o))))
        (let extract ((x body)
                      (vars '())
                      (i 0))
          (match x
            ((('define x val) . rest)
             (if (contains? val o)
                 (extract #f vars i)
                 (extract rest vars i)))
            ((((or 'let 'let* 'letrec 'letrec*) (y ...) . body))
             (let ((ordered? (memq (car x) '(let* letrec*))))
               (let lp ((ls y) (vars vars) (j i))
                 (cond
                  ((pair? ls)
                   (match (car ls)
                     (((? o?) ('if ('pair? (? o?)) ('cdr (? o?)) default))
                      (lp (cdr ls) vars (+ j 1)))
                     (((? o?) expr)
                      (extract #f vars i))
                     ((v ('if ('pair? (? o?)) ('car (? o?)) default))
                      (lp (cdr ls) (cons (cons v (if ordered? j i)) vars) j))
                     ((v ('and ('pair? (? o?)) ('car (? o?))))
                      (lp (cdr ls) (cons (cons v (if ordered? j i)) vars) j))
                     ((v ('if ('and ('pair? (? o?)) ('pair? ('cdr (? o?))))
                             ('cadr (? o?))
                             default))
                      (lp (cdr ls) (cons (cons v (if ordered? j i)) vars) j))
                     (else
                      (lp (cdr ls) vars j))))
                  (else
                   (extract body vars j))))))
            ((((or 'let-optionals 'let-optionals*) ls ((var default) ...)
               . body))
             (let lp ((ls var) (vars vars) (i i))
               (cond
                ((pair? ls)
                 (lp (cdr ls) (cons (cons (caar ls) i) vars) (+ i 1)))
                (else
                 (extract body vars i)))))
            (else
             (let ((opts (map car (sort vars < cdr)))
                   (dotted? (contains? x o)))
               (append (reverse pre)
                       (cond
                        ((and (pair? opts) dotted?)
                         (list (append opts o)))
                        (dotted?
                         o)
                        (else
                         (list opts)))))))))))))

(define (get-signature proc source form)
  (match form
    (('define (name args ...) . body)
     (list (cons name args)))
    (('define (name . args) . body)
     (list (cons name (get-optionals args body))))
    (('define-syntax name ('syntax-rules () (clause . body) ...))
     ;; TODO: smarter summary
     (map (lambda (x) (cons name (cdr x)))
          (filter external-clause? clause)))
    ((procedure? proc)
     (cond ((procedure-signature proc) => list) (else '())))
    (else
     '())))

(define (get-ffi-signatures form)
  (match form
    (('define-c ret-type (or (name _) name) (args ...))
     (list (cons name
                 (map (lambda (x) (if (pair? x) (last x) x))
                      (remove (lambda (x)
                                (and (pair? x)
                                     (memq (car x) '(value result))))
                              args)))))
    (('define-c-const type (or (name _) name))
     (list (list 'const: type name)))
    (((or 'define-c-struct 'define-c-class 'define-c-type) name . rest)
     (let lp ((ls rest) (res '()))
       (cond
        ((null? ls)
         (reverse res))
        ((eq? 'predicate: (car ls))
         (lp (cddr ls) (cons (list (cadr ls) 'obj) res)))
        ((eq? 'constructor: (car ls))
         (lp (cddr ls)
             (cons (if (pair? (cadr ls)) (cadr ls) (list (cadr ls))) res)))
        ((pair? (car ls))
         (lp (cdr ls)
             (append (if (pair? (cddr (cdar ls)))
                         (list (list (car (cddr (cdar ls))) name (caar ls)))
                         '())
                     (list (list (cadr (cdar ls)) name))
                     res)))
        ((symbol? (car ls))
         (lp (cddr ls) res))
        (else
         (lp (cdr ls) res)))))
    (else
     '())))

(define section-number
  (let ((sections '(section subsection subsubsection subsubsubsection)))
    (lambda (x)
      (cond ((memq x sections) => length)
            ((memq x '(procedure macro)) (section-number 'subsection))
            (else 0)))))

(define (section>=? x n)
  (and (pair? x)
       (if (memq (car x) '(div))
           (find (lambda (y) (section>=? y n)) (sxml-body x))
           (>= (section-number (car x)) n))))

(define (extract-sxml tag x)
  (and (pair? x)
       (cond ((if (pair? tag) (memq (car x) tag) (eq? tag (car x))) x)
             ((memq (car x) '(div))
              (any (lambda (y) (extract-sxml tag y)) (sxml-body x)))
             (else #f))))

(define (section-describes? x name)
  (let ((name (symbol->string name)))
    (and (pair? x) (pair? (cdr x))
         (let* ((str (sxml-strip (cadr x)))
                (op (string-first-token str " \t\r\n()#")))
           (or (string=? op name)
               ;; FIXME: hack for loop iterators
               (and (string=? op "for")
                    (string-contains str (string-append "(" name " "))))))))

;; write a signature handling a trailing list as [optional] parameters
(define (write-signature sig)
  (if (and (list? sig)
           (> (length sig) 1)
           (pair? (last sig))
           (not (any pair? (drop-right sig 1))))
      (call-with-output-string
        (lambda (out)
          (display "(" out)
          (write (car sig) out)
          (let lp ((ls sig))
            (cond
             ((pair? (car ls))
              (display " [" out)
              (write (caar ls) out)
              (let lp ((ls (cdar ls)))
                (cond
                 ((pair? ls)
                  (display " " out)
                  (write (car ls) out)
                  (lp (cdr ls)))
                 ((not (null? ls))
                  (display " . " out)
                  (write ls out))))
              (display "])" out))
             (else
              (display " " out)
              (write (car ls) out)
              (lp (cdr ls)))))))
      (write-to-string sig)))

(define (insert-signature orig-ls name sig)
  (cond
   ((not (pair? sig))
    orig-ls)
   (else
    (let ((name
           (or name
               (if (eq? 'const: (caar sig)) (cadr (cdar sig)) (caar sig)))))
      (let lp ((ls orig-ls) (rev-pre '()))
        (cond
         ((or (null? ls)
              (section>=? (car ls) (section-number 'subsection)))
          `(,@(reverse rev-pre)
            ,@(if (and (pair? ls)
                       (section-describes?
                        (extract-sxml '(subsection procedure macro)
                                      (car ls))
                        name))
                  '()
                  `((subsection
                     tag: ,(write-to-string name)
                     (rawcode
                      ,@(if (eq? 'const: (caar sig))
                            `((i ,(write-to-string (car (cdar sig))) ": ")
                              ,(write-to-string (cadr (cdar sig))))
                            (intersperse (map write-signature sig) '(br)))))))
            ,@ls))
         (else
          (lp (cdr ls) (cons (car ls) rev-pre)))))))))

;; Extract inline scribble documentation (with the ;;> prefix) from a
;; source file, associating any signatures from the provided defs when
;; available and not overridden in the docs.
(define (extract-file-docs file all-defs strict? . o)
  ;; extract (<file> . <line>) macro source or
  ;; (<offset> <file . <line>>) procedure source
  (define (source-line source)
    (and (pair? source)
         (if (string? (car source))
             (and (equal? file (car source))
                  (number? (cdr source))
                  (cdr source))
             (and (number? (car source))
                  (pair? (cdr source))
                  (equal? file (cadr source))
                  (cddr source)))))
  (call-with-input-file file
    (lambda (in)
      (let* ((lang (or (and (pair? o) (car o)) 'scheme))
             ;; filter to only defs found in this file
             (defs (filter-map
                    (lambda (x)
                      (let ((line (source-line (third x))))
                        (and line 
                             ;; (name value line)
                             `(,(car x) ,(cadr x) ,line))))
                    all-defs)))
        (let lp ((lines '()) (cur '()) (res '()) (last-line 0))
          (define (collect)
            (if (pair? lines)
                (append
                 (reverse
                  (call-with-input-string
                      (string-concatenate (reverse lines) "\n")
                    scribble-parse))
                 cur)
                cur))
          (skip-horizontal-whitespace in)
          (cond
           ((eof-object? (peek-char in))
            (append (collect) res))
           ((eqv? #\newline (peek-char in))
            (read-char in)
            (lp lines cur res last-line))
           ((eqv? #\; (peek-char in))
            (read-char in)
            (cond
             ((and (eqv? #\; (peek-char in))
                   (begin (read-char in) (eqv? #\> (peek-char in))))
              (read-char in)
              (if (eqv? #\space (peek-char in)) (read-char in))
              (lp (cons (read-line in) lines) cur res last-line))
             (else
              (let lp ()
                (cond ((eqv? #\; (peek-char in))
                       (read-char in)
                       (lp))))
              (let ((line (read-line in))
                    (cur (collect)))
                ;; ";;/" attaches the docs to the preceding form
                (if (equal? line "/")
                    (lp '() '() (append cur res) last-line)
                    (lp '() cur res last-line))))))
           (else  ;; found a top-level expression
            (let* ((cur (collect))
                   (form (read in))
                   (line (port-line in))
                   ;; find all procedures defined by form
                   (procs (filter (lambda (x) (<= last-line (third x) line))
                                  (filter third defs)))
                   ;; the the signature for the form
                   (sigs
                    (cond
                     ((eq? lang 'ffi)
                      (filter
                       (lambda (x)
                         (assq (if (eq? 'const: (car x)) (third x) (car x))
                               defs))
                       (get-ffi-signatures form)))
                     ((= 1 (length procs))
                      (get-signature (caar procs) (cdar procs) form))
                     (else
                      (get-signature #f #f form)))))
              (cond
               ((and strict?
                     (or (not (pair? sigs)) (not (assq (caar sigs) defs))))
                ;; drop unrelated docs in strict mode
                (lp '() '() res line))
               ((and (eq? lang 'ffi) (pair? sigs))
                (lp '() '() (append (insert-signature cur #f sigs) res) line))
               ((and (eq? lang 'scheme) (= 1 (length procs)))
                (lp '() '() (append (insert-signature cur (caar procs) sigs)
                                    res)
                    line))
               (else
                (lp '() '() (append cur res) line)))))))))))

;; utility to get the source position of an object
(define (object-source x)
  (cond ((opcode? x) #f)
        ((bytecode? x)
         (let ((src (bytecode-source x)))
           (if (and (vector? src) (positive? (vector-length src)))
               (vector-ref src 0)
               src)))
        ((procedure? x) (object-source (procedure-code x)))
        ((macro? x) (macro-source x))
        (else #f)))

;; extract documentation from a module
(define (extract-module-docs mod-name strict? . o)
  (let ((mod (load-module mod-name)))
    (if (not mod)
        (error "couldn't find module" mod-name)
        (let* ((exports (if (pair? o) (car o) (module-exports mod)))
               (defs
                 (map (lambda (x)
                        (let ((val (module-ref mod x)))
                          `(,x ,val ,(object-source val))))
                      exports)))
          (append
           (cond
            ((find-module-file (module-name->file mod-name))
             => (lambda (f) (reverse (extract-file-docs f defs strict? 'module))))
            (else '()))
           (reverse (append-map (lambda (x) (extract-file-docs x defs strict?))
                                (module-includes mod)))
           (reverse (append-map (lambda (x) (extract-file-docs x defs strict? 'ffi))
                                (module-shared-includes mod))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-module-docs mod-name . o)
  (let ((out (if (pair? o) (car o) (current-output-port)))
        (render (or (and (pair? o) (pair? (cdr o)) (cadr o))
                    sxml-display-as-text)))
    (render
     (generate-docs
      `((title ,(write-to-string mod-name))
        ,@(extract-module-docs mod-name #f))
      (make-module-doc-env mod-name))
     out)))

(define (print-module-binding-docs mod-name var . o)
  (let ((out (if (pair? o) (car o) (current-output-port)))
        (render (or (and (pair? o) (pair? (cdr o)) (cadr o))
                    sxml-display-as-text)))
    (render
     (generate-docs
      (extract-module-docs mod-name #t (list var))
      (make-module-doc-env mod-name))
     out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (procedure-docs proc)
  (let ((mod (and (procedure? proc) (containing-module proc))))
    (and mod
         (generate-docs
          (extract-module-docs (car mod) #t (list (procedure-name proc)))
          (make-module-doc-env (car mod))))))

(define (print-procedure-docs proc . o)
  (let ((out (if (pair? o) (car o) (current-output-port)))
        (render (or (and (pair? o) (pair? (cdr o)) (cadr o))
                    sxml-display-as-text))
        (docs (procedure-docs proc)))
    (if docs (render docs out))))
