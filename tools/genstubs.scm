#! chibi-scheme -s

(define types '())
(define funcs '())

(define (cat . args)
  (for-each (lambda (x) (if (procedure? x) (x) (display x))) args))

(define (x->string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        (else (error "non-stringable object" x))))

(define (strip-extension path)
  (let lp ((i (- (string-length path) 1)))
    (cond ((<= i 0) path)
          ((eq? #\. (string-ref path i)) (substring path 0 i))
          (else (lp (- i 1))))))

(define (string-concatenate-reverse ls)
  (cond ((null? ls) "")
        ((null? (cdr ls)) (car ls))
        (else (string-concatenate (reverse ls)))))

(define (string-replace str c r)
  (let ((len (string-length str)))
    (let lp ((from 0) (i 0) (res '()))
      (define (collect) (if (= i from) res (cons (substring str from i) res)))
      (cond
       ((>= i len) (string-concatenate-reverse (collect)))
       ((eqv? c (string-ref str i))
        (lp (+ i 1) (+ i 1) (cons r (collect))))
       (else
        (lp from (+ i 1) res))))))

(define (mangle x)
  (string-replace
   (string-replace (string-replace (x->string x) #\- "_") #\? "_p")
   #\! "_x"))

(define (string-scan c str . o)
  (let ((limit (string-length str)))
    (let lp ((i (if (pair? o) (car o) 0)))
      (cond ((>= i limit) #f)
            ((eqv? c (string-ref str i)) i)
            (else (lp (+ i 1)))))))

(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

(define (with-output-to-string thunk)
  (call-with-output-string
    (lambda (out)
      (let ((old-out (current-output-port)))
        (current-output-port out)
        (thunk)
        (current-output-port old-out)))))

(define (definite-article x)
  (define (vowel? c)
    (memv c '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U)))
  (define (vowel-exception? str)
    (member (string-downcase str)
            '("european" "ewe" "unicorn" "unicycle" "university" "user")))
  (define (consonant-exception? str)
    ;; not "historic" according to elements of style
    (member (string-downcase str)
            '("heir" "herb" "herbal" "herbivore" "honest" "honor" "hour")))
  (let* ((full-str (with-output-to-string (lambda () (cat x))))
         (i (string-scan #\space full-str))
         (str (if i (substring full-str 0 i) full-str)))
    (string-append
     (cond
      ((equal? str "") "a ")
      ((vowel? (string-ref str 0)) (if (vowel-exception? str) "a " "an "))
      (else (if (consonant-exception? str) "an " "a ")))
     full-str)))

(define (func-name func)
  (caddr func))

(define (func-scheme-name x)
  (if (pair? x) (car x) x))

(define (func-c-name x)
  (if (pair? x) (cadr x) x))

(define (stub-name sym)
  (string-append "sexp_" (mangle sym) "_stub"))

(define (type-id-name sym)
  (string-append "sexp_" (mangle sym) "_type_id"))

(define (signed-int-type? type)
  (memq type '(short int long)))

(define (unsigned-int-type? type)
  (memq type '(unsigned-short unsigned-int unsigned-long size_t pid_t)))

(define (int-type? type)
  (or (signed-int-type? type) (unsigned-int-type? type)))

(define (float-type? type)
  (memq type '(float double long-double)))

(define (c-declare . args)
  (apply cat args)
  (newline))

(define (c-system-include header)
  (cat "\n#include <" header ">\n"))

(define-syntax define-c-struct
  (er-macro-transformer
   (lambda (expr rename compare)
     (set! types (cons (cdr expr) types))
     `(cat "\nstatic sexp_uint_t " ,(type-id-name (cadr expr)) ";\n"))))

(define-syntax define-c
  (er-macro-transformer
   (lambda (expr rename compare)
     (set! funcs (cons (cons (stub-name (func-scheme-name (caddr expr)))
                             (cdr expr))
                       funcs))
     #f)))

(define (with-parsed-type type proc)
  (let lp ((type type) (free? #f) (const? #f) (null-ptr? #f)
           (pointer? #f) (struct? #f) (link? #f) (result? #f))
    (define (next) (if (null? (cddr type)) (cadr type) (cdr type)))
    (case (and (pair? type) (car type))
      ((free) (lp (next) #t const? null-ptr? pointer? struct? link? result?))
      ((const) (lp (next) free? #t null-ptr? pointer? struct? link? result?))
      ((maybe-null) (lp (next) free? const? #t pointer? struct? link? result?))
      ((pointer) (lp (next) free? const? null-ptr? #t struct? link? result?))
      ((struct) (lp (next) free? const? null-ptr? pointer? #t link? result?))
      ((link) (lp (next) free? const? null-ptr? pointer? struct? #t result?))
      ((result) (lp (next) free? const? null-ptr? pointer? struct? link? #t))
      (else (proc type free? const? null-ptr? pointer? struct? link? result?)))))

(define (get-base-type type)
  (with-parsed-type type (lambda (x . args) x)))

(define (c->scheme-converter type val . o)
  (with-parsed-type
   type
   (lambda (type free? const? null-ptr? pointer? struct? link? result?)
     (cond
      ((eq? type 'void)
       (cat "((" val "), SEXP_VOID)"))
      ((memq type '(sexp errno))
       (cat val))
      ((eq? type 'time_t)
       (cat "sexp_make_integer(ctx, sexp_shift_epoch(" val "))"))
      ((int-type? type)
       (cat "sexp_make_integer(ctx, " val ")"))
      ((eq? 'string type)
       (cat "sexp_c_string(ctx, " val ", -1)"))
      ((eq? 'input-port type)
       (cat "sexp_make_input_port(ctx, " val ", SEXP_FALSE)"))
      ((eq? 'output-port type)
       (cat "sexp_make_output_port(ctx, " val ", SEXP_FALSE)"))
      (else
       (let ((ctype (assq type types)))
         (cond
          (ctype
           (cat "sexp_make_cpointer(ctx, "  (type-id-name type) ", "
                val ", " (or (and (pair? o) (car o)) "SEXP_FALSE") ", "
                (if free? 1 0) ")"))
          (else
           (error "unknown type" type)))))))))

(define (scheme->c-converter type val)
  (with-parsed-type
   type
   (lambda (type free? const? null-ptr? pointer? struct? link? result?)
     (cond
      ((eq? 'sexp type)
       (cat val))
      ((eq? type 'time_t)
       (cat "sexp_uint_value(sexp_unshift_epoch(" val "))"))
      ((signed-int-type? type)
       (cat "sexp_sint_value(" val ")"))
      ((unsigned-int-type? type)
       (cat "sexp_uint_value(" val ")"))
      ((eq? 'string type)
       (cat "sexp_string_data(" val ")"))
      (else
       (let ((ctype (assq type types)))
         (cond
          (ctype
           (cat (if null-ptr?
                    "sexp_cpointer_maybe_null_value"
                    "sexp_cpointer_value")
                "(" val ")"))
          (else
           (error "unknown type" type)))))))))

(define (type-predicate type)
  (with-parsed-type
   type
   (lambda (type free? const? null-ptr? pointer? struct? link? result?)
     (cond
      ((int-type? type) "sexp_exact_integerp")
      ((float-type? type) "sexp_flonump")
      ((eq? 'string type) "sexp_stringp")
      (else #f)))))

(define (type-name type)
  (with-parsed-type
   type
   (lambda (type free? const? null-ptr? pointer? struct? link? result?)
     (cond
       ((int-type? type) "integer")
       ((float-type? type) "flonum")
       (else type)))))

(define (type-c-name type)
  (with-parsed-type
   type
   (lambda (base-type free? const? null-ptr? pointer? struct? link? result?)
     (let ((struct? (assq base-type types)))
       (string-append
        (if const? "const " "")
        (if struct? "struct " "")
        (string-replace (symbol->string base-type) #\- #\space)
        (if struct? "*" "")
        (if pointer? "*" ""))))))

(define (check-type arg type)
  (with-parsed-type
   type
   (lambda (base-type free? const? null-ptr? pointer? struct? link? result?)
     (cond
      ((or (int-type? base-type) (float-type? base-type) (eq? 'string base-type))
       (cat (type-predicate type) "(" arg ")"))
      (else
       (cond
        ((assq base-type types)
         (cat
          (if null-ptr? "(" "")
          "(sexp_pointerp(" arg  ")"
          " && (sexp_pointer_tag(" arg  ") == " (type-id-name base-type) "))"
          (lambda () (if null-ptr? (cat " || sexp_not(" arg "))")))))
        (else
         (display "WARNING: don't know how to check: " (current-error-port))
         (write type (current-error-port))
         (newline (current-error-port))
         (cat "1"))))))))

(define (validate-type arg type)
  (with-parsed-type
   type
   (lambda (base-type free? const? null-ptr? pointer? struct? link? result?)
     (cond
      ((or (int-type? base-type) (float-type? base-type) (eq? 'string base-type))
       (cat
        "  if (! " (lambda () (check-type arg type)) ")\n"
        "    return sexp_type_exception(ctx, \"not "
        (definite-article (type-name type)) "\", "
        arg ");\n"))
      (else
       (cond
        ((assq base-type types)
         (cat
          "  if (! " (lambda () (check-type arg type)) ")\n"
          "    return sexp_type_exception(ctx, \"not "
          (definite-article type) "\", " arg ");\n"))
        (else
         (display "WARNING: don't know how to validate: " (current-error-port))
         (write type (current-error-port))
         (newline (current-error-port))
         (write type))))))))

(define (get-func-result func)
  (let lp ((ls (cadddr func)))
    (and (pair? ls)
         (if (memq 'result (car ls))
             (car ls)
             (lp (cdr ls))))))

(define (get-func-args func)
  (let lp ((ls (cadddr func)) (res '()))
    (if (pair? ls)
        (if (and (pair? (car ls))
                 (or (memq 'result (car ls)) (memq 'value (car ls))))
            (lp (cdr ls) res)
            (lp (cdr ls) (cons (car ls) res)))
        (reverse res))))

(define (write-func func)
  (let ((ret-type (cadr func))
        (result (get-func-result func))
        (args (get-func-args func)))
    (cat "static sexp " (car func) "(sexp ctx")
    (let lp ((ls args) (i 0))
      (cond ((pair? ls)
             (cat ", sexp arg" i)
             (lp (cdr ls) (+ i 1)))))
    (cat ") {\n  sexp res;\n")
    (if (eq? 'errno ret-type) (cat "  int err;\n"))
    (if result (cat "  " (type-c-name result) " tmp;\n"))
    (let lp ((ls args) (i 0))
      (cond ((pair? ls)
             (validate-type (string-append "arg" (number->string i)) (car ls))
             (lp (cdr ls) (+ i 1)))))
    (cat (if (eq? 'errno ret-type) "  err = " "  res = "))
    (c->scheme-converter
     ret-type
     (lambda ()
       (cat (func-c-name (func-name func)) "(")
       (let lp ((ls (cadddr func)) (i 0))
         (cond ((pair? ls)
                (cat (cond
                      ((eq? (car ls) result)
                       "&tmp")
                      ((and (pair? (car ls)) (memq 'value (car ls)))
                       => (lambda (x) (write (cadr x)) ""))
                      (else
                       (lambda ()
                         (scheme->c-converter
                          (car ls)
                          (string-append "arg" (number->string i))))))
                     (if (pair? (cdr ls)) ", " ""))
                (lp (cdr ls) (+ i 1)))))
       (cat ")")))
    (cat ";\n")
    (if (eq? 'errno ret-type)
        (if result
            (cat "  res = (err ? SEXP_FALSE : "
                 (lambda () (c->scheme-converter result "tmp"))
                 ");\n")
            (cat "  res = sexp_make_boolean(! err);\n")))
    (cat "  return res;\n"
         "}\n\n")))

(define (write-func-binding func)
  (cat "  sexp_define_foreign(ctx, env, "
       (lambda () (write (symbol->string (func-scheme-name (func-name func)))))
       ", " (length (get-func-args func))  ", " (car func) ");\n"))

(define (write-type type)
  (let ((name (car type))
        (type (cdr type)))
    (with-parsed-type
     type
     (lambda (base-type free? const? null-ptr? pointer? struct? link? result?)
       (cat "  name = sexp_c_string(ctx, \"" (type-name name) "\", -1);\n"
            "  " (type-id-name name)
            " = sexp_unbox_fixnum(sexp_register_c_type(ctx, name, "
            (cond ((memq 'finalizer: base-type)
                   => (lambda (x) (stub-name (cadr x))))
                  (else "sexp_finalize_c_type"))
            "));\n")
       (cond
        ((memq 'predicate: base-type)
         => (lambda (x)
              (let ((pred (cadr x)))
                (cat "  tmp = sexp_make_type_predicate(ctx, name, "
                     "sexp_make_fixnum(" (type-id-name name) "));\n"
                     "  name = sexp_intern(ctx, \"" pred "\");\n"
                     "  sexp_env_define(ctx, env, name, tmp);\n")))))))))

(define (type-getter-name type name field)
  (string-append "sexp_" (x->string (type-name name))
                 "_get_" (x->string (get-base-type (cadr field)))))

(define (write-type-getter type name field)
  (with-parsed-type
   (car field)
   (lambda (field-type free? const? null-ptr? pointer? struct? link? result?)
     (cat "static sexp " (type-getter-name type name field)
          " (sexp ctx, sexp x) {\n"
          (lambda () (validate-type "x" name))
          "  return "
          (lambda ()
            (c->scheme-converter
             field-type
             (string-append "((struct " (mangle name) "*)"
                            "sexp_cpointer_value(x))"
                            (if struct? "." "->")
                            (x->string (cadr field)))
             (and (or struct? link?) "x")))
          ";\n"
          "}\n\n"))))

(define (type-setter-name type name field)
  (string-append "sexp_" (x->string (type-name name))
                 "_set_" (x->string (get-base-type (car field)))))

(define (write-type-setter type name field)
  (with-parsed-type
   (car field)
   (lambda (field-type free? const? null-ptr? pointer? struct? link? result?)
     (cat "static sexp " (type-setter-name type name field)
         " (sexp ctx, sexp x, sexp v) {\n"
         (lambda () (validate-type "x" name))
         (lambda () (validate-type "v" (car field)))
         "  "
         (lambda () (c->scheme-converter
                 field-type
                 (string-append "((struct " (mangle name) "*)"
                                "sexp_cpointer_value(x))"
                                (if struct? "." "->")
                                (x->string (cadr field)))))
         " = v;\n"
         "  return SEXP_VOID;"
         "}\n\n"))))

(define (write-type-funcs type)
  (let ((name (car type))
        (type (cdr type)))
    (with-parsed-type
     type
     (lambda (base-type free? const? null-ptr? pointer? struct? link? result?)
       (cond
        ((memq 'finalizer: base-type)
         => (lambda (x)
              (cat "static sexp " (stub-name (cadr x))
                   " (sexp ctx, sexp x) {\n"
                   "  if (sexp_cpointer_freep(x))\n"
                   "    " (cadr x) "(sexp_cpointer_value(x));\n"
                   "  return SEXP_VOID;\n"
                   "}\n\n"))))
       (cond
        ((memq 'constructor: base-type)
         => (lambda (x)
              (let ((make (caadr x))
                    (args (cdadr x)))
                (cat "static sexp " (stub-name make)
                     " (sexp ctx"
                     (lambda () (for-each (lambda (x) (cat ", sexp " x)) args))
                     ") {\n"
                     "  struct " (type-name name) " *r;\n"
                     "  sexp res = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer) + sizeof(struct " (type-name name) "), "
                     (type-id-name name)
                     ");\n"
                     "  sexp_cpointer_value(res) = sexp_cpointer_body(res);\n"
                     "  r = sexp_cpointer_value(res);\n"
                     "  return res;\n"
                     "}\n\n")
                (set! funcs
                      (cons (list (stub-name make) 'void make args) funcs))))))
       (for-each
        (lambda (field)
          (cond
           ((and (pair? field) (pair? (cdr field)))
            (cond
             ((and (pair? (cddr field)) (caddr field))
              (write-type-getter type name field)
              (set! funcs
                    (cons (list (type-getter-name type name field)
                                (car field) (caddr field) (list name))
                          funcs))))
            (cond
             ((and (pair? (cddr field))
                   (pair? (cdddr field))
                   (car (cdddr field)))
              (write-type-setter type name field)
              (set! funcs
                    (cons (list (type-setter-name type name field)
                                (car field) (cadddr field)
                                (list name (car field)))
                          funcs)))))))
        base-type)))))

(define (write-init)
  (newline)
  (for-each write-func funcs)
  (for-each write-type-funcs types)
  (cat "sexp sexp_init_library (sexp ctx, sexp env) {\n"
       "  sexp_gc_var2(name, tmp);\n"
       "  sexp_gc_preserve2(ctx, name, tmp);\n")
  (for-each write-type types)
  (for-each write-func-binding funcs)
  (cat "  sexp_gc_release2(ctx);\n"
       "  return SEXP_VOID;\n"
       "}\n\n"))

(define (generate file)
  (display "/* automatically generated by chibi genstubs */\n")
  (c-system-include "chibi/eval.h")
  (load file)
  (write-init))

(define (main args)
  (case (length args)
    ((1)
     (with-output-to-file (string-append (strip-extension (car args)) ".c")
       (lambda () (generate (car args)))))
    ((2)
     (if (equal? "-" (cadr args))
         (generate (car args))
         (with-output-to-file (cadr args) (lambda () (generate (car args))))))
    (else
     (error "usage: genstubs <file.stub> [<output.c>]"))))

(main (command-line-arguments))

