#! /usr/bin/env chibi-scheme

;; Note: this evolved as a throw-away script to provide certain core
;; modules, and so is a mess.  Tread carefully.

;; Simple C FFI.  "genstubs.scm file.stub" will read in the C function
;; FFI definitions from file.stub and output the appropriate C
;; wrappers into file.c.  You can then compile that file with:
;;
;;   cc -fPIC -shared file.c -lchibi-scheme
;;
;; (or using whatever flags are appropriate to generate shared libs on
;; your platform) and then the generated .so file can be loaded
;; directly with load, or portably using (include-shared "file") in a
;; module definition (note that include-shared uses no suffix).

;; The goal of this interface is to make access to C types and
;; functions easy, without requiring the user to write any C code.
;; That means the stubber needs to be intelligent about various C
;; calling conventions and idioms, such as return values passed in
;; actual parameters.  Writing C by hand is still possible, and
;; several of the core modules provide C interfaces directly without
;; using the stubber.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Struct Interface
;;
;; (define-c-struct struct-name
;;   [predicate: predicate-name]
;;   [constructor: constructor-name]
;;   [finalizer: c_finalizer_name]
;;   (type c_field_name getter-name setter-name) ...)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function Interface
;;
;; (define-c return-type name-spec (arg-type ...))
;;
;; where name-space is either a symbol name, or a list of
;; (scheme-name c_name).  If just a symbol, the C name is taken
;; to be the same with -'s replaced by _'s.
;;
;; arg-type is a type suitable for input validation and conversion.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Types
;;
;; Types
;;
;; Basic Types
;;   void
;;   boolean
;;   char
;;   sexp (no conversions)
;;
;; Integer Types:
;;   signed-char short int long
;;   unsigned-char unsigned-short unsigned-int unsigned-long size_t pid_t
;;   time_t (in seconds, but using the chibi epoch of 2010/01/01)
;;   errno (as a return type returns #f on error)
;;
;; Float Types:
;;   float double long-double
;;
;; String Types:
;;   string - a null-terminated char*
;;   env-string - a VAR=VALUE string represented as a (VAR . VALUE) pair inScheme
;;   in addition you can use (array char) as a string
;;
;; Port Types:
;;   input-port output-port
;;   port-or-fd - an fd-backed port or a fixnum
;;
;; Struct Types:
;;
;; Struct types are by default just referred to by the bare
;; struct-name from define-c-struct, and it is assumed you want a
;; pointer to that type.  To refer to the full struct, use the struct
;; modifier, as in (struct struct-name).

;; Type modifiers
;;
;; Any type may also be written as a list of modifiers followed by the
;; type itself.  The supported modifiers are:
;;
;; const: prepends the "const" C type modifier
;;        * as a return or result parameter, makes non-immediates immutable
;;
;; free:  it's Scheme's responsibility to "free" this resource
;;        * as a return or result parameter, registers the freep flag
;;          this causes the type finalizer to be run when GCed
;;
;; maybe-null: this pointer type may be NULL
;;        * as a result parameter, NULL is translated to #f
;;          normally this would just return a wrapped NULL pointer
;;        * as an input parameter, #f is translated to NULL
;;          normally this would be a type error
;;
;; pointer: create a pointer to this type
;;        * as a return parameter, wraps the result in a vanilla cpointer
;;        * as a result parameter, boxes then unboxes the value
;;
;; struct: treat this struct type as a struct, not a pointer
;;        * as an input parameter, dereferences the pointer
;;        * as a type field, indicates a nested struct
;;
;; link:  add a gc link
;;        * as a field getter, link to the parent object, so the
;;          parent won't be GCed so long as we have a reference
;;          to the child.  this behavior is automatic for nested
;;          structs.
;;
;; result: return a result in this parameter
;;        * if there are multiple results (including the return type),
;;          they are all returned in a list
;;        * if there are any result parameters, a return type
;;          of errno returns #f on failure, and as eliminated
;;          from the list of results otherwise
;;
;; (value <expr>): specify a fixed value
;;        * as an input parameter, this parameter is not provided
;;          in the Scheme API but always passed as <expr>
;;
;; (default <expr>): specify a default value
;;        * as the final input parameter, makes the Scheme parameter
;;          optional, defaulting to <expr>
;;
;; (array <type> [<length>])  an array type
;;        * length must be specified for return and result parameters
;;        * if specified, length can be any of
;;        ** an integer, for a fixed size
;;        ** the symbol null, indicating a NULL-terminated array

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(define *types* '())
(define *funcs* '())
(define *consts* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type objects

(define (parse-type type . o)
  (cond
   ((vector? type)
    type)
   (else
    (let lp ((type type) (free? #f) (const? #f) (null-ptr? #f)
             (ptr? #f) (struct? #f) (link? #f) (result? #f) (array #f)
             (value #f) (default? #f))
      (define (next) (if (null? (cddr type)) (cadr type) (cdr type)))
      (case (and (pair? type) (car type))
        ((free)
         (lp (next) #t const? null-ptr? ptr? struct? link? result? array value default?))
        ((const)
         (lp (next) free? #t null-ptr? ptr? struct? link? result? array value default?))
        ((maybe-null)
         (lp (next) free? const? #t ptr? struct? link? result? array value default?))
        ((pointer)
         (lp (next) free? const? null-ptr? #t struct? link? result? array value default?))
        ((struct)
         (lp (next) free? const? null-ptr? ptr? #t link? result? array value default?))
        ((link)
         (lp (next) free? const? null-ptr? ptr? struct? #t result? array value default?))
        ((result)
         (lp (next) free? const? null-ptr? ptr? struct? link? #t array value default?))
        ((array)
         (lp (cadr type) free? const? null-ptr? ptr? struct? link? result? (if (pair? (cddr type)) (caddr type) #t) value default?))
        ((value)
         (lp (cddr type) free? const? null-ptr? ptr? struct? link? result? array (cadr type) default?))
        ((default)
         (lp (cddr type) free? const? null-ptr? ptr? struct? link? result? array (cadr type) #t))
        (else
         (vector (if (and (pair? type) (null? (cdr type))) (car type) type) free? const? null-ptr? ptr? struct? link? result? array value default? (and (pair? o) (car o)))))))))

(define (type-base type) (vector-ref type 0))
(define (type-free? type) (vector-ref type 1))
(define (type-const? type) (vector-ref type 2))
(define (type-null? type) (vector-ref type 3))
(define (type-pointer? type) (vector-ref type 4))
(define (type-struct? type) (vector-ref type 5))
(define (type-link? type) (vector-ref type 6))
(define (type-result? type) (vector-ref type 7))
(define (type-array type) (vector-ref type 8))
(define (type-value type) (vector-ref type 9))
(define (type-default? type) (vector-ref type 10))
(define (type-index type) (vector-ref type 11))

(define (type-auto-expand? type)
  (and (pair? (type-array type))
       (memq 'auto-expand (type-array type))))

(define (type-index-string type)
  (if (integer? (type-index type))
      (number->string (type-index type))
      ""))

(define (struct-fields ls)
  (let lp ((ls ls) (res '()))
    (cond ((null? ls) (reverse res))
          ((symbol? (car ls)) (lp (cddr ls) res))
          (else (lp (cdr ls) (cons (car ls) res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type predicates

(define (signed-int-type? type)
  (memq type '(signed-char short int long boolean)))

(define (unsigned-int-type? type)
  (memq type '(unsigned-char unsigned-short unsigned-int unsigned-long
               size_t off_t time_t clock_t dev_t ino_t mode_t nlink_t
               uid_t gid_t pid_t blksize_t blkcnt_t sigval_t)))

(define (int-type? type)
  (or (signed-int-type? type) (unsigned-int-type? type)))

(define (float-type? type)
  (memq type '(float double long-double long-long-double)))

(define (string-type? type)
  (or (memq type '(char* string env-string non-null-string))
      (and (vector? type)
           (type-array type)
           (not (type-pointer? type))
           (eq? 'char (type-base type)))))

(define (port-type? type)
  (memq type '(port input-port output-port)))

(define (error-type? type)
  (memq type '(errno non-null-string non-null-pointer)))

(define (array-type? type)
  (and (type-array type) (not (eq? 'char (type-base type)))))

(define (basic-type? type)
  (let ((type (parse-type type)))
    (and (not (type-array type))
         (not (void-pointer-type? type))
         (not (assq (type-base type) *types*)))))

(define (void-pointer-type? type)
  (or (and (eq? 'void (type-base type)) (type-pointer? type))
      (eq? 'void* (type-base type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function objects

(define (parse-func func)
  (if (not (and (= 3 (length func))
                (or (identifier? (cadr func)) (list (cadr func)))
                (list (caddr func))))
      (error "bad function definition" func))
  (let* ((ret-type (parse-type (car func)))
         (scheme-name (if (pair? (cadr func)) (caadr func) (cadr func)))
         (c-name (if (pair? (cadr func))
                     (cadadr func)
                     (mangle scheme-name)))
         (stub-name (if (and (pair? (cadr func)) (pair? (cddadr func)))
                        (car (cddadr func))
                        (generate-stub-name scheme-name))))
    (let lp ((ls (caddr func))
             (i 0)
             (results '())
             (c-args '())
             (s-args '()))
      (cond
       ((null? ls)
        (if (> i 6)
            (error "FFI currently only supports up to 6 scheme args" func))
        (vector scheme-name c-name stub-name ret-type
                (reverse results) (reverse c-args) (reverse s-args)))
       (else
        (let ((type (parse-type (car ls) i)))
          (cond
           ((type-result? type)
            (lp (cdr ls) (+ i 1) (cons type results) (cons type c-args) s-args))
           ((and (type-value type) (not (type-default? type)))
            (lp (cdr ls) (+ i 1) results (cons type c-args) s-args))
           (else
            (lp (cdr ls) (+ i 1) results (cons type c-args) (cons type s-args)))
           )))))))

(define (func-scheme-name func) (vector-ref func 0))
(define (func-c-name func) (vector-ref func 1))
(define (func-stub-name func) (vector-ref func 2))
(define (func-ret-type func) (vector-ref func 3))
(define (func-results func) (vector-ref func 4))
(define (func-c-args func) (vector-ref func 5))
(define (func-scheme-args func) (vector-ref func 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define (cat . args)
  (for-each (lambda (x) (if (procedure? x) (x) (display x))) args))

(define (x->string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        (else (error "non-stringable object" x))))

(define (filter pred ls)
  (cond ((null? ls) '())
        ((pred (car ls)) (cons (car ls) (filter pred (cdr ls))))
        (else (filter pred (cdr ls)))))

(define (remove pred ls)
  (cond ((null? ls) '())
        ((pred (car ls)) (filter pred (cdr ls)))
        (else (cons (car ls) (filter pred (cdr ls))))))

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
       ((eqv? c (string-ref str i)) (lp (+ i 1) (+ i 1) (cons r (collect))))
       (else (lp from (+ i 1) res))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; naming

(define (c-char? c)
  (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\_ #\- #\! #\?))))

(define (c-escape str)
  (let ((len (string-length str)))
    (let lp ((from 0) (i 0) (res '()))
      (define (collect) (if (= i from) res (cons (substring str from i) res)))
      (cond
       ((>= i len) (string-concatenate-reverse (collect)))
       ((not (c-char? (string-ref str i))) (lp (+ i 1) (+ i 1) (cons "_" (cons (number->string (char->integer (string-ref str i)) 16) (collect)))))
       (else (lp from (+ i 1) res))))))

(define (mangle x)
  (string-replace
   (string-replace (string-replace (c-escape (x->string x)) #\- "_") #\? "_p")
   #\! "_x"))

(define (generate-stub-name sym)
  (string-append "sexp_" (mangle sym) "_stub"))

(define (type-id-name sym)
  (string-append "sexp_" (mangle sym) "_type_id"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .stub file interface

(define (c-declare . args)
  (apply cat args)
  (newline))

(define (c-include header)
  (cat "\n#include \"" header "\"\n"))

(define (c-system-include header)
  (cat "\n#include <" header ">\n"))

(define (parse-struct-like ls)
  (let lp ((ls ls) (res '()))
    (cond
     ((null? ls)
      (reverse res))
     ((symbol? (car ls))
      (lp (cddr ls) (cons (cadr ls) (cons (car ls) res))))
     ((pair? (car ls))
      (lp (cdr ls) (cons (cons (parse-type (caar ls)) (cdar ls)) res)))
     (else
      (lp (cdr ls) (cons (car ls) res))))))

(define-syntax define-struct-like
  (er-macro-transformer
   (lambda (expr rename compare)
     (set! *types*
           `((,(cadr expr)
              ,@(parse-struct-like (cddr expr)))
             ,@*types*))
     `(cat "\nstatic sexp_uint_t " ,(type-id-name (cadr expr)) ";\n"))))

(define-syntax define-c-struct
  (er-macro-transformer
   (lambda (expr rename compare)
     `(define-struct-like ,(cadr expr) type: struct ,@(cddr expr)))))

(define-syntax define-c-class
  (er-macro-transformer
   (lambda (expr rename compare)
     `(define-struct-like ,(cadr expr) type: class ,@(cddr expr)))))

(define-syntax define-c-type
  (er-macro-transformer
   (lambda (expr rename compare)
     `(define-struct-like ,(cadr expr) ,@(cddr expr)))))

(define-syntax define-c
  (er-macro-transformer
   (lambda (expr rename compare)
     (set! *funcs* (cons (parse-func (cdr expr)) *funcs*))
     #f)))

(define-syntax define-c-const
  (er-macro-transformer
   (lambda (expr rename compare)
     (set! *consts*
           (cons (cons (parse-type (cadr expr)) (cddr expr)) *consts*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C code generation

(define (c->scheme-converter type val . o)
  (let ((base (type-base type)))
    (cond
     ((and (eq? base 'void) (not (type-pointer? type)))
      (cat "((" val "), SEXP_VOID)"))
     ((or (eq? base 'sexp) (error-type? base))
      (cat val))
     ((eq? base 'boolean)
      (cat "sexp_make_boolean(" val ")"))
     ((eq? base 'time_t)
      (cat "sexp_make_integer(ctx, sexp_shift_epoch(" val "))"))
     ((unsigned-int-type? base)
      (cat "sexp_make_unsigned_integer(ctx, " val ")"))
     ((signed-int-type? base)
      (cat "sexp_make_integer(ctx, " val ")"))
     ((eq? base 'char)
      (if (type-array type)
          (cat "sexp_c_string(ctx, " val ", -1)")
          (cat "sexp_make_character(ctx, " val ")")))
     ((eq? 'env-string base)
      (cat "(p=strchr(" val ", '=') ? "
           "sexp_cons(ctx, str=sexp_c_string(ctx, " val ", p - " val "), str=sexp_c_string(ctx, p, -1))"
           " : sexp_cons(ctx, str=" val ", SEXP_FALSE)"))
     ((string-type? base)
      (cat "sexp_c_string(ctx, " val ", -1)"))
     ((eq? 'input-port base)
      (cat "sexp_make_input_port(ctx, " val ", SEXP_FALSE)"))
     ((eq? 'output-port base)
      (cat "sexp_make_output_port(ctx, " val ", SEXP_FALSE)"))
     (else
      (let ((ctype (assq base *types*))
            (void*? (void-pointer-type? type)))
        (cond
         ((or ctype void*?)
          (cat "sexp_make_cpointer(ctx, "
               (if void*? "SEXP_CPOINTER" (type-id-name base)) ", "
               val ", " (or (and (pair? o) (car o)) "SEXP_FALSE") ", "
               (if (or (type-free? type)
                       (and (type-result? type) (not (basic-type? type))))
                   1
                   0)
               ")"))
         (else
          (error "unknown type" base))))))))

(define (scheme->c-converter type val)
  (let* ((type (parse-type type))
         (base (type-base type)))
    (cond
     ((eq? base 'sexp)
      (cat val))
     ((eq? base 'boolean)
      (cat "sexp_truep(" val ")"))
     ((eq? base 'time_t)
      (cat "sexp_unshift_epoch(sexp_uint_value(" val "))"))
     ((signed-int-type? base)
      (cat "sexp_sint_value(" val ")"))
     ((unsigned-int-type? base)
      (cat "sexp_uint_value(" val ")"))
     ((eq? base 'char)
      (cat "sexp_unbox_character(" val ")"))
     ((eq? base 'env-string)
      (cat "sexp_concat_env_string(" val ")"))
     ((string-type? base)
      (cat "sexp_string_data(" val ")"))
     ((eq? base 'port-or-fd)
      (cat "(sexp_portp(" val ") ? fileno(sexp_port_stream(" val "))"
           " : sexp_unbox_fixnum(" val "))"))
     ((port-type? base)
      (cat "sexp_port_stream(" val ")"))
     (else
      (let ((ctype (assq base *types*))
            (void*? (void-pointer-type? type)))
        (cond
         ((or ctype void*?)
          (cat "(" (type-c-name type) ")"
               (if (type-null? type)
                   "sexp_cpointer_maybe_null_value"
                   "sexp_cpointer_value")
               "(" val ")"))
         (else
          (error "unknown type" base))))))))

(define (type-predicate type)
  (let ((base (type-base (parse-type type))))
    (cond
     ((int-type? base) "sexp_exact_integerp")
     ((float-type? base) "sexp_flonump")
     ((string-type? base) "sexp_stringp")
     ((eq? base 'char) "sexp_charp")
     ((eq? base 'boolean) "sexp_booleanp")
     ((eq? base 'port) "sexp_portp")
     ((eq? base 'input-port) "sexp_iportp")
     ((eq? base 'output-port) "sexp_oportp")
     (else #f))))

(define (type-name type)
  (let ((base (type-base (parse-type type))))
   (cond
    ((int-type? base) "integer")
    ((float-type? base) "flonum")
    ((eq? 'boolean base) "int")
    (else base))))

(define (base-type-c-name base)
  (case base
    ((string env-string non-null-string) "char*")
    (else (symbol->string base))))

(define (type-struct-type type)
  (let ((type-spec (assq (if (vector? type) (type-base type) type) *types*)))
    (cond ((and type-spec (memq 'type: type-spec)) => cadr)
          (else #f))))

(define (type-c-name type)
  (let* ((type (parse-type type))
         (base (type-base type))
         (type-spec (assq base *types*))
         (struct-type (type-struct-type type)))
    (string-append
     (if (type-const? type) "const " "")
     (if struct-type (string-append (symbol->string struct-type) " ") "")
     (string-replace (base-type-c-name base) #\- " ")
     (if type-spec "*" "")
     (if (type-pointer? type) "*" ""))))

(define (check-type arg type)
  (let* ((type (parse-type type))
         (base (type-base type)))
    (cond
     ((eq? base 'env-string)
      (cat "(sexp_pairp(" arg ") && sexp_stringp(sexp_car(" arg
           ")) && sexp_stringp(sexp_cdr(" arg ")))"))
     ((or (int-type? base) (float-type? base)
          (string-type? base) (port-type? base))
      (cat (type-predicate type) "(" arg ")"))
     ((or (assq base *types*) (void-pointer-type? type))
      (cat
       (if (type-null? type) "(" "")
       "(sexp_pointerp(" arg  ")"
       " && (sexp_pointer_tag(" arg  ") == "
       (if (void-pointer-type? type) "SEXP_CPOINTER" (type-id-name base)) "))"
       (lambda () (if (type-null? type) (cat " || sexp_not(" arg "))")))))
     (else
      (display "WARNING: don't know how to check: " (current-error-port))
      (write type (current-error-port))
      (newline (current-error-port))
      (cat "1")))))

(define (type-id-number type)
  (let ((base (type-base type)))
    (cond
     ((int-type? base) "SEXP_FIXNUM")
     ((float-type? base) "SEXP_FLONUM")
     ((string-type? base) "SEXP_STRING")
     ((eq? base 'char) "SEXP_CHAR")
     ((eq? base 'boolean) "SEXP_BOOLEAN")
     ((eq? base 'port) "SEXP_IPORT")
     ((eq? base 'input-port) "SEXP_IPORT")
     ((eq? base 'output-port) "SEXP_OPORT")
     (else (type-id-name base)))))

(define (write-validator arg type)
  (let* ((type (parse-type type))
         (array (type-array type))
         (base-type (type-base type)))
    (cond
     ((and array (not (string-type? type)))
      (cond
       ((number? array)
        (cat "  if (!sexp_listp(ctx, " arg ")"
             "      || sexp_unbox_fixnum(sexp_length(" arg ")) != " array ")\n"
             "    return sexp_type_exception(ctx, self, SEXP_PAIR, " arg ");\n")))
      (cat "  for (res=" arg "; sexp_pairp(res); res=sexp_cdr(res))\n"
           "    if (! " (lambda () (check-type "sexp_car(res)" type)) ")\n"
           "      return sexp_xtype_exception(ctx, self, \"not a list of "
           (type-name type) "s\", " arg ");\n")
      (if (not (number? array))
          (cat "  if (! sexp_nullp(res))\n"
               "    return sexp_xtype_exception(ctx, self, \"not a list of "
               (type-name type) "s\", " arg ");\n")))
     ((eq? base-type 'port-or-fd)
      (cat "  if (! (sexp_portp(" arg ") || sexp_fixnump(" arg ")))\n"
           "    return sexp_xtype_exception(ctx, self, \"not a port or file descriptor\"," arg ");\n"))
     ((or (int-type? base-type)
          (float-type? base-type)
          (string-type? base-type)
          (port-type? base-type))
      (cat
       "  if (! " (lambda () (check-type arg type)) ")\n"
       "    return sexp_type_exception(ctx, self, "
       (type-id-number type) ", " arg ");\n"))
     ((or (assq base-type *types*) (void-pointer-type? type))
      (cat
       "  if (! " (lambda () (check-type arg type)) ")\n"
       "    return sexp_type_exception(ctx, self, "
       (type-id-number type) ", " arg ");\n"))
     ((eq? 'sexp base-type))
     ((string-type? type)
      (write-validator arg 'string))
     (else
      (display "WARNING: don't know how to validate: " (current-error-port))
      (write type (current-error-port))
      (newline (current-error-port))))))

(define (write-parameters args)
  (lambda () (for-each (lambda (a) (cat ", sexp arg" (type-index a))) args)))

(define (get-array-length func x)
  (let ((len (if (pair? (type-array x))
                 (car (reverse (type-array x)))
                 (type-array x))))
    (if (number? len)
        len
        (and (symbol? len)
             (let* ((str (symbol->string len))
                    (len2 (string-length str)))
               (and (> len2 3)
                    (string=? "arg" (substring str 0 3))
                    (let ((i (string->number (substring str 3 len2))))
                      (if i
                          (let ((y (list-ref (func-c-args func) i)))
                            (or (type-value y) len))))))))))

(define (write-locals func)
  (define (arg-res x)
    (string-append "res" (type-index-string x)))
  (let* ((ret-type (func-ret-type func))
         (results (func-results func))
         (scheme-args (func-scheme-args func))
         (return-res? (not (error-type? (type-base ret-type))))
         (preserve-res? (> (+ (length results)) (if return-res? 0 1)))
         (single-res? (and (= 1 (length results)) (not return-res?)))
         (tmp-string? (any (lambda (a)
                             (and (type-array a)
                                  (string-type? (type-base a))))
                           (cons ret-type results)))
         (gc-vars (map arg-res results))
         (gc-vars (if tmp-string? (cons "str" gc-vars) gc-vars))
         (gc-vars (if preserve-res? (cons "res" gc-vars) gc-vars))
         (sexps (if preserve-res? '() '("res")))
         (num-gc-vars (length gc-vars))
         (ints (if (or return-res?
                       (memq (type-base ret-type)
                             '(non-null-string non-null-pointer)))
                   '()
                   '("err")))
         (ints (if (or (array-type? ret-type)
                       (any array-type? results)
                       (any array-type? scheme-args))
                   (cons "i" ints)
                   ints)))
    (case (type-base ret-type)
      ((non-null-string) (cat "  char *err;\n"))
      ((non-null-pointer) (cat "  void *err;\n")))
    (cond
     ((pair? ints)
      (cat "  int " (car ints))
      (for-each (lambda (x) (display ", ") (display x)) (cdr ints))
      (cat ";\n")))
    (if (any (lambda (a) (eq? 'env-string (type-base a)))
             (cons ret-type results))
        (cat "  char *p;\n"))
    (for-each
     (lambda (x)
       (let ((len (get-array-length func x)))
         (cat "  " (type-c-name (type-base x)) " ")
         (if (and (type-array x) (not (number? len)))
             (cat "*"))
         (cat (if (type-auto-expand? x) "buf" "tmp") (type-index-string x))
         (if (number? len)
             (cat "[" len "]"))
         (cat ";\n")
         (if (or (vector? len) (type-auto-expand? x))
             (cat "  int len" (type-index x) ";\n"))
         (if (type-auto-expand? x)
             (cat "  " (type-c-name (type-base x))
                  " *tmp" (type-index-string x) ";\n"))))
     (append (if (type-array ret-type) (list ret-type) '())
             results
             (remove type-result? (filter type-array scheme-args))))
    (for-each
     (lambda (arg)
       (cond
        ((and (type-pointer? arg) (basic-type? arg))
         (cat "  " (type-c-name (type-base arg))
              " tmp" (type-index arg) ";\n"))))
     scheme-args)
    (cond
     ((pair? sexps)
      (cat "  sexp " (car sexps))
      (for-each (lambda (x) (display ", ") (display x)) (cdr sexps))
      (cat ";\n")))
    (cond
     ((pair? gc-vars)
      (cat "  sexp_gc_var" num-gc-vars "(")
      (display (car gc-vars))
      (for-each (lambda (x) (display ", ") (display x)) (cdr gc-vars))
      (cat ");\n")
      (cat "  sexp_gc_preserve" num-gc-vars "(ctx")
      (for-each (lambda (x) (display ", ") (display x)) gc-vars)
      (cat ");\n")))))

(define (write-validators args)
  (for-each
   (lambda (a)
     (write-validator (string-append "arg" (type-index-string a)) a))
   args))

(define (write-temporaries func)
  (for-each
   (lambda (a)
     (let ((len (and (type-array a) (get-array-length func a))))
       (cond
        ((and (type-array a) (or (vector? len) (type-auto-expand? a)))
         (cat "  len" (type-index a) " = "
              (lambda ()
                (if (number? len) (cat len) (scheme->c-converter 'int len)))
              ";\n"
              "  tmp" (type-index a) " = buf" (type-index a) ";\n")))
       (cond
        ((and (not (type-result? a)) (type-array a) (not (string-type? a)))
         (if (not (number? (type-array a)))
             (cat "  tmp" (type-index a)
                  " = (" (type-c-name (type-base a)) "*) malloc("
                  "(sexp_unbox_fixnum(sexp_length(ctx, arg" (type-index a)
                  "))+1) * sizeof(tmp" (type-index a) "[0]));\n"))
         (cat "  for (i=0, res=arg" (type-index a)
              "; sexp_pairp(res); res=sexp_cdr(res), i++) {\n"
              "    tmp" (type-index a) "[i] = "
              (lambda () (scheme->c-converter (type-base a) "sexp_car(res)"))
              ";\n"
              "  }\n")
         (if (not (number? (type-array a)))
             (cat "  tmp" (type-index a) "[i] = NULL;\n")))
        ((and (type-result? a) (not (basic-type? a))
              (not (type-free? a)) (not (type-pointer? a))
              (not (type-auto-expand? a))
              (or (not (type-array a))
                  (not (integer? len))))
         (cat "  tmp" (type-index a) " = malloc("
              (if (and (symbol? len) (not (eq? len 'null)))
                  (lambda () (cat (lambda () (scheme->c-converter 'unsigned-int len))
                              "*sizeof(tmp" (type-index a) "[0])"))
                  (lambda () (cat "sizeof(tmp" (type-index a) "[0])")))
              ");\n"))
        ((and (type-pointer? a) (basic-type? a))
         (cat "  tmp" (type-index a) " = "
              (lambda ()
                (scheme->c-converter
                 a
                 (string-append "arg" (type-index-string a))))
              ";\n")))))
   (func-c-args func)))

(define (write-actual-parameter func arg)
  (cond
   ((and (not (type-default? arg)) (type-value arg))
    => (lambda (x)
         (cond
          ((any (lambda (y)
                  (and (type-array y)
                       (type-auto-expand? y)
                       (eq? x (get-array-length func y))))
                (func-c-args func))
           => (lambda (y) (cat "len" (type-index y))))
          (else (write x)))))
   ((or (type-result? arg) (type-array arg))
    (cat (if (or (type-pointer? arg) (type-free? arg) (basic-type? arg))
             "&"
             "")
         "tmp" (type-index arg)))
   ((and (type-pointer? arg) (basic-type? arg))
    (cat "&tmp" (type-index arg)))
   (else
    (scheme->c-converter
     arg
     (string-append "arg" (type-index-string arg))))))

(define (write-call func)
  (let ((ret-type (func-ret-type func))
        (c-name (func-c-name func))
        (c-args (func-c-args func)))
    (if (any type-auto-expand? (func-c-args func))
        (cat " loop:\n"))
    (cat (cond ((error-type? (type-base ret-type)) "  err = ")
               ((type-array ret-type) "  tmp = ")
               (else "  res = ")))
    ((if (type-array ret-type)
         (lambda (t f x) (f))
         c->scheme-converter)
     ret-type
     (lambda ()
       (cat c-name "(")
       (for-each
        (lambda (arg)
          (if (> (type-index arg) 0) (cat ", "))
          (write-actual-parameter func arg))
        c-args)
       (cat ")"))
     (cond
      ((any type-link? (func-c-args func))
       => (lambda (a) (string-append "arg" (type-index-string a))))
      (else #f)))
    (cat ";\n")
    (if (type-array ret-type)
        (write-result ret-type))))

(define (write-result result)
  (let ((res (string-append "res" (type-index-string result)))
        (tmp (string-append "tmp" (type-index-string result))))
    (cond
     ((and (type-array result) (eq? 'char (type-base result)))
      (cat "  " res " = " (lambda () (c->scheme-converter result tmp)) ";\n"))
     ((type-array result)
      (cat "  " res " = SEXP_NULL;\n")
      (let ((auto-expand?
             (and (pair? (type-array result))
                  (memq 'auto-expand (type-array result))))
            (len (if (pair? (type-array result))
                     (car (reverse (type-array result)))
                     (type-array result))))
        (cond
         ((eq? 'null len)
          (cat "  for (i=0; " tmp "[i]; i++) {\n"
               "    sexp_push(ctx, " res ", "
               (if (eq? 'string (type-base result))
                   "str="
                   (lambda () (cat "SEXP_VOID);\n    sexp_car(" res ") = ")))
               (lambda () (c->scheme-converter result (lambda () (cat tmp "[i]"))))
               ");\n"
               "  }\n"
               "  " res " = sexp_nreverse(ctx, " res ");\n"))
         (else
          (cat "  for (i=" len "-1; i>=0; i--) {\n"
               "    sexp_push(ctx, " res ", SEXP_VOID);\n"
               "    sexp_car(" res ") = "
               (lambda () (c->scheme-converter result (lambda () (cat tmp "[i]"))))
               ";\n"
               "  }\n")))))
     (else
      (cat "  " res " = ")
      (c->scheme-converter
       result
       (string-append "tmp" (type-index-string result)))
      (cat ";\n")))))

(define (write-results func)
  (let ((error-res? (error-type? (type-base (func-ret-type func))))
        (results (func-results func)))
    (if error-res?
        (cat "  if ("
             (if (memq (type-base (func-ret-type func))
                       '(non-null-string non-null-pointer))
                 "!"
                 "")
             "err) {\n"
             (cond
              ((any type-auto-expand? (func-c-args func))
               => (lambda (a)
                    (lambda ()
                      (let ((len (get-array-length func a))
                            (i (type-index a)))
                        (if (number? len)
                            (cat "  if (len" i " != " len ")\n"
                                 "    free(tmp" i ");\n"))
                        (cat "  len" i " *= 2;\n"
                             "  tmp" i
                             " = malloc(len" i "*sizeof(tmp" i "[0]));\n"
                             "  goto loop;\n")))))
              (else
               "  res = SEXP_FALSE;\n"))
             "  } else {\n"))
    (if (null? results)
        (if error-res?
            (cat "  res = SEXP_TRUE;\n"))
        (for-each write-result results))
    (cond
     ((> (length results) (if error-res? 1 0))
      (if error-res?
          (cat "  res = SEXP_NULL;\n")
          (cat "  res = sexp_cons(ctx, res, SEXP_NULL);\n"))
      (for-each
       (lambda (x)
         (if error-res?
             (cat "  sexp_push(ctx, res, res" (type-index x) ");\n")
             (cat "  sexp_push(ctx, res, sexp_car(res));\n"
                  "  sexp_cadr(res) = res" (type-index x) ";\n")))
       (reverse results)))
     ((pair? results)
      (cat "  res = res" (type-index (car results)) ";\n")))
    (if error-res?
        (cat "  }\n"))))

(define (write-free type)
  (if (type-array type)
      (cat "  free(tmp" (type-index-string type) ");\n")))

(define (write-cleanup func)
  (for-each write-free (func-scheme-args func))
  (for-each
   (lambda (a)
     (cond
      ((type-auto-expand? a)
       (let ((len (get-array-length func a))
             (i (type-index a)))
         (if (number? len)
             (cat "  if (len" i " != " len ")\n"
                  "    free(tmp" i ");\n"))))
      ((and (type-result? a) (not (basic-type? a))
            (not (assq (type-base a) *types*))
            (not (type-free? a)) (not (type-pointer? a))
            (or (not (type-array a))
                (not (integer? (get-array-length func a)))))
       ;; the above is hairy - basically this frees temporary strings
       (cat "  free(tmp" (type-index a) ");\n"))))
   (func-c-args func))
  (let* ((results (func-results func))
         (return-res? (not (error-type? (type-base (func-ret-type func)))))
         (preserve-res? (> (+ (length results)) (if return-res? 0 1)))
         (single-res? (and (= 1 (length results)) (not return-res?)))
         (tmp-string? (any (lambda (a)
                             (and (type-array a)
                                  (string-type? (type-base a))))
                           (cons (func-ret-type func)
                                 (func-results func))))
         (gc-vars results)
         (gc-vars (if tmp-string? (cons "str" gc-vars) gc-vars))
         (gc-vars (if preserve-res? (cons "res" gc-vars) gc-vars))
         (num-gc-vars (length gc-vars)))
    (cond
     ((pair? gc-vars)
      (cat "  sexp_gc_release" num-gc-vars "(ctx);\n")))))

(define (write-func func)
  (cat "static sexp " (func-stub-name func)
       " (sexp ctx sexp_api_params(self, n)"
       (write-parameters (func-scheme-args func)) ") {\n")
  (write-locals func)
  (write-validators (func-scheme-args func))
  (write-temporaries func)
  (write-call func)
  (write-results func)
  (write-cleanup func)
  (cat "  return res;\n"
       "}\n\n"))

(define (parameter-default? x)
  (and (pair? x)
       (member x '((current-input-port)
                   (current-output-port)
                   (current-error-port)))))

(define (write-default x) ;; this is a hack but very convenient
  (lambda ()
    (let ((value (type-value x)))
      (cond
       ((equal? value '(current-input-port))
        (cat "\"*current-input-port*\""))
       ((equal? value '(current-output-port))
        (cat "\"*current-output-port*\""))
       ((equal? value '(current-error-port))
        (cat "\"*current-error-port*\""))
       (else
        (c->scheme-converter x value))))))

(define (write-func-binding func)
  (let ((default (and (pair? (func-scheme-args func))
                      (type-default? (car (reverse (func-scheme-args func))))
                      (car (reverse (func-scheme-args func))))))
    (cat (if default
             (if (parameter-default? (type-value default))
                 "  sexp_define_foreign_param(ctx, env, "
                 "  sexp_define_foreign_opt(ctx, env, ")
             "  sexp_define_foreign(ctx, env, ")
         (lambda () (write (symbol->string (func-scheme-name func))))
         ", " (length (func-scheme-args func))  ", "
         (if default "(sexp_proc1)" "")
         (func-stub-name func)
         (if default ", " "")
         (if default (write-default default) "")
         ");\n")))

(define (write-type type)
  (let ((name (car type))
        (type (cdr type)))
    (cat "  name = sexp_c_string(ctx, \"" (type-name name) "\", -1);\n"
         "  " (type-id-name name)
         " = sexp_unbox_fixnum(sexp_register_c_type(ctx, name, "
         (cond ((memq 'finalizer: type)
                => (lambda (x) (generate-stub-name (cadr x))))
               (else "sexp_finalize_c_type"))
         "));\n")
    (cond
     ((memq 'predicate: type)
      => (lambda (x)
           (let ((pred (cadr x)))
             (cat "  tmp = sexp_make_type_predicate(ctx, name, "
                  "sexp_make_fixnum(" (type-id-name name) "));\n"
                  "  name = sexp_intern(ctx, \"" pred "\", "
                  (string-length (x->string pred)) ");\n"
                  "  sexp_env_define(ctx, env, name, tmp);\n")))))))

(define (type-getter-name type name field)
  (string-append "sexp_" (x->string (type-name (parse-type name)))
                 "_get_" (x->string (type-base (parse-type (cadr field))))))

(define (write-type-getter type name field)
  (cat "static sexp " (type-getter-name type name field)
       " (sexp ctx sexp_api_params(self, n), sexp x) {\n"
       (lambda () (write-validator "x" name))
       "  return "
       (lambda ()
         (c->scheme-converter
          (car field)
          (string-append (if (type-struct? (car field)) "&" "")
                         "((" (x->string (or (type-struct-type name) ""))
                         " " (mangle name) "*)"
                         "sexp_cpointer_value(x))" "->"
                         (x->string (cadr field)))
          (and (or (type-struct? (car field)) (type-link? (car field))) "x")))
       ";\n"
       "}\n\n"))

(define (type-setter-name type name field)
  (string-append "sexp_" (x->string (type-name (parse-type name)))
                 "_set_" (x->string (type-base (parse-type (cadr field))))))

(define (write-type-setter-assignment type name field dst val)
  (cond
   ((type-struct? (car field))
    ;; assign to a nested struct - copy field-by-field
    (let ((field-type
           (cond ((assq (type-name (car field)) *types*) => cdddr)
                 (else (cdr field)))))
      (lambda ()
        (for-each
         (lambda (subfield)
           (let ((subname (x->string (cadr subfield))))
             (cat
              "  "
              (string-append dst "." (x->string (cadr subfield)))
              " = "
              (string-append
               "((" (x->string (or (type-struct-type (type-name (car field))) ""))
               " " (mangle (type-name (car field))) "*)" "sexp_cpointer_value(" val "))"
               "->" (x->string (cadr subfield)))
              ";\n")))
         (struct-fields field-type)))))
   (else
    (lambda ()
      (cat "  " dst " = " (lambda () (scheme->c-converter (car field) val)) ";\n")))))

(define (write-type-setter type name field)
  (cat "static sexp " (type-setter-name type name field)
       " (sexp ctx sexp_api_params(self, n), sexp x, sexp v) {\n"
       (lambda () (write-validator "x" name))
       (lambda () (write-validator "v" (car field)))
       (write-type-setter-assignment
        type name field
        (string-append "((" (x->string (or (type-struct-type name) ""))
                       " " (mangle name) "*)" "sexp_cpointer_value(x))"
                        "->" (x->string (cadr field)))
        "v")
       "  return SEXP_VOID;\n"
       "}\n\n"))

(define (write-type-funcs type)
  (let ((name (car type))
        (type (cdr type)))
    ;; maybe write finalizer
    (cond
     ((memq 'finalizer: type)
      => (lambda (x)
           (cat "static sexp " (generate-stub-name (cadr x))
                " (sexp ctx sexp_api_params(self, n), sexp x) {\n"
                "  if (sexp_cpointer_freep(x))\n"
                "    " (cadr x) "(sexp_cpointer_value(x));\n"
                "  return SEXP_VOID;\n"
                "}\n\n"))))
    ;; maybe write constructor
    (cond
     ((memq 'constructor: type)
      => (lambda (x)
           (let ((make (caadr x))
                 (args (cdadr x)))
             (cat "static sexp " (generate-stub-name make)
                  " (sexp ctx sexp_api_params(self, n)"
                  (lambda ()
                    (let lp ((ls args) (i 0))
                      (cond ((pair? ls)
                             (cat ", sexp arg" i)
                             (lp (cdr ls) (+ i 1))))))
                  ") {\n"
                  "  struct " (type-name name) " *r;\n"
                  "  sexp_gc_var1(res);\n"
                  "  sexp_gc_preserve1(ctx, res);\n"
                  ;; "  res = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer) + sizeof(struct " (type-name name) "), "
                  ;; (type-id-name name)
                  ;; ");\n"
                  ;; "  r = sexp_cpointer_value(res) = sexp_cpointer_body(res);\n"
                  "  res = sexp_alloc_tagged(ctx, sexp_sizeof(cpointer), "
                  (type-id-name name)
                  ");\n"
                  "  r = sexp_cpointer_value(res) = malloc(sizeof(struct "
                  (type-name name) "));\n"
                  "  sexp_freep(res) = 1;\n"
                  (lambda ()
                    (let lp ((ls args) (i 0))
                      (cond
                       ((pair? ls)
                        (let* ((a (car ls))
                               (field
                                (any (lambda (f) (and (pair? f) (eq? a (cadr f))))
                                     (cddr x))))
                          (if field
                              (cat "  r->" (cadr field) " = "
                                   (lambda ()
                                     (scheme->c-converter
                                      (car field)
                                      (string-append "arg"
                                                     (number->string i))))
                                   ";\n"))
                          (lp (cdr ls) (+ i 1)))))))
                  "  sexp_gc_release1(ctx);\n"
                  "  return res;\n"
                  "}\n\n")
             (set! *funcs*
                   (cons (parse-func `(void ,make ,args)) *funcs*))))))
    ;; write field accessors
    (for-each
     (lambda (field)
       (cond
        ((and (pair? field) (pair? (cdr field)))
         (cond
          ((and (pair? (cddr field)) (caddr field))
           (write-type-getter type name field)
           (set! *funcs*
                 (cons (parse-func
                        `(,(car field)
                          (,(caddr field)
                           #f
                           ,(type-getter-name type name field))
                          (,name)))
                       *funcs*))))
         (cond
          ((and (pair? (cddr field))
                (pair? (cdddr field))
                (car (cdddr field)))
           (write-type-setter type name field)
           (set! *funcs*
                 (cons (parse-func
                        `(,(car field)
                          (,(car (cdddr field))
                           #f
                           ,(type-setter-name type name field))
                          (,name ,(car field))))
                       *funcs*)))))))
     (struct-fields type))))

(define (write-const const)
  (let ((scheme-name (if (pair? (cadr const)) (caadr const) (cadr const)))
        (c-name (if (pair? (cadr const)) (cadadr const) (mangle (cadr const)))))
    (cat "  name = sexp_intern(ctx, \"" scheme-name "\", "
         (string-length (x->string scheme-name)) ");\n"
         "  sexp_env_define(ctx, env, name, tmp="
         (lambda () (c->scheme-converter (car const) c-name)) ");\n")))

(define (write-utilities)
  (define (input-env-string? x)
    (and (eq? 'env-string (type-base x)) (not (type-result? x))))
  (cond
   ((any (lambda (f)
           (or (any input-env-string? (func-results f))
               (any input-env-string? (func-scheme-args f))))
         *funcs*)
    (cat "static char* sexp_concat_env_string (sexp x) {\n"
         "  int klen=sexp_string_length(sexp_car(x)), vlen=sexp_string_length(sexp_cdr(x));\n"
         "  char *res = (char*) malloc(klen+vlen+2);\n"
         "  strncpy(res, sexp_string_data(sexp_car(x)), klen);\n"
         "  res[sexp_string_length(sexp_car(x))] = '=';\n"
         "  strncpy(res+sexp_string_length(sexp_car(x)), sexp_string_data(sexp_cdr(x)), vlen);\n"
         "  res[len-1] = '\\0';\n"
         "  return res;\n"
         "}\n\n"))))

(define (write-init)
  (newline)
  (write-utilities)
  (for-each write-func *funcs*)
  (for-each write-type-funcs *types*)
  (cat "sexp sexp_init_library (sexp ctx sexp_api_params(self, n), sexp env) {\n"
       "  sexp_gc_var2(name, tmp);\n"
       "  sexp_gc_preserve2(ctx, name, tmp);\n")
  (for-each write-const *consts*)
  (for-each write-type *types*)
  (for-each write-func-binding *funcs*)
  (cat "  sexp_gc_release2(ctx);\n"
       "  return SEXP_VOID;\n"
       "}\n\n"))

(define (generate file)
  (display "/* automatically generated by chibi genstubs */\n")
  (c-system-include "chibi/eval.h")
  (load file)
  (write-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main

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
