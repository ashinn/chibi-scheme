;; meta.scm -- meta langauge for describing modules
;; Copyright (c) 2009-2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modules

(define *this-module* '())
(define *this-path* '())

(define (make-module exports env meta) (vector exports env meta #f))
(define (%module-exports mod) (vector-ref mod 0))
(define (module-env mod) (vector-ref mod 1))
(define (module-env-set! mod env) (vector-set! mod 1 env))
(define (module-meta-data mod) (vector-ref mod 2))
(define (module-meta-data-set! mod x) (vector-set! mod 2 x))

(define (module-exports mod)
  (or (%module-exports mod)
      (if (module-env mod)
          (env-exports (module-env mod))
          '())))

(define (module-name->strings ls res)
  (if (null? ls)
      res
      (let ((str (cond ((symbol? (car ls)) (symbol->string (car ls)))
                       ((number? (car ls)) (number->string (car ls)))
                       ((string? (car ls)) (car ls))
                       (else (error "invalid module name" (car ls))))))
        (module-name->strings (cdr ls) (cons "/" (cons str res))))))

(define (module-name->file name)
  (string-concatenate
   (reverse (cons ".sld" (cdr (module-name->strings name '()))))))

(define (module-name-prefix name)
  (string-concatenate (reverse (cdr (cdr (module-name->strings name '()))))))

(define load-module-definition
  (let ((meta-env (current-environment)))
    (lambda (name)
      (let* ((file (module-name->file name))
             (path (find-module-file file)))
        (if path (load path meta-env))))))

(define (find-module name)
  (cond
   ((assoc name *modules*) => cdr)
   (else
    (load-module-definition name)
    (cond ((assoc name *modules*) => cdr)
          (else #f)))))

(define (add-module! name module)
  (set! *modules* (cons (cons name module) *modules*)))

(define (delete-module! name)
  (let lp ((ls *modules*) (prev #f))
    (cond ((null? ls))
          ((equal? name (car (car ls)))
           (if prev
               (set-cdr! prev (cdr ls))
               (set! *modules* (cdr ls))))
          (else (lp (cdr ls) ls)))))

(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a) (symbol->string b))))

(define (symbol-drop a b)
  (let ((as (symbol->string a))
        (bs (symbol->string b)))
    (if (and (> (string-length bs) (string-length as))
             (string=? as (substring bs 0 (string-length as))))
        (string->symbol (substring bs (string-length as)))
        b)))

(define (warn msg . args)
  (display msg (current-error-port))
  (display ":" (current-error-port))
  (for-each (lambda (a)
              (display " " (current-error-port))
              (write a (current-error-port)))
            args)
  (newline (current-error-port)))

(define (to-id id) (if (pair? id) (car id) id))
(define (from-id id) (if (pair? id) (cdr id) id))
(define (id-filter pred ls)
  (cond ((null? ls) '())
        ((pred (to-id (car ls))) (cons (car ls) (id-filter pred (cdr ls))))
        (else (id-filter pred (cdr ls)))))

(define (resolve-import x)
  (cond
   ((not (and (pair? x) (list? x)))
    (error "invalid import syntax" x))
   ((and (memq (car x) '(prefix drop-prefix))
         (symbol? (car (cddr x))) (list? (cadr x)))
    (let ((mod-name+imports (resolve-import (cadr x))))
      (cons (car mod-name+imports)
            (map (lambda (i)
                   (cons ((if (eq? (car x) 'drop-prefix)
                              symbol-drop
                              symbol-append)
                          (car (cddr x))
                          (to-id i))
                         (from-id i)))
                 (or (cdr mod-name+imports)
                     (module-exports (find-module (car mod-name+imports))))))))
   ((and (pair? (cdr x)) (pair? (cadr x)))
    (if (memq (car x) '(only except rename))
        (let* ((mod-name+imports (resolve-import (cadr x)))
               (imp-ids (or (cdr mod-name+imports)
                            (and (not (eq? 'only (car x)))
                                 (module-exports
                                  (find-module (car mod-name+imports)))))))
          ;; (if (not (eq? 'only (car x)))
          ;;     (let ((unbound
          ;;            (id-filter (lambda (i) (not (memq i imp-ids))) (cddr x))))
          ;;       (if (pair? unbound)
          ;;           (warn "import excepting unbound identifiers" unbound))))
          (cons (car mod-name+imports)
                (case (car x)
                  ((only)
                   (cddr x))
                  ((except)
                   (id-filter (lambda (i) (not (memq i (cddr x)))) imp-ids))
                  ((rename)
                   (map (lambda (i)
                          (let ((rename (assq (to-id i) (cddr x))))
                            (if rename (cons (cadr rename) (from-id i)) i)))
                        imp-ids)))))
        (error "invalid import modifier" x)))
   ((find-module x)
    => (lambda (mod) (cons x (%module-exports mod))))
   (else
    (error "couldn't find import" x))))

(define (eval-module name mod . o)
  (let ((env (if (pair? o) (car o) (make-environment)))
        (meta (module-meta-data mod))
        (dir (module-name-prefix name)))
    (define (load-modules files extension fold? . o)
      (for-each
       (lambda (f)
         (let ((f (string-append dir f extension)))
           (cond
            ((find-module-file f)
             => (lambda (path)
                  (cond (fold?
                         (let ((in (open-input-file path)))
                           (set-port-fold-case! in #t)
                           (load in env)))
                        (else
                         (load path env)))))
            ((and (pair? o) (car o)) ((car o)))
            (else (error "couldn't find include" f)))))
       files))
    ;; catch cyclic references
    (cond
     ((procedure? meta)
      (meta env))
     (else
      (module-meta-data-set!
       mod
       `((error "module attempted to reference itself while loading" ,name)))
      (for-each
       (lambda (x)
         (case (and (pair? x) (car x))
           ((import import-immutable)
            (for-each
             (lambda (m)
               (let* ((mod2-name+imports (resolve-import m))
                      (mod2 (load-module (car mod2-name+imports))))
                 (%import env (module-env mod2) (cdr mod2-name+imports) #t)))
             (cdr x)))))
       meta)
      (protect
          (exn (else
                (module-meta-data-set! mod meta)
                (if (not (any (lambda (x)
                                (and (pair? x)
                                     (memq (car x) '(import import-immutable))))
                              meta))
                    (warn "WARNING: exception inside module with no imports - did you forget to (import (scheme base)) in" name))
                (raise-continuable exn)))
        (for-each
         (lambda (x)
           (case (and (pair? x) (car x))
             ((include)
              (load-modules (cdr x) "" #f))
             ((include-ci)
              (load-modules (cdr x) "" #t))
             ((include-shared)
              (load-modules (cdr x) *shared-object-extension* #f))
             ((include-shared-optionally)
              (load-modules (list (cadr x)) *shared-object-extension* #f
                            (lambda () (load-modules (cddr x) "" #f))))
             ((body begin)
              (for-each (lambda (expr) (eval expr env)) (cdr x)))
             ((error)
              (apply error (cdr x)))))
         meta))
      (module-meta-data-set! mod meta)
      (warn-undefs env #f)
      env))))

(define (environment . ls)
  (let ((env (make-environment)))
    (for-each
     (lambda (m)
       (let* ((mod2-name+imports (resolve-import m))
              (mod2 (load-module (car mod2-name+imports))))
         (%import env (module-env mod2) (cdr mod2-name+imports) #t)))
     ls)
    env))

(define (load-module name)
  (let ((mod (find-module name)))
    (if (and mod (not (module-env mod)))
        (module-env-set! mod (eval-module name mod)))
    mod))

(define-syntax meta-begin begin)
(define-syntax meta-define define)

(define define-library-transformer
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((name (cadr expr))
           (body (cddr expr))
           (tmp (rename 'tmp))
           (this-module (rename '*this-module*))
           (add-module! (rename 'add-module!))
           (_make-module (rename 'make-module))
           (_define (rename 'meta-define))
           (_lambda (rename 'lambda))
           (_let (rename 'let))
           (_map (rename 'map))
           (_if (rename 'if))
           (_cond (rename 'cond))
           (_set! (rename 'set!))
           (_quote (rename 'quote))
           (_and (rename 'and))
           (_= (rename '=))
           (_eq? (rename 'eq?))
           (_pair? (rename 'pair?))
           (_null? (rename 'null?))
           (_reverse (rename 'reverse))
           (_append (rename 'append))
           (_assq (rename 'assq))
           (_=> (rename '=>))
           (_else (rename 'else))
           (_length (rename 'length))
           (_identifier->symbol (rename 'identifier->symbol))
           (_error (rename 'error))
           (_cons (rename 'cons))
           (_car (rename 'car))
           (_cdr (rename 'cdr))
           (_caar (rename 'caar))
           (_cadr (rename 'cadr))
           (_cdar (rename 'cdar))
           (_cddr (rename 'cddr)))
       ;; Check for suspicious defines.
       (for-each
        (lambda (x)
          (if (and (pair? x) (memq (strip-syntactic-closures (car x))
                                   '(define define-syntax)))
              (warn "suspicious use of define in library declarations - did you forget to wrap it in begin?" x)))
        (cdr expr))
       ;; Generate the library wrapper.
       (set! *this-path*
             (cons (string-concatenate
                    (module-name->strings (cdr (reverse name)) '()))
                   *this-path*))
       `(,_let ((,tmp ,this-module))
          (,_define (rewrite-export x)
            (,_if (,_pair? x)
                (,_if (,_and (,_= 3 (,_length x))
                             (,_eq? (,_quote rename)
                                    (,_identifier->symbol (,_car x))))
                    (,_cons (,_car (,_cddr x)) (,_cadr x))
                    (,_error "invalid module export" x))
                x))
          (,_define (extract-exports)
            (,_cond
             ((,_assq (,_quote export-all) ,this-module)
              ,_=> (,_lambda (x)
                     (,_if (,_pair? (,_cdr x))
                         (,_error "export-all takes no parameters" x))
                     #f))
             (,_else
              (,_let lp ((ls ,this-module) (res (,_quote ())))
                (,_cond
                 ((,_null? ls) res)
                 ((,_and (,_pair? (,_car ls))
                         (,_eq? (,_quote export) (,_caar ls)))
                  (lp (,_cdr ls)
                      (,_append (,_map rewrite-export (,_cdar ls)) res)))
                 (,_else (lp (,_cdr ls) res)))))))
          (,_set! ,this-module (,_quote ()))
          ,@body
          (,add-module! (,_quote ,name)
                        (,_make-module (extract-exports)
                                       #f
                                       (,_reverse ,this-module)))
          (,_set! ,this-module ,tmp)
          (,(rename 'pop-this-path)))))))

(define-syntax define-library define-library-transformer)
(define-syntax module define-library-transformer)

(define-syntax pop-this-path
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (pair? *this-path*)
         (set! *this-path* (cdr *this-path*)))
     #f)))

(define-syntax include-library-declarations
  (er-macro-transformer
   (lambda (expr rename compare)
     (let lp1 ((ls (cdr expr)) (res '()))
       (cond
        ((pair? ls)
         (let* ((file (car ls))
                (rel-path (if (pair? *this-path*)
                              (string-append (car *this-path*) "/" file)
                              file)))
           (cond
            ((find-module-file rel-path)
             => (lambda (path)
                  (call-with-input-file path
                    (lambda (in)
                      (let lp2 ((res res))
                        (let ((x (read in)))
                          (if (eof-object? x)
                              (lp1 (cdr ls) res)
                              (lp2 (cons x res)))))))))
            (else
             (error "couldn't find include-library-declarations file" file)))))
        (else
         `(,(rename 'meta-begin)
           ,@(reverse res)
           (,(rename 'set!) ,(rename '*this-module*)
            (,(rename 'cons) (,(rename 'quote)
                              ,(cons 'include-library-declarations (cdr expr)))
             ,(rename '*this-module*))))))))))

(define-syntax define-meta-primitive
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((name (cadr expr)))
       `(define-syntax ,name
          (er-macro-transformer
           (lambda (expr rename compare)
             (let ((this-module (rename '*this-module*))
                   (_set! (rename 'set!))
                   (_cons (rename 'cons))
                   (_quote (rename 'syntax-quote)))
               `(,_set! ,this-module
                        (,_cons (,_quote ,(cons ',name (cdr expr)))
                                ,this-module))))))))))

(define-meta-primitive import)
(define-meta-primitive import-immutable)
(define-meta-primitive export)
(define-meta-primitive export-all)
(define-meta-primitive include)
(define-meta-primitive include-ci)
(define-meta-primitive include-shared)
(define-meta-primitive include-shared-optionally)
(define-meta-primitive body)
(define-meta-primitive begin)

;; The `import' binding used by (chibi) and (scheme base), etc.
(define-syntax repl-import
  (er-macro-transformer
   (let ((meta-env (current-environment)))
     (lambda (expr rename compare)
       (let lp ((ls (cdr expr)) (res '()))
         (cond
          ((null? ls)
           (cons (rename 'meta-begin) (reverse res)))
          (else
           (let ((mod+imps (resolve-import (car ls))))
             (cond
              ((pair? mod+imps)
               (lp (cdr ls)
                   (cons `(,(rename '%import)
                           #f
                           (,(rename 'module-env)
                            (,(rename 'load-module)
                             (,(rename 'quote) ,(car mod+imps))))
                           (,(rename 'quote) ,(cdr mod+imps))
                           #f)
                         res)))
              (else
               (error "couldn't find module" (car ls))))))))))))

(define *modules*
  (list
   (cons '(chibi)
         ;; capture a static copy of the current environment to serve
         ;; as the (chibi) module
         (let ((env (make-environment)))
           (%import env (interaction-environment) #f #t)
           (make-module #f (env-parent env) '((include "init-7.scm")))))
   (cons '(chibi primitive)
         (make-module #f #f (lambda (env) (primitive-environment 7))))
   (cons '(meta)
         (make-module #f (current-environment) '()))
   (cons '(srfi 0)
         (make-module (list 'cond-expand)
                      (current-environment)
                      (list (list 'export 'cond-expand))))))
