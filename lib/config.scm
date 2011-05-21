;; config.scm -- configuration module
;; Copyright (c) 2009-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modules

(define *this-module* '())

(define (make-module exports env meta) (vector exports env meta #f))
(define (%module-exports mod) (vector-ref mod 0))
(define (module-env mod) (vector-ref mod 1))
(define (module-meta-data mod) (vector-ref mod 2))
(define (module-env-set! mod env) (vector-set! mod 1 env))

(define (module-exports mod)
  (or (%module-exports mod) (env-exports (module-env mod))))

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
   (reverse (cons ".module" (cdr (module-name->strings name '()))))))

(define (module-name-prefix name)
  (string-concatenate (reverse (cdr (cdr (module-name->strings name '()))))))

(define (load-module-definition name)
  (let* ((file (module-name->file name))
         (path (find-module-file file)))
    (if path (load path *config-env*))))

(define (find-module name)
  (cond
   ((assoc name *modules*) => cdr)
   (else
    (load-module-definition name)
    (cond ((assoc name *modules*) => cdr)
          (else #f)))))

(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a) (symbol->string b))))

(define (to-id id) (if (pair? id) (car id) id))
(define (from-id id) (if (pair? id) (cdr id) id))
(define (id-filter pred ls)
  (cond ((null? ls) '())
        ((pred (to-id (car ls))) (cons (car ls) (id-filter pred (cdr ls))))
        (else (id-filter pred (cdr ls)))))

(define (resolve-import x)
  (cond
   ((not (and (pair? x) (list? x)))
    (error "invalid module syntax" x))
   ((and (pair? (cdr x)) (pair? (cadr x)))
    (if (memq (car x) '(only except rename))
        (let* ((mod-name+imports (resolve-import (cadr x)))
               (imp-ids (or (cdr mod-name+imports)
                            (and (not (eq? 'only (car x)))
                                 (module-exports
                                  (find-module (car mod-name+imports)))))))
          (cons (car mod-name+imports)
                (case (car x)
                  ((only)
                   (if (not imp-ids)
                       (cddr x)
                       (id-filter (lambda (i) (memq i (cddr x))) imp-ids)))
                  ((except)
                   (id-filter (lambda (i) (not (memq i (cddr x)))) imp-ids))
                  ((rename)
                   (map (lambda (i)
                          (let ((rename (assq (to-id i) (cddr x))))
                            (if rename (cons (cadr rename) (from-id i)) i)))
                        imp-ids)))))
        (error "invalid import modifier" x)))
   ((and (eq? 'prefix (car x)) (symbol? (cadr x)) (list? (caddr x)))
    (let ((mod-name+imports (resolve-import (caddr x))))
      (cons (car mod-name+imports)
            (map (lambda (i)
                   (cons (symbol-append (cadr x) (if (pair? i) (car i) i))
                         (if (pair? i) (cdr i) i)))
                 (cdr mod-name+imports)))))
   ((find-module x)
    => (lambda (mod) (cons x (%module-exports mod))))
   (else
    (error "couldn't find import" x))))

(define (eval-module name mod)
  (let ((env (make-environment))
        (dir (module-name-prefix name)))
    (define (load-modules files extension)
      (for-each
       (lambda (f)
         (let ((f (string-append dir f extension)))
           (cond ((find-module-file f) => (lambda (x) (load x env)))
                 (else (error "couldn't find include" f)))))
       files))
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
     (module-meta-data mod))
    (for-each
     (lambda (x)
       (case (and (pair? x) (car x))
         ((include)
          (load-modules (cdr x) ""))
         ((include-shared)
          (cond-expand
           (dynamic-loading (load-modules (cdr x) *shared-object-extension*))
           (else #f)))
         ((body begin)
          (for-each (lambda (expr) (eval expr env)) (cdr x)))))
     (module-meta-data mod))
    env))

(define (load-module name)
  (let ((mod (find-module name)))
    (if (and mod (not (module-env mod)))
        (module-env-set! mod (eval-module name mod)))
    mod))

(define-syntax define-module
  (er-macro-transformer
   (lambda (expr rename compare)
     (let ((name (cadr expr))
           (body (cddr expr)))
       `(let ((tmp *this-module*))
          (define (rewrite-export x)
            (if (pair? x)
                (if (and (= 3 (length x))
                         (eq? 'rename (identifier->symbol (car x))))
                    (cons (caddr x) (cadr x))
                    (error "invalid module export" x))
                x))
          (set! *this-module* '())
          ,@body
          (set! *this-module* (reverse *this-module*))
          (let ((exports
                 (cond ((assq 'export *this-module*)
                        => (lambda (x) (map rewrite-export (cdr x))))
                       (else '()))))
            (set! *modules*
                  (cons (cons ',name (make-module exports #f *this-module*))
                        *modules*)))
          (set! *this-module* tmp))))))

(define-syntax module
  (er-macro-transformer
   (lambda (expr rename compare)
     (cons (rename 'define-module) (cdr expr)))))

(define-syntax define-config-primitive
  (er-macro-transformer
   (lambda (expr rename compare)
     `(define-syntax ,(cadr expr)
        (er-macro-transformer
         (lambda (expr rename compare)
           `(set! *this-module* (cons ',expr *this-module*))))))))

(define-config-primitive import)
(define-config-primitive import-immutable)
(define-config-primitive export)
(define-config-primitive include)
(define-config-primitive include-shared)
(define-config-primitive body)
(define-config-primitive begin)

(define *modules*
  (list (cons '(scheme) (make-module #f (interaction-environment)
                                     '((include "init.scm"))))
        (cons '(config) (make-module #f (current-environment) '()))
        (cons '(srfi 0) (make-module (list 'cond-expand)
                                     (current-environment)
                                     (list (list 'export 'cond-expand))))
        (cons '(srfi 46) (make-module (list 'syntax-rules)
                                      (current-environment)
                                      (list (list 'export 'syntax-rules))))))
