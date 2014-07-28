;; modules.scm -- module introspection utilities
;; Copyright (c) 2011-2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Module introspection library.

(define (file->sexp-list file)
  (call-with-input-file file
    (lambda (in)
      (let lp ((res '()))
        (let ((x (read in)))
          (if (eof-object? x)
              (reverse res)
              (lp (cons x res))))))))

(define (module? x)
  (and (vector? x)
       (>= (vector-length x) 4)
       (or (list? (vector-ref x 0)) (not (vector-ref x 0)))))

(define (module-ast mod) (vector-ref mod 3))
(define (module-ast-set! mod x) (vector-set! mod 3 x))

(define (module-name mod)
  (if (pair? mod)
      (car mod)
      (let lp ((ls *modules*))
        (and (pair? ls)
             (if (eq? mod (cdar ls))
                 (caar ls)
                 (lp (cdr ls)))))))

(define (module-dir mod)
  (let ((name (module-name mod)))
    (if (member name '((chibi) (meta)))
        ""
        (module-name-prefix name))))

(define (module-metas mod metas)
  (let ((mod (if (module? mod) mod (find-module mod))))
    (let lp ((ls (module-meta-data mod)) (res '()))
      (cond
       ((not (pair? ls)) (reverse res))
       ((and (pair? (car ls)) (memq (caar ls) metas))
        (lp (cdr ls) (append (reverse (cdar ls)) res)))
       (else (lp (cdr ls) res))))))

(define (module-extract-declaration-files mod decls)
  (let* ((mod (if (module? mod) mod (find-module mod)))
         (dir (module-dir mod)))
    (define (module-file f)
      (find-module-file (string-append dir f)))
    (map module-file (reverse (module-metas mod decls)))))

(define (module-includes mod)
  (module-extract-declaration-files mod '(include)))

(define (module-include-library-declarations mod)
  (module-extract-declaration-files mod '(include-library-declarations)))

(define (module-shared-includes mod)
  (let* ((mod (if (module? mod) mod (find-module mod)))
         (dir (module-dir mod)))
    (define (module-file f)
      (find-module-file (string-append dir f ".stub")))
    (let lp ((ls (reverse (module-metas mod '(include-shared)))) (res '()))
      (cond ((null? ls) (reverse res))
            ((module-file (car ls)) => (lambda (x) (lp (cdr ls) (cons x res))))
            (else (lp (cdr ls) res))))))

(define (analyze-module-source name mod recursive?)
  (let ((env (module-env mod))
        (dir (module-dir mod)))
    (define (include-source file)
      (cond ((find-module-file (string-append dir file))
             => (lambda (x) (cons 'body (file->sexp-list x))))
            (else (error "couldn't find include" file))))
    (let lp ((ls (module-meta-data mod)) (res '()))
      (cond
       ((not (pair? ls))
        (reverse res))
       (else
        (case (and (pair? (car ls)) (caar ls))
          ((import import-immutable)
           (for-each
            (lambda (m)
              (let* ((mod2-name+imports (resolve-import m))
                     (mod2-name (car mod2-name+imports)))
                (if recursive?
                    (analyze-module mod2-name #t))))
            (cdar ls))
           (lp (cdr ls) res))
          ((include)
           (lp (append (map include-source (cdar ls)) (cdr ls)) res))
          ((body begin)
           (let lp2 ((ls2 (cdar ls)) (res res))
             (cond
              ((pair? ls2)
               (lp2 (cdr ls2) (cons (analyze (car ls2) env) res)))
              (else
               (lp (cdr ls) res)))))
          (else
           (lp (cdr ls) res))))))))

(define (analyze-module name . o)
  (let ((recursive? (and (pair? o) (car o)))
        (res (load-module name)))
    (if (not (module-ast res))
        (module-ast-set! res (analyze-module-source name res recursive?)))
    res))

(define (module-ref mod var-name . o)
  (let ((cell (env-cell (module-env (if (module? mod) mod (load-module mod)))
                        var-name)))
    (if cell
        (cdr cell)
        (if (pair? o) (car o) (error "no binding in module" mod var-name)))))

(define (module-contains? mod var-name)
  (and (env-cell (module-env (if (module? mod) mod (load-module mod))) var-name)
       #t))

(define (module-defines? name mod var-name)
  (if (not (module-ast mod))
      (module-ast-set! mod (analyze-module-source name mod #f)))
  (let lp ((ls (module-ast mod)))
    (and (pair? ls)
         (or (and (set? (car ls))
                  (eq? var-name (ref-name (set-var (car ls)))))
             (lp (cdr ls))))))

(define (containing-module x)
  (let lp1 ((ls (reverse *modules*)))
    (and (pair? ls)
         (let ((env (module-env (cdar ls))))
           (let lp2 ((e-ls (if (environment? env) (env-exports env) '())))
             (if (null? e-ls)
                 (lp1 (cdr ls))
                 (let ((cell (env-cell env (car e-ls))))
                   (if (and (eq? x (cdr cell))
                            (module-defines? (caar ls) (cdar ls) (car cell)))
                       (car ls)
                       (lp2 (cdr e-ls))))))))))

(define (procedure-analysis x . o)
  (let ((name (if (procedure? x) (procedure-name x) x))
        (mod (or (and (pair? o) (car o)) (containing-module x))))
    (and mod
         (let lp ((ls (module-ast (analyze-module (module-name mod)))))
           (and (pair? ls)
                (cond
                 ((and (set? (car ls))
                       (eq? name (ref-name (set-var (car ls)))))
                  (set-value (car ls)))
                 ((seq? (car ls))
                  (lp (append (seq-ls (car ls)) (cdr ls))))
                 (else
                  (lp (cdr ls)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finding all available modules

(define (module-file? file)
  (let ((len (string-length file)))
    (and (> len 4) (equal? ".sld" (substring file (- len 4))))))

(define (read-static-modules file)
  (protect (exn (else '()))
    (call-with-input-file file
      (lambda (in)
        (let lp ((res '()))
          (let ((expr (read in)))
            (cond
             ((eof-object? expr)
              res)
             ((and (pair? expr) (eq? 'define-library (car expr)))
              (let ((name (cadr expr))
                    (exports (cond ((assq 'export (cddr expr)) => cdr)
                                   (else '()))))
                (lp (cons (cons name (make-module exports #f #f)) res))))
             (else
              (lp res)))))))))

(define no-module-depth-limit 2)

(define (available-modules-in-directory dir depth res)
  (call-with-values
      (lambda ()
        (partition file-directory?
                   (map (lambda (f) (string-append dir "/" f))
                        (remove (lambda (f) (member f '("." "..")))
                                (directory-files dir)))))
    (lambda (dirs files)
      (let ((mods (append-map read-static-modules
                              (filter module-file? files))))
        (if (and (null? mods) (>= depth no-module-depth-limit))
            res
            (let ((depth (if (pair? mods) 0 (+ 1 depth))))
              (let lp ((ls dirs) (res (append mods res)))
                (if (null? ls)
                    res
                    (lp (cdr ls)
                        (available-modules-in-directory (car ls) depth res)
                        )))))))))

(define (available-modules)
  (let lp ((ls (current-module-path)) (res *modules*))
    (if (null? ls)
        res
        (lp (cdr ls)
            (available-modules-in-directory (car ls) 0 res)))))

(define (modules-exporting-identifier name)
  (let lp ((ls (available-modules))
           (res '()))
    (cond
     ((null? ls) (reverse res))
     ((and (memq name (module-exports (cdar ls)))
           (not (assoc (caar ls) res)))
      (lp (cdr ls) (cons (car ls) res)))
     (else (lp (cdr ls) res)))))
