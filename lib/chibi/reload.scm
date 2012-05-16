;; reload.scm -- automatic module reloading
;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define last-modified-time (current-seconds))

(define reload-verbose? (make-parameter #f))

(define (warn msg . args)
  (let ((err (current-error-port)))
    (display msg err)
    (display ":" err)
    (for-each (lambda (a)
                (display " " err)
                (if (string? a) (display a err) (write a err)))
              args)
    (newline err)))

(define (reload module-name)
  (if (reload-verbose?)
      (warn "Reloading module" module-name))
  (let ((old-module (find-module module-name)))
    ;; Remove old entry in modules list.
    (delete-module! module-name)
    (guard (exn (else (warn "Error loading module definition" module-name)
                      (print-exception exn)
                      (print-stack-trace)
                      (add-module! module-name old-module)))
      (load-module-definition module-name)
      (let ((module (find-module module-name)))
        (cond
         ((not module) (warn "Couldn't find module" module-name))
         (else
          (guard (exn (else (warn "Error loading module" module-name)
                            (print-exception exn)
                            (print-stack-trace)
                            (delete-module! module-name)
                            (add-module! module-name old-module)))
            (let ((env (eval-module module-name module)))
              (%import (module-env module) env (env-exports env) #f)))))))))

(define (file-modified? path)
  (and path (> (file-modification-time path) last-modified-time)))

(define (module-definition-modified? module-name module)
  (file-modified? (find-module-file (module-name->file module-name))))

(define (module-includes-modified? module-name module)
  (let ((dir (module-name-prefix module-name)))
    (any
     (lambda (x)
       (and (pair? x) (memq (car x) '(include include-ci))
            (any file-modified?
                 (map (lambda (f) (find-module-file (string-append dir f)))
                      (cdr x)))))
     (module-meta-data module))))

(define (module-modified? module-name module)
  (or (module-definition-modified? module-name module)
      (module-includes-modified? module-name module)))

(define (reload-modified-modules)
  (for-each
   (lambda (x)
     (if (module-modified? (car x) (cdr x))
         (reload (car x))))
   *modules*)
  (set! last-modified-time (current-seconds)))
