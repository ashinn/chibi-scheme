
(define-library (chibi snow interface)
  (export warn info message die input input-password input-number yes-or-no?
          restore-history save-history)
  (import (scheme base) (scheme char) (scheme read) (scheme write)
          (scheme file) (scheme process-context) (srfi 1)
          (chibi config) (chibi pathname) (chibi show)
          (chibi term edit-line))
  (cond-expand
   (chibi
    (import (chibi filesystem)))
   (chicken
    (import posix)
    (begin
      (define (create-directory* dir) (create-directory dir #t))))
   (sagittarius
    (import (only (sagittarius) create-directory)
            (chibi string))
    (begin
      (define (create-directory* dir . o)
        (let ((mode (if (pair? o) (car o) #o755)))
          (or (file-directory? dir)
              (create-directory dir mode)
              (let* ((start (string-cursor-start dir))
                     (slash
                      (string-find-right dir #\/ start
                                         (string-skip-right dir #\/))))
                (and (string-cursor>? slash start)
                     (let ((parent (substring-cursor dir start slash)))
                       (and (not (equal? parent dir))
                            (not (file-exists? parent))
                            (create-directory* parent mode)
                            (create-directory dir mode)))))))))))
  (include "interface.scm"))
