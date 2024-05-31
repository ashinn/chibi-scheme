
(define-library (chibi system)
  (export get-host-name
          user? user-name user-password
          user-id user-group-id user-gecos user-home user-shell
          group? group-name group-password group-id
          current-user-id current-group-id
          current-effective-user-id current-effective-group-id
          set-current-user-id! set-current-effective-user-id!
          set-current-group-id! set-current-effective-group-id!
          current-session-id create-session
          set-root-directory!)
  (import (chibi))
  (include-shared "system")
  (cond-expand
   (emscripten)
   (else
    (export user-information group-information)
    (body
     (define (safe-car x) (and (pair? x) (car x)))
     (define (user-information user)
       (safe-car (if (string? user)
                     (getpwnam_r user (make-string 1024))
                     (getpwuid_r user (make-string 1024)))))
     (define (group-information group)
       (safe-car (if (string? group)
                     (getgrnam-safe group (make-string 1024))
                     (getgrgid-safe group (make-string 1024)))))))))
