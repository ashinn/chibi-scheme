
(define-library (chibi system)
  (export user-information user? user-name user-password
          user-id user-group-id user-gecos user-home user-shell
          current-user-id current-group-id
          current-effective-user-id current-effective-group-id
          set-current-user-id! set-current-effective-user-id!
          set-current-group-id! set-current-effective-group-id!
          current-session-id create-session
          set-root-directory!)
  (import (chibi))
  (include-shared "system")
  (body
   (define (user-information user)
     (car (if (string? user)
              (getpwnam_r user (make-string 1024))
              (getpwuid_r user (make-string 1024)))))))
