
(cond-expand
 (modules (import (chibi system) (only (chibi test) test-begin test test-end)))
 (else #f))

(test-begin "system")

(test #t (user? (user-information (current-user-id))))
(test #f (user? #f))
(test #f (user? (list #f)))
(test #t (string? (user-name (user-information (current-user-id)))))
(test #t (string? (user-password (user-information (current-user-id)))))
(test #t (integer? (user-id (user-information (current-user-id)))))
(test #t (integer? (user-group-id (user-information (current-user-id)))))
(test #t (string? (user-gecos (user-information (current-user-id)))))
(test #t (string? (user-home (user-information (current-user-id)))))
(test #t (string? (user-shell (user-information (current-user-id)))))

(test (current-user-id) (user-id (user-information (current-user-id))))
(test (current-group-id) (user-group-id (user-information (current-user-id))))

(test (user-id (user-information (current-user-id)))
    (user-id (user-information (user-name (user-information (current-user-id))))))

(test #t (integer? (current-session-id)))

;; stress test user-name
(test (user-name (user-information (current-user-id)))
    (user-name (user-information (current-user-id))))
(define u (user-information (current-user-id)))
(test (user-name u) (user-name (user-information (current-user-id))))
(define un (user-name (user-information (current-user-id))))
(test un (user-name (user-information (current-user-id))))

(test-end)
