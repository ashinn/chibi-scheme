
(define-syntax when
  (syntax-rules ()
    ((when test . body)
     (if test (begin . body)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test . body)
     (when (not test) . body))))
