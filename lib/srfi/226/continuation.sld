(define-library (srfi 226 continuation)
  (export
    call-with-continuation-prompt
    abort-current-continuation
    call-with-current-continuation
    call-with-composable-continuation
    call-with-non-composable-continuation
    call-in-continuation
    call-in
    return-to
    continuation-prompt-available?
    call-with-continuation-barrier
    dynamic-wind
    call/cc
    unwind-protect)
  (import (only (chibi)
                call-with-continuation-prompt
                abort-current-continuation
                call-with-current-continuation
                call-with-composable-continuation
                call-with-non-composable-continuation
                call-in-continuation
                call-in
                return-to
                continuation-prompt-available?
                call-with-continuation-barrier
                dynamic-wind)
          (scheme base))

  (begin
    (define-syntax unwind-protect
      (syntax-rules ()
        ((unwind-protect protected-expr cleanup-expr ...)
         (dynamic-wind
           (lambda () (values))
           (lambda () (call-with-continuation-barrier (lambda () protected-expr)))
           (lambda () (values) cleanup-expr ...))))))

  )

;; Local Variables:
;; mode: scheme
;; End:
