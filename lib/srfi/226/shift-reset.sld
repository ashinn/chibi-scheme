(define-library (srfi 226 shift-reset)
  (export
    reset-at
    shift-at
    reset
    shift)
  (import
    (scheme base)
    (srfi 226 prompt)
    (srfi 226 continuation))

  (begin
    (define-syntax reset-at
      (syntax-rules ()
        ((reset tag e1 e2 ...)
         (call-with-continuation-prompt
          (lambda ()
            e1 e2 ...)
          tag))))

    (define-syntax shift-at
      (syntax-rules ()
        ((shift tag-expr k e1 e2 ...)
         (let ((tag tag-expr))
           (call-with-composable-continuation
            (lambda (c)
              (define k (lambda args (reset-at tag (apply c args))))
              (abort-current-continuation tag
                (lambda ()
                  e1 e2 ...))))))))

    (define-syntax reset
      (syntax-rules ()
        ((reset e1 e2 ...)
         (reset-at (default-continuation-prompt-tag) e1 e2 ...))))

    (define-syntax shift
      (syntax-rules ()
        ((shift k e1 e2 ...)
         (shift-at (default-continuation-prompt-tag) k e1 e2 ...)))))

  )

;; Local Variables:
;; mode: scheme
;; End:
