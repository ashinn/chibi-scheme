(define-library (srfi 226 prompt)
  (export
    make-continuation-prompt-tag
    default-continuation-prompt-tag
    continuation-prompt-tag?)
  (import
    (chibi))
  (begin

    (define (default-continuation-prompt-tag)
      %default-continuation-prompt-tag)

    (define make-continuation-prompt-tag
      (let ((counter 1))
        (lambda name*
          (set! counter (+ counter 1))
          (cons counter name*))))

    (define (continuation-prompt-tag? obj) #t)))

;; Local Variables:
;; mode: scheme
;; End:
