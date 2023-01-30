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
  (import (chibi)
          (only (scheme base) call/cc))

  (begin
    (define (abort-current-continuation prompt-tag . arg*)
      (if (not (%metacontinuation-contains-prompt?
                (%current-metacontinuation)
                prompt-tag))
          (error "abort-current-continuation: no prompt with the given tag in current continuation"
                 prompt-tag))
      (let f ()
        (if (null? (%current-winders))
            (let ((mf (car (%current-metacontinuation))))
              (if (eq? (%metacontinuation-frame-tag mf) prompt-tag)
                  (let ((handler (%metacontinuation-frame-handler mf)))
                    (%pop-metacontinuation-frame!)
                    (%abort-to
                     (%metacontinuation-frame-continuation mf)
                     (%metacontinuation-frame-winders mf)
                     (lambda ()
                       (apply handler arg*))))
                  (begin
                    (%pop-metacontinuation-frame!)
                    (f))))
            (%wind-to
             '()
             f
             (lambda ()
               (if (not (%metacontinuation-contains-prompt?
                         (%current-metacontinuation)
                         prompt-tag))
                   (error
                    "abort-current-continuation: lost prompt with the given tag during abort of the current continuation"
                    prompt-tag))
               (f))))))

    (define (call-with-composable-continuation proc . tag*)
      (let ((prompt-tag (if (null? tag*)
                            %default-continuation-prompt-tag
                            (car tag*))))
        (%call-with-current-continuation
         (lambda (k)
           (proc
            (%make-composable-continuation
             (%take-metacontinuation prompt-tag #t)
             k
             (%current-winders)
             prompt-tag))))))

    (define (%make-composable-continuation mk k winders prompt-tag)
      (%make-continuation
       mk
       k
       winders
       prompt-tag
       (lambda (thunk)
         (%call-in-composable-continuation mk k winders thunk))
       #f))

    (define (%call-in-composable-continuation mk k winders thunk)
      (%call-in-empty-marks
       (lambda ()
         (%abort-to-composition (reverse mk) k winders thunk #f))))

    (define (call-in-continuation k proc . args)
      ((%continuation-resume-k k) (lambda () (apply proc args))))

    (define (call-in k proc . args)
      ((%continuation-resume-k k) (lambda () (apply proc args))))

    (define (return-to k . args)
      (lambda (k args)
        ((%continuation-resume-k k) (lambda () (apply values args)))))

    (define (continuation-prompt-available? tag . k*)
      (if (null? k*)
          (%metacontinuation-contains-prompt? (%current-metacontinuation) tag)
          (let ((k (car k*)))
            (or (and (%continuation-non-composable? k)
                     (eq? (%continuation-prompt-tag k) tag))
                (%metacontinuation-contains-prompt? (%continuation-metacontinuation k) tag)))))

    (define (call-with-continuation-barrier thunk)
      (%call-in-empty-marks %continuation-barrier-tag #f thunk))

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
