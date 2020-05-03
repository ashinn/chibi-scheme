
(import (chibi ast) (chibi time) (scheme cxr) (srfi 33) (srfi 39))

(define (timeval->milliseconds tv)
  (quotient (+ (* 1000000 (timeval-seconds tv)) (timeval-microseconds tv))
            1000))

(define (timeval-diff start end)
  (- (timeval->milliseconds end)
     (timeval->milliseconds start)))

(define (time* thunk)
  (call-with-output-string
    (lambda (out)
      (gc)
      (let* ((start (car (get-time-of-day)))
             (start-rusage (get-resource-usage))
             (gc-start (gc-usecs))
             (gc-start-count (gc-count))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             (result (parameterize ((current-output-port out)) (thunk)))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             (end (car (get-time-of-day)))
             (end-rusage (get-resource-usage))
             (gc-end (gc-usecs))
             (gc-msecs (quotient (- gc-end gc-start) 1000))
             (real-msecs (timeval-diff start end))
             (user-msecs
              (timeval-diff (resource-usage-time start-rusage)
                            (resource-usage-time end-rusage)))
             (system-msecs
              (timeval-diff (resource-usage-system-time start-rusage)
                            (resource-usage-system-time end-rusage))))
        (display "user: ")
        (display user-msecs)
        (display " system: ")
        (display system-msecs)
        (display " real: ")
        (display real-msecs)
        (display " gc: ")
        (display gc-msecs)
        (display " (")
        (display (- (gc-count) gc-start-count))
        (display " times)\n")
        (display "result: ")
        (write result)
        (newline)
        result))))

(define-syntax time
  (syntax-rules ()
    ((_ expr) (time* (lambda () expr)))))
