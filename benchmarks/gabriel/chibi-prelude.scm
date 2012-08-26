
(import (chibi time) (scheme cxr) (srfi 33))

(define (timeval->milliseconds tv)
  (quotient (+ (* 1000000 (timeval-seconds tv)) (timeval-microseconds tv))
            1000))

(define (time* thunk)
  (call-with-output-string
    (lambda (out)
      (let* ((orig-output-port (current-output-port))
             (_ (current-output-port out))
             (start (car (get-time-of-day)))
             (result (thunk))
             (end (car (get-time-of-day)))
             (_ (current-output-port orig-output-port))
             (msecs (- (timeval->milliseconds end)
                       (timeval->milliseconds start))))
        (display "user: ")
        (display msecs)
        (display " system: 0")
        (display " real: ")
        (display msecs)
        (display " gc: 0")
        (newline)
        (display "result: ")
        (write result)
        (newline)
        result))))

(define-syntax time
  (syntax-rules ()
    ((_ expr) (time* (lambda () expr)))))
