
(define-library (chibi time)
  (export current-seconds get-time-of-day time
          seconds->time seconds->string time->seconds time->string
          make-timeval make-tm timeval-seconds timeval-microseconds
          timezone-offset timezone-dst-time
          time-second time-minute time-hour time-day time-month time-year
          time-day-of-week time-day-of-year time-dst? time-timezone-name
          time-offset
          tm? timeval? timezone?)
  (cond-expand
   (emscripten)
   (else
    (export set-time-of-day!)))
  (cond-expand
   ((or bsd linux)
    (export rusage? resource-usage-time resource-usage-system-time
            resource-usage-max-rss resource-usage/self
            resource-usage/children get-resource-usage))
   (else))
  (import (chibi))
  (include-shared "time")
  (begin
    (define (timeval->milliseconds tv)
      (quotient (+ (* 1000000 (timeval-seconds tv))
                   (timeval-microseconds tv))
                1000))
    (define (time* name thunk)
      (let* ((start (car (get-time-of-day)))
             (result (thunk))
             (end (car (get-time-of-day)))
             (msecs (- (timeval->milliseconds end)
                       (timeval->milliseconds start))))
        (display name (current-error-port))
        (display ": " (current-error-port))
        (display msecs (current-error-port))
        (display " ms\n" (current-error-port))
        result))
    (define-syntax time
      (syntax-rules ()
        ((time expr)
         (time (call-with-output-string (lambda (out) (write 'expr out))) expr))
        ((time name expr)
         (time* name (lambda () expr)))))))
