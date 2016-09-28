
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
  (cond-expand
   (chibi
    (import (chibi))
    (include-shared "time"))
   (else
    (import (scheme base) (scheme write) (scheme time)
            (rename (srfi 19) (time-second srfi-19:time-second)))
    (begin
      ;; a SRFI-19 `date' is a datetime, which in C is a tm (time) struct
      (define tm? date?)
      (define time-second date-second)
      (define time-minute date-minute)
      (define time-hour date-hour)
      (define time-day date-day)
      (define time-month date-month)
      (define (time-year x) (- (date-year x) 1900))
      (define time-day-of-week date-week-day)
      (define time-day-of-year date-year-day)
      (define (seconds->time seconds)
        (time-tai->date (make-time time-tai 0 (exact (round seconds)))))
      (define current-seconds current-second)
      (define (get-time-of-day)
        (list (current-time) time-utc))
      (define (timeval-seconds tv) (srfi-19:time-second tv))
      (define (timeval-microseconds tv) (/ (time-nanosecond tv) 1000)))))
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
         (let ((out (open-output-string)))
           (write 'expr out)
           (time (get-output-string out) expr)))
        ((time name expr)
         (time* name (lambda () expr)))))))
