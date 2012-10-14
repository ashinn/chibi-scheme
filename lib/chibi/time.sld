
(define-library (chibi time)
  (export current-seconds get-time-of-day set-time-of-day!
          seconds->time seconds->string time->seconds time->string
          make-timeval timeval-seconds timeval-microseconds
          timezone-offset timezone-dst-time
          time-second time-minute time-hour time-day time-month time-year
          time-day-of-week time-day-of-year time-dst?
          tm? timeval? timezone?)
  (import (chibi))
  (include-shared "time"))
