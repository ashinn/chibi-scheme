
(define-library (chibi time)
  (export current-seconds get-time-of-day set-time-of-day!
          seconds->time seconds->string time->seconds time->string
          make-timeval make-tm timeval-seconds timeval-microseconds
          timezone-offset timezone-dst-time
          time-second time-minute time-hour time-day time-month time-year
          time-day-of-week time-day-of-year time-dst? time-timezone-name
          time-offset
          tm? timeval? timezone?)
  (cond-expand
   ((or bsd linux)
    (export rusage? resource-usage-time resource-usage-system-time
            resource-usage-max-rss resource-usage/self
            resource-usage/children get-resource-usage))
   (else))
  (import (chibi))
  (include-shared "time"))
