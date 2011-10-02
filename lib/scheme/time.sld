
(define-library (scheme time)
  (import (scheme))
  (export current-second current-jiffy jiffies-per-second)
  (include-shared "time.c")
  (begin
    (define current-jiffy current-second)
    (define (jiffies-per-second) 1)))
