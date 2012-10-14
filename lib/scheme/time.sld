;; time.sld - (scheme time) library definition
;;
;; Copyright (c) 2012 Alex Shinn. All rights reserved.
;; Copyright (c) 2012 Alan Watson. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-library (scheme time)
  (export current-second current-jiffy jiffies-per-second)
  (import (chibi))
  (import (scheme process-context))
  (import (scheme time tai))
  (include-shared "time")
  ;; If the environment variable SEXP_CLOCK_TYPE is set, its value
  ;; is used for the system clock type. If the environment variable
  ;; SEXP_CLOCK_TYPE is not set, then the system clock is ntp-like
  ;; if current-ntp-clock-values is a procedure and is posix-like
  ;; otherwise.
  (cond-expand
   (ntp
    (begin
      (define clock-type
        (cond
         ((get-environment-variable "SEXP_CLOCK_TYPE")
          => (lambda (x)
               (let ((type (string->symbol x)))
                 (case type
                   ((posix-like tai-like ntp-like) type)
                   (else
                    (display "invalid value for SEXP_CLOCK_TYPE: " (current-error-port))
                    (write x (current-error-port))
                    (display " - expected \"ntp-like\", \"posix-like\" or \"tai-like\"\n" (current-error-port))
                    'ntp-like)))))
         (else 'ntp-like)))))
   (else
    (begin (define clock-type 'posix-like))))
  (begin
    ;; The value of the environment variable SEXP_CLOCK_EPOCH_OFFSET
    ;; specifies the offset of the system clock relative to the
    ;; standard epoch (1970-01-01 00:00:00 UTC for posix-like and
    ;; ntp-like clocks and 1970-01-01 00:00:00 TAI for tai-like system
    ;; clocks). If it not set, the offset is assumed to be zero. In
    ;; call-with-current-clock-values, the offset is added to the
    ;; current seconds before calling the procedure argument.
    (define epoch-offset
      (exact->inexact
       (string->number
        (or (get-environment-variable "SEXP_CLOCK_EPOCH_OFFSET")
            "0"))))

    ;; Normally, one does not need to specify either the clock type or
    ;; clock epoch explicitly. One case where it might be necessary is
    ;; if the system clock runs on the TAI-10 timescale. In this case,
    ;; one should set SEXP_CLOCK_TYPE to "tai-like" and
    ;; SEXP_CLOCK_EPOCH_OFFSET to -10.
    ;;
    ;; The call-with-current-clock-values obtains the clock values
    ;; from the current-ntp-clock-values procedure, if the system
    ;; clock is ntp-like, and from the current-clock-second procedure
    ;; otherwise.
    (define call-with-current-clock-values
      (cond-expand
       (ntp
        (case clock-type
          ((ntp-like)
           (lambda (p)
             (let ((values-pair (current-ntp-clock-values)))
               (p (+ (car values-pair) epoch-offset) (cdr values-pair)))))
          (else
           (lambda (p) (p (+ (current-clock-second) epoch-offset) #f)))))
       (else
        (lambda (p) (p (+ (current-clock-second) epoch-offset) #f)))))

    ;; Exported interface.
    (define current-second
      (make-tai-clock clock-type call-with-current-clock-values))
    (define (current-jiffy)
      (inexact->exact (round (* (current-second) (jiffies-per-second)))))
    (define (jiffies-per-second) 1000)))
