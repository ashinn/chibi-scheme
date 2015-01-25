;; Copyright (c) 2012 Alan Watson. All rights reserved. BSD-style
;; license: http://synthcode.com/license.txt

;; This library implements TAI clocks with an epoch of 1970-01-01
;; 00:00:00 TAI.

(define-library (scheme time tai)
  
  (export make-tai-clock)
  
  (import (scheme base))
  (import (scheme time tai-to-utc-offset))
  
  (begin
    
    (define seconds-per-day (* 24.0 60.0 60.0))
    
    (define (make-tai-clock-from-tai-like-clock call-with-current-clock-values)
      (define (consumer second leap-second-indicator)
        second)
      (lambda ()
        (call-with-current-clock-values consumer)))
    
    (define (make-tai-clock-from-posix-like-clock call-with-current-clock-values)
      (define (consumer second leap-second-indicator)
        (+ second (tai-to-utc-offset-at-utc-day (/ second seconds-per-day))))
      (lambda ()
        (call-with-current-clock-values consumer)))
    
    (define (make-tai-clock-from-ntp-like-clock call-with-current-clock-values)
      (define (consumer second leap-second-indicator)
        (+ second
           (tai-to-utc-offset-at-utc-day (/ second seconds-per-day))
           (if leap-second-indicator 1.0 0.0)))
      (lambda ()
        (call-with-current-clock-values consumer)))
    
    ;; (make-tai-clock type call-with-current-clock-values)
    ;;
    ;; The make-tai-clock procedure returns a procedure that, when
    ;; called with no arguments, returns an estimate of the number of
    ;; TAI seconds since 1970-01-01 00:00:00 TAI.
    ;;
    ;; The type and call-with-current-clock-values argument should
    ;; conform to the descriptions in the documentation of the (clock
    ;; system-clock) library.
    
    (define (make-tai-clock type call-with-current-clock-values)
      (case type
        ((tai-like)
         (make-tai-clock-from-tai-like-clock call-with-current-clock-values))
        ((posix-like)
         (make-tai-clock-from-posix-like-clock call-with-current-clock-values))
        ((ntp-like)
         (make-tai-clock-from-ntp-like-clock call-with-current-clock-values))
        (else
         (error "invalid clock type" type))))))
