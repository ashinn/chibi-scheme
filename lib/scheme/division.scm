;;;; division.scm -- portable R7RS (scheme division) implementation
;;
;; This code is written by Alex Shinn and placed in the
;; Public Domain.  All warranties are disclaimed.
;;
;; This is basically the simplest possible implementation.  Note the
;; code below assumes that either 1) exact ratios are supported and
;; are handled correctly by floor, ceiling and round, or 2) that
;; you're using a simple implementation with only fixnums and flonums.
;; In the intermediate case where you have bignums but no ratios there
;; will be a loss of precision for large values.
;;
;; We handle both cases by the use of the cond-expand form in
;; division.sld to conditionally define copy-exactness2.  In case 1,
;; no adjustment is needed, whereas in case 2 we want to convert the
;; intermediate result back to exact if both inputs were exact.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The builtin quotient and remainder implement truncation - the
;; fractional part is always discarded.

(define truncate-quotient quotient)
(define truncate-remainder remainder)
(define (truncate/ n m)
  (values (truncate-quotient n m) (truncate-remainder n m)))

;; Floor, ceiling and round just compose their corresponding function
;; with division to determine the quotient, and compute the remainder
;; from that.

(define (floor-quotient n m)
  (copy-exactness2 n m (floor (/ n m))))
(define (floor-remainder n m)
  (- n (* m (floor-quotient n m))))
(define (floor/ n m)
  (values (floor-quotient n m) (floor-remainder n m)))

(define (ceiling-quotient n m)
  (copy-exactness2 n m (ceiling (/ n m))))
(define (ceiling-remainder n m)
  (- n (* m (ceiling-quotient n m))))
(define (ceiling/ n m)
  (values (ceiling-quotient n m) (ceiling-remainder n m)))

(define (round-quotient n m)
  (copy-exactness2 n m (round (/ n m))))
(define (round-remainder n m)
  (- n (* m (round-quotient n m))))
(define (round/ n m)
  (values (round-quotient n m) (round-remainder n m)))

;; Euclidean is defined as floor if the divisor is negative, and
;; ceiling otherwise.

(define (euclidean-quotient n m)
  (if (> m 0) (floor-quotient n m) (ceiling-quotient n m)))
(define (euclidean-remainder n m)
  (- n (* m (euclidean-quotient n m))))
(define (euclidean/ n m)
  (values (euclidean-quotient n m) (euclidean-remainder n m)))

;; Centered places the remainder in the half-open interval
;; [-m/2, m/2).

(define (centered-remainder n m)
  (let ((r (euclidean-remainder n m))
        (m/2 (abs (/ m 2))))
    (cond ((< r (- m/2)) (+ r (abs m)))
          ((>= r m/2) (- r (abs m)))
          (else r))))
(define (centered-quotient n m)
  (quotient (- n (centered-remainder n m)) m))
(define (centered/ n m)
  (values (centered-quotient n m) (centered-remainder n m)))
