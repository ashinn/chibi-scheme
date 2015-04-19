
;;> Library for computing (optionally continuously) compounded interest.

(define-library (euler interest)
  (export compound-interest)
  (import (scheme base) (scheme inexact))
  (begin
    ;;> Returns the total amount starting at \var{base} increasing at
    ;;> the given interest rate \var{rate}, for the given \var{duration}.
    ;;> Compounds at optional \var{interval} intervals, which default
    ;;> to +inf.0 for continuous.
    (define (compound-interest base rate duration . o)
      (let ((interval (or (and (pair? o) (car o)) +inf.0)))
        (if (finite? interval)
            (* base (expt (+ 1 (/ rate interval)) (* duration interval)))
            (* base (exp (* rate duration))))))))
