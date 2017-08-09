
(define fl= =)
(define fl< <)
(define fl> >)
(define fl<= <=)
(define fl>= >=)
(define flodd? odd?)
(define fleven? even?)
(define (flunordered? x y) (or (flnan? x) (flnan? y)))
(define flinteger? integer?)
(define flzero? zero?)
(define flpositive? positive?)
(define flnegative? negative?)
(define flonum exact->inexact)

(define fl+ +)
(define fl- -)
(define fl* *)
(define fl/ /)
(define flmax max)
(define flmin min)
(define (flabsdiff x y) (abs (- x y)))
(define flnumerator numerator)
(define fldenominator denominator)
(define flround round)

(define (flsquare x) (fl* x x))

(define (flsgn x) (flcopysign 1.0 x))

(define (fldenormalized? x)
  (eqv? FP_SUBNORMAL (fpclassify x)))

(define (flatan x . o)
  (if (pair? o)
      (flatan2 x (car o))
      (flatan1 x)))

(define (flinteger-fraction x)
  (let ((ls (modf x))) (values (cadr ls) (car ls))))

(define (flnormalized-fraction-exponent x)
  (apply values (frexp x)))

(define (flremquo x y)
  (apply values (remquo x y)))

(define (flloggamma x)
  (apply values (lgamma_r x)))
