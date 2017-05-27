
(define fx=? =)
(define fx<? <)
(define fx>? >)
(define fx<=? <=)
(define fx>=? >=)
(define fxzero? zero?)
(define fxpositive? positive?)
(define fxnegative? negative?)
(define fxodd? odd?)
(define fxeven? even?)
(define fxmax max)
(define fxmin min)
(define fx+ +)
(define fx- -)
(define fx* *)
(define fxquotient quotient)
(define fxremainder remainder)
(define fxabs abs)
(define fxsquare square)
(define fxsqrt exact-integer-sqrt)

(define fx-width
  (if (fixnum? (expt 2 32)) 62 30))

(define fx-greatest
  (- (expt 2 fx-width) 1))

(define fx-least
  (- -1 fx-greatest))

(define (fxneg x) (- x))

(define (fx+/carry i j k)
  (let ((s (+ i j k)))
    (call-with-values (lambda () (balanced/ s (expt 2 fx-width)))
      (lambda (q r) (values r q)))))

(define (fx-/carry i j k)
  (let ((d (- i j k)))
    (call-with-values (lambda () (balanced/ d (expt 2 fx-width)))
      (lambda (q r) (values r q)))))

(define (fx+*/carry i j k)
  (let ((s (+ (* i j) k)))
    (call-with-values (lambda () (balanced/ s (expt 2 fx-width)))
      (lambda (q r) (values r q)))))

(define fxarithmetic-shift-left fxarithmetic-shift)

(define (fxarithmetic-shift-right i count)
  (fxarithmetic-shift i (- count)))

(define (fxbit-set? index i)
  (or (bit-set? index i)
      (and (negative? i)
           (>= index (fxlength i)))))
