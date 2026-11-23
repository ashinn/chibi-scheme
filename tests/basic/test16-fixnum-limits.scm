
;; Overflow at the negative fixnum limit, through the inlined fast
;; paths, the generic dispatch (via apply) and variadic calls.
(define max-fx 4611686018427387903)
(define min-fx (- -1 max-fx))
(define (id x) x)

(write (list (+ min-fx -1) (+ (id min-fx) (id -1)) (apply + (list min-fx -1))
             (+ min-fx -1 0) (+ max-fx 1) (apply + (list max-fx 1))))
(newline)
(write (list (- min-fx 1) (- (id min-fx) (id 1)) (apply - (list min-fx 1))
             (- min-fx 1 1) (- (id min-fx)) (apply - (list min-fx))
             (- max-fx -1) (apply - (list max-fx -1))))
(newline)
