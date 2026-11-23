
;; A global's self tail calls must go through its binding, so that
;; rebinding it (e.g. by trace) affects the recursive calls too.

(define (count-down n) (if (zero? n) 'done (count-down (- n 1))))
(define orig count-down)
(define calls 0)
(set! count-down (lambda (n) (set! calls (+ calls 1)) (orig n)))
(display (count-down 5))
(newline)
(display calls)
(newline)
