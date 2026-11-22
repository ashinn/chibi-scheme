
;; A nested leaf lambda (with its shorter native prologue) generated
;; before a self tail call must not clobber the enclosing loop start.
(define g #f)
(define (f ls i res)
  (if (null? ls)
      res
      (begin
        (set! g (map (lambda (x) x) (list i)))
        (f (cdr ls) (+ i 1) (cons g res)))))
(write (f '(a b c) 0 '()))
(newline)
