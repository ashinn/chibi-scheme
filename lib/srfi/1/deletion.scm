
(define (delete x ls . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (if (eq? eq eq?)
        (let lp ((ls ls) (rev '())) ;; fast path for delq
          (let ((tail (memq x ls)))
            (if tail
                (lp (cdr tail) (take-up-to-reverse ls tail rev))
                (if (pair? rev) (append-reverse! rev ls) ls))))
        (filter (lambda (y) (eq x y)) ls))))

(define delete! delete)

(define (delete-duplicates ls . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (let lp ((ls ls) (res '()))
      (if (pair? ls)
          (lp (cdr ls) (if (member (car ls) res) res (cons (car ls) res)))
          (reverse! res)))))

(define delete-duplicates! delete-duplicates)

