
(define (record? x)
  (is-a? x Object))

(define (record-rtd x)
  (type-of x))

(define (rtd-name x) (string->symbol (type-name x)))

(define (rtd-parent x) (type-parent x))

(define (rtd-field-names x)
  (list->vector
   (map (lambda (x) (if (pair? x) (cadr x) x)) (type-slots x))))

(define (rtd-all-field-names x)
  (let lp ((x x) (res '()))
    (let ((res (append (vector->list (rtd-field-names x)) res)))
      (let ((p (type-parent x)))
        (if (type? p)
            (lp p res)
            (list->vector res))))))

(define (rtd-field-mutable? rtd x)
  (let lp ((ls (type-slots rtd)))
    (cond ((null? ls)
           (let ((p (type-parent rtd)))
             (if (type? p)
                 (rtd-field-mutable? p x)
                 (error "unknown field" rtd x))))
          ((eq? x (car ls)))
          ((and (pair? (car ls)) (eq? x (cadr (car ls))))
           (not (eq? 'immutable (caar ls))))
          (else (lp (cdr ls))))))
