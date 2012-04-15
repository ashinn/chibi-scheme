
;;> Cycle-aware equality.  Returns @scheme{#t} iff @scheme{a} and
;;> @scheme{b} are @scheme{equal?}, including cycles.  Another way
;;> to think of it is they are @scheme{equiv} if they print the
;;> same, assuming all elements can be printed.

(define (equiv? a b)
  (let ((equivs (make-hash-table eq?)))
    (define (get-equivs x)
      (or (hash-table-ref/default equivs x #f)
          (let ((tmp (make-hash-table eq?)))
            (hash-table-set! equivs x tmp)
            tmp)))
    (define (merge! tab x)
      (hash-table-set! tab x tab)
      (cond ((hash-table-ref/default equivs x #f)
             => (lambda (tab2)
                  (hash-table-walk tab2 (lambda (key value)
                                          (hash-table-set! tab key tab)))))))
    (define (equiv? a b)
      (cond
       ((eq? a b))
       ((pair? a)
        (and (pair? b)
             (let ((a-tab (get-equivs a)))
               (hash-table-ref
                a-tab
                b
                (lambda ()
                  (merge! a-tab b)
                  (and (equiv? (car a) (car b))
                       (equiv? (cdr a) (cdr b))))))))
       ((vector? a)
        (and (vector? b)
             (= (vector-length a) (vector-length b))
             (let ((a-tab (get-equivs a)))
               (hash-table-ref
                a-tab
                b
                (lambda ()
                  (merge! a-tab b)
                  (let lp ((i (- (vector-length a) 1)))
                    (or (< i 0)
                        (and (equiv? (vector-ref a i) (vector-ref b i))
                             (lp (- i 1))))))))))
       (else
        (equal? a b))))
    (let ((res (equal?/bounded a b 1000000)))
      (and res (or (> res 0) (equiv? a b)) #t))))
