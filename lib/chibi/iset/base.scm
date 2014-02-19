
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The most basic set interface.  We provide a data type, low-level
;; constructor, and membership test.  This allows libraries to provide
;; an iset API without needing to include the full iset library.

(define-record-type Integer-Set
  (%make-iset start end bits left right)
  iset?
  (start iset-start iset-start-set!)
  (end   iset-end   iset-end-set!)
  (bits  iset-bits  iset-bits-set!)
  (left  iset-left  iset-left-set!)
  (right iset-right iset-right-set!))

(define (make-iset . opt)
  (if (null? opt)
      (%make-iset 0 0 0 #f #f)
      (let ((end (if (pair? (cdr opt)) (cadr opt) (car opt))))
        (%make-iset (car opt) end #f #f #f))))

(define (iset-contains? iset n)
  (let lp ((is iset))
    (let ((start (iset-start is)))
      (if (< n start)
          (let ((left (iset-left is))) (and left (lp left)))
          (let ((end (iset-end is)))
            (if (> n end)
                (let ((right (iset-right is))) (and right (lp right)))
                (let ((bits (iset-bits is)))
                  (or (not bits)
                      (bit-set? (- n start) bits)))))))))
