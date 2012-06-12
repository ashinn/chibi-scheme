
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for constructing and joining isets.

(define bits-thresh 128)  ; within 128 we join into a bitmap
(define bits-max 512)     ; don't make bitmaps larger than this

(define (bit-set n index)
  (bitwise-ior n (arithmetic-shift 1 index)))

(define (bit-clear n index)
  (if (bit-set? index n)
      (- n (arithmetic-shift 1 index))
      n))

(define (iset . args)
  (list->iset args))

(define (list->iset! ls iset)
  (for-each (lambda (i) (iset-adjoin1! iset i)) ls)
  iset)

(define (list->iset ls . opt)
  (list->iset! ls (if (pair? opt) (iset-copy (car opt)) (make-iset))))

(define (iset-copy iset)
  (and iset
       (%make-iset
        (iset-start iset)
        (iset-end iset)
        (iset-bits iset)
        (iset-copy (iset-left iset))
        (iset-copy (iset-right iset)))))

(define (iset-copy-node iset)
  (%make-iset (iset-start iset) (iset-end iset) (iset-bits iset) #f #f))

(define (iset-max-end iset)
  (cond ((iset-right iset) => iset-max-end)
        (else (iset-end iset))))

(define (iset-min-start iset)
  (cond ((iset-left iset) => iset-min-start)
        (else (iset-start iset))))

(define (iset-insert-left! iset new)
  (let ((left (iset-left iset)))
    (if (and left (< (iset-end new) (iset-start left)))
        (iset-right-set! new left)
        (iset-left-set! new left)))
  (iset-left-set! iset new))

(define (iset-insert-right! iset new)
  (let ((right (iset-right iset)))
    (if (and right (< (iset-end new) (iset-start right)))
        (iset-right-set! new right)
        (iset-left-set! new right)))
  (iset-right-set! iset new))

(define (range->bits start end)
  (- (arithmetic-shift 1 (+ 1 (- end start))) 1))

(define (iset-squash-bits! iset)
  (let ((bits (iset-bits iset)))
    (if (and bits (= bits (range->bits (iset-start iset) (iset-end iset))))
        (iset-bits-set! iset #f))))

(define (iset-adjoin1! iset n)
  (cond
   ((iset-empty? iset)
    (iset-start-set! iset n)
    (iset-end-set! iset n)
    (iset-bits-set! iset #f))
   (else
    (let ((start (iset-start iset))
          (end (iset-end iset))
          (bits (iset-bits iset)))
      (cond
       ((< n start)
        (let ((s-diff (- start n)))
          (if (let* ((left (iset-left iset))
                     (m-end (and left (iset-max-end left))))
                (and m-end
                     (or (< n m-end)
                         (< (- n m-end) s-diff))))
              (iset-adjoin1! (iset-left iset) n)
              (cond
               ((and (< s-diff bits-thresh)
                     (< (- end n) bits-max))
                (iset-start-set! iset n)
                (let ((bits2 (arithmetic-shift (or bits (range->bits start end))
                                               s-diff)))
                  (iset-bits-set! iset (+ bits2 1))
                  (iset-squash-bits! iset)))
               (else (iset-insert-left! iset (make-iset n)))))))
       ((> n end)
        (let ((e-diff (- n end)))
          (if (let* ((right (iset-right iset))
                     (m-start (and right (iset-min-start right))))
                (and m-start
                     (or (> n m-start)
                         (> (- n m-start) e-diff))))
              (iset-adjoin1! (iset-right iset) n)
              (cond
               ((and (< e-diff bits-thresh)
                     (< (- n start) bits-max))
                (iset-end-set! iset n)
                (iset-bits-set! iset (bit-set (or bits (range->bits start end))
                                              (- n start)))
                (iset-squash-bits! iset))
               (else (iset-insert-right! iset (make-iset n)))))))
       (bits
        (iset-bits-set! iset (bit-set (iset-bits iset) (- n start)))
        (iset-squash-bits! iset)))))))

(define (iset-adjoin-node! a b)
  (cond
   ((iset-empty? a)
    (iset-start-set! a (iset-start b))
    (iset-end-set! a (iset-end b))
    (iset-bits-set! a (iset-bits b)))
   ((not (iset-empty? b))
    (let ((a-start (iset-start a))
          (a-end (iset-end a))
          (a-bits (iset-bits a))
          (b-start (iset-start b))
          (b-end (iset-end b))
          (b-bits (iset-bits b)))
      (cond
       ;;         aaaa...
       ;; ...bbbb
       ((< b-end a-start)
        (let ((near-diff (- a-start b-end))
              (start-diff (- a-start b-start))
              (far-diff (- a-end b-start)))
          (if (let* ((left (iset-left a))
                     (m-end (and left (iset-max-end left))))
                (and m-end
                     (or (< b-end m-end)
                         (< (- b-end m-end) near-diff))))
              (iset-adjoin-node! (iset-left a) b)
              (cond
               ((and (< near-diff bits-thresh)
                     (< far-diff bits-max))
                (let ((bits (arithmetic-shift
                             (or a-bits (range->bits a-start a-end))
                             start-diff))
                      (lo-bits (or b-bits (range->bits b-start b-end))))
                  (iset-start-set! a b-start)
                  (iset-bits-set! a (bitwise-ior bits lo-bits))
                  (iset-squash-bits! a)))
               (else (iset-insert-left! a (iset-copy-node b)))))))
       ;; ...aaaa
       ;;         bbbb...
       ((> b-start a-end)
        (let ((near-diff (- b-start a-end))
              (start-diff (- b-start a-start))
              (far-diff (- b-end a-start)))
          (if (let* ((right (iset-right a))
                     (m-start (and right (iset-min-start right))))
                (and m-start
                     (or (> b-start m-start)
                         (> (- b-start m-start) near-diff))))
              (iset-adjoin-node! (iset-right a) b)
              (cond
               ((and (< near-diff bits-thresh)
                     (< far-diff bits-max))
                (iset-end-set! a b-end)
                (iset-bits-set!
                 a
                 (bitwise-ior
                  (or a-bits (range->bits a-start a-end))
                  (arithmetic-shift
                   (or b-bits (range->bits b-start b-end))
                   start-diff)))
                (iset-squash-bits! a))
               (else (iset-insert-right! a (iset-copy-node b)))))))
       ;; aaaa...
       ;;   bbbb...
       ((> b-start a-start)
        (iset-end-set! a (max a-end b-end))
        (cond
         ((or a-bits b-bits)
          (iset-bits-set!
           a
           (bitwise-ior
            (or a-bits (range->bits a-start a-end))
            (arithmetic-shift
             (or b-bits (range->bits b-start b-end))
             (- b-start a-start))))
          (iset-squash-bits! a))))
       ;;   aaaa...
       ;; bbbb...
       ((< b-start a-start)
        (iset-start-set! a b-start)
        (iset-end-set! a (max a-end b-end))
        (cond
         ((or a-bits b-bits)
          (iset-bits-set!
           a
           (bitwise-ior
            (arithmetic-shift
             (or a-bits (range->bits a-start a-end))
             (- a-start b-start))
            (or b-bits (range->bits b-start b-end))))
          (iset-squash-bits! a))))
       ;; aaaa...
       ;; bbbb...
       (else
        (iset-end-set! a (max a-end b-end))
        (cond
         ((or a-bits b-bits)
          (iset-bits-set!
           a
           (bitwise-ior
            (or a-bits (range->bits a-start a-end))
            (or b-bits (range->bits b-start b-end))))
          (iset-squash-bits! a)))))))))

(define (iset-adjoin! iset . ls)
  (list->iset! ls iset))

(define (iset-adjoin iset . ls)
  (list->iset ls iset))

;; delete directly in this node
(define (%iset-delete1! iset n)
  (let ((start (iset-start iset))
        (end (iset-end iset))
        (bits (iset-bits iset)))
    (cond
     (bits
      (iset-bits-set! iset (bit-clear bits (- n start))))
     ((= n start)
      (if (= n end)
          (iset-bits-set! iset 0)
          (iset-start-set! iset (+ n 1))))
     ((= n end)
      (iset-end-set! iset (- n 1)))
     (else
      (iset-end-set! iset (- n 1))
      (iset-insert-right! iset (make-iset (+ n 1) end))))))

(define (iset-delete1! iset n)
  (let lp ((is iset))
    (let ((start (iset-start is)))
      (if (< n start)
          (let ((left (iset-left is)))
            (if left (lp left)))
          (let ((end (iset-end is)))
            (if (> n end)
                (let ((right (iset-right is)))
                  (if right (lp right)))
                (%iset-delete1! is n)))))))

(define (iset-delete! iset . args)
  (for-each (lambda (i) (iset-delete1! iset i)) args)
  iset)

(define (iset-delete iset . args)
  (apply iset-delete! (iset-copy iset) args))

(define (iset-map proc iset)
  (iset-fold (lambda (i is) (iset-adjoin! is i)) (make-iset) iset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level set operations.
;;
;; Union is optimized to work at the node level.  Intersection and
;; difference iterate over individual elements and so have a lot of
;; room for improvement, at the expense of the complexity of
;; iset-adjoin-node!.

(define (iset-union2! a b)
  (iset-for-each-node
   (lambda (is)
     (iset-adjoin-node! a is))
   b))

(define (iset-union! . args)
  (let* ((a (and (pair? args) (car args)))
         (b (and (pair? args) (pair? (cdr args)) (cadr args))))
    (cond
     (b
      (iset-union2! a b)
      (apply iset-union! a (cddr args)))
     (a a)
     (else (make-iset)))))

(define (iset-union . args)
  (if (null? args)
    (make-iset)
    (apply iset-union! (iset-copy (car args)) (cdr args))))

(define (iset-intersection! a . args)
  (let ((b (and (pair? args) (car args))))
    (cond
     (b
      (iset-for-each
       (lambda (i) (if (not (iset-contains? b i)) (iset-delete1! a i)))
       a)
      (apply iset-intersection! a (cdr args)))
     (else a))))

(define (iset-intersection a . args)
  (apply iset-intersection! (iset-copy a) args))

(define (iset-difference! a . args)
  (if (null? args)
      a
      (begin
        (iset-for-each (lambda (i) (iset-delete1! a i)) (car args))
        (apply iset-difference! a (cdr args)))))

(define (iset-difference a . args)
  (apply iset-difference! (iset-copy a) args))
