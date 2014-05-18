
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for constructing and joining isets.

(define bits-thresh 128)  ; within 128 we join into a bitmap
;;(define bits-max 512)     ; don't make bitmaps larger than this

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

(define (iset-should-merge-left? a b)
  (and (< (- (iset-start a) (iset-end b))
          bits-thresh)
       (or (not (iset-left a))
           (> (iset-start b) (iset-max-end (iset-left a))))))

(define (iset-should-merge-right? a b)
  (and (< (- (iset-start b) (iset-end a))
          bits-thresh)
       (or (not (iset-right a))
           (< (iset-end b) (iset-min-start (iset-right a))))))

(define (iset-merge-left! a b)
  (if (or (iset-bits a) (iset-bits b)
          (< (+ 1 (iset-end b)) (iset-start a)))
      (let* ((a-bits (or (iset-bits a)
                         (range->bits (iset-start a) (iset-end a))))
             (b-bits (or (iset-bits b)
                         (range->bits (iset-start b) (iset-end b))))
             (shift (- (iset-start a) (iset-start b)))
             (bits (bitwise-ior b-bits (arithmetic-shift a-bits shift))))
        (iset-bits-set! a bits)))
  (iset-start-set! a (iset-start b)))

(define (iset-merge-right! a b)
  (if (or (iset-bits a) (iset-bits b)
          (< (+ 1 (iset-end a)) (iset-start b)))
      (let* ((a-bits (or (iset-bits a)
                         (range->bits (iset-start a) (iset-end a))))
             (b-bits (or (iset-bits b)
                         (range->bits (iset-start b) (iset-end b))))
             (shift (- (iset-start b) (iset-start a)))
             (bits (bitwise-ior a-bits (arithmetic-shift b-bits shift))))
        (iset-bits-set! a bits)))
  (iset-end-set! a (iset-end b)))

(define (iset-adjoin1! is n)
  (iset-adjoin-node! is (%make-iset n n #f #f #f)))

;; adjoin just the node b (ignoring left/right) to the full iset a
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
       ((<= b-end a-start)
        (if (iset-should-merge-left? a b)
            (iset-merge-left! a b)
            (iset-adjoin-node-left! a b)))
       ;; ...aaaa
       ;;         bbbb...
       ((>= b-start a-end)
        (if (iset-should-merge-right? a b)
            (iset-merge-right! a b)
            (iset-adjoin-node-right! a b)))
       ;; ...aaaaa...
       ;;  ...bb...
       ((and (>= b-start a-start) (<= b-end a-end))
        (if a-bits
            (let ((b-bits (arithmetic-shift
                           (or b-bits (range->bits b-start b-end))
                           (- b-start a-start))))
              (iset-bits-set! a (bitwise-ior a-bits b-bits))
              (iset-squash-bits! a))))
       (else
        ;; general case: split, recurse, join sides
        (let ((ls (iset-node-split b a-start a-end)))
          (if (car ls)
              (iset-adjoin-node-left! a (car ls)))
          (iset-adjoin-node! a (cadr ls))
          (if (car (cddr ls))
              (iset-adjoin-node-right! a (car (cddr ls)))))))))))

(define (iset-adjoin-node-left! iset node)
  (if (iset-left iset)
      (iset-adjoin-node! (iset-left iset) node)
      (iset-left-set! iset node)))

(define (iset-adjoin-node-right! iset node)
  (if (iset-right iset)
      (iset-adjoin-node! (iset-right iset) node)
      (iset-right-set! iset node)))

;; start and/or end are inside the node, split into:
;;   1. node before start, if any
;;   2. node between start and end
;;   3. node after end, if any
(define (iset-node-split node start end)
  (list (and (< (iset-start node) start)
             (iset-node-extract node (iset-start node) (- start 1)))
        (iset-node-extract node start end)
        (and (> (iset-end node) end)
             (iset-node-extract node (+ end 1) (iset-end node)))))

(define (iset-node-extract node start end)
  (let ((bits (and (iset-bits node)
                   (arithmetic-shift (iset-bits node)
                                     (- (iset-start node) start)))))
    (%make-iset start end bits #f #f)))

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
  (iset-fold (lambda (i is) (iset-adjoin! is (proc i))) (make-iset) iset))

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
