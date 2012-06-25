
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimizing Iset Representation

(define (iset-balance iset)
  (and iset
       (let ((nodes '()))
         (iset-for-each-node
          (lambda (is) (set! nodes (cons (iset-copy-node is) nodes)))
          iset)
         (let reduce ((nodes (reverse nodes)))
           (let ((len (length nodes)))
             (case len
               ((0) #f)
               ((1) (car nodes))
               (else
                (let ((mid (quotient len 2)))
                  (let lp ((i 0) (ls nodes) (left '()))
                    (if (= i mid)
                        (let ((res (car ls)))
                          (iset-left-set! res (reduce (reverse left)))
                          (iset-right-set! res (reduce (cdr ls)))
                          res)
                        (lp (+ i 1) (cdr ls) (cons (car ls) left))))))))))))

(define (iset-balance! iset)
  (iset-balance iset))

;; remove leading 0's in bits before squashing
(define (iset-trim-and-squash-bits! is)
  (if (iset-bits is)
      (let ((end (iset-end is)))
        (let lp ((bits (iset-bits is))
                 (start (iset-start is)))
          (cond
           ((zero? bits)
            (iset-start-set! is start)
            (iset-bits-set! is 0))
           ((>= start end)
            (iset-start-set! is start)
            (iset-bits-set! is #f)
            (if (even? (arithmetic-shift bits -1))
                (iset-end-set! is start)))
           ((even? bits)
            (lp (arithmetic-shift bits -1) (+ start 1)))
           (else
            (iset-start-set! is start)
            (iset-bits-set! is bits))))))
  (iset-squash-bits! is)
  is)

;; overwrite a node in place
(define (iset-set-node! a b)
  (iset-start-set! a (iset-start b))
  (iset-end-set! a (iset-end b))
  (iset-bits-set! a (iset-bits b)))

;; safe to insert left since we've already visited all left nodes
(define (iset-node-replace! is nodes)
  (cond
   ((pair? nodes)
    (iset-set-node! is (car nodes))
    (let loop ((is is) (ls (cdr nodes)))
      (cond
       ((pair? ls)
        (iset-insert-left! is (car ls))
        (loop (iset-left is) (cdr ls))))))))

;; compact a list of consecutive bit ranges for an iset
(define (iset-node-split-ranges! is ranges)
  (let ((start (iset-start is))
        (end (iset-end is))
        (bits (iset-bits is)))
    (let lp ((ls (reverse ranges)) (nodes '()) (last 0))
      (if (pair? ls)
          (let ((lo (caar ls)) (hi (cdar ls)))
            (lp (cdr ls)
                (cons (make-iset (+ start lo) (+ start hi -1))
                      (if (< last lo) ;; trailing bit range
                          (cons (iset-trim-and-squash-bits!
                                 (%make-iset
                                  (+ start last)
                                  (+ start lo -1)
                                  (extract-bit-field (- lo last) last bits)
                                  #f
                                  #f))
                                nodes)
                          nodes))
                hi))
          (let ((nodes
                 (if (< (+ start last) end) ;; trailing bit range
                     (cons (iset-trim-and-squash-bits!
                            (%make-iset (+ start last)
                                        end
                                        (arithmetic-shift bits (- last))
                                        #f
                                        #f))
                           nodes)
                     nodes)))
            (iset-node-replace! is nodes))))))

;; Compact bit ranges of long consecutive chars in a single node into
;; ranges.  Loop over the bits, and convert any consecutive bit
;; patterns longer than span into new start/end nodes.
(define (iset-optimize-node! is span)
  (iset-squash-bits! is)
  (let* ((bits (iset-bits is))
         (len (and bits (integer-length bits))))
    (cond
     (bits
      (letrec
          ((full  ;; in a full bit range from [since..i)
            (lambda (i since ranges)
              (cond
               ((or (>= i len) (not (bit-set? i bits)))
                ;; if the current span is long enough, push to ranges
                (if (>= (- i since) span)
                    (sparse (+ i 1) (cons (cons since i) ranges))
                    (sparse (+ i 1) ranges)))
               (else
                (full (+ i 1) since ranges)))))
           (sparse  ;; [i-1] is not set
            (lambda (i ranges)
              (cond
               ((>= i len)
                ;; done - if there are any ranges to compact, do so
                (if (pair? ranges)
                    (iset-node-split-ranges! is ranges)))
               ((bit-set? i bits)
                (full (+ i 1) i ranges))
               (else
                (sparse (+ i 1) ranges))))))
        (sparse 0 '()))))))

;; Remove empty nodes.
(define (%iset-prune! is)
  (cond
   ((not is)
    #f)
   (else
    (iset-left-set! is (%iset-prune! (iset-left is)))
    (iset-right-set! is (%iset-prune! (iset-right is)))
    (if (and (eq? 0 (iset-bits is))
             (not (iset-left is))
             (not (iset-right is)))
        #f
        is))))

(define (iset-prune! is)
  (or (%iset-prune! is) (iset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iset-optimize! is . opt)
  (let ((span (if (pair? opt) (car opt) (* 40 8)))
        (is (iset-prune! is)))
    (iset-for-each-node (lambda (node) (iset-optimize-node! node span)) is)
    (iset-prune! is)))

(define (iset-optimize iset . opt)
  (apply iset-optimize! (iset-copy iset) opt))

;; write an efficient expression which evaluates to the iset
(define (iset->code iset)
  (and iset
       `(%make-iset ,(iset-start iset)
                    ,(iset-end iset)
                    ,(iset-bits iset)
                    ,(iset->code (iset-left iset))
                    ,(iset->code (iset-right iset)))))
