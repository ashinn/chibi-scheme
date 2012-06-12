
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursors

(define (iset-empty? iset)
  (and (iset? iset)
       (cond ((iset-bits iset) => zero?) (else #f))
       (let ((l (iset-left iset))) (or (not l) (iset-empty? l)))
       (let ((r (iset-right iset))) (or (not r) (iset-empty? r)))))

(define-record-type Iset-Cursor
  (make-iset-cursor node pos stack)
  iset-cursor?
  (node iset-cursor-node iset-cursor-node-set!)
  (pos iset-cursor-pos iset-cursor-pos-set!)
  (stack iset-cursor-stack iset-cursor-stack-set!))

;; Create a new iset cursor pointing to the first element of iset,
;; with an optional stack argument.
(define (%iset-cursor iset . o)
  (iset-cursor-advance
   (make-iset-cursor iset
                     (or (iset-bits iset) (iset-start iset))
                     (if (pair? o) (car o) '()))))

(define (iset-cursor iset . o)
  (let ((stack (if (pair? o) (car o) '())))
    (if (iset-left iset)
        (iset-cursor (iset-left iset) (cons iset stack))
        (%iset-cursor iset stack))))

;; Continue to the next node in the search stack.
(define (iset-cursor-pop cur)
  (let ((node (iset-cursor-node cur))
        (stack (iset-cursor-stack cur)))
    (cond
     ((iset-right node)
      (iset-cursor (iset-right node) stack))
     ((pair? stack)
      (%iset-cursor (car stack) (cdr stack)))
     (else
      cur))))

;; Advance to the next node+pos that can be referenced if at the end
;; of this node's range.
(define (iset-cursor-advance cur)
  (let ((node (iset-cursor-node cur))
        (pos (iset-cursor-pos cur)))
    (cond
     ((if (iset-bits node) (zero? pos) (> pos (iset-end node)))
      (iset-cursor-pop cur))
     (else cur))))

(define (iset-cursor-next iset cur)
  (iset-cursor-advance
   (let ((node (iset-cursor-node cur))
         (pos (iset-cursor-pos cur))
         (stack (iset-cursor-stack cur)))
     (let ((pos (if (iset-bits node) (bitwise-and pos (- pos 1)) (+ pos 1))))
       (make-iset-cursor node pos stack)))))

(define (iset-ref iset cur)
  (let ((node (iset-cursor-node cur))
        (pos (iset-cursor-pos cur)))
    (cond
     ((iset-bits node)
      (if (zero? pos)
          (error "cursor reference past end of iset")
          (+ (iset-start node)
             (integer-length (- pos (bitwise-and pos (- pos 1))))
             -1)))
     (else
      (if (> pos (iset-end node))
          (error "cursor reference past end of iset")
          pos)))))

(define (end-of-iset? cur)
  (let ((node (iset-cursor-node cur)))
    (and (if (iset-bits node)
             (zero? (iset-cursor-pos cur))
             (> (iset-cursor-pos cur) (iset-end node)))
         (not (iset-right node))
         (null? (iset-cursor-stack cur)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equality

(define (iset2= is1 is2)
  (let lp ((cur1 (iset-cursor is1))
           (cur2 (iset-cursor is2)))
    (cond ((end-of-iset? cur1) (end-of-iset? cur2))
          ((end-of-iset? cur2) #f)
          ((= (iset-ref is1 cur1) (iset-ref is2 cur2))
           (lp (iset-cursor-next is1 cur1) (iset-cursor-next is2 cur2)))
          (else
           #f))))

(define (iset2<= is1 is2)
  (let lp ((cur1 (iset-cursor is1))
           (cur2 (iset-cursor is2)))
    (cond ((end-of-iset? cur1))
          ((end-of-iset? cur2) #f)
          (else
           (let ((i1 (iset-ref is1 cur1))
                 (i2 (iset-ref is1 cur2)))
             (cond ((< i1 i2)
                    (lp (iset-cursor-next is1 cur1) cur2))
                   ((= i1 i2)
                    (lp (iset-cursor-next is1 cur1)
                        (iset-cursor-next is2 cur2)))
                   (else
                    #f)))))))

(define (iset= . o)
  (or (null? o)
      (let lp ((a (car o)) (ls (cdr o)))
        (or (null? ls) (and (iset2= a (car ls)) (lp (car ls) (cdr ls)))))))

(define (iset<= . o)
  (or (null? o)
      (let lp ((a (car o)) (ls (cdr o)))
        (or (null? ls) (and (iset2<= a (car ls)) (lp (car ls) (cdr ls)))))))

(define (iset>= . o)
  (apply iset<= (reverse o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding

(define (iset-fold-node kons knil iset)
  (let lp ((is iset) (acc knil))
    (let* ((left (iset-left is))
           (acc (kons is (if left (lp left acc) acc)))
           (right (iset-right is)))
      (if right (lp right acc) acc))))

(define (iset-fold kons knil iset)
  (iset-fold-node
   (lambda (is acc)
     (let ((start (iset-start is))
           (end (iset-end is))
           (bits (iset-bits is)))
       (if bits
           (let ((limit (+ 1 (- end start))))
             (do ((n1 bits n2)
                  (n2 (bitwise-and bits (- bits 1)) (bitwise-and n2 (- n2 1)))
                  (acc acc (kons (+ start (integer-length (- n1 n2)) -1) acc)))
                 ((zero? n1) acc)))
           (do ((i start (+ i 1))
                (acc acc (kons i acc)))
               ((> i end) acc)))))
   knil
   iset))

(define (iset-for-each-node proc iset)
  (iset-fold-node (lambda (node acc) (proc node)) #f iset))

(define (iset-for-each proc iset)
  (iset-fold (lambda (i acc) (proc i)) #f iset))

(define (iset->list iset)
  (reverse (iset-fold cons '() iset)))

(define (iset-size iset)
  (iset-fold-node
   (lambda (is acc)
     (let ((bits (iset-bits is)))
       (+ acc (if bits
                  (bit-count bits)
                  (+ 1 (- (iset-end is) (iset-start is)))))))
   0
   iset))
