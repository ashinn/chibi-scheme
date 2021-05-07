;; Miscellaneous Functions

(define (translation? x)
  (and (vector? x) (not (vector-empty? x)) (vector-every exact-integer? x)))

(define (permutation? x)
  (and (translation? x)
       (let* ((len (vector-length x))
              (seen (make-vector len 0)))
         (let lp ((i 0))
           (or (>= i len)
               (and (< -1 (vector-ref x i) len)
                    (zero? (vector-ref seen (vector-ref x i)))
                    (begin
                      (vector-set! seen (vector-ref x i) 1)
                      (lp (+ i 1)))))))))

;; Intervals

(define-record-type Interval
  (%%make-interval lb ub)
  interval?
  (lb interval-lb)
  (ub interval-ub))

(define (%make-interval lo hi)
  (assert (and (translation? lo)
               (translation? hi)
               (= (vector-length lo) (vector-length hi))
               (vector-every < lo hi)))
  (%%make-interval lo hi))

(define (make-interval x . o)
  (if (pair? o)
      (%make-interval x (car o))
      (%make-interval (make-vector (vector-length x) 0) x)))

(define (interval-dimension iv)
  (vector-length (interval-lb iv)))

(define (interval-lower-bound iv i) (vector-ref (interval-lb iv) i))
(define (interval-upper-bound iv i) (vector-ref (interval-ub iv) i))
(define (interval-lower-bounds->list iv) (vector->list (interval-lb iv)))
(define (interval-upper-bounds->list iv) (vector->list (interval-ub iv)))
(define (interval-lower-bounds->vector iv) (vector-copy (interval-lb iv)))
(define (interval-upper-bounds->vector iv) (vector-copy (interval-ub iv)))

(define (interval= iv1 iv2)
  (assert (and (interval? iv1) (interval? iv2)))
  (and (equal? (interval-lb iv1) (interval-lb iv2))
       (equal? (interval-ub iv1) (interval-ub iv2))))

(define (interval-volume iv)
  (vector-fold (lambda (acc lower upper) (* acc (- upper lower)))
               1
               (interval-lb iv) (interval-ub iv)))

(define (interval-subset? iv1 iv2)
  (assert (and (interval? iv1) (interval? iv2)
               (= (interval-dimension iv1) (interval-dimension iv2))))
  (and (vector-every >= (interval-lb iv1) (interval-lb iv2))
       (vector-every <= (interval-ub iv1) (interval-ub iv2))))

(define (interval-contains-multi-index? iv i0 . o)
  (assert (interval? iv))
  (let ((i (list->vector (cons i0 o))))
    (assert (and (= (interval-dimension iv) (vector-length i))
                 (vector-every integer? i)))
    (and (vector-every >= i (interval-lb iv))
         (vector-every < i (interval-ub iv)))))

(define (interval-projections iv rd)
  (values (make-interval (vector-copy (interval-lb iv) 0 rd)
                         (vector-copy (interval-ub iv) 0 rd))
          (make-interval (vector-copy (interval-lb iv) rd)
                         (vector-copy (interval-ub iv) rd))))

(define (rev-index-next! rev-index rev-lowers rev-uppers)
  (cond
   ((null? rev-index) #f)
   ((< (caar rev-index) (- (car rev-uppers) 1))
    (set-car! (car rev-index) (+ 1 (caar rev-index)))
    #t)
   (else
    (set-car! (car rev-index) (car rev-lowers))
    (rev-index-next! (cdr rev-index) (cdr rev-lowers) (cdr rev-uppers)))))

(define (interval-cursor iv)
  (let* ((rev-lowers (reverse (interval-lower-bounds->list iv)))
         (rev-uppers (reverse (interval-upper-bounds->list iv)))
         (multi-index (interval-lower-bounds->list iv))
         (rev-index (pair-fold cons '() multi-index)))
    (vector multi-index rev-index rev-lowers rev-uppers)))

(define (interval-cursor-get ivc)
  (vector-ref ivc 0))

(define (interval-cursor-next! ivc)
  (and (rev-index-next! (vector-ref ivc 1)
                        (vector-ref ivc 2)
                        (vector-ref ivc 3))
       (vector-ref ivc 0)))

(define (interval-cursor-next ivc)
  (let* ((multi-index (list-copy (vector-ref ivc 0)))
         (ivc (vector multi-index
                      (pair-fold cons '() multi-index)
                      (vector-ref ivc 2)
                      (vector-ref ivc 3))))
    (and (rev-index-next! (vector-ref ivc 1)
                          (vector-ref ivc 2)
                          (vector-ref ivc 3))
         (values ivc (vector-ref ivc 0)))))

(define (interval-fold kons knil iv)
  (case (interval-dimension iv)
    ((1)
     (let ((end (interval-upper-bound iv 0)))
       (do ((i (interval-lower-bound iv 0) (+ i 1))
            (acc knil (kons acc i)))
           ((>= i end) acc))))
    ((2)
     (let ((end0 (interval-upper-bound iv 0))
           (start1 (interval-lower-bound iv 1))
           (end1 (interval-upper-bound iv 1)))
       (do ((i (interval-lower-bound iv 0) (+ i 1))
            (acc knil
                 (do ((j start1 (+ j 1))
                      (acc acc (kons acc i j)))
                     ((>= j end1) acc))))
           ((>= i end0) acc))))
    (else
     (let ((ivc (interval-cursor iv)))
       (let lp ((acc knil))
         (let ((acc (apply kons acc (interval-cursor-get ivc))))
           (if (interval-cursor-next! ivc)
               (lp acc)
               acc)))))))

(define (interval-for-each f iv)
  (interval-fold (lambda (acc . multi-index) (apply f multi-index)) #f iv)
  (if #f #f))

(define (interval-dilate iv lower-diffs upper-diffs)
  (assert (= (interval-dimension iv)
             (vector-length lower-diffs)
             (vector-length upper-diffs)))
  (make-interval (vector-map + (interval-lb iv) lower-diffs)
                 (vector-map + (interval-ub iv) upper-diffs)))

(define (interval-intersect iv0 . o)
  (let ((ls (cons iv0 o)))
    (assert (and (every interval? ls)
                 (or (null? o) (apply = (map interval-dimension ls)))))
    (let ((lower (apply vector-map max (map interval-lb ls)))
          (upper (apply vector-map min (map interval-ub ls))))
      (and (vector-every < lower upper)
           (make-interval lower upper)))))

(define (interval-translate iv translation)
  (assert (translation? translation))
  (interval-dilate iv translation translation))

(define (interval-permute iv perm)
  (assert (and (interval? iv) (permutation? perm)))
  (let* ((len (interval-dimension iv))
         (lower (make-vector len))
         (upper (make-vector len)))
    (assert (= len (vector-length perm)))
    (do ((i 0 (+ i 1)))
        ((>= i len) (make-interval lower upper))
      (vector-set! lower i (interval-lower-bound iv (vector-ref perm i)))
      (vector-set! upper i (interval-upper-bound iv (vector-ref perm i))))))

(define (interval-rotate iv dim)
  (let ((lower (interval-lb iv))
        (upper (interval-ub iv)))
    (make-interval (vector-append (vector-copy lower dim)
                                  (vector-copy lower 0 dim))
                   (vector-append (vector-copy upper dim)
                                  (vector-copy upper 0 dim)))))

(define (interval-scale iv scales)
  (assert (and (interval? iv)
               (vector? scales)
               (= (interval-dimension iv) (vector-length scales))
               (vector-every exact-integer? scales)
               (vector-every positive? scales)))
  (make-interval
   (vector-map (lambda (u s) (exact (ceiling (/ u s))))
               (interval-ub iv)
               scales)))

(define (interval-cartesian-product iv0 . o)
  (make-interval (apply vector-append (map interval-lb (cons iv0 o)))
                 (apply vector-append (map interval-ub (cons iv0 o)))))

;; Storage Classes

(define-record-type Storage-Class
  (make-storage-class getter setter checker maker copier length default)
  storage-class?
  (getter storage-class-getter)
  (setter storage-class-setter)
  (checker storage-class-checker)
  (maker storage-class-maker)
  (copier storage-class-copier)
  (length storage-class-length)
  (default storage-class-default))

(define generic-storage-class
  (make-storage-class
   vector-ref vector-set! (lambda (x) #t) make-vector
   vector-copy! vector-length #f))

;; Parameters

;; Note safety is ignored in this implementation.
(define specialized-array-default-safe?
  (make-parameter #f (lambda (x) (assert (boolean? x)) x)))

(define specialized-array-default-mutable?
  (make-parameter #t (lambda (x) (assert (boolean? x)) x)))

;; Arrays

(define-record-type Array
  (%%make-array domain getter setter storage body coeffs indexer safe? adjacent?)
  array?
  (domain array-domain)
  (getter array-getter)
  (setter array-setter %array-setter-set!)
  (storage array-storage-class)
  (body array-body)
  (coeffs array-coeffs)
  (indexer array-indexer)
  (safe? array-safe?)
  (adjacent? array-adjacent? array-adjacent?-set!))

(define (%make-array domain getter setter storage body coeffs
                     indexer safe? adjacent?)
  (assert (and (interval? domain)
               (procedure? getter)
               (or (not setter) (procedure? setter))
               (or (not storage) (storage-class? storage))))
  (%%make-array
   domain getter setter storage body coeffs indexer safe? adjacent?))

(define (make-array domain getter . o)
  (assert (and (interval? domain) (procedure? getter)))
  (%make-array domain getter (and (pair? o) (car o)) #f #f #f #f #f #f))

(define (array-dimension a)
  (interval-dimension (array-domain a)))

(define (mutable-array? x)
  (and (array? x) (array-setter x) #t))

(define (array-ref array . multi-index)
  (apply (array-getter array) multi-index))

(define (array-set! array val . multi-index)
  (apply (array-setter array) val multi-index))

(define (specialized-getter body indexer getter)
  (lambda multi-index
    (getter body (apply indexer multi-index))))

(define (specialized-setter body indexer setter)
  (lambda (val . multi-index)
    (setter body (apply indexer multi-index) val)))


;; Indexing

(define (indexer->coeffs indexer domain . o)
  (let* ((verify? (and (pair? o) (car o)))
         (res (make-vector (+ 1 (interval-dimension domain)) 0))
         (multi-index (interval-lower-bounds->list domain))
         (base (apply indexer multi-index)))
    (vector-set! res 0 base)
    (let lp ((i 1)
             (ls multi-index)
             (offset base)
             (count 0))
      (cond
       ((null? ls)
        (if (and verify? (zero? count))
            (lp 1 multi-index offset (+ count 1))
            res))
       ((= (+ 1 (interval-lower-bound domain (- i 1)))
           (interval-upper-bound domain (- i 1)))
        (lp (+ i 1) (cdr ls) offset count))
       (else
        (let ((dir (if (and (> count 0)
                            (= (+ (car ls) 1)
                               (interval-upper-bound domain (- i 1))))
                       -1
                       1)))
          (set-car! ls (+ (car ls) dir))
          (let* ((offset2 (apply indexer multi-index))
                 (coeff (* dir (- offset2 offset))))
            (cond
             ((> count 0)
              (and (= coeff (vector-ref res i))
                   (lp (+ i 1) (cdr ls) offset2 count)))
             (else
              (vector-set! res i coeff)
              (vector-set! res 0 (- (vector-ref res 0)
                                    (* coeff
                                       (interval-lower-bound domain (- i 1)))))
              (lp (+ i 1) (cdr ls) offset2 count))))))))))

(define (coeffs->indexer coeffs domain)
  (case (vector-length coeffs)
    ((2)
     (let ((a (vector-ref coeffs 0))
           (b (vector-ref coeffs 1)))
       (lambda (x) (+ a (* b x)))))
    ((3)
     (let ((a (vector-ref coeffs 0))
           (b (vector-ref coeffs 1))
           (c (vector-ref coeffs 2)))
       (lambda (x y) (+ a (* b x) (* c y)))))
    ((4)
     (let ((a (vector-ref coeffs 0))
           (b (vector-ref coeffs 1))
           (c (vector-ref coeffs 2))
           (d (vector-ref coeffs 3)))
       (lambda (x y z) (+ a (* b x) (* c y) (* d z)))))
    (else
     (lambda multi-index
       (let ((lim (vector-length coeffs)))
         (let lp ((ls multi-index)
                  (i 1)
                  (res (vector-ref coeffs 0)))
           (cond
            ((null? ls)
             (if (< i lim)
                 (error "multi-index too short for domain" multi-index domain)
                 res))
            ((>= i lim)
             (error "multi-index too long for domain" multi-index domain))
            (else
             (lp (cdr ls)
                 (+ i 1)
                 (+ res (* (car ls) (vector-ref coeffs i))))))))))))

(define (default-coeffs domain)
  (let* ((dim (interval-dimension domain))
         (res (make-vector (+ 1 dim))))
    (vector-set! res 0 0)
    (vector-set! res dim 1)
    (let lp ((i (- dim 1))
             (scale 1))
      (cond
       ((< i 0)
        res)
       ((= (+ 1 (interval-lower-bound domain i))
           (interval-upper-bound domain i))
        (vector-set! res (+ i 1) 0)
        (lp (- i 1) scale))
       (else
        (let ((coeff (* scale  (- (interval-upper-bound domain i)
                                  (interval-lower-bound domain i)))))
          (vector-set! res (+ i 1) scale)
          (vector-set! res 0 (- (vector-ref res 0)
                                (* scale (interval-lower-bound domain i))))
          (lp (- i 1) coeff)))))))

(define (default-indexer domain)
  (coeffs->indexer (default-coeffs domain) domain))

;; Converts the raw integer index to the multi-index in domain that
;; would map to it using the default indexer (i.e. iterating over the
;; possible multi-indices in domain in lexicographic order would
;; produce 0 through volume-1).
(define (invert-default-index domain raw-index)
  (let lp ((index raw-index)
           (i 0)
           (scale (/ (interval-volume domain)
                     (max 1
                          (- (interval-upper-bound domain 0)
                             (interval-lower-bound domain 0)))))
           (res '()))
    (cond
     ((>= (+ i 1) (interval-dimension domain))
      (reverse (cons (+ index (interval-lower-bound domain i)) res)))
     (else
      (let ((digit (quotient index scale)))
        (lp (- index (* digit scale))
            (+ i 1)
            (/ scale
               (max 1
                    (- (interval-upper-bound domain (+ i 1))
                       (interval-lower-bound domain (+ i 1)))))
            (cons (+ digit
                     (interval-lower-bound domain i))
                  res)))))))

;; Specialized arrays

(define (%make-specialized domain storage body coeffs indexer
                           safe? mutable? adjacent?)
  (%make-array
   domain
   (specialized-getter body indexer (storage-class-getter storage))
   (and mutable?
        (specialized-setter body indexer (storage-class-setter storage)))
   storage
   body
   coeffs
   indexer
   safe?
   adjacent?))

(define (make-specialized-array domain . o)
  (let* ((storage (if (pair? o) (car o) generic-storage-class))
         (safe? (if (and (pair? o) (pair? (cdr o)))
                    (cadr o)
                    (specialized-array-default-safe?)))
         (body ((storage-class-maker storage)
                (interval-volume domain)
                (storage-class-default storage)))
         (coeffs (default-coeffs domain))
         (indexer (coeffs->indexer coeffs domain)))
    (assert (boolean? safe?))
    (%make-specialized domain storage body coeffs indexer safe? #t #t)))

(define (specialized-array? x)
  (and (array? x) (array-storage-class x) #t))

(define (compute-array-elements-in-order? array)
  (let ((indexer (array-indexer array)))
    (call-with-current-continuation
     (lambda (return)
       (interval-fold
        (lambda (prev . multi-index)
          (let ((i (apply indexer multi-index)))
            (if (and prev (not (= i (+ prev 1))))
                (return #f)
                i)))
        #f
        (array-domain array))
       #t))))

(define (array-elements-in-order? array)
  (assert (specialized-array? array))
  (let ((res (array-adjacent? array)))
    (when (eq? res 'unknown)
      (set! res (compute-array-elements-in-order? array))
      (array-adjacent?-set! array res))
    res))

(define (specialized-array-share array new-domain project)
  (assert (and (specialized-array? array) (interval? new-domain)))
  (let* ((body (array-body array))
         (coeffs
          (indexer->coeffs
           (lambda multi-index
             (call-with-values
                 (lambda () (apply project multi-index))
               (array-indexer array)))
           new-domain))
         (indexer
          (coeffs->indexer coeffs new-domain))
         (storage (array-storage-class array)))
    (%make-specialized new-domain storage body coeffs indexer
                       (array-safe? array) (array-setter array) 'unknown)))
