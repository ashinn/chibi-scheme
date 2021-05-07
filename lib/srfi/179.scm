
;; Homogeneous storage classes

;; Define a storage class with an optimized -copy!
(define-syntax define-storage-class
  (syntax-rules ()
    ((define-storage-class name ref set elt? make len default)
     (define name
       (make-storage-class
        ref set elt? make
        (lambda (to at from start end)
          (let ((limit (min end (+ start (- (len to) at)))))
            (if (<= at start)
                (do ((i at (+ i 1)) (j start (+ j 1)))
                    ((>= j limit))
                  (set to i (ref from j)))
                (do ((i (+ at (- end start 1)) (- i 1)) (j (- limit 1) (- j 1)))
                    ((< j start))
                  (set to i (ref from j))))))
        len default)))))

(define-storage-class s8-storage-class
  s8vector-ref s8vector-set! s8? make-s8vector s8vector-length 0)

(define-storage-class s16-storage-class
  s16vector-ref s16vector-set! s16? make-s16vector s16vector-length 0)

(define-storage-class s32-storage-class
  s32vector-ref s32vector-set! s32? make-s32vector s32vector-length 0)

(define-storage-class s64-storage-class
  s64vector-ref s64vector-set! s64? make-s64vector s64vector-length 0)

(define-storage-class u1-storage-class
  u1vector-ref u1vector-set! u1? make-u1vector u1vector-length 0)

(define-storage-class u8-storage-class
  u8vector-ref u8vector-set! u8? make-u8vector u8vector-length 0)

(define-storage-class u16-storage-class
  u16vector-ref u16vector-set! u16? make-u16vector u16vector-length 0)

(define-storage-class u32-storage-class
  u32vector-ref u32vector-set! u32? make-u32vector u32vector-length 0)

(define-storage-class u64-storage-class
  u64vector-ref u64vector-set! u64? make-u64vector u64vector-length 0)

(define-storage-class f32-storage-class
  f32vector-ref f32vector-set! f32? make-f32vector f32vector-length 0)

(define-storage-class f64-storage-class
  f64vector-ref f64vector-set! f64? make-f64vector f64vector-length 0)

(define-storage-class c64-storage-class
  c64vector-ref c64vector-set! c64? make-c64vector c64vector-length 0)

(define-storage-class c128-storage-class
  c128vector-ref c128vector-set! c128? make-c128vector c128vector-length 0)

;; TODO: implement
(define f8-storage-class #f)
(define f16-storage-class #f)

;; Array transformations

(define (array-copy array . o)
  (assert (array? array))
  (let* ((storage (if (pair? o) (car o) generic-storage-class))
         (o (if (pair? o) (cdr o) '()))
         (new-domain (or (and (pair? o) (car o)) (array-domain array)))
         (o (if (pair? o) (cdr o) '()))
         (mutable? (if (pair? o) (car o) (specialized-array-default-mutable?)))
         (o (if (pair? o) (cdr o) '()))
         (safe? (if (pair? o) (car o) (specialized-array-default-safe?))))
    (assert (and (storage-class? storage) (interval? new-domain)
                 (boolean? mutable?) (boolean? safe?)))
    (let* ((body ((storage-class-maker storage)
                    (interval-volume new-domain)
                    (storage-class-default storage)))
           (coeffs (default-coeffs new-domain))
           (indexer (coeffs->indexer coeffs new-domain))
           (getter (specialized-getter body indexer
                                       (storage-class-getter storage)))
           (setter (specialized-setter body indexer
                                       (storage-class-setter storage)))
           (res (%make-specialized new-domain storage body coeffs indexer
                                   safe? #t #t)))
      (array-assign! res array)
      (unless mutable?
        (%array-setter-set! res #f))
      res)))

(define (array-curry array inner-dimension)
  (call-with-values
      (lambda () (interval-projections (array-domain array) inner-dimension))
    (lambda (outer-domain inner-domain)
      (cond
       ((specialized-array? array)
        (make-array
         outer-domain
         (lambda outer-index
           (specialized-array-share
            array
            inner-domain
            (lambda inner-index
              (apply values (append outer-index inner-index)))))))
       (else
        (make-array
         outer-domain
         (lambda outer-index
           (make-array
            inner-domain
            (lambda inner-index
              (apply array-ref array (append outer-index inner-index)))
            (and
             (mutable-array? array)
             (lambda (val . inner-index)
               (apply array-set! array val (append outer-index inner-index))
               ))))))))))

(define (array-extract array new-domain)
  (assert (and (array? array)
               (interval? new-domain)
               (interval-subset? new-domain (array-domain array))))
  (if (specialized-array? array)
      (specialized-array-share array new-domain values)
      (make-array new-domain (array-getter array) (array-setter array))))

(define (array-tile array sizes)
  (assert (and (array? array)
               (vector? sizes)
               (= (array-dimension array) (vector-length sizes))
               (vector-every exact-integer? sizes)
               (vector-every >= sizes (interval-lower-bounds->vector
                                       (array-domain array)))
               (vector-every < sizes (interval-upper-bounds->vector
                                      (array-domain array)))))
  (let ((domain (make-interval
                 (vector-map
                  (lambda (lo hi s) (exact (ceiling (/ (- hi lo) s))))
                  (interval-lower-bounds->vector (array-domain array))
                  (interval-upper-bounds->vector (array-domain array))
                  sizes))))
    (make-array
     domain
     (lambda multi-index
       (array-extract
        array
        (make-interval
         (vector-map
          (lambda (i lo s) (+ lo (* i s)))
          multi-index
          (interval-lower-bound (array-domain array))
          sizes)
         (vector-map
          (lambda (i lo hi s)
            (min hi (+ lo (* (+ i 1) s))))
          multi-index
          (interval-lb (array-domain array))
          (interval-ub (array-domain array))
          sizes)))))))

(define (array-translate array translation)
  (let ((new-domain (interval-translate (array-domain array) translation))
        (translation-ls (vector->list translation)))
    (if (specialized-array? array)
        (specialized-array-share
         array
         new-domain
         (lambda multi-index
           (apply values (map - multi-index translation-ls))))
        (make-array
         new-domain
         (lambda multi-index
           (apply array-ref array (map - multi-index translation-ls)))
         (and (mutable-array? array)
              (lambda (val . multi-index)
                (apply array-set! array val
                       (map - multi-index translation-ls))))))))

(define (permute ls permutation)
  (let ((vec (list->vector ls))
        (len (vector-length permutation)))
    (do ((i (- len 1) (- i 1))
         (res '() (cons (vector-ref vec (vector-ref permutation i)) res)))
        ((< i 0) res))))

(define (inverse-permutation permutation)
  (list->vector
   (map car
        (list-sort (lambda (a b) (< (cdr a) (cdr b)))
                   (map cons
                        (iota (vector-length permutation))
                        (vector->list permutation))))))

(define (array-permute array permutation)
  (assert (permutation? permutation))
  (let ((new-domain (interval-permute (array-domain array) permutation))
        (perm^-1 (inverse-permutation permutation)))
    (if (specialized-array? array)
        (specialized-array-share
         array
         new-domain
         (lambda multi-index
           (let ((perm-index (permute multi-index perm^-1)))
             (apply values perm-index))))
        (make-array
         new-domain
         (lambda multi-index
           (let ((perm-index (permute multi-index perm^-1)))
             (apply array-ref array perm-index)))
         (and (mutable-array? array)
              (lambda (val . multi-index)
                (apply array-set! array val (permute multi-index perm^-1))))))))

(define (array-rotate array dim)
  (let ((left (iota (- (array-dimension array) dim) dim))
        (right (iota dim)))
    (array-permute array (list->vector (append left right)))))

(define (array-reverse array . o)
  (assert (array? array))
  (let ((flip? (if (pair? o) (car o) (make-vector (array-dimension array) #t))))
    (assert (and (vector? flip?)
                 (= (array-dimension array) (vector-length flip?))
                 (vector-every boolean? flip?)))
    (let* ((flips (vector->list flip?))
           (domain (array-domain array))
           (lowers (interval-lower-bounds->list domain))
           (uppers (interval-upper-bounds->list domain))
           (flip-multi-index
            (lambda (multi-index)
              (map (lambda (i flip-i? lo hi)
                     (if flip-i? (- (+ lo hi -1) i) i))
                   multi-index
                   flips
                   lowers
                   uppers))))
      (if (specialized-array? array)
          (specialized-array-share array
                                   domain
                                   (lambda multi-index
                                     (apply values
                                            (flip-multi-index multi-index))))
          (make-array
           domain
           (lambda multi-index
             (apply array-ref array (flip-multi-index multi-index)))
           (and
            (mutable-array? array)
            (lambda (val . multi-index)
              (apply array-set! array val (flip-multi-index multi-index))
              )))))))

(define (array-sample array scales)
  (unless (vector-every zero?
                        (interval-lower-bounds->vector (array-domain array)))
    (error "can only sample an array with zero lower bounds" array))
  (let ((scales-ls (vector->list scales))
        (new-domain (interval-scale (array-domain array) scales)))
    (if (specialized-array? array)
        (specialized-array-share
         array
         new-domain
         (lambda multi-index
           (apply values (map * multi-index scales-ls))))
        (make-array
         new-domain
         (lambda multi-index
           (apply array-ref array (map * multi-index scales-ls)))
         (and
          (mutable-array? array)
          (lambda (val . multi-index)
            (apply array-set! array val (map * multi-index scales-ls))))))))

(define (array-outer-product op array1 array2)
  (assert (and (procedure? op) (array? array1) (array? array2)))
  (make-array (interval-cartesian-product (array-domain array1)
                                          (array-domain array2))
              (let ((getter1 (array-getter array1))
                    (getter2 (array-getter array2))
                    (dim1 (array-dimension array1)))
                (lambda multi-index
                  (op (apply getter1 (take multi-index dim1))
                      (apply getter2 (drop multi-index dim1)))))))

(define (same-dimensions? ls)
  (or (null? ls)
      (null? (cdr ls))
      (and (equal? (array-dimension (car ls)) (array-dimension (cadr ls)))
           (same-dimensions? (cdr ls)))))

(define (array-map f array . arrays)
  (make-array (array-domain array)
              (let* ((ls (cons array arrays))
                     (getters (map array-getter ls)))
                (assert (same-dimensions? ls))
                (lambda multi-index
                  (apply f (map (lambda (g) (apply g multi-index)) getters))))))

(define (array-for-each f array . arrays)
  (interval-for-each
   (let* ((ls (cons array arrays))
          (getters (map array-getter ls)))
     (assert (same-dimensions? ls))
     (lambda multi-index
       (apply f (map (lambda (g) (apply g multi-index)) getters))))
   (array-domain array)))

(define (array-fold kons knil array)
  (interval-fold (lambda (acc . multi-index)
                   (kons (apply array-ref array multi-index) acc))
                 knil
                 (array-domain array)))

(define (array-fold-right kons knil array)
  (fold-right kons knil (array->list array)))

(define (array-reduce op array)
  (let* ((domain (array-domain array))
         (init-index (interval-lower-bounds->list domain))
         (knil (list 'first-element)))
    (interval-fold
     (lambda (acc . multi-index)
       (if (eq? acc knil)
           (apply array-ref array multi-index)
           (op acc (apply array-ref array multi-index))))
     knil
     domain)))

(define (array-any pred array . arrays)
  (assert (same-dimensions? (cons array arrays)))
  (call-with-current-continuation
   (lambda (return)
     (apply array-for-each
            (lambda args (cond ((apply pred args) => return)))
            #f
            array
            arrays)
     #f)))

(define (array-every pred array . arrays)
  (assert (same-dimensions? (cons array arrays)))
  (call-with-current-continuation
   (lambda (return)
     (interval-fold
      (let ((getters (map array-getter (cons array arrays))))
        (lambda (acc . multi-index)
          (or (apply pred (map (lambda (g) (apply g multi-index)) getters))
              (return #f))))
      #t
      (array-domain array)))))

(define (array->list array)
  (reverse (array-fold cons '() array)))

(define (list->array ls domain . o)
  (let* ((storage (if (pair? o) (car o) generic-storage-class))
         (mutable? (if (and (pair? o) (pair? (cdr o)))
                       (cadr o)
                       (specialized-array-default-mutable?)))
         (safe? (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                    (car (cddr o))
                    (specialized-array-default-safe?)))
         (res (make-specialized-array domain storage safe?)))
    (assert (and (interval? domain) (storage-class? storage)
                 (boolean? mutable?) (boolean? safe?)))
    (interval-fold
     (lambda (ls . multi-index)
       (apply array-set! res (car ls) multi-index)
       (cdr ls))
     ls
     domain)
    res))

(define (array-assign! destination source)
  (assert (and (mutable-array? destination) (array? source)))
  (let ((getter (array-getter source))
        (setter (array-setter destination)))
    (cond
     ((interval= (array-domain destination) (array-domain source))
      (interval-for-each
       (case (array-dimension destination)
         ((1) (lambda (i) (setter (getter i) i)))
         ((2) (lambda (i j) (setter (getter i j) i j)))
         ((3) (lambda (i j k) (setter (getter i j k) i j k)))
         (else
          (lambda multi-index
            (apply setter (apply getter multi-index) multi-index))))
       (array-domain source)))
     (else
      (assert (and (array-elements-in-order? destination)
                   (equal? (interval-volume (array-domain destination))
                           (interval-volume (array-domain source)))))
      (let* ((ivc (interval-cursor (array-domain destination)))
             (dst-index (interval-cursor-get ivc)))
        (interval-for-each
         (lambda multi-index
           (apply setter (apply getter multi-index) dst-index)
           (interval-cursor-next! ivc))
         (array-domain source)))))
    destination))

(define (reshape-without-copy array new-domain)
  (let* ((domain (array-domain array))
         (orig-indexer (array-indexer array))
         (tmp-indexer (default-indexer new-domain))
         (new-indexer
          (lambda multi-index
            (apply orig-indexer
                   (invert-default-index domain
                                         (apply tmp-indexer multi-index)))))
         (new-coeffs (indexer->coeffs new-indexer new-domain #t))
         (flat-indexer (coeffs->indexer new-coeffs new-domain))
         (new-indexer (coeffs->indexer new-coeffs new-domain))
         (body (array-body array))
         (storage (array-storage-class array))
         (res
          (%make-specialized new-domain storage body new-coeffs flat-indexer
                             (array-safe? array) (array-setter array)
                             (array-adjacent? array))))
    (let ((multi-index (interval-lower-bounds->list domain))
          (orig-default-indexer (default-indexer domain)))
      (let lp ((i 0)
               (ls multi-index))
        (let ((reshaped-index
               (invert-default-index
                new-domain
                (apply orig-default-indexer multi-index))))
          (cond
           ((not (equal? (apply flat-indexer reshaped-index)
                         (apply orig-indexer multi-index)))
            #f)
           ((null? ls)
            res)
           ((= (+ 1 (interval-lower-bound domain i))
               (interval-upper-bound domain i))
            (lp (+ i 1) (cdr ls)))
           (else
            (set-car! ls (+ 1 (car ls)))
            (lp (+ i 1) (cdr ls)))))))))

(define (specialized-array-reshape array new-domain . o)
  (assert (and (specialized-array? array)
               (= (interval-volume (array-domain array))
                  (interval-volume new-domain))))
  (let ((copy-on-failure? (and (pair? o) (car o))))
    (cond
     ((reshape-without-copy array new-domain))
     (copy-on-failure?
      (let ((res (make-specialized-array
                  new-domain
                  (array-storage-class array)
                  (array-safe? array))))
        (array-assign! res array)
        res))
     (else
      (error "can't reshape" array new-domain)))))
