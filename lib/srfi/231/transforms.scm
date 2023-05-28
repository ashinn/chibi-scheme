
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
        len default (lambda (data) #t) (lambda (data) data))))))

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

(define-storage-class char-storage-class
  (lambda (vec i) (integer->char (u32vector-ref vec i)))
  (lambda (vec i ch) (u32vector-set! vec i (char->integer ch)))
  char? make-u32vector u32vector-length 0)

;; TODO: implement
(define f8-storage-class #f)
(define f16-storage-class #f)

;; Array transformations

(define (array-copy array . o)
  (assert (array? array))
  (let ((specialized? (specialized-array? array))
        (domain (array-domain array)))
    (let* ((storage (cond ((pair? o) (car o))
                          (specialized? (array-storage-class array))
                          (else  generic-storage-class)))
           (o (if (pair? o) (cdr o) '()))
           (mutable? (cond ((pair? o) (car o))
                           (specialized? (and (array-setter array) #t))
                           (else (specialized-array-default-mutable?))))
           (o (if (pair? o) (cdr o) '()))
           (safe? (cond ((pair? o) (car o))
                        (specialized? (array-safe? array))
                        (else (specialized-array-default-safe?)))))
      (assert
       (and (storage-class? storage) (boolean? mutable?) (boolean? safe?)))
      (let* ((body ((storage-class-maker storage)
                    (interval-volume domain)
                    (storage-class-default storage)))
             (coeffs (default-coeffs domain))
             (indexer (coeffs->indexer coeffs domain))
             (getter (specialized-getter body indexer
                                         (storage-class-getter storage)))
             (setter (specialized-setter body indexer
                                         (storage-class-setter storage)))
             (res (%make-specialized domain storage body coeffs indexer
                                     safe? #t #t)))
        (array-assign! res array)
        (unless mutable?
          (%array-setter-set! res #f))
        res))))

(define array-copy! array-copy)

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
               (vector-every <= sizes (interval-ub (array-domain array)))))
  (let ((domain (make-interval
                 (vector-map
                  (lambda (lo hi s) (exact (ceiling (/ (- hi lo) s))))
                  (interval-lb (array-domain array))
                  (interval-ub (array-domain array))
                  sizes))))
    (make-array
     domain
     (lambda multi-index
       (array-extract
        array
        (make-interval
         (vector-map
          (lambda (i lo s) (+ lo (* i s)))
          (list->vector multi-index)
          (interval-lb (array-domain array))
          sizes)
         (vector-map
          (lambda (i lo hi s)
            (min hi (+ lo (* (+ i 1) s))))
          (list->vector multi-index)
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

(define (array-inner-product A f g B)
  (array-outer-product
   (lambda (a b) (array-reduce f (array-map g a b)))
   (array-copy (array-curry A 1))
   (array-copy
    (array-curry (array-permute B (index-rotate (array-dimension B) 1))))))

(define (same-dimensions? ls)
  (or (null? ls)
      (null? (cdr ls))
      (and (equal? (array-dimension (car ls)) (array-dimension (cadr ls)))
           (same-dimensions? (cdr ls)))))

(define (same-domains? ls)
  (or (null? ls)
      (null? (cdr ls))
      (and (interval= (array-domain (car ls)) (array-domain (cadr ls)))
           (same-domains? (cdr ls)))))

(define (array-map f array . arrays)
  (make-array (array-domain array)
              (let* ((ls (cons array arrays))
                     (getters (map array-getter ls)))
                (assert (same-dimensions? ls))
                (lambda multi-index
                  (apply f (map (lambda (g) (apply g multi-index)) getters))))))

(define (array-for-each f array . arrays)
  (if (null? arrays)
      (interval-for-each
       (let ((g (array-getter array)))
         (case (array-dimension array)
           ((1)
            (lambda (i) (f (g i))))
           ((2)
            (lambda (i j) (f (g i j))))
           (else
            (lambda multi-index
              (f (apply g multi-index))))))
       (array-domain array))
      (interval-for-each
       (let* ((lower (interval-lower-bounds->list (array-domain array)))
              (ls (cons array arrays))
              (getters
               (cons (array-getter (car ls))
                     (map (lambda (ar)
                            (let ((getter (array-getter ar)))
                              (lambda multi-index
                                (apply getter multi-index))))
                          (cdr ls)))))
         (assert (same-domains? ls))
         (lambda multi-index
           (apply f (map (lambda (g) (apply g multi-index)) getters))))
       (array-domain array))))

(define (array-foldl kons knil array)
  (interval-fold (lambda (acc . multi-index)
                   (kons (apply array-ref array multi-index) acc))
                 knil
                 (array-domain array)))

(define (array-foldr kons knil array)
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
  (reverse (array-foldl cons '() array)))

(define (list->array domain ls . o)
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

(define (array->vector array)
  (list->vector (array->list array)))

(define (vector->array domain vec . o)
  (apply list->array domain (vector->list vec o)))

(define (array-assign! destination source)
  (assert (and (mutable-array? destination) (array? source)
               (interval= (array-domain destination) (array-domain source))))
  (let ((getter (array-getter source))
        (setter (array-setter destination)))
    (interval-for-each
     (case (array-dimension destination)
       ((1) (lambda (i) (setter (getter i) i)))
       ((2) (lambda (i j) (setter (getter i j) i j)))
       ((3) (lambda (i j k) (setter (getter i j k) i j k)))
       (else
        (lambda multi-index
          (apply setter (apply getter multi-index) multi-index))))
     (array-domain source))
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

(define (flatten ls)
  (if (pair? (car ls))
      (append-map flatten ls)
      ls))

(define (list*->array dimension nested-ls . o)
  (let lp ((ls nested-ls) (lens '()) (d dimension))
    (cond
     ((positive? d)
      (lp (car ls) (cons (length ls) lens) (- d 1)))
     (else
      (apply list->array
             (make-interval (list->vector (reverse lens)))
             (flatten nested-ls)
             o)))))

(define (array->list* a)
  (case (array-dimension a)
    ((0) (array-ref a))
    ((1)
     (let ((domain (array-domain a)))
       (map (lambda (i) (array-ref a i))
            (iota (interval-width domain 0)
                  (interval-lower-bound domain 0)))))
    (else
     (let ((domain (array-domain a))
           (b (array-curry a 1)))
       (map (lambda (i) (array->list* (array-ref b i)))
            (iota (interval-width domain 0)
                  (interval-lower-bound domain 0)))))))

(define (array->vector* a)
  (case (array-dimension a)
    ((0) (array-ref a))
    ((1)
     (let ((domain (array-domain a)))
       (vector-map (lambda (i) (array-ref a i))
                   (vector-iota (interval-width domain 0)
                                (interval-lower-bound domain 0)))))
    (else
     (let ((domain (array-domain a))
           (b (array-curry a 1)))
       (vector-map (lambda (i) (array->vector* (array-ref b i)))
                   (vector-iota (interval-width domain 0)
                                (interval-lower-bound domain 0)))))))

(define (flatten-vec vec)
  (if (vector? (vector-ref vec 0))
      (append-map flatten-vec vec)
      (vector->list vec)))

(define (vector*->array dimension nested-vec . o)
  (let lp ((vec nested-vec) (lens '()) (d dimension))
    (cond
     ((positive? d)
      (lp (vector-ref vec 0) (cons (vector-length vec) lens) (- d 1)))
     (else
      (apply list->array
             (make-interval (list->vector (reverse lens)))
             (flatten-vec nested-vec)
             o)))))

(define (dimensions-compatible? a-domain b-domain axis)
  (and (= (interval-dimension a-domain) (interval-dimension b-domain))
       (let lp ((d (- (interval-dimension a-domain) 1)))
         (or (negative? d)
             (and (or (= d axis)
                      (= (- (interval-upper-bound a-domain d)
                            (interval-lower-bound a-domain d))
                         (- (interval-upper-bound b-domain d)
                            (interval-lower-bound b-domain d))))
                  (lp (- d 1)))))))

(define (array-append axis arrays . o)
  (assert (and (exact-integer? axis)
               (pair? arrays)
               (every array? arrays)
               (< -1 axis (array-dimension (car arrays)))))
  (let* ((a (car arrays))
         (a-domain (array-domain a))
         (storage (if (pair? o) (car o) generic-storage-class))
         (mutable? (if (and (pair? o) (pair? (cdr o)))
                       (cadr o)
                       (specialized-array-default-mutable?)))
         (safe? (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                    (car (cddr o))
                    (specialized-array-default-safe?))))
    (assert (every (lambda (b)
                     (dimensions-compatible? a-domain (array-domain b) axis))
                   (cdr arrays)))
    (let* ((a-lo (interval-lower-bounds->vector a-domain))
           (c-lo (make-vector (interval-dimension a-domain) 0))
           (c-hi (interval-widths a-domain)))
      (vector-set! c-hi
                   axis
                   (fold (lambda (b sum)
                           (+ sum (interval-width (array-domain b) axis)))
                         (vector-ref c-hi axis)
                         (cdr arrays)))
      (let* ((c-domain (make-interval c-lo c-hi))
             (c (make-specialized-array c-domain storage mutable? safe?))
             (b-trans (make-vector (array-dimension a) 0)))
        (array-assign!
         (array-extract c (make-interval c-lo (interval-widths a-domain)))
         (array-translate a (vector-map - a-lo)))
        (let lp ((arrays (cdr arrays))
                 (b-offset (- (interval-upper-bound a-domain axis)
                              (interval-lower-bound a-domain axis))))
          (if (null? arrays)
              c
              (let* ((b (car arrays))
                     (b-domain (array-domain b))
                     (b-offset2 (+ b-offset (interval-width b-domain axis)))
                     (b-lo (make-vector (interval-dimension b-domain) 0))
                     (b-hi (interval-widths b-domain)))
                (vector-set! b-lo axis b-offset)
                (vector-set! b-hi axis b-offset2)
                (vector-set! b-trans axis (- b-offset))
                (let ((view (array-translate
                             (array-extract c (make-interval b-lo b-hi))
                             b-trans)))
                  (array-assign! view b)
                  (lp (cdr arrays) b-offset2)))))))))

(define array-append! array-append)

(define (array-stack axis arrays . o)
  (assert (and (exact-integer? axis)
               (pair? arrays)
               (every array? arrays)
               (<= 0 axis (array-dimension (car arrays)))))
  (let ((a (car arrays))
        (storage (if (pair? o) (car o) generic-storage-class))
        (mutable? (if (and (pair? o) (pair? (cdr o)))
                      (cadr o)
                      (specialized-array-default-mutable?)))
        (safe? (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                   (car (cddr o))
                   (specialized-array-default-safe?))))
    (assert (every (lambda (b)
                     (interval= (array-domain a)
                                (array-domain b)))
                   (cdr arrays)))
    (let* ((a-lbs (interval-lower-bounds->list (array-domain a)))
           (a-ubs (interval-upper-bounds->list (array-domain a)))
           (domain
            (make-interval
             `#(,@(take a-lbs axis) 0 ,@(drop a-lbs axis))
             `#(,@(take a-ubs axis) ,(length arrays) ,@(drop a-ubs axis))))
           (res (make-specialized-array domain
                                        (or (array-storage-class a)
                                            generic-storage-class)))
           (perm `#(,axis ,@(delete axis (iota (+ 1 (array-dimension a))))))
           (permed (if (zero? axis) res (array-permute res perm)))
           (curried (array-curry permed 1))
           (get-view (array-getter curried)))
      (let lp ((ls arrays) (i 0))
        (cond
         ((null? ls) res)
         (else
          (array-assign! (get-view i) (car ls))
          (lp (cdr ls) (+ i 1))))))))

(define array-stack! array-stack)

(define (array-block a . o)
  (let ((storage (if (pair? o) (car o) generic-storage-class))
        (mutable? (if (and (pair? o) (pair? (cdr o)))
                      (cadr o)
                      (specialized-array-default-mutable?)))
        (safe? (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                   (car (cddr o))
                   (specialized-array-default-safe?))))
    (assert (and (array? a) (not (interval-empty? (array-domain a)))))
    (let* ((a-domain (array-domain a))
           (get (array-getter a))
           (tile0 (apply get (interval-lower-bounds->list a-domain))))
      (assert (array? tile0))
      (let* ((domain (make-interval
                      (vector-append (interval-widths a-domain)
                                     (interval-widths (array-domain tile0)))))
             (scales (vector->list (interval-widths a-domain)))
             (res (make-specialized-array domain storage mutable? safe?)))
        (error "TODO: array-block copy data unimplemented")
        res))))

(define array-block! array-block)

(define (array-decurry a . o)
  (let* ((storage (if (pair? o) (car o) generic-storage-class))
         (mutable? (if (and (pair? o) (pair? (cdr o)))
                       (cadr o)
                       (specialized-array-default-mutable?)))
         (safe? (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                    (car (cddr o))
                    (specialized-array-default-safe?)))
         (a-domain (array-domain a))
         (elt0 (apply array-ref a (interval-lower-bounds->list a-domain)))
         (elt-domain (array-domain elt0))
         (domain (interval-cartesian-product a-domain elt-domain))
         (res (make-specialized-array domain storage mutable? safe?))
         (curried-res (array-curry res (interval-dimension elt-domain))))
    ;; Prepare a res with the flattened domain, create a new curried
    ;; view of the res with the same domain as a, and assign each
    ;; curried view from a to the res.
    (array-for-each array-assign! curried-res a)
    res))

(define array-decurry! array-decurry)
