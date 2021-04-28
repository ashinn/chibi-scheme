;; Miscellaneous Functions

(define (translation? x)
  (and (vector? x) (vector-every exact-integer? x)))

(define (permutation? x)
  (and (translation? x)
       (let* ((len (vector-length x))
              (seen (make-u1vector len)))
         (let lp ((i 0))
           (or (>= i len)
               (and (< -1 (vector-ref x i) len)
                    (zero? (u1vector-ref seen (vector-ref x i)))
                    (begin
                      (u1vector-set! seen (vector-ref x i) 1)
                      (lp (+ i 1)))))))))

(define (all-equal? ls)
  (or (null? ls)
      (null? (cdr ls))
      (and (equal? (car ls) (cadr ls))
           (all-equal? (cdr ls)))))

;; Intervals

(define-record-type Interval
  (%%make-interval lb ub)
  interval?
  (lb interval-lb)
  (ub interval-ub))

(define (%make-interval lo hi)
  (assert (translation? lo)
          (translation? hi)
          (not (vector-empty? lo))
          (not (vector-empty? hi))
          (= (vector-length lo) (vector-length hi))
          (vector-every < lo hi))
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
  (assert (interval? iv1) (interval? iv2))
  (equal? iv1 iv2))

(define (interval-volume iv)
  (vector-fold (lambda (acc lower upper) (* acc (- upper lower)))
               1
               (interval-lb iv) (interval-ub iv)))

(define (interval-subset? iv1 iv2)
  (assert (interval? iv1) (interval? iv2)
          (= (interval-dimension iv1) (interval-dimension iv2)))
  (and (vector-every >= (interval-lb iv1) (interval-lb iv2))
       (vector-every <= (interval-ub iv1) (interval-ub iv2))))

(define (interval-contains-multi-index? iv i0 . o)
  (assert (interval? iv))
  (let ((i (list->vector (cons i0 o))))
    (assert (= (interval-dimension iv) (vector-length i))
            (vector-every integer? i))
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

(define (interval-fold kons knil iv . o)
  (case (interval-dimension iv)
    ((1)
     (let ((end (interval-upper-bound iv 0)))
       (do ((i (if (pair? o) (car o) (interval-lower-bound iv 0))
               (+ i 1))
            (acc knil (kons acc i)))
           ((>= i end) acc))))
    ((2)
     (let ((end0 (interval-upper-bound iv 0))
           (start1 (if (pair? o) (cadr o) (interval-lower-bound iv 1)))
           (end1 (interval-upper-bound iv 1)))
       (do ((i (if (pair? o) (car o) (interval-lower-bound iv 0))
               (+ i 1))
            (acc knil
                 (do ((j start1 (+ j 1))
                      (acc acc (kons acc i j)))
                     ((>= j end1) acc))))
           ((>= i end0) acc))))
    (else
     (let* ((rev-lowers (reverse (interval-lower-bounds->list iv)))
            (rev-uppers (reverse (interval-upper-bounds->list iv)))
            (multi-index
             (list-copy (if (pair? o) o (interval-lower-bounds->list iv))))
            (rev-index (pair-fold cons '() multi-index)))
       (let lp ((acc knil))
         (let ((acc (apply kons acc multi-index)))
           (if (rev-index-next! rev-index rev-lowers rev-uppers)
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
    (assert (every interval? ls)
            (or (null? o) (apply = (map interval-dimension ls))))
    (let ((lower (apply vector-map max (map interval-lb ls)))
          (upper (apply vector-map min (map interval-ub ls))))
      (and (vector-every < lower upper)
           (make-interval lower upper)))))

(define (interval-translate iv translation)
  (assert (translation? translation))
  (interval-dilate iv translation translation))

(define (interval-permute iv perm)
  (assert (interval? iv) (permutation? perm))
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
  (assert (interval? iv)
          (vector? scales)
          (= (interval-dimension iv) (vector-length scales))
          (vector-every exact-integer? scales)
          (vector-every positive? scales))
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

(define generic-storage-class
  (make-storage-class
   vector-ref vector-set! (lambda (x) #t) make-vector
   vector-copy! vector-length #f))

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

(define f8-storage-class #f)
(define f16-storage-class #f)

;; Arrays

(define-record-type Array
  (%%make-array domain getter setter storage body coeffs indexer safe?)
  array?
  (domain array-domain)
  (getter array-getter)
  (setter array-setter)
  (storage array-storage-class)
  (body array-body)
  (coeffs array-coeffs)
  (indexer array-indexer)
  (safe? array-safe?))

(define (%make-array domain getter setter storage body coeffs indexer safe?)
  (assert (interval? domain)
          (procedure? getter)
          (or (not setter) (procedure? setter))
          (or (not storage) (storage-class? storage)))
  (%%make-array domain getter setter storage body coeffs indexer safe?))

(define (make-array domain getter . o)
  (assert (interval? domain) (procedure? getter))
  (%make-array domain getter (and (pair? o) (car o)) #f #f #f #f #f))

(define (array-dimension a)
  (interval-dimension (array-domain a)))

(define (mutable-array? x)
  (and (array? x) (array-setter x) #t))

(define specialized-array-default-safe?
  (make-parameter #f (lambda (x) (assert (boolean? x)) x)))

(define specialized-array-default-mutable?
  (make-parameter #t (lambda (x) (assert (boolean? x)) x)))

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
       ((= (+ 1 (car ls)) (interval-upper-bound domain (- i 1)))
        (lp (+ i 1) (cdr ls) offset count))
       (else
        (set-car! ls (+ 1 (car ls)))
        (let* ((offset2 (apply indexer multi-index))
               (coeff (- offset2 offset)))
          (cond
           ((> count 0)
            (and (= coeff (vector-ref res i))
                 (lp (+ i 1) (cdr ls) offset2 count)))
           (else
            (vector-set! res i coeff)
            (lp (+ i 1) (cdr ls) offset2 count)))))))))

(define (coeffs->indexer coeffs domain)
  (case (vector-length coeffs)
    ((2)
     (let ((a (vector-ref coeffs 0))
           (b (vector-ref coeffs 1))
           (lo-x (interval-lower-bound domain 0)))
       (lambda (x) (+ a (* b (- x lo-x))))))
    ((3)
     (let ((a (vector-ref coeffs 0))
           (b (vector-ref coeffs 1))
           (c (vector-ref coeffs 2))
           (lo-x (interval-lower-bound domain 0))
           (lo-y (interval-lower-bound domain 1)))
       (lambda (x y) (+ a (* b (- x lo-x)) (* c (- y lo-y))))))
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
                 (+ res (* (- (car ls) (interval-lower-bound domain (- i 1)))
                           (vector-ref coeffs i))))))))))))

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
          (lp (- i 1) coeff)))))))

(define (default-indexer domain)
  (coeffs->indexer (default-coeffs domain) domain))

;; converts the raw integer index to the multi-index in domain that
;; would map to it using the default indexer.
(define (invert-default-index domain raw-index)
  (let lp ((index raw-index)
           (i (- (interval-dimension domain) 1))
           (scale 1)
           (res '()))
    (if (negative? i)
        res
        (let* ((width (- (interval-upper-bound domain i)
                         (interval-lower-bound domain i)))
               (elt (modulo index width)))
          (lp (quotient (- index elt) scale)
              (- i 1)
              (* scale width)
              (cons (+ elt (interval-lower-bound domain i)) res))))))

;; Specialized arrays

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
    (%make-array
     domain
     (specialized-getter body indexer (storage-class-getter storage))
     (specialized-setter body indexer (storage-class-setter storage))
     storage
     body
     coeffs
     indexer
     safe?)))

(define (specialized-array? x)
  (and (array? x) (array-storage-class x) #t))

(define (array-elements-in-order? array)
  (assert (specialized-array? array))
  ;; TODO: speed this up and/or cache it
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

(define (specialized-array-share array new-domain project)
  (assert (specialized-array? array) (interval? new-domain))
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
    (%make-array
     new-domain
     (specialized-getter body indexer (storage-class-getter storage))
     (specialized-setter body indexer (storage-class-setter storage))
     storage
     body
     coeffs
     indexer
     (array-safe? array))))

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
    (assert (storage-class? storage) (interval? new-domain)
            (boolean? mutable?) (boolean? safe?))
    (let* ((body ((storage-class-maker storage)
                    (interval-volume new-domain)
                    (storage-class-default storage)))
           (coeffs (default-coeffs new-domain))
           (indexer (coeffs->indexer coeffs new-domain))
           (getter (specialized-getter body indexer
                                       (storage-class-getter storage)))
           (setter (specialized-setter body indexer
                                       (storage-class-setter storage)))
           (res (%make-array new-domain getter setter
                             storage body coeffs indexer safe?)))
        (array-assign! res array))))

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
  (assert (array? array)
          (interval? new-domain)
          (interval-subset? new-domain (array-domain array)))
  (if (specialized-array? array)
      (specialized-array-share array new-domain values)
      (make-array new-domain
                  (array-getter array)
                  (array-setter array))))

(define (array-tile array sizes)
  (assert (array? array)
          (vector? sizes)
          (= (array-dimension array) (vector-length sizes))
          (vector-every exact-integer? sizes)
          (vector-every >= sizes (interval-lower-bounds->vector
                                  (array-domain array)))
          (vector-every < sizes (interval-upper-bounds->vector
                                 (array-domain array))))
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
          (interval-lower-bound (array-domain array))
          (interval-upper-bound (array-domain array))
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
   (map
    car
    (list-sort
     (lambda (a b) (< (cdr a) (cdr b)))
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
    (assert (vector? flip?)
            (= (array-dimension array) (vector-length flip?))
            (vector-every boolean? flip?))
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
  (assert (procedure? op) (array? array1) (array? array2))
  (make-array (interval-cartesian-product (array-domain array1)
                                          (array-domain array2))
              (let ((getter1 (array-getter array1))
                    (getter2 (array-getter array2))
                    (dim1 (array-dimension array1)))
                (lambda multi-index
                  (op (apply getter1 (take multi-index dim1))
                      (apply getter2 (drop multi-index dim1)))))))

(define (array-map f array . arrays)
  (make-array (array-domain array)
              (let* ((ls (cons array arrays))
                     (getters (map array-getter ls)))
                (assert (all-equal? (map array-dimension ls)))
                (lambda multi-index
                  (apply f (map (lambda (g) (apply g multi-index)) getters))))))

(define (array-for-each f array . arrays)
  (interval-for-each
   (let* ((ls (cons array arrays))
          (getters (map array-getter ls)))
     (assert (all-equal? (map array-dimension ls)))
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
  ;; (let* ((domain (array-domain array))
  ;;        (init-index (interval-lower-bounds->list domain))
  ;;        (knil (apply array-ref array init-index)))
  ;;   (if (rev-index-next! (pair-fold cons '() init-index)
  ;;                        (reverse (interval-lower-bounds->list domain))
  ;;                        (reverse (interval-upper-bounds->list domain)))
  ;;       (apply interval-fold
  ;;              (lambda (acc . multi-index)
  ;;                (op acc (apply array-ref array multi-index)))
  ;;              knil
  ;;              domain
  ;;              init-index)
  ;;       knil))
  (reduce (lambda (elt acc) (op acc elt)) 'never-used (array->list array)))

(define (array-any pred array . arrays)
  (assert (all-equal? (map array-dimension (cons array arrays))))
  (call-with-current-continuation
   (lambda (return)
     (apply array-for-each
            (lambda args (if (apply pred args) (return #t)))
            array
            arrays)
     #f)))

(define (array-every pred array . arrays)
  (assert (all-equal? (map array-dimension (cons array arrays))))
  (call-with-current-continuation
   (lambda (return)
     ;; TODO: return last value
     (apply array-for-each
            (lambda args (if (not (apply pred args)) (return #f)))
            array
            arrays)
     #t)))

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
    (assert (interval? domain) (storage-class? storage)
            (boolean? mutable?) (boolean? safe?))
    (interval-fold
     (lambda (ls . multi-index)
       (apply array-set! res (car ls) multi-index)
       (cdr ls))
     ls
     domain)
    res))

(define (array-assign! destination source)
  (assert (array? destination)
          (mutable-array? destination)
          (array? source)
          (or (equal? (array-domain destination) (array-domain source))
              (and (array-elements-in-order? destination)
                   (equal? (interval-volume (array-domain destination))
                           (interval-volume (array-domain source))))))
  (let ((getter (array-getter source))
        (setter (array-setter destination)))
    (if (equal? (array-domain destination) (array-domain source))
        (interval-for-each
         (lambda multi-index
           (apply setter (apply getter multi-index) multi-index))
         (array-domain source))
        (let* ((dst-domain (array-domain destination))
               (rev-lowers (reverse (interval-lower-bounds->list dst-domain)))
               (rev-uppers (reverse (interval-upper-bounds->list dst-domain)))
               (dst-index (list-copy (interval-lower-bounds->list dst-domain)))
               (rev-index (pair-fold cons '() dst-index)))
          (interval-for-each
           (lambda multi-index
             (apply setter (apply getter multi-index) dst-index)
             (rev-index-next! rev-index rev-lowers rev-uppers))
           (array-domain source))))
    destination))

(define (reshape-indexer array new-domain)
  (let ((orig-indexer (array-indexer array))
        (tmp-indexer (default-indexer new-domain)))
    (indexer->coeffs
     (lambda multi-index
       (apply orig-indexer
              (invert-default-index (array-domain array)
                                    (apply tmp-indexer multi-index))))
     new-domain
     #t)))

(define (specialized-array-reshape array new-domain . o)
  (assert (specialized-array? array)
          (= (interval-volume (array-domain array))
             (interval-volume new-domain)))
  (let ((copy-on-failure? (and (pair? o) (car o))))
    (cond
     ((reshape-indexer array new-domain)
      => (lambda (new-coeffs)
           (let* ((new-indexer (coeffs->indexer new-coeffs new-domain))
                  (body (array-body array))
                  (storage (array-storage-class array)))
             (%make-array
              new-domain
              (specialized-getter body
                                  new-indexer
                                  (storage-class-getter storage))
              (specialized-setter body
                                  new-indexer
                                  (storage-class-setter storage))
              storage
              body
              new-coeffs
              new-indexer
              (array-safe? array)))))
     (copy-on-failure?
      (let* ((res (make-specialized-array
                   new-domain
                   (array-storage-class array)
                   (array-safe? array)))
             (setter (array-setter res))
             (multi-index (interval-lower-bounds->list new-domain))
             (rev-index (pair-fold cons '() multi-index))
             (rev-lowers (reverse (interval-lower-bounds->list new-domain)))
             (rev-uppers (reverse (interval-upper-bounds->list new-domain))))
        (array-for-each
         (lambda (x)
           (apply setter x multi-index)
           (rev-index-next! rev-index rev-lowers rev-uppers))
         array)
        res))
     (else
      (error "can't reshape" array new-domain)))))
