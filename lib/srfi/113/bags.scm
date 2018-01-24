
(define-record-type Bag (make-bag table comparator) bag?
  (table bag-table)
  (comparator bag-comparator))

(define (bag comparator . elts)
  (let ((res (make-bag (make-hash-table comparator) comparator)))
    (for-each (lambda (x) (bag-adjoin! res x)) elts)
    res))

(define (bag-unfold comparator stop? mapper successor seed)
  (let ((mapper (lambda (acc) (let ((elt (mapper acc))) (values elt 1)))))
    (make-bag (hash-table-unfold stop? mapper successor seed comparator)
              comparator)))

(define (bag-contains? bag element)
  (hash-table-contains? (bag-table bag) element))

(define (bag-empty? bag)
  (zero? (bag-size bag)))

(define (bag-disjoint? bag1 bag2)
  (if (< (hash-table-size (bag-table bag2))
         (hash-table-size (bag-table bag1)))
      (bag-disjoint? bag2 bag1)
      (let ((ht (bag-table bag2)))
        (not (hash-table-find (lambda (key value) (hash-table-contains? ht key))
                              (bag-table bag1)
                              (lambda () #f))))))

(define (bag-member bag element default)
  ;; (let ((cell (hash-table-cell (bag-table bag) element #f)))
  ;;   (if cell (car cell) default))
  (if (hash-table-contains? (bag-table bag) element)
      element
      default))

(define (bag-element-comparator bag)
  (bag-comparator bag))

(define (bag-adjoin bag . elts)
  (apply bag-adjoin! (bag-copy bag) elts))

(define (bag-adjoin! bag . elts)
  (for-each (lambda (elt)
              (hash-table-update!/default (bag-table bag)
                                          elt
                                          (lambda (count) (+ 1 count))
                                          0))
            elts)
  bag)

(define (bag-replace bag element)
  (bag-replace! (bag-copy bag) element))

(define (bag-replace! bag element)
  (when (hash-table-contains? (bag-table bag) element)
    (hash-table-delete! (bag-table bag) element)
    (hash-table-set! (bag-table bag) element 1))
  bag)

(define (bag-delete bag . elts)
  (bag-delete-all bag elts))

(define (bag-delete! bag . elts)
  (bag-delete-all! bag elts))

(define (bag-delete-all bag element-list)
  (bag-delete-all! (bag-copy bag) element-list))

(define (bag-delete-all! bag element-list)
  (let ((ht (bag-table bag)))
    (for-each (lambda (elt)
                (let ((count (- (hash-table-ref/default ht elt 0) 1)))
                  (cond
                   ((positive? count) (hash-table-set! ht elt count))
                   ((zero? count) (hash-table-delete! ht elt)))))
              element-list))
  bag)

(define bag-search!
  (let ((not-found (list 'not-found)))
    (lambda (bag element failure success)
      (let ((elt (hash-table-ref/default (bag-table bag) element not-found)))
        (if (eq? elt not-found)
            (failure (lambda (obj)
                       (hash-table-set! (bag-table bag) element 1)
                       (values bag obj))
                     (lambda (obj)
                       (values bag obj)))
            (success elt
                     (lambda (new-element obj)
                       (hash-table-delete! (bag-table bag) element)
                       (bag-adjoin! bag new-element)
                       (values bag obj))
                     (lambda (obj)
                       (hash-table-delete! (bag-table bag) element)
                       (values bag obj))))))))

(define (bag-size bag)
  (hash-table-fold (bag-table bag) (lambda (elt count acc) (+ count acc)) 0))

(define (bag-find predicate bag failure)
  (call-with-current-continuation
   (lambda (return)
     (hash-table-for-each
      (lambda (elt count) (if (predicate elt) (return elt)))
      (bag-table bag))
     (failure))))

(define (bag-count predicate bag)
  (hash-table-fold (lambda (elt count acc) (+ acc (if (predicate elt) count 0)))
                   0
                   (bag-table bag)))

(define (bag-any? predicate bag)
  (and (hash-table-find (lambda (key value) (predicate key))
                        (bag-table bag)
                        (lambda () #f))
       #t))

(define (bag-every? predicate bag)
  (not (bag-any? (lambda (x) (not (predicate x))) bag)))

(define (bag-map comparator proc s)
  (bag-fold (lambda (elt res) (bag-adjoin! res (proc elt)))
            (bag comparator)
            s))

(define (bag-for-each proc bag)
  (hash-table-for-each (lambda (elt count)
                         (let lp ((i count))
                           (when (positive? i)
                             (proc elt)
                             (lp (- i 1)))))
                       (bag-table bag)))

(define (bag-fold proc nil bag)
  (hash-table-fold (lambda (elt count acc)
                     (let lp ((i count) (acc acc))
                       (if (zero? i)
                           acc
                           (lp (- i 1) (proc elt acc)))))
                   nil
                   (bag-table bag)))

(define (bag-filter predicate st)
  (bag-fold (lambda (elt res)
              (if (predicate elt) (bag-adjoin! res elt) res))
            (bag (bag-comparator st))
            st))

(define bag-filter! bag-filter)

(define (bag-remove predicate bag)
  (bag-filter (lambda (elt) (not (predicate elt))) bag))

(define bag-remove! bag-remove)

(define (bag-partition predicate bag)
  (values (bag-filter predicate bag)
          (bag-remove predicate bag)))

(define bag-partition! bag-partition)

(define (bag-copy bag)
  (make-bag (hash-table-copy (bag-table bag))
            (bag-comparator bag)))

(define (bag->list bag)
  (hash-table-keys (bag-table bag)))

(define (list->bag comparator list)
  (fold (lambda (elt bag) (bag-adjoin! bag elt)) (bag comparator) list))

(define (list->bag! bag list)
  (fold (lambda (elt bag) (bag-adjoin! bag elt)) bag list))

(define (comparable-bags? bag1 bag2)
  (or (eq? (bag-comparator bag1) (bag-comparator bag2))
      (error "can't compare bags with different comparators" bag1 bag2)))

(define (bag=? bag1 . bags)
  (or (null? bags)
      (and (comparable-bags? bag1 (car bags))
           (= (bag-size bag1) (bag-size (car bags)))
           (bag-every? (lambda (elt) (bag-contains? bag1 elt)) (car bags))
           (apply bag=? bags))))

(define (bag<? bag1 . bags)
  (or (null? bags)
      (and (comparable-bags? bag1 (car bags))
           (< (bag-size bag1) (bag-size (car bags)))
           (bag-every? (lambda (elt) (bag-contains? (car bags) elt)) bag1)
           (apply bag<? bags))))

(define (bag>? . bags)
  (apply bag<? (reverse bags)))

(define (bag<=? bag1 . bags)
  (or (null? bags)
      (and (comparable-bags? bag1 (car bags))
           (<= (bag-size bag1) (bag-size (car bags)))
           (bag-every? (lambda (elt) (bag-contains? (car bags) elt)) bag1)
           (apply bag<=? bags))))

(define (bag>=? . bags)
  (apply bag<=? (reverse bags)))

(define (bag-union bag1 . bags)
  (apply bag-union! (bag-copy bag1) bags))

(define (bag-intersection bag1 . bags)
  (apply bag-intersection! (bag-copy bag1) bags))

(define (bag-difference bag1 . bags)
  (apply bag-difference! (bag-copy bag1) bags))

(define (bag-xor bag1 bag2)
  (bag-xor! (bag-copy bag1) bag2))

(define (bag-union! bag1 . bags)
  (if (null? bags)
      bag1
      (and (comparable-bags? bag1 (car bags))
           (begin
             (hash-table-for-each
              (lambda (elt count)
                (hash-table-update!/default (bag-table bag1)
                                            elt
                                            (lambda (c) (max c count))
                                            count))
              (bag-table (car bags)))
             (apply bag-union! bag1 (cdr bags))))))

(define (bag-intersection! bag1 . bags)
  (if (null? bags)
      bag1
      (and (comparable-bags? bag1 (car bags))
           (let ((ht (bag-table (car bags))))
             (hash-table-for-each
              (lambda (elt count)
                (let ((count2 (min count (hash-table-ref/default ht elt 0))))
                  (if (positive? count2)
                      (hash-table-set! (bag-table bag1) elt count2)
                      (hash-table-delete! (bag-table bag1) elt))))
              (bag-table bag1))
             (apply bag-intersection! bag1 (cdr bags))))))

(define (bag-difference! bag1 . bags)
  (if (null? bags)
      bag1
      (and (comparable-bags? bag1 (car bags))
           (let ((ht (bag-table (car bags))))
             (hash-table-for-each
              (lambda (elt count)
                (let ((count2 (- count (hash-table-ref/default ht elt 0))))
                  (if (positive? count2)
                      (hash-table-set! (bag-table bag1) elt count2)
                      (hash-table-delete! (bag-table bag1) elt))))
              (bag-table bag1))
             (apply bag-difference! bag1 (cdr bags))))))

(define (bag-xor! bag1 bag2)
  (and (comparable-bags? bag1 bag2)
       (let ((ht1 (bag-table bag1))
             (ht2 (bag-table bag2)))
         (hash-table-for-each
          (lambda (elt count)
            (let ((count2 (abs (- count (hash-table-ref/default ht1 elt 0)))))
              (if (positive? count2)
                  (hash-table-set! ht1 elt count2)
                  (hash-table-delete! ht1 elt))))
          ht2)
         bag1)))

(define (bag-sum bag1 . bags)
  (apply bag-sum! (bag-copy bag1) bags))

(define (bag-sum! bag1 . bags)
  (if (null? bags)
      bag1
      (and (comparable-bags? bag1 (car bags))
           (begin
             (hash-table-for-each
              (lambda (elt count)
                (hash-table-update!/default (bag-table bag1)
                                            elt
                                            (lambda (c) (+ c count))
                                            count))
              (bag-table (car bags)))
             (apply bag-sum! bag1 (cdr bags))))))

(define (bag-product n bag)
  (bag-product! n (bag-copy bag)))

(define (bag-product! n bag)
  (for-each
   (lambda (elt)
     (hash-table-update! (bag-table bag) elt (lambda (count) (* n count))))
   (hash-table-keys (bag-table bag)))
  bag)

(define (bag-unique-size bag)
  (hash-table-size (bag-table bag)))

(define (bag-element-count bag element)
  (hash-table-ref/default (bag-table bag) element 0))

(define (bag-for-each-unique proc bag)
  (hash-table-for-each proc (bag-table bag)))

(define (bag-fold-unique proc nil bag)
  (hash-table-fold proc nil (bag-table bag)))

(define (bag-increment! bag element count)
  (let* ((ht (bag-table bag))
         (count2 (+ count (hash-table-ref/default ht element 0))))
    (if (positive? count2)
        (hash-table-set! ht element count2)
        (hash-table-delete! ht element))))

(define (bag-decrement! bag element count)
  (bag-increment! bag element (- count)))

(define (bag->set bag)
  (let ((ht (hash-table-copy (bag-table bag))))
    (hash-table-map! (lambda (key count) key) ht)
    (make-set ht (bag-comparator bag))))

(define (set->bag set)
  (set->bag! (bag (set-comparator set)) set))

(define (set->bag! bag set)
  (set-for-each (lambda (elt) (bag-adjoin! bag elt)) set)
  bag)

(define (bag->alist bag)
  (hash-table->alist (bag-table bag)))

(define (alist->bag comparator alist)
  (let ((res (bag comparator)))
    (for-each (lambda (x) (bag-increment! res (car x) (cdr x))) alist)
    res))

(define the-bag-comparator
  (make-comparator bag? bag=? bag<? hash))
