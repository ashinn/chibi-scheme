
(define-record-type Set (make-set table comparator) set?
  (table set-table)
  (comparator set-comparator))

(define (set comparator . elts)
  (let ((res (make-hash-table comparator)))
    (for-each (lambda (x) (hash-table-set! res x x)) elts)
    (make-set res comparator)))

(define (set-unfold comparator stop? mapper successor seed)
  (let ((mapper (lambda (acc) (let ((elt (mapper acc))) (values elt elt)))))
    (make-set (hash-table-unfold stop? mapper successor seed comparator)
              comparator)))

(define (set-contains? set element)
  (hash-table-contains? (set-table set) element))

(define (set-empty? set)
  (zero? (set-size set)))

(define (set-disjoint? set1 set2)
  (if (< (hash-table-size (set-table set2))
         (hash-table-size (set-table set1)))
      (set-disjoint? set2 set1)
      (let ((ht (set-table set2)))
        (not (hash-table-find (lambda (key value) (hash-table-contains? ht key))
                              (set-table set1)
                              (lambda () #f))))))

(define (set-member set element default)
  (hash-table-ref/default (set-table set) element default))

(define (set-element-comparator set)
  (set-comparator set))

(define (set-adjoin set . elts)
  (apply set-adjoin! (set-copy set) elts))

(define (set-adjoin! set . elts)
  (for-each (lambda (elt) (hash-table-set! (set-table set) elt elt)) elts)
  set)

(define (set-replace set element)
  (set-replace! (set-copy set) element))

(define (set-replace! set element)
  (when (hash-table-contains? (set-table set) element)
    (hash-table-delete! (set-table set) element)
    (hash-table-set! (set-table set) element element))
  set)

(define (set-delete set . elts)
  (set-delete-all set elts))

(define (set-delete! set . elts)
  (set-delete-all! set elts))

(define (set-delete-all set element-list)
  (set-delete-all! (set-copy set) element-list))

(define (set-delete-all! set element-list)
  (for-each (lambda (elt) (hash-table-delete! (set-table set) elt))
            element-list)
  set)

(define set-search!
  (let ((not-found (list 'not-found)))
    (lambda (set element failure success)
      (let ((elt (hash-table-ref/default (set-table set) element not-found)))
        (if (eq? elt not-found)
            (failure (lambda (obj)
                       (hash-table-set! (set-table set) element element)
                       (values set obj))
                     (lambda (obj)
                       (values set obj)))
            (success elt
                     (lambda (new-element obj)
                       (hash-table-delete! (set-table set) elt)
                       (hash-table-set! (set-table set) new-element new-element)
                       (values set obj))
                     (lambda (obj)
                       (hash-table-delete! (set-table set) element)
                       (values set obj))))))))

(define (set-size set)
  (hash-table-size (set-table set)))

(define (set-find predicate set failure)
  (call-with-current-continuation
   (lambda (return)
     (hash-table-for-each
      (lambda (elt _) (if (predicate elt) (return elt)))
      (set-table set))
     (failure))))

(define (set-count predicate set)
  (hash-table-count (lambda (key value) (predicate key)) (set-table set)))

(define (set-any? predicate set)
  (and (hash-table-find (lambda (key value) (predicate key))
                        (set-table set)
                        (lambda () #f))
       #t))

(define (set-every? predicate set)
  (not (set-any? (lambda (x) (not (predicate x))) set)))

(define (set-map comparator proc s)
  (set-fold (lambda (elt res) (set-adjoin! res (proc elt)))
            (set comparator)
            s))

(define (set-for-each proc set)
  (hash-table-for-each (lambda (elt _) (proc elt)) (set-table set)))

(define (set-fold proc nil set)
  (hash-table-fold (lambda (elt _ acc) (proc elt acc)) nil (set-table set)))

(define (set-filter predicate st)
  (set-fold (lambda (elt res)
              (if (predicate elt) (set-adjoin! res elt) res))
            (set (set-comparator st))
            st))

(define set-filter! set-filter)

(define (set-remove predicate set)
  (set-filter (lambda (elt) (not (predicate elt))) set))

(define set-remove! set-remove)

(define (set-partition predicate set)
  (values (set-filter predicate set)
          (set-remove predicate set)))

(define set-partition! set-partition)

(define (set-copy set)
  (make-set (hash-table-copy (set-table set))
            (set-comparator set)))

(define (set->list set)
  (hash-table-keys (set-table set)))

(define (list->set comparator list)
  (fold (lambda (elt set) (set-adjoin! set elt)) (set comparator) list))

(define (list->set! set list)
  (fold (lambda (elt set) (set-adjoin! set elt)) set list))

(define (comparable-sets? set1 set2)
  (or (eq? (set-comparator set1) (set-comparator set2))
      (error "can't compare sets with different comparators" set1 set2)))

(define (set=? set1 . sets)
  (or (null? sets)
      (and (comparable-sets? set1 (car sets))
           (= (set-size set1) (set-size (car sets)))
           (set-every? (lambda (elt) (set-contains? set1 elt)) (car sets))
           (apply set=? sets))))

(define (set<? set1 . sets)
  (or (null? sets)
      (and (comparable-sets? set1 (car sets))
           (< (set-size set1) (set-size (car sets)))
           (set-every? (lambda (elt) (set-contains? (car sets) elt)) set1)
           (apply set<? sets))))

(define (set>? . sets)
  (apply set<? (reverse sets)))

(define (set<=? set1 . sets)
  (or (null? sets)
      (and (comparable-sets? set1 (car sets))
           (<= (set-size set1) (set-size (car sets)))
           (set-every? (lambda (elt) (set-contains? (car sets) elt)) set1)
           (apply set<=? sets))))

(define (set>=? . sets)
  (apply set<=? (reverse sets)))

(define (set-union set1 . sets)
  (apply set-union! (set-copy set1) sets))

(define (set-intersection set1 . sets)
  (apply set-intersection! (set-copy set1) sets))

(define (set-difference set1 . sets)
  (apply set-difference! (set-copy set1) sets))

(define (set-xor set1 set2)
  (set-xor! (set-copy set1) set2))

(define (set-union! set1 . sets)
  (if (null? sets)
      set1
      (and (comparable-sets? set1 (car sets))
           (apply set-union!
                  (set-fold (lambda (elt set) (set-adjoin! set elt)) set1 (car sets))
                  (cdr sets)))))

(define (set-intersection! set1 . sets)
  (if (null? sets)
      set1
      (and (comparable-sets? set1 (car sets))
           (apply set-intersection!
                  (set-filter! (lambda (elt) (set-contains? (car sets) elt)) set1)
                  (cdr sets)))))

(define (set-difference! set1 . sets)
  (if (null? sets)
      set1
      (and (comparable-sets? set1 (car sets))
           (apply set-difference!
                  (set-remove! (lambda (elt) (set-contains? (car sets) elt)) set1)
                  (cdr sets)))))

(define (set-xor! set1 set2)
  (and (comparable-sets? set1 set2)
       (set-union (set-remove (lambda (elt) (set-contains? set2 elt)) set1)
                  (set-remove (lambda (elt) (set-contains? set1 elt)) set2))))

(define the-set-comparator
  (make-comparator set? set=? set<? hash))
