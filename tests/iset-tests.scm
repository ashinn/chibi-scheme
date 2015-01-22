
(cond-expand
 (modules
  (import (chibi) (chibi iset) (chibi iset optimize) (srfi 1) (chibi test)))
 (else #f))

(define (test-name iset op)
  (call-with-output-string
    (lambda (out)
      (let* ((ls (iset->list iset))
             (ls (if (> (length ls) 10)
                     `(,@(take ls 5) ... ,@(take-right ls 5))
                     ls)))
        (write `(,(car op) (iset ,@ls) ,@(cdr op)) out)))))

(test-begin "iset")

;; Tests to perform repeated operations on an iset.  The first element
;; in each list is a list of integers to initialize the set `is', which
;; we generate and verify the size and round-trip list conversion.
;; Subsequent elements are abbreviated operations on is:
;;
;;   (+ a ...)     (iset-adjoin! a) ...
;;   (- a ...)     (iset-delete! a) ...
;;   (= a ...)     (test (list a ...) (iset->list is))
;;   (<= a ...)    (test-assert (iset<= is (iset a ...)))
;;   (? a ...)     (test-assert (iset-contains? is a)) ...
;;   (!? a ...)    (test-not (iset-contains? is a)) ...
;;   (u a ...)     (iset-union is (iset a ...))
;;   (u: a b)      (iset-union is (make-iset a b))
;;   (i a ...)     (iset-intersection is (iset a ...))
;;   (d a ...)     (iset-difference is (iset a ...))
;;   (m f)         (iset-map f is)
;;   (s size)      (test size (iset-size iset))
;;   (z [empty?])  (test empty? (iset-empty? iset))
(let ((tests
       `(;; construction
         ((1 128 127))
         ((129 2 127))
         ((1 -128 -126))
         ((1 2 3 1000 1005))
         ((97308 97827 97845 97827))
         ((1 2 3 4 5 6 7 8))
         ((2 3 4 5 6 7 8))
         ((1 3 4 5 6 7 8))
         ((1 2 4 5 6 7 8))
         ((1 2 3 5 6 7 8))
         ((1 2 3 4 6 7 8))
         ((1 2 3 4 5 7 8))
         ((1 2 3 4 5 6 8))
         ((1 2 3 4 5 6 7))
         ;; ordering
         ((97) (<= 97 117))
         ((117) (<= 97 117))
         ;; individual elements
         (() (+ 99) (u 3 50) (? 99))
         (() (+ 1) (+ 1000) (+ -1000) (+ 3) (+ -1))
         ((0) (z #f) (- 0) (z))
         ((0 1 2) (- 1) (- 2) (? 0))
         ;; union
         ((17 29) (u 7 29))
         ((2 3 4) (u 1 2 3 4 5))
         ((1 2 3 4 5) (u 2 3 4))
         ((1 2 3 1000 2000) (u 1 4))
         ((1 3) (u 1 4) (= 1 3 4))
         ((1 3) (u 3 4) (= 1 3 4))
         ((1) (u 1 3) (= 1 3))
         ((3) (u 1 3) (= 1 3))
         ((1 4) (u 3 4 5) (= 1 3 4 5))
         ((1 2 3 4) (u 5 6 7 8) (= 1 2 3 4 5 6 7 8))
         ((1 3 4) (u 5 6 7 8) (= 1 3 4 5 6 7 8))
         ((1 2 4) (u 5 6 7 8) (= 1 2 4 5 6 7 8))
         ((1 2 3) (u 5 6 7 8) (= 1 2 3 5 6 7 8))
         ((1 2 3 4) (u 6 7 8) (= 1 2 3 4 6 7 8))
         ((1 2 3 4) (u 5 7 8) (= 1 2 3 4 5 7 8))
         ((1 2 3 4) (u 5 6 8) (= 1 2 3 4 5 6 8))
         ((1 2 3) (u 6 7 8) (= 1 2 3 6 7 8))
         ((1 3) (u 6 8) (= 1 3 6 8))
         ((1 2 3 4 1001 1002)
          (u 1003 1004 2001 2002 2003 2004)
          (= 1 2 3 4 1001 1002 1003 1004 2001 2002 2003 2004))
         ((1 2 4 1001 1002)
          (u 1003 1004 2001 2002 2003 2004)
          (= 1 2 4 1001 1002 1003 1004 2001 2002 2003 2004))
         ((1 2 3 4 1001 1002)
          (u 1004 2001 2002 2003 2004)
          (= 1 2 3 4 1001 1002 1004 2001 2002 2003 2004))
         ((1 2 3 4 1001 1002)
          (u 1003 1004 2001 2003 2004)
          (= 1 2 3 4 1001 1002 1003 1004 2001 2003 2004))
         (() (u: 349 680) (u: 682 685))
         (() (u: 64434 64449) (u: 65020 65021) (u #xFE62))
         (() (u: 716 747) (u: 750 1084))
         (() (u: 48 57) (u: 65 90) (u: 97 122) (u 45 46 95 126) (? 119))
         ;; intersection
         ((1 2 3 4 5) (i 1) (= 1))
         ((1 2 3 4 5) (i 1 2) (= 1 2))
         ((1 2 3 4 5) (i 1 2 3) (= 1 2 3))
         ((1 2 3 4 5) (i 2 3) (= 2 3))
         ((1 2 3 4 5) (i 2 3 4) (= 2 3 4))
         ((1 2 3 4 5) (i 5) (= 5))
         ((1 2 3 4 5) (i 4 5) (= 4 5))
         ((1 2 3 4 5) (i 1 2 3 4 5) (= 1 2 3 4 5))
         ((1 2 3 4 5) (i 0 1 5 6) (= 1 5))
         ;; difference
         ((1 2 3 4 5) (d 1) (!? 0) (? 2 3 4 5) (!? 6))
         ((1 2 3 4 5) (d 1 2) (!? 0) (? 3 4 5) (!? 6))
         ((1 2 3 4 5) (d 1 2 3) (!? 0) (? 4 4) (!? 6))
         ((1 2 3 4 5) (d 2 3) (!? 0) (? 1 4 5) (!? 6))
         ((1 2 3 4 5) (d 2 3 4) (!? 0) (? 1 5) (!? 6))
         ((1 2 3 4 5) (d 5) (!? 0) (? 1 2 3 4) (!? 6))
         ((1 2 3 4 5) (d 4 5) (!? 0) (? 1 2 3) (!? 6))
         ((1 2 3 4 5) (d 1 2 3 4 5) (z))
         ((1 2 3 4 5) (d 0 1 5 6) (? 2 3 4))
         ;; map
         ((1 2 3) (m ,(lambda (x) (+ x 1))) (= 2 3 4))
         )))
  (for-each
   (lambda (tst)
     (let* ((ls (car tst))
            (is (list->iset ls))
            (ls2 (delete-duplicates ls =)))
       ;; initial creation and sanity checks
       (test-assert (lset= equal? ls2 (iset->list is)))
       (test (length ls2) (iset-size is))
       (test-assert (call-with-output-string
                      (lambda (out)
                        (display "init: " out)
                        (write ls out)))
           (every
            (lambda (x) (iset-contains? is x))
            ls))
       (test (iset-contains? is 42) (member 42 ls))
       ;; additional operations
       (for-each
        (lambda (op)
          (let ((name (test-name is op)))
            (case (car op)
              ((+)
               (for-each
                (lambda (x) (iset-adjoin! is x))
                (cdr op))
               (test-assert name (iset-contains? is (cadr op))))
              ((-)
               (for-each
                (lambda (x) (iset-delete! is x))
                (cdr op))
               (test-assert name (not (iset-contains? is (cadr op)))))
              ((=)
               (test name (cdr op) (iset->list is))
               (test-assert name (iset= (list->iset (cdr op)) is)))
              ((<=)
               (test-assert name (iset<= is (list->iset (cdr op)))))
              ((?)
               (test-assert name
                 (every (lambda (x) (iset-contains? is x)) (cdr op))))
              ((!?)
               (test-assert name
                 (every (lambda (x) (not (iset-contains? is x))) (cdr op))))
              ((d)
               (set! is (iset-difference is (list->iset (cdr op))))
               (test-assert name
                 (every
                  (lambda (x) (not (iset-contains? is x)))
                  (cdr op))))
              ((i) (set! is (iset-intersection is (list->iset (cdr op)))))
              ((u u:)
               (let ((arg (cond ((eq? 'u: (car op))
                                 (make-iset (cadr op) (car (cddr op))))
                                ((iset? (cadr op)) (cadr op))
                                (else (list->iset (cdr op))))))
                 (set! is (iset-union is arg)))
               (test-assert name
                 (every (lambda (x)
                          (or (not (integer? x))
                              (iset-contains? is x)))
                        (cdr op))))
              ((m) (set! is (iset-map (cadr op) is)))
              ((s) (test (iset-size is) (cadr op)))
              ((z) (test (iset-empty? is) (if (pair? (cdr op)) (cadr op) #t)))
              (else (error "unknown operation" (car op))))))
        (cdr tst))
       ;; optimization
       (let* ((is2 (iset-optimize is))
              (is3 (iset-balance is))
              (is4 (iset-balance is2)))
         (test-assert (iset= is is2))
         (test-assert (iset= is is3))
         (test-assert (iset= is is4)))))
   tests))

(let ((a (%make-iset 65 90 #f #f (%make-iset 97 122 #f #f #f)))
      (b (list->iset '(45 46 95 126))))
  (test-assert (iset-contains? (iset-union a b) 119))
  (test-assert (iset-contains? (iset-union b a) 119))) 

(test-end)
