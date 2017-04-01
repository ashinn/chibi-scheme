
(define-record-type Comparator
  (make-comparator type-test equality ordering hash)
  comparator?
  (type-test comparator-type-test-predicate)
  (equality comparator-equality-predicate)
  (ordering comparator-ordering-predicate)
  (hash comparator-hash-function))

(define-syntax hash-bound
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (fixnum? (- (expt 2 62) 1))
         (- (expt 2 62) 1)
         (- (expt 2 30) 1)))))

(define-syntax hash-salt
  (er-macro-transformer
   (let ((salt (or (string->number
                    (or (get-environment-variable "CHIBI_HASH_SALT") ""))
                   (random-integer (hash-bound)))))
     (lambda (expr rename compare)
       salt))))

(define-syntax comparator-if<=>
  (syntax-rules ()
    ((comparator-if<=> comparator obj1 obj2 less equal greater)
     (let ((cmp comparator)
           (o1 obj1)
           (o2 obj2))
       (cond
        (((comparator-equality-predicate cmp) o1 o2) equal)
        (((comparator-ordering-predicate cmp) o1 o2) less)
        (else greater))))
    ((comparator-if<=> obj1 obj2 less equal greater)
     (comparator-if<=> (make-default-comparator) obj1 obj2 less equal greater))))

(define (comparator-ordered? comparator)
  (and (comparator-ordering-predicate comparator) #t))

(define (comparator-hashable? comparator)
  (and (comparator-hash-function comparator) #t))

(define (comparator-test-type comparator obj)
  ((comparator-type-test-predicate comparator) obj))

(define (comparator-check-type comparator obj)
  (or (comparator-test-type comparator obj)
      (error "not an object of the comparator type" comparator obj)))

(define (comparator-hash comparator obj)
  ((comparator-hash-function comparator) obj))

(define default-comparators
  (make-parameter '()))

(define (comparator-register-default! comparator)
  (default-comparators (cons comparator (default-comparators))))

(define (make-pair-comparator car-comparator cdr-comparator)
  (make-comparator
   (lambda (x)
     (and (pair? x)
          ((comparator-type-test-predicate car-comparator) (car x))
          ((comparator-type-test-predicate cdr-comparator) (cdr x))))
   (lambda (x y)
     (and ((comparator-equality-predicate car-comparator) (car x) (car y))
          ((comparator-equality-predicate cdr-comparator) (cdr x) (cdr y))))
   (lambda (x y)
     (if ((comparator-equality-predicate car-comparator) (car x) (car y))
         ((comparator-ordering-predicate cdr-comparator) (cdr x) (cdr y))
         ((comparator-ordering-predicate car-comparator) (car x) (car y))))
   (lambda (x)
     (bitwise-xor ((comparator-hash-function car-comparator) (car x))
                  ((comparator-hash-function cdr-comparator) (cdr x))))))

(define (make-list-comparator element-comparator type-test empty? head tail)
  (make-comparator
   (lambda (x)
     (and (type-test x)
          (let lp ((ls x))
            (or (empty? ls)
                (and ((comparator-type-test-predicate element-comparator) (head ls))
                     (lp (tail ls)))))))
   (lambda (x y)
     (let lp ((ls1 x) (ls2 y))
       (cond
        ((empty? ls1) (empty? ls2))
        ((empty? ls2) #f)
        (else
         (and ((comparator-equality-predicate element-comparator) (head ls1) (head ls2))
              (lp (tail ls1) (tail ls2)))))))
   (lambda (x y)
     (let lp ((ls1 x) (ls2 y))
       (cond
        ((empty? ls1) (not (empty? ls2)))
        ((empty? ls2) #f)
        (else
         (let ((a (head ls1)) (b (head ls2)))
           (if ((comparator-equality-predicate element-comparator) a b)
               (lp (tail ls1) (tail ls2))
               ((comparator-ordering-predicate element-comparator) a b)))))))
   (lambda (x)
     (let lp ((ls x) (acc 0))
       (if (empty? ls)
           acc
           (lp (tail ls)
               (bitwise-xor ((comparator-hash-function element-comparator) (head ls))
                            acc)))))
   ))

(define (make-vector-comparator element-comparator type-test length ref)
  (make-comparator
   (lambda (x)
     (and (type-test x)
          (let ((len (length x)))
            (let lp ((i 0))
              (or (>= i len)
                  (and ((comparator-type-test-predicate element-comparator) (ref x i))
                       (lp (+ i 1))))))))
   (lambda (x y)
     (let ((lenx (length x)) (leny (length y)))
       (and
        (= lenx leny)
        (let lp ((i 0))
          (or (>= i lenx)
              (let ((a (ref x i)) (b (ref y i)))
                (and ((comparator-equality-predicate element-comparator) a b)
                     (lp (+ i 1)))))))))
   (lambda (x y)
     (let ((lenx (length x)) (leny (length y)))
       (cond
        ((< lenx leny) #t)
        ((> lenx leny) #f)
        (else
         (let lp ((i 0))
           (and (< i lenx)
                (let ((a (ref x i)) (b (ref y i)))
                  (if ((comparator-equality-predicate element-comparator) a b)
                      (lp (+ i 1))
                      ((comparator-ordering-predicate element-comparator) a b)))))))))
   (lambda (x)
     (let ((len (length x)))
       (let lp ((i 0) (acc 0))
         (if (>= i len)
             acc
             (lp (+ i 1)
                 (bitwise-xor ((comparator-hash-function element-comparator) (ref x i))
                              acc))))))
   ))

(define (make-eq-comparator)
  (make-comparator (lambda (x) #t) eq? object-cmp hash-by-identity))

(define (make-eqv-comparator)
  (make-comparator (lambda (x) #t) eqv? object-cmp hash))

(define (make-equal-comparator)
  (make-comparator (lambda (x) #t) equal? object-cmp hash))

(define boolean-hash hash)

(define char-hash hash)

(define (char-ci-hash ch)
  (hash (char-foldcase ch)))

(define symbol-hash hash)

(define number-hash hash)

(define (default-hash x . o)
  (if (string? x) (string-hash x) (hash x)))

(define default-comparator
  (make-comparator
   (lambda (x) #t)
   (lambda (x y)
     (let lp ((ls (default-comparators)))
       (cond ((null? ls)
              (equal? x y))
             ((and (comparator-test-type (car ls) x)
                   (comparator-test-type (car ls) y))
              ((comparator-equality-predicate (car ls)) x y))
             (else
              (lp (cdr ls))))))
   (lambda (x y)
     (let lp ((ls (default-comparators)))
       (cond ((null? ls)
              (negative? (object-cmp x y)))
             ((and (comparator-test-type (car ls) x)
                   (comparator-test-type (car ls) y))
              ((comparator-ordering-predicate (car ls)) x y))
             (else
              (lp (cdr ls))))))
   default-hash))

(define (make-default-comparator)
  default-comparator)

(define (=? comparator o1 o2 . o)
  (let ((eq (comparator-equality-predicate comparator)))
    (and (eq o1 o2)
         (let lp ((ls o))
           (or (null? ls)
               (and (eq o1 (car ls))
                    (lp (cdr ls))))))))

(define (<? comparator o1 o2 . o)
  (let ((less (comparator-ordering-predicate comparator)))
    (and (less o1 o2)
         (let lp ((prev o2) (ls o))
           (or (null? ls)
               (and (less prev (car ls))
                    (lp (car ls) (cdr ls))))))))

(define (<=? comparator o1 o2 . o)
  (let ((less (comparator-ordering-predicate comparator)))
    (and (not (less o2 o1))
         (let lp ((prev o2) (ls o))
           (or (null? ls)
               (and (not (less (car ls) prev))
                    (lp (car ls) (cdr ls))))))))

(define (>? comparator . o)
  (apply <? comparator (reverse o)))

(define (>=? comparator . o)
  (apply <=? comparator (reverse o)))
