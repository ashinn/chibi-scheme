
(define (char-alphabetic? ch) (char-set-contains? char-set:letter ch))
(define (char-lower-case? ch) (char-set-contains? char-set:lower-case ch))
(define (char-upper-case? ch) (char-set-contains? char-set:upper-case ch))
(define (char-numeric? ch) (char-set-contains? char-set:digit ch))
(define (char-whitespace? ch) (char-set-contains? char-set:whitespace ch))

(define (char-downcase ch)
  (let ((n (char->integer ch)))
    (let lp ((ls char-downcase-offsets))
      (cond
       ((null? ls)
        (let lp ((lo 0) (hi (vector-length char-downcase-map)))
          (if (> lo hi)
              ch
              (let* ((mid (+ lo (* (quotient (- hi lo) 4) 2)))
                     (m (vector-ref char-downcase-map mid)))
                (cond
                 ((= n m)
                  (integer->char (vector-ref char-downcase-map (+ mid 1))))
                 ((< n m)
                  (lp lo (- mid 2)))
                 (else
                  (lp (+ mid 2) hi)))))))
       ((iset-contains? (caar ls) n)
        (integer->char (+ n (cdar ls))))
       (else (lp (cdr ls)))))))

(define char-foldcase char-downcase)

(define (char-upcase ch)
  (let ((n (char->integer ch)))
    (let lp ((ls char-downcase-offsets))
      (cond
       ((null? ls)
        (let lp ((lo 0) (hi (vector-length char-upcase-map)))
          (if (> lo hi)
              ch
              (let* ((mid (+ lo (* (quotient (- hi lo) 4) 2)))
                     (m (vector-ref char-upcase-map mid)))
                (cond
                 ((= n m)
                  (integer->char (vector-ref char-upcase-map (+ mid 1))))
                 ((< n m)
                  (lp lo (- mid 2)))
                 (else
                  (lp (+ mid 2) hi)))))))
       ((iset-contains? (caar ls) (- n (cdar ls)))
        (integer->char (- n (cdar ls))))
       (else (lp (cdr ls)))))))

(define (char-ci<=? a b) (char<=? (char-foldcase a) (char-foldcase b)))
(define (char-ci<? a b) (char<? (char-foldcase a) (char-foldcase b)))
(define (char-ci=? a b) (char=? (char-foldcase a) (char-foldcase b)))
(define (char-ci>=? a b) (char>=? (char-foldcase a) (char-foldcase b)))
(define (char-ci>? a b) (char>? (char-foldcase a) (char-foldcase b)))

(define (string-downcase str)
  (string-map char-downcase str))

(define (string-upcase str)
  (string-map char-upcase str))

(define (string-foldcase str)
  (string-map char-foldcase str))

(define (string-ci<=? a b) (string<=? (string-foldcase a) (string-foldcase b)))
(define (string-ci<? a b) (string<? (string-foldcase a) (string-foldcase b)))
(define (string-ci=? a b) (string=? (string-foldcase a) (string-foldcase b)))
(define (string-ci>=? a b) (string>=? (string-foldcase a) (string-foldcase b)))
(define (string-ci>? a b) (string>? (string-foldcase a) (string-foldcase b)))
