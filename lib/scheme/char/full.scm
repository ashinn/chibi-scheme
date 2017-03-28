
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
        (let lp ((lo 0) (hi (- (vector-length char-downcase-map) 2)))
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
        (let lp ((lo 0) (hi (- (vector-length char-upcase-map) 2)))
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

(define (char-cmp-ci op a ls)
  (let lp ((op op) (a (char->integer (char-foldcase a))) (ls ls))
    (if (null? ls)
        #t
        (let ((b (char->integer (char-downcase (car ls)))))
          (and (op a b) (lp op b (cdr ls)))))))

(define (char-ci=? a . ls) (char-cmp-ci = a ls))
(define (char-ci<? a . ls) (char-cmp-ci < a ls))
(define (char-ci>? a . ls) (char-cmp-ci > a ls))
(define (char-ci<=? a . ls) (char-cmp-ci <= a ls))
(define (char-ci>=? a . ls) (char-cmp-ci >= a ls))

(define (char-get-special-case ch off)
  (let ((i (char->integer ch)))
    (let lp ((a 0) (b (vector-length special-cases)))
      (let* ((mid (+ a (quotient (- b a) 2)))
             (vec (vector-ref special-cases mid))
             (val (vector-ref vec 0)))
        (cond ((< i val) (and (< mid b) (lp a mid)))
              ((> i val) (and (> mid a) (lp mid b)))
              (else (vector-ref vec off)))))))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (string-down-or-fold-case str fold?)
  (call-with-output-string
    (lambda (out)
      (let ((in (open-input-string str)))
        (let lp ()
          (let ((ch (read-char in)))
            (cond
             ((not (eof-object? ch))
              (write-string
               (if (and (not fold?) (eqv? ch #\x03A3))
                   (let ((ch2 (peek-char in)))
                     (if (or (eof-object? ch2)
                             (not (char-set-contains? char-set:letter ch2)))
                         #\x03C2
                         #\x03C3))
                   (or (char-get-special-case ch 1) (char-downcase ch)))
               out)
              (lp)))))))))

(define (string-downcase str) (string-down-or-fold-case str #f))
(define (string-foldcase str) (string-down-or-fold-case str #t))

(define (string-upcase str)
  (call-with-output-string
    (lambda (out)
      (string-for-each
       (lambda (ch)
         (write-string (if (memv ch '(#\x03C2 #\x03C3))
                           #\x03A3
                           (or (char-get-special-case ch 3)
                               (char-upcase ch)))
                       out))
       str))))

(define (string-cmp-ci op a ls)
  (let lp ((op op) (a (string-foldcase a)) (ls ls))
    (if (null? ls)
        #t
        (let ((b (string-foldcase (car ls))))
          (and (op a b) (lp op b (cdr ls)))))))

(define (string-ci=? a . ls) (string-cmp-ci string=? a ls))
(define (string-ci<? a . ls) (string-cmp-ci string<? a ls))
(define (string-ci>? a . ls) (string-cmp-ci string>? a ls))
(define (string-ci<=? a . ls) (string-cmp-ci string<=? a ls))
(define (string-ci>=? a . ls) (string-cmp-ci string>=? a ls))
