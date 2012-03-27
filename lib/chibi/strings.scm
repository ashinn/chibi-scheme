;; strings.scm -- cursor-oriented string library
;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (string-null? str)
  (equal? str ""))

;; TODO: support character sets
(define (make-char-predicate x)
  (cond ((procedure? x) x)
        ((char? x) (lambda (ch) (eq? ch x)))
        (else (error "invalid character predicate" x))))

(define (complement pred) (lambda (x) (not (pred x))))

(define (string-every x str)
  (let ((pred (make-char-predicate x))
        (end (string-cursor-end str)))
    (let lp ((i (string-cursor-start str)))
      (if (string-cursor>=? i end)
          #t
          (and (pred (string-cursor-ref str i))
               (lp (string-cursor-next str i)))))))

(define (string-any x str)
  (not (string-every (complement (make-char-predicate x)) str)))

(define (string-index str x . o)
  (let ((pred (make-char-predicate x))
        (end (string-cursor-end str)))
    (let lp ((i (if (pair? o) (car o) (string-cursor-start str))))
      (cond ((string-cursor>=? i end) #f)
            ((pred (string-ref str i)) i)
            (else (lp (string-cursor-next str i)))))))

(define (string-index-right str x . o)
  (let ((pred (make-char-predicate x))
        (end (string-cursor-start str)))
    (let lp ((i (if (pair? o)
                    (car o)
                    (string-cursor-prev str (string-cursor-end str)))))
      (cond ((string-cursor<? i end) #f)
            ((pred (string-ref str i)) i)
            (else (lp (string-cursor-prev str i)))))))

(define (string-skip str x . o)
  (apply string-index (complement (make-char-predicate x)) o))

(define (string-skip-right str x . o)
  (apply string-index-right (complement (make-char-predicate x)) o))

(define string-join string-concatenate)

(define (string-split str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) " ")))
        (end (string-cursor-end str)))
    (let lp ((i (string-cursor-start str)) (res '()))
      (let ((j (string-index str pred i)))
        (if j
            (lp (string-cursor-next str j)
                (cons (substring-cursor str i j) res))
            (reverse (cons (substring-cursor str i end) res)))))))

(define (string-trim-left str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) " "))))
    (substring-cursor str (string-skip str pred))))

(define (string-trim-right str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) " "))))
    (substring-cursor str
                      (string-cursor-start str)
                      (string-skip-right str pred))))

(define (string-trim str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) " "))))
    (string-trim-right (string-trim-left str pred) pred)))

(define (string-mismatch prefix str)
  (let ((end1 (string-cursor-end prefix))
        (end2 (string-cursor-end str)))
    (let lp ((i (string-cursor-start prefix))
             (j (string-cursor-start str)))
      (if (or (string-cursor>=? i end1)
              (string-cursor>=? j end2)
              (not (eq? (string-ref prefix i) (string-ref str j))))
          j
          (lp (string-cursor-next prefix i) (string-cursor-next str j))))))

(define (string-mismatch-right suffix str)
  (let ((end1 (string-cursor-start suffix))
        (end2 (string-cursor-start str)))
    (let lp ((i (string-cursor-prev suffix (string-cursor-end suffix)))
             (j (string-cursor-prev str (string-cursor-end str))))
      (if (or (string-cursor<? i end1)
              (string-cursor<? j end2)
              (not (eq? (string-ref suffix i) (string-ref str j))))
          j
          (lp (string-cursor-prev suffix i) (string-cursor-prev str j))))))

;; TODO: These definitions are specific to the Chibi implementation of
;; cursors.  Possibly the mismatch API should be modified to allow an
;; efficient portable definition.
(define (string-prefix? prefix str)
  (= (string-cursor-end prefix) (string-mismatch prefix str)))

(define (string-suffix? suffix str)
  (= (string-cursor-prev suffix (string-cursor-start suffix))
     (- (string-mismatch-right suffix str)
        (- (string-cursor-end str) (string-cursor-end suffix)))))

(define (string-fold kons knil str)
  (let ((end (string-cursor-end str)))
    (let lp ((i (string-cursor-start str)) (acc knil))
      (if (string-cursor>=? i end)
          acc
          (lp (string-cursor-next str i)
              (kons (string-cursor-ref str i) acc))))))

(define (string-fold-right kons knil str)
  (let ((end (string-cursor-end str)))
    (let lp ((i (string-cursor-start str)))
      (if (string-cursor>=? i end)
          knil
          (kons (string-cursor-ref str i) (lp (string-cursor-next str i)))))))

(define (string-count str x)
  (let ((pred (make-char-predicate x)))
    (string-fold (lambda (ch count) (if (pred ch) (+ count 1) count)) 0 str)))

(define (string-for-each proc str)
  (let ((end (string-cursor-end str)))
    (let lp ((i (string-cursor-start str)))
      (cond ((string-cursor<? i end)
             (proc (string-cursor-ref str i))
             (lp (string-cursor-next str i)))))))

(define (string-map proc str)
  (call-with-output-string
    (lambda (out)
      (string-for-each (lambda (ch) (write-char (proc ch) out)) str))))

(define (make-string-searcher needle)
  (lambda (haystack) (string-contains haystack needle)))
