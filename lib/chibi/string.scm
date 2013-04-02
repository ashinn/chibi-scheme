;; strings.scm -- cursor-oriented string library
;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (string-null? str)
  (equal? str ""))

(define (make-char-predicate x)
  (cond ((procedure? x) x)
        ((char? x) (lambda (ch) (eq? ch x)))
        ((char-set? x) (lambda (ch) (char-set-contains? x ch)))
        (else (error "invalid character predicate" x))))

(define (complement pred) (lambda (x) (not (pred x))))

(define (string-any x str)
  (let ((pred (make-char-predicate x))
        (end (string-cursor-end str)))
    (and (string-cursor>? end (string-cursor-start str))
         (let lp ((i (string-cursor-start str)))
           (let ((i2 (string-cursor-next str i))
                 (ch (string-cursor-ref str i)))
             (if (string-cursor>=? i2 end)
                 (pred ch)  ;; tail call
                 (or (pred ch) (lp i2))))))))

(define (string-every x str)
  (not (string-any (complement (make-char-predicate x)) str)))

(define (string-find str x . o)
  (let ((pred (make-char-predicate x))
        (end (string-cursor-end str)))
    (let lp ((i (if (pair? o) (car o) (string-cursor-start str))))
      (cond ((string-cursor>=? i end) end)
            ((pred (string-cursor-ref str i)) i)
            (else (lp (string-cursor-next str i)))))))

(define (string-find-right str x . o)
  (let ((pred (make-char-predicate x))
        (end (string-cursor-start str)))
    (let lp ((i (if (pair? o) (car o) (string-cursor-end str))))
      (let ((i2 (string-cursor-prev str i)))
        (cond ((string-cursor<? i2 end) end)
              ((pred (string-cursor-ref str i2)) i)
              (else (lp i2)))))))

(define (string-skip str x . o)
  (apply string-find str (complement (make-char-predicate x)) o))

(define (string-skip-right str x . o)
  (apply string-find-right str (complement (make-char-predicate x)) o))

(define string-join string-concatenate)

(define (string-split str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) #\space)))
        (limit (if (and (pair? o) (pair? (cdr o)))
                   (cadr o)
                   (+ 1 (string-size str))))
        (start (string-cursor-start str))
        (end (string-cursor-end str)))
    (if (string-cursor>=? start end)
        '()
        (let lp ((i start) (n 1) (res '()))
          (cond
           ((>= n limit)
            (reverse (cons (substring-cursor str i) res)))
           (else
            (let* ((j (string-find str pred i))
                   (res (cons (substring-cursor str i j) res)))
              (if (string-cursor>=? j end)
                  (reverse res)
                  (lp (string-cursor-next str j) (+ n 1) res)))))))))

(define (string-trim-left str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) #\space))))
    (substring-cursor str (string-skip str pred))))

(define (string-trim-right str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) #\space))))
    (substring-cursor str
                      (string-cursor-start str)
                      (string-skip-right str pred))))

(define (string-trim str . o)
  (let* ((pred (if (pair? o) (car o) #\space))
         (left (string-skip str pred))
         (right (string-skip-right str pred)))
    (if (string-cursor>=? left right)
        ""
        (substring-cursor str left right))))

(define (string-mismatch prefix str)
  (let ((end1 (string-cursor-end prefix))
        (end2 (string-cursor-end str)))
    (let lp ((i (string-cursor-start prefix))
             (j (string-cursor-start str)))
      (if (or (string-cursor>=? i end1)
              (string-cursor>=? j end2)
              (not (eq? (string-cursor-ref prefix i) (string-cursor-ref str j))))
          j
          (lp (string-cursor-next prefix i) (string-cursor-next str j))))))

(define (string-mismatch-right suffix str)
  (let ((end1 (string-cursor-start suffix))
        (end2 (string-cursor-start str)))
    (let lp ((i (string-cursor-prev suffix (string-cursor-end suffix)))
             (j (string-cursor-prev str (string-cursor-end str))))
      (if (or (string-cursor<? i end1)
              (string-cursor<? j end2)
              (not (eq? (string-cursor-ref suffix i) (string-cursor-ref str j))))
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

(define (string-fold kons knil str . los)
  (if (null? los)
      (let ((end (string-cursor-end str)))
	(let lp ((i (string-cursor-start str)) (acc knil))
	  (if (string-cursor>=? i end)
	      acc
	      (lp (string-cursor-next str i)
		  (kons (string-cursor-ref str i) acc)))))
      (let ((los (cons str los)))
	(let lp ((is (map string-cursor-start los))
		 (acc knil))
	  (if (any (lambda (str i)
		     (string-cursor>=? i (string-cursor-end str)))
		   los is)
	      acc
	      (lp (map string-cursor-next los is)
		  (apply kons (append (map string-cursor-ref los is)
				      (list acc)))))))))

(define (string-fold-right kons knil str)
  (let ((end (string-cursor-end str)))
    (let lp ((i (string-cursor-start str)))
      (if (string-cursor>=? i end)
          knil
          (kons (string-cursor-ref str i) (lp (string-cursor-next str i)))))))

(define (string-count str x)
  (let ((pred (make-char-predicate x)))
    (string-fold (lambda (ch count) (if (pred ch) (+ count 1) count)) 0 str)))

(define (string-for-each proc str . los)
  (if (null? los)
      (string-fold (lambda (ch a) (proc ch)) #f str)
      (let ((los (cons str los)))
	(let lp ((is (map string-cursor-start los)))
	  (cond
	   ((any (lambda (str i)
		   (string-cursor>=? i (string-cursor-end str)))
		 los is))
	   (else
	    (apply proc (map string-cursor-ref los is))
	    (lp (map string-cursor-next los is))))))))

(define (string-map proc str . los)
  (call-with-output-string
    (lambda (out)
      (apply string-for-each
	     (lambda args (write-char (apply proc args) out))
	     str los))))

(define (make-string-searcher needle)
  (lambda (haystack) (string-contains haystack needle)))
