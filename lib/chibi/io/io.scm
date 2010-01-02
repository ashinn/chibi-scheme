;; io.scm -- various input/output utilities
;; Copyright (c) 2010 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define eof
  (call-with-input-string " "
    (lambda (in) (read-char in) (read-char in))))

(define (string-copy! dst start src from to)
  (do ((i from (+ i 1)) (j start (+ j 1)))
      ((>= i to))
    (string-set! dst j (string-ref src i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reading and writing

(define (write-line str . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (display str out)
    (newline out)))

(define (read-line . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (n (if (and (pair? o) (pair? (cdr o))) (car (cdr o)) 8192)))
    (let ((res (%read-line n in)))
      (if (not res) eof res))))

(define (read-string n . o)
  (let ((in (if (pair? o) (car o) (current-input-port))))
    (let ((res (%read-string n in)))
      (if (if (pair? res) (= 0 (car res)) #t)
          eof
          (cadr res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; higher order port operations

(define (port-fold kons knil . o)
  (let ((read (if (pair? o) (car o) read))
        (in (if (and (pair? o) (pair? (cdr o)))
                (car (cdr o))
                (current-input-port))))
    (let lp ((acc knil))
      (let ((x (read in)))
        (if (eof-object? x) acc (lp (kons x acc)))))))

(define (port-fold-right kons knil . o)
  (let ((read (if (pair? o) (car o) read))
        (in (if (and (pair? o) (pair? (cdr o)))
                (car (cdr o))
                (current-input-port))))
    (let lp ()
      (let ((x (read in)))
        (if (eof-object? x) knil (kons x (lp)))))))

(define (port-map fn . o)
  (reverse (apply port-fold (lambda (x ls) (cons (fn x) ls)) '() o)))

(define (port->list read in)
  (port-map (lambda (x) x) read in))

(define (port->sexp-list in)
  (port->list read in))

(define (port->string-list in)
  (port->list read-line in))

(define (port->string in)
  (string-concatenate (port->list (lambda (in) (read-string 1024 in)) in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom port utilities

(define (make-custom-input-port read . o)
  (let ((seek (and (pair? o) (car o)))
        (close (and (pair? o) (pair? (cdr o)) (car (cdr o)))))
    (%make-custom-input-port read seek close)))

(define (make-custom-output-port write . o)
  (let ((seek (and (pair? o) (car o)))
        (close (and (pair? o) (pair? (cdr o)) (car (cdr o)))))
    (%make-custom-output-port write seek close)))

(define (make-null-output-port)
  (make-custom-output-port (lambda (str n) 0)))

(define (make-broadcast-port . ports)
  (make-custom-output-port
   (lambda (str n)
     (for-each (lambda (p) (write-string str n p)) ports)
     n)))

(define (make-filtered-output-port filter out)
  (make-custom-output-port
   (lambda (str n)
     (let* ((len (string-length str))
            (s1 (if (= n len) str (substring str 0 n)))
            (s2 (filter s1)))
       (if (string? s2)
           (write-string s2 (string-length s2) out))))))

(define (make-concatenated-port . ports)
  (make-custom-input-port
   (lambda (str n)
     (if (null? ports)
         0
         (let lp ((i (read-string! str n (car ports))))
           (cond
            ((>= i n)
             i)
            (else
             (set! ports (cdr ports))
             (cond
              ((null? ports)
               i)
              (else
               (let* ((s (read-string (- n i) (car ports)))
                      (len (if (string? s) (string-length s) 0)))
                 (if (and (string? str) (> len 0))
                     (string-copy! str i s 0 len))
                 (lp (+ i len))))))))))))

(define (make-generated-input-port generator)
  (let ((buf "")
        (len 0)
        (offset 0))
    (make-custom-input-port
     (lambda (str n)
       (cond
        ((>= (- len offset) n)
         (string-copy! str 0 buf offset (+ offset n))
         (set! offset (+ offset n))
         n)
        (else
         (string-copy! str 0 buf offset len)
         (let lp ((i (- len offset)))
           (set! buf (generator))
           (cond
            ((not (string? buf))
             (set! buf "")
             (set! len 0)
             (set! offset 0)
             (- n i))
            (else
             (set! len (string-length buf))
             (set! offset 0)
             (cond
              ((>= (- len offset) (- n i))
               (string-copy! str i buf offset (+ offset (- n i)))
               (set! offset (+ offset (- n i)))
               n)
              (else
               (string-copy! str i buf offset len)
               (lp (+ i (- len offset))))))))))))))

(define (make-filtered-input-port filter in)
  (make-generated-input-port
   (lambda ()
     (let ((res (read-string 1024 in)))
       (if (string? res) (filter res) res)))))
