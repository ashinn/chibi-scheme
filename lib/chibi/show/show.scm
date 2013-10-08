;; show.scm -- additional combinator formatters
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacing

;;> Output a single newline.
(define nl (displayed "\n"))

;;> "Fresh line" - output a newline iff we're not at the start of a
;;> fresh line.
(define fl
  (fn (col) (if (zero? col) nothing nl)))

;;> Move to a given tab-stop (using spaces, not tabs).
(define (tab-to . o)
  (fn (col pad-char)
    (let* ((tab-width (if (pair? o) (car o) 8))
           (rem (modulo col tab-width)))
      (if (positive? rem)
          (displayed (make-string (- tab-width rem) pad-char))
          nothing))))

;;> Move to an explicit column.
(define (space-to where)
  (fn (col pad-char)
    (displayed (make-string (max 0 (- where col)) pad-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String transformations

(define (with-string-transformer proc . ls)
  (fn (output)
    (let ((output* (lambda (str) (fn () (output (proc str))))))
      (with ((output output*)) (each-in-list ls)))))

(define (upcased . ls) (apply with-string-transformer string-upcase ls))
(define (downcased . ls) (apply with-string-transformer string-downcase ls))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Padding and trimming

(define (padded/both width . ls)
  (call-with-output
   (each-in-list ls)
   (lambda (str)
     (fn (string-width pad-char)
       (let ((diff (- width (string-width str))))
         (if (positive? diff)
             (let* ((diff/2 (quotient diff 2))
                    (left (make-string diff/2 pad-char))
                    (right (if (even? diff)
                               left
                               (make-string (+ 1 diff/2) pad-char))))
               (each right str left))
             (displayed str)))))))

(define (padded width . ls)
  (fn ((col1 col))
    (each (each-in-list ls)
          (fn ((col2 col) pad-char)
            (displayed (make-string (max 0 (- width (- col2 col1)))
                                    pad-char))))))

(define padded/right padded)

(define (padded/left width . ls)
  (call-with-output
   (each-in-list ls)
   (lambda (str)
     (fn (string-width pad-char)
       (let ((diff (- width (string-width str))))
         (each (make-string diff pad-char) str))))))

;; General buffered trim - capture the output apply a trimmer.
(define (trimmed/buffered width producer proc)
  (call-with-output
   producer
   (lambda (str)
     (fn (string-width)
       (let* ((str-width (string-width str))
              (diff (- str-width width)))
         (displayed (if (positive? diff)
                        (proc str str-width diff)
                        str)))))))

(define (trimmed width . ls)
  (trimmed/buffered
   width
   (each-in-list ls)
   (lambda (str str-width diff)
     (fn (ellipsis string-width col)
       (let* ((ell (if (char? ellipsis) (string ellipsis) (or ellipsis "")))
              (ell-len (string-width ell))
              (diff (- (+ str-width ell-len) width)))
         (each (if (negative? diff)
                   nothing
                   (substring str 0 (- width ell-len)))
               ell))))))

(define trimmed/right trimmed)

(define (trimmed/left width . ls)
  (trimmed/buffered
   width
   (each-in-list ls)
   (lambda (str str-width diff)
     (fn (ellipsis string-width)
       (let* ((ell (if (char? ellipsis) (string ellipsis) (or ellipsis "")))
              (ell-len (string-width ell))
              (diff (- (+ str-width ell-len) width)))
         (each ell
               (if (negative? diff)
                   nothing
                   (substring str diff))))))))

(define (trimmed/both width . ls)
  (trimmed/buffered
   width
   (each-in-list ls)
   (lambda (str str-width diff)
     (fn (ellipsis string-width)
       (let* ((ell (if (char? ellipsis) (string ellipsis) (or ellipsis "")))
              (ell-len (string-width ell))
              (diff (- (+ str-width ell-len ell-len) width))
              (left (quotient diff 2))
              (right (- (string-width str) (quotient (+ diff 1) 2))))
         (if (negative? diff)
             ell
             (each ell (substring str left right) ell)))))))

(define (trimmed/lazy width . ls)
  (fn (orig-output string-width)
    (call-with-current-continuation
     (lambda (return)
       (let ((chars-written 0)
             (output (or orig-output output-default)))
         (define (output* str)
           (let ((len (string-width str)))
             (set! chars-written (+ chars-written len))
             (if (> chars-written width)
                 (let* ((end (max 0 (- len (- chars-written width))))
                        (s (substring str 0 end)))
                   (each (output s)
                         (update! (output orig-output))
                         (fn () (return nothing))))
                 (output str))))
         (with ((output output*))
           (each-in-list ls)))))))

(define (fitted width . ls)
  (padded width (trimmed width (each-in-list ls))))
(define fitted/right fitted)
(define (fitted/left width . ls)
  (padded/left width (trimmed/left width (each-in-list ls))))
(define (fitted/both width . ls)
  (padded/both width (trimmed/both width (each-in-list ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joining and interspersing

(define (joined/general elt-f last-f dot-f init-ls sep)
  (fn ()
    (let lp ((ls init-ls))
      (cond
       ((pair? ls)
        (each (if (eq? ls init-ls) nothing sep)
              ((if (and last-f (null? (cdr ls))) last-f elt-f) (car ls))
              (lp (cdr ls))))
       ((and dot-f (not (null? ls)))
        (each (if (eq? ls init-ls) nothing sep) (dot-f ls)))
       (else
        nothing)))))

(define (joined elt-f ls . o)
  (joined/general elt-f #f #f ls (if (pair? o) (car o) "")))

(define (joined/prefix elt-f ls . o)
  (if (null? ls)
      nothing
      (let ((sep (if (pair? o) (car o) "")))
        (each sep (joined elt-f ls sep)))))

(define (joined/suffix elt-f ls . o)
  (if (null? ls)
      nothing
      (let ((sep (if (pair? o) (car o) "")))
        (each (joined elt-f ls sep) sep))))

(define (joined/last elt-f last-f ls . o)
  (joined/general elt-f last-f #f ls (if (pair? o) (car o) "")))

(define (joined/dot elt-f dot-f ls . o)
  (joined/general elt-f #f dot-f ls (if (pair? o) (car o) "")))
