
(import (srfi 95))

(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test name expr expect)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((str (call-with-output-string (lambda (out) (display name out))))
             (res expr))
         (display str)
         (write-char #\space)
         (display (make-string (max 0 (- 72 (string-length str))) #\.))
         (flush-output)
         (cond
          ((equal? res expect)
           (set! *tests-passed* (+ *tests-passed* 1))
           (display " [PASS]\n"))
          (else
           (display " [FAIL]\n")
           (display "    expected ") (write expect)
           (display " but got ") (write res) (newline))))))))

(define (test-report)
  (write *tests-passed*)
  (display " out of ")
  (write *tests-run*)
  (display " passed (")
  (write (* (/ *tests-passed* *tests-run*) 100))
  (display "%)")
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run tests

(test "sort null" (sort '()) '())
(test "sort null <" (sort '() <) '())
(test "sort null < car" (sort '() < car) '())
(test "sort list" (sort '(7 5 2 8 1 6 4 9 3)) '(1 2 3 4 5 6 7 8 9))
(test "sort list <" (sort '(7 5 2 8 1 6 4 9 3) <) '(1 2 3 4 5 6 7 8 9))
(test "sort list < car" (sort '((7) (5) (2) (8) (1) (6) (4) (9) (3)) < car)
  '((1) (2) (3) (4) (5) (6) (7) (8) (9)))
(test "sort list (lambda (a b) (< (car a) (car b)))"
    (sort '((7) (5) (2) (8) (1) (6) (4) (9) (3))
          (lambda (a b) (< (car a) (car b))))
  '((1) (2) (3) (4) (5) (6) (7) (8) (9)))

(test-report)
