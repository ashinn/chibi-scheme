
(import (srfi 69))

(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test expect expr)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((str (call-with-output-string
                    (lambda (out)
                      (write *tests-run*)
                      (display ". ")
                      (display 'expr out))))
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

(test
 'white
 (let ((ht (make-hash-table eq?)))
   (hash-table-set! ht 'cat 'black)
   (hash-table-set! ht 'dog 'white)
   (hash-table-set! ht 'elephant 'pink)
   (hash-table-ref/default ht 'dog #f)))

(test
 'white
 (let ((ht (make-hash-table equal?)))
   (hash-table-set! ht "cat" 'black)
   (hash-table-set! ht "dog" 'white)
   (hash-table-set! ht "elephant" 'pink)
   (hash-table-ref/default ht "dog" #f)))

(test
 'white
 (let ((ht (make-hash-table string-ci=? string-ci-hash)))
   (hash-table-set! ht "cat" 'black)
   (hash-table-set! ht "dog" 'white)
   (hash-table-set! ht "elephant" 'pink)
   (hash-table-ref/default ht "DOG" #f)))

(test 625
 (let ((ht (make-hash-table)))
   (do ((i 0 (+ i 1))) ((= i 1000))
     (hash-table-set! ht i (* i i)))
   (hash-table-ref/default ht 25 #f)))

(test-report)

