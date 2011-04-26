
(cond-expand
 (modules (import (srfi 69) (only (chibi test) test-begin test test-end)))
 (else #f))

(test-begin "hash")

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

(test-end)

