;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
(define-library (srfi 167 memory test)

  (export run-tests)

  (import (scheme base))
  (import (chibi test))
  (import (srfi 158))
  (import (srfi 167 memory))
  (import (srfi 167 engine))

  (begin

    (define (run-tests)

      (define engine (make-default-engine))

      (test
       #t
       (let ((okvs (engine-open engine #f)))
         (engine-close engine okvs)
         #t))

      (test
       #vu8(1 2 3 42)
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #vu8(13 37) #vu8(1 2 3 42))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (engine-ref engine transaction #vu8(13 37))))))
           (engine-close engine okvs)
           out)))

      (test
       #vu8(42)
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #vu8(13 37) #vu8(1 2 3 42))))
         ;; overwrite
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #vu8(13 37) #vu8(42))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (engine-ref engine transaction #vu8(13 37))))))
           (engine-close engine okvs)
           out)))

      (test
       (list (cons #vu8(20 16) #vu8(2)) (cons #vu8(20 17) #vu8(3)))
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #vu8(20 18) #vu8(4))
                                  (engine-set! engine transaction #vu8(20 16) #vu8(2))
                                  (engine-set! engine transaction #vu8(20 15) #vu8(1))
                                  (engine-set! engine transaction #vu8(20 19) #vu8(5))
                                  (engine-set! engine transaction #vu8(20 17) #vu8(3))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list
                                              (engine-range engine transaction #vu8(20 16) #t #vu8(20 18) #f))))))
           (engine-close engine okvs)
           out)))

      (test
       (list (cons #vu8(20 16) #vu8(2)) (cons #vu8(20 17 01) #vu8(3)))
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #vu8(20 18) #vu8(4))
                                  (engine-set! engine transaction #vu8(20 16) #vu8(2))
                                  (engine-set! engine transaction #vu8(20 15) #vu8(1))
                                  (engine-set! engine transaction #vu8(20 19) #vu8(5))
                                  ;; #vu8(20 17 01) lexicographically less than #vu8(20 18)
                                  (engine-set! engine transaction #vu8(20 17 01) #vu8(3))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list
                                              (engine-range engine transaction #vu8(20 16) #t #vu8(20 18) #f))))))
           (engine-close engine okvs)
           out)))

      (test
       '((#vu8(20 16) . #vu8(2))
         (#vu8(20 16 1) . #vu8(2))
         (#vu8(20 17) . #vu8(3))
         (#vu8(20 17 1) . #vu8(2)))
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #vu8(20 17 01) #vu8(2))
                                  (engine-set! engine transaction #vu8(20 17) #vu8(3))
                                  (engine-set! engine transaction #vu8(42 42) #vu8(5))
                                  (engine-set! engine transaction #vu8(01 02) #vu8(1))
                                  (engine-set! engine transaction #vu8(20 16) #vu8(2))
                                  (engine-set! engine transaction #vu8(20 16 01) #vu8(2))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list (engine-prefix-range engine transaction #vu8(20)))))))
           (engine-close engine okvs)
           out)))

      (test
       '(
         (#vu8(20 17) . #vu8(3))
         (#vu8(20 16 1) . #vu8(2))
         )
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #vu8(20 17 01) #vu8(2))
                                  (engine-set! engine transaction #vu8(20 17) #vu8(3))
                                  (engine-set! engine transaction #vu8(42 42) #vu8(5))
                                  (engine-set! engine transaction #vu8(01 02) #vu8(1))
                                  (engine-set! engine transaction #vu8(20 16) #vu8(2))
                                  (engine-set! engine transaction #vu8(20 16 01) #vu8(2))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list (engine-prefix-range engine transaction
                                                                                   #vu8(20)
                                                                                   '((offset . 1)
                                                                                     (limit . 2)
                                                                                     (reverse? #t))))))))
           (engine-close engine okvs)
           out)))

      (test
       '()
       (let ((keys '(#vu8(1 42 0 20 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0 21 102 21 103)
                         #vu8(1 42 0 21 1 21 102 21 103 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0)
                         #vu8(1 42 0 21 2 21 103 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0 21 102))))
         (let ((okvs (engine-open engine #f)))
           ;; set
           (engine-in-transaction engine okvs
                                  (lambda (transaction)
                                    (let loop ((keys keys))
                                      (unless (null? keys)
                                        (engine-set! engine transaction (car keys) #vu8(2))
                                        (loop (cdr keys))))))
           ;; get
           (let* ((prefix #vu8(1 42 0 20 2 57 98 57 55 54 97 104 97 104 50 51 113 110 52 102 121 97 99 49 53 120 99 118 48 100 0))
                  (out (engine-in-transaction engine okvs
                                              (lambda (transaction)
                                                (generator->list (engine-prefix-range engine transaction prefix))))))
             (engine-close engine okvs)
             out)))))))
