;;;; `vector-edit' tests

;;; Copyright MMIV-MMXV Arthur A. Gleckler.  All rights reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

(define (run-vector-edit-tests)
  (test-begin "vector-edit")

  (test-group "(vector-without)"
    (define (check expected start end)
      (let ((v #(0 1 2 3 4)))
	(test expected (vector-without v start end))))
    (check #(0 1 2 3 4) 0 0)
    (check #() 0 5)
    (check #(1 2 3 4) 0 1)
    (check #(2 3 4) 0 2)
    (check #(0 2 3 4) 1 2)
    (check #(0 3 4) 1 3)
    (check #(0 1 4) 2 4)
    (check #(0 1 2 3) 4 5)
    (check #(0 1 2) 3 5))

  (test-group "(vector-edit empty)"
    (let ((array (vector 0 1 2)))
      (test #(0 1 2) (vector-edit array))))

  (test-group "(vector-edit adjacent-adds)"
    (let ((array (vector 0 1 2)))
      (test #(0 1 2 3 4)
	    (vector-edit array
			 (add 3 3)
			 (add 3 4)))))

  (test-group "(vector-edit adjacent-drops)"
    (let ((array (vector 0 1 2 3 4 5)))
      (test #(0 1 2)
	    (vector-edit array
			 (drop 3 1)
			 (drop 4 2)))))

  (test-group "(vector-edit alternating-add-drop)"
    (let ((array (vector 0 1 2 2 2 3 4 6 6 6 6 7 9)))
      (test #(0 1 2 3 4 5 6 7 8 9)
	    (vector-edit array
			 (drop 3 2)
			 (add 7 5)
			 (drop 8 3)
			 (add 12 8)))))

  (test-end))