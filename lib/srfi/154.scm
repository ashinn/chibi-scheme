;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights Reserved.

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
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-syntax closed-lambda
  (syntax-rules ()
    ((closed-lambda formals body)
     (let ((dynamic-environment (current-dynamic-environment)))
       (lambda formals
         (with-dynamic-environment dynamic-environment (lambda ()
                                                         body)))))))

(define-record-type <dynamic-environment>
  (make-dynamic-environment proc)
  dynamic-environment?
  (proc dynamic-environment-proc))


(define (current-dynamic-environment)
  (call-with-current-continuation
   (lambda (return)
     (let-values
         (((k thunk)
           (call-with-current-continuation
            (lambda (c)
              (return
               (make-dynamic-environment (lambda (thunk)
                                           (call-with-current-continuation
                                            (lambda (k)
                                              (c k thunk))))))))))
       (call-with-values thunk k)))))


(define (with-dynamic-environment dynamic-environment thunk)
  ((dynamic-environment-proc dynamic-environment) thunk))
