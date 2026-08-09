;; SPDX-FileCopyrightText: 2026 Artyom Bologov
;; SPDX-License-Identifier: MIT

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

(define-syntax define-check
  (syntax-rules ()
    ((_ name predicate)
     (define name predicate))))
(define-syntax define-values-checked
  (syntax-rules ()
    ((_ (var ...) (check ...) form)
     (define-values (var ...)
       (let-values (((var ...) form))
         (values-checked (check ...) var ...))))))
(define-syntax declare-checked
  (syntax-rules ()
    ((_ name predicate)
     (when #f #f))
    ((_ (name . args))
     (when #f #f))
    ((_ (name . args) => return)
     (when #f #f))))
