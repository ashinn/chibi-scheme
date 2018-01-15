;;; Copyright (C) William D Clinger (2016).
;;; 
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

(define-library (srfi 135 kernel8)

  (export

   ;; for internal use only

   complain               ; for reporting illegal arguments

   text-rtd               ; FIXME: for debugging only
   %new-text              ; FIXME: for debugging only
   text.k text.chunks     ; FIXME: for debugging only

   %text-length           ; non-checking version
   %text-ref              ; non-checking version
;  %subtext               ; non-checking version
   %string->text          ; 1-argument version

   N                      ; preferred text size for pieces of long texts
   the-empty-text         ; there should be only one empty text

   ;; will be exported by (srfi 135)

   text?
   text-tabulate
   text-length
   text-ref
   subtext
   textual-concatenate
   )

  (import (scheme base))

  (include "kernel8.body.scm"))
