;; Adapted for R7RS from original SRFI 41 r5rs.ss.

;; Copyright (C) 2007 by Philip L. Bewig of Saint Louis, Missouri,
;; USA.  All rights reserved.  Permission is hereby granted, free of
;; charge, to any person obtaining a copy of this software and
;; associated documentation files (the "Software"), to deal in the
;; Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute,
;; sublicense, and/or sell copies of the Software, and to permit
;; persons to whom the Software is furnished to do so, subject to the
;; following conditions: The above copyright notice and this
;; permission notice shall be included in all copies or substantial
;; portions of the Software.  THE SOFTWARE IS PROVIDED "AS IS",
;; WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
;; LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; unit tests

(define-library (srfi 41 test)
  (import (scheme base) (srfi 41) (chibi test))
  (export run-tests)
  (begin

    (define (add1 n) (+ n 1))
    (define strm123 (stream 1 2 3))
    (define (lsec proc . args)
      (lambda x (apply proc (append args x))))
    (define (rsec proc . args)
      (lambda x (apply proc (reverse (append (reverse args) (reverse x))))))
    (define nats
      (stream-cons 0 (stream-map add1 nats)))

    ;; executing (run-tests) should produce no output
    (define (run-tests)

      (test-begin "srfi-41: streams")

      ;; stream-null
      (test #t (stream? stream-null))
      (test #t (stream-null? stream-null))
      (test #f (stream-pair? stream-null))
      
      ;; stream-cons
      (test #t (stream? (stream-cons 1 stream-null)))
      (test #f (stream-null? (stream-cons 1 stream-null)))
      (test #t (stream-pair? (stream-cons 1 stream-null)))
      
      ;; stream?
      (test #t (stream? stream-null))
      (test #t (stream? (stream-cons 1 stream-null)))
      (test #f (stream? "four"))
      
      ;; stream-null?
      (test #t (stream-null? stream-null))
      (test #f (stream-null? (stream-cons 1 stream-null)))
      (test #f (stream-null? "four"))
      
      ;; stream-pair?
      (test #f (stream-pair? stream-null))
      (test #t (stream-pair? (stream-cons 1 stream-null)))
      (test #f (stream-pair? "four"))
      
      ;; stream-car
      (test-error (stream-car "four")) ; "stream-car: non-stream"
      (test-error (stream-car stream-null)) ; "stream-car: null stream"
      (test 1 (stream-car strm123))
      
      ;; stream-cdr
      (test-error (stream-cdr "four")) ; "stream-cdr: non-stream"
      (test-error (stream-cdr stream-null)) ; "stream-cdr: null stream"
      (test 2 (stream-car (stream-cdr strm123)))

      ;; stream-lambda
      (test
          '(2 4 6)
          (stream->list
           (letrec ((double
                     (stream-lambda (strm)
                                    (if (stream-null? strm)
                                        stream-null
                                        (stream-cons
                                         (* 2 (stream-car strm))
                                         (double (stream-cdr strm)))))))
             (double strm123))))

      ;; define-stream
      (test
          '(2 4 6)
          (stream->list
           (let ()
             (define-stream (double strm)
               (if (stream-null? strm)
                   stream-null
                   (stream-cons
                    (* 2 (stream-car strm))
                    (double (stream-cdr strm)))))
             (double strm123))))

      ;; list->stream
      (test-error (list->stream "four")) ; "list->stream: non-list argument"
      (test '() (stream->list (list->stream '())))
      (test '(1 2 3) (stream->list (list->stream '(1 2 3))))

      ;; port->stream
      (let* ((p (open-input-string "; Copyright 2007"))
             (s (port->stream p)))
        (test-error (port->stream "four"))
        (test "; Copyright" (list->string (stream->list 11 s)) )
        (close-input-port p))

      ;; stream
      (test '() (stream->list (stream)))
      (test '(1) (stream->list (stream 1)))
      (test '(1 2 3) (stream->list (stream 1 2 3)))

      ;; stream->list
      (test-error (stream->list '())) ; "stream->list: non-stream argument"
      (test-error (stream->list "four" strm123)) ; "stream->list: non-integer count"
      (test-error (stream->list -1 strm123)) ; "stream->list: negative count"
      (test '() (stream->list (stream)))
      (test '(1 2 3) (stream->list strm123))
      (test '(1 2 3) (stream->list 5 strm123))
      (test '(1 2 3) (stream->list 3 (stream-from 1)))
      
      ;; stream-append
      (test-error (stream-append "four")) ; "stream-append: non-stream argument"
      (test '(1 2 3) (stream->list (stream-append strm123)))
      (test '(1 2 3 1 2 3) (stream->list (stream-append strm123 strm123)))
      (test '(1 2 3 1 2 3 1 2 3)
          (stream->list (stream-append strm123 strm123 strm123)))
      (test '(1 2 3) (stream->list (stream-append strm123 stream-null)))
      (test '(1 2 3) (stream->list (stream-append stream-null strm123)))
      
      ;; stream-concat
      (test-error (stream-concat "four")) ; "stream-concat: non-stream argument"
      (test '(1 2 3) (stream->list (stream-concat (stream strm123))))
      (test '(1 2 3 1 2 3)
          (stream->list (stream-concat (stream strm123 strm123))))

      ;; stream-constant
      (test 1 (stream-ref (stream-constant 1) 100))
      (test 1 (stream-ref (stream-constant 1 2) 100))
      (test 1 (stream-ref (stream-constant 1 2 3) 3))

      ;; stream-drop
      (test-error (stream-drop "four" strm123)) ; "stream-drop: non-integer argument"
      (test-error (stream-drop -1 strm123)) ; "stream-drop: negative argument"
      (test-error (stream-drop 2 "four")) ; "stream-drop: non-stream argument"
      (test '() (stream->list (stream-drop 0 stream-null)))
      (test '(1 2 3) (stream->list (stream-drop 0 strm123)))
      (test '(2 3) (stream->list (stream-drop 1 strm123)))
      (test '() (stream->list (stream-drop 5 strm123)))

      ;; stream-drop-while
      (test-error ; "stream-drop-while: non-procedural argument"
       (stream-drop-while "four" strm123))
      (test-error ; "stream-drop-while: non-stream argument"
       (stream-drop-while odd? "four"))
      (test '() (stream->list (stream-drop-while odd? stream-null)))
      (test '(2 3) (stream->list (stream-drop-while odd? strm123)))
      (test '(1 2 3) (stream->list (stream-drop-while even? strm123)))
      (test '() (stream->list (stream-drop-while positive? strm123)))
      (test '(1 2 3) (stream->list (stream-drop-while negative? strm123)))

      ;; stream-filter
      (test-error ; "stream-filter: non-procedural argument"
       (stream-filter "four" strm123))
      (test-error (stream-filter odd? '())) ; "stream-filter: non-stream argument"
      (test #t (stream-null? (stream-filter odd? (stream))))
      (test '(1 3) (stream->list (stream-filter odd? strm123)))
      (test '(2) (stream->list (stream-filter even? strm123)))
      (test '(1 2 3) (stream->list (stream-filter positive? strm123)))
      (test '() (stream->list (stream-filter negative? strm123)))
      (let loop ((n 10))
        (test #t (odd? (stream-ref (stream-filter odd? (stream-from 0)) n)))
        (if (positive? n) (loop (- n 1))))
      (let loop ((n 10))
        (test #f (even? (stream-ref (stream-filter odd? (stream-from 0)) n)))
        (if (positive? n) (loop (- n 1))))

      ;; stream-fold
      (test-error ; "stream-fold: non-procedural argument"
       (stream-fold "four" 0 strm123))
      (test-error (stream-fold + 0 '())) ; "stream-fold: non-stream argument"
      (test 6 (stream-fold + 0 strm123))

      ;; stream-for-each
      (test-error ; "stream-for-each: non-procedural argument"
       (stream-for-each "four" strm123))
      (test-error ; "stream-for-each: no stream arguments"
       (stream-for-each +))
      (test-error ; "stream-for-each: non-stream argument"
       (stream-for-each + "four"))
      (test 6
          (let ((sum 0))
            (stream-for-each (lambda (x) (set! sum (+ sum x))) strm123)
            sum))

      ;; stream-from
      (test-error (stream-from "four")) ; "stream-from: non-numeric starting number"
      (test-error (stream-from 1 "four")) ; "stream-from: non-numeric step size"
      (test 100 (stream-ref (stream-from 0) 100))
      (test 201 (stream-ref (stream-from 1 2) 100))
      (test -100 (stream-ref (stream-from 0 -1) 100))

      ;; stream-iterate
      (test-error (stream-iterate "four" 0)) ; "stream-iterate: non-procedural argument"
      (test '(1 2 3) (stream->list 3 (stream-iterate (lsec + 1) 1)))

      ;; stream-length
      (test-error (stream-length "four")) ; "stream-length: non-stream argument"
      (test 0 (stream-length (stream)))
      (test 3 (stream-length strm123))

      ;; stream-let
      (test '(2 4 6)
          (stream->list
           (stream-let loop ((strm strm123))
                       (if (stream-null? strm)
                           stream-null
                           (stream-cons
                            (* 2 (stream-car strm))
                            (loop (stream-cdr strm)))))))

      ;; stream-map
      (test-error (stream-map "four" strm123)) ; "stream-map: non-procedural argument"
      (test-error (stream-map odd?)) ; "stream-map: no stream arguments"
      (test-error (stream-map odd? "four")) ; "stream-map: non-stream argument"
      (test '(-1 -2 -3) (stream->list (stream-map - strm123)))
      (test '(2 4 6) (stream->list (stream-map + strm123 strm123)))
      (test '(2 4 6) (stream->list (stream-map + strm123 (stream-from 1))))
      (test '(2 4 6) (stream->list (stream-map + (stream-from 1) strm123)))
      (test '(3 6 9) (stream->list (stream-map + strm123 strm123 strm123)))

      ;; stream-match
      (test-error (stream-match '(1 2 3) (_ 'ok))) ; "stream-match: non-stream argument"
      (test-error (stream-match strm123 (() 42))) ; "stream-match: pattern failure"
      (test 'ok (stream-match stream-null (() 'ok)))
      (test 'ok (stream-match strm123 (() 'no) (else 'ok)))
      (test 1 (stream-match (stream 1) (() 'no) ((a) a)))
      (test 'ok (stream-match (stream 1) (() 'no) ((_) 'ok)))
      (test '(1 2 3) (stream-match strm123 ((a b c) (list a b c))))
      (test 1 (stream-match strm123 ((a . _) a)))
      (test '(1 2) (stream-match strm123 ((a b . _) (list a b))))
      (test '(1 2 3)
          (stream-match strm123 ((a b . c) (list a b (stream-car c)))))
      (test '(1 2 3) (stream-match strm123 (s (stream->list s))))
      (test 'ok (stream-match strm123 ((a . _) (= a 1) 'ok)))
      (test 'no (stream-match strm123 ((a . _) (= a 2) 'yes) (_ 'no)))
      (test 'no (stream-match strm123 ((a b c) (= a b) 'yes) (_ 'no)))
      (test 'yes (stream-match (stream 1 1 2) ((a b c) (= a b) 'yes) (_ 'no)))

      ;; stream-of
      (test '(7 15 31)
          (stream->list
           (stream-of (+ y 6)
                      (x in (stream-range 1 6))
                      (odd? x)
                      (y is (* x x)))))
      (test '(1 2 3 4 2 4 6 8 3 6 9 12)
          (stream->list
           (stream-of (* x y)
                      (x in (stream-range 1 4))
                      (y in (stream-range 1 5)))))
      (test 1 (stream-car (stream-of 1)))

      ;; stream-range
      (test-error (stream-range "four" 0)) ; "stream-range: non-numeric starting number"
      (test-error (stream-range 0 "four")) ; "stream-range: non-numeric ending number"
      (test-error (stream-range 1 2 "three")) ; "stream-range: non-numeric step size"
      (test '(0 1 2 3 4) (stream->list (stream-range 0 5)))
      (test '(5 4 3 2 1) (stream->list (stream-range 5 0)))
      (test '(0 2 4) (stream->list (stream-range 0 5 2)))
      (test '(5 3 1) (stream->list (stream-range 5 0 -2)))
      (test '() (stream->list (stream-range 0 1 -1)))

      ;; stream-ref
      (test-error (stream-ref '() 4)) ; "stream-ref: non-stream argument"
      (test-error (stream-ref nats 3.5)) ; "stream-ref: non-integer argument"
      (test-error (stream-ref nats -3)) ; "stream-ref: negative argument"
      (test-error (stream-ref strm123 5)) ; "stream-ref: beyond end of stream"
      (test 1 (stream-ref strm123 0))
      (test 2 (stream-ref strm123 1))
      (test 3 (stream-ref strm123 2))

      ;; stream-reverse
      (test-error (stream-reverse '())) ; "stream-reverse: non-stream argument"
      (test '() (stream->list (stream-reverse (stream))))
      (test '(3 2 1) (stream->list (stream-reverse strm123)))

      ;; stream-scan
      (test-error ; "stream-scan: non-procedural argument"
       (stream-scan "four" 0 strm123))
      (test-error (stream-scan + 0 '())) ; "stream-scan: non-stream argument"
      (test '(0 1 3 6) (stream->list (stream-scan + 0 strm123)))

      ;; stream-take
      (test-error (stream-take 5 "four")) ; "stream-take: non-stream argument"
      (test-error (stream-take "four" strm123)) ; "stream-take: non-integer argument"
      (test-error (stream-take -4 strm123)) ; "stream-take: negative argument"
      (test '() (stream->list (stream-take 5 stream-null)))
      (test '() (stream->list (stream-take 0 stream-null)))
      (test '() (stream->list (stream-take 0 strm123)))
      (test '(1 2) (stream->list (stream-take 2 strm123)))
      (test '(1 2 3) (stream->list (stream-take 3 strm123)))
      (test '(1 2 3) (stream->list (stream-take 5 strm123)))

      ;; stream-take-while
      (test-error ; "stream-take-while: non-stream argument"
       (stream-take-while odd? "four"))
      (test-error ; "stream-take-while: non-procedural argument"
       (stream-take-while "four" strm123))
      (test '(1) (stream->list (stream-take-while odd? strm123)))
      (test '() (stream->list (stream-take-while even? strm123)))
      (test '(1 2 3) (stream->list (stream-take-while positive? strm123)))
      (test '() (stream->list (stream-take-while negative? strm123)))

      ;; stream-unfold
      (test-error ; "stream-unfold: non-procedural mapper"
       (stream-unfold "four" odd? + 0))
      (test-error ; "stream-unfold: non-procedural pred?"
       (stream-unfold + "four" + 0))
      (test-error ; "stream-unfold: non-procedural generator"
       (stream-unfold + odd? "four" 0))
      (test '(0 1 4 9 16 25 36 49 64 81)
          (stream->list (stream-unfold (rsec expt 2) (rsec < 10) (rsec + 1) 0)))

      ;; stream-unfolds
      (test
          '(0 1 2 3 4)
          (stream->list
           (stream-unfolds
            (lambda (x)
              (let ((n (car x)) (s (cdr x)))
                (if (zero? n)
                    (values 'dummy '())
                    (values
                     (cons (- n 1) (stream-cdr s))
                     (list (stream-car s))))))
            (cons 5 (stream-from 0)))))

      ;; stream-zip
      (test-error (stream-zip)) ; "stream-zip: no stream arguments"
      (test-error (stream-zip "four")) ; "stream-zip: non-stream argument"
      (test-error (stream-zip strm123 "four")) ; "stream-zip: non-stream argument"
      (test '() (stream->list (stream-zip strm123 stream-null)))
      (test '((1) (2) (3)) (stream->list (stream-zip strm123)))
      (test '((1 1) (2 2) (3 3)) (stream->list (stream-zip strm123 strm123)))
      (test '((1 1) (2 2) (3 3))
          (stream->list (stream-zip strm123 (stream-from 1))))
      (test '((1 1 1) (2 2 2) (3 3 3))
          (stream->list (stream-zip strm123 strm123 strm123)))

      (test-end))))
