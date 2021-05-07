#|
Adapted from original SRFI reference test suite:

SRFI 179: Nonempty Intervals and Generalized Arrays (Updated)

Copyright 2016, 2018, 2020 Bradley J Lucier.
All Rights Reserved.

Permission is hereby granted, free of charge,
to any person obtaining a copy of this software
and associated documentation files (the "Software"),
to deal in the Software without restriction,
including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice
(including the next paragraph) shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
|#

;;; A test program for SRFI 179:
;;; Nonempty Intervals and Generalized Arrays (Updated)

(define-library (srfi 179 test)
  (import (scheme base) (scheme cxr) (scheme complex)
          (scheme file) (scheme list) (scheme read)
          (scheme sort) (scheme vector) (scheme write)
          (chibi test)
          (srfi 27) (srfi 143) (srfi 144) (srfi 160 base) (srfi 179))
  (export run-tests)
  (begin
    ;; Just run 1 pass of the randomized tests.
    ;; TODO: remove all randomized tests.
    (define tests 1)

    (define (random a . b)
      (if (pair? b)
          (+ a (random-integer (- (car b) a)))
          (random-integer a)))

    (define (random-sample n . o)
      (let ((l (if (pair? o) (car o) 4)))
        (list->vector (map (lambda (i)
                             (random 1 l))
                           (iota n)))))

    (define (random-f64vector n)
      (let ((res (make-f64vector n)))
        (do ((i 0 (+ i 1)))
            ((= i n) res)
          (f64vector-set! res i (random-real)))))

    (define (f64vector->list vec)
      (do ((i (- (f64vector-length vec) 1) (- i 1))
           (res '() (cons (f64vector-ref vec i) res)))
          ((< i 0) res)))

    (define (random-permutation n)
      (let ((result (make-vector n)))
        ;; fill it
        (do ((i 0 (fx+ i 1)))
            ((fx=? i n))
          (vector-set! result i i))
        ;; permute it
        (do ((i 0 (fx+ i 1)))
            ((fx=? i n)
             ;;(write `(random-permutation ,n -> ,result)) (newline)
             result)
          (vector-swap! result i (random i n)))))

    (define (inverse-permutation permutation)
      (list->vector
       (map
        car
        (list-sort
         (lambda (a b) (< (cdr a) (cdr b)))
         (map cons
              (iota (vector-length permutation))
              (vector->list permutation))))))

    (define (vector-permute v permutation)
      (let* ((n (vector-length v))
             (result (make-vector n)))
        (do ((i 0 (+ i 1)))
            ((= i n) result)
          (vector-set! result i (vector-ref v (vector-ref permutation i))))))

    (define (in-order < l)
      (or (null? l)
          (null? (cdr l))
          (and (< (car l) (cadr l))
               (in-order < (cdr l)))))

    (define (local-iota a b)
      (if (= a b)
          '()
          (cons a (local-iota (+ a 1) b))))

    (define (all-elements lower upper)
      (if (null? (cdr lower))
          (map list (local-iota (car lower) (car upper)))
          (apply append (map (lambda (x)
                               (map (lambda (y)
                                      (cons x y))
                                    (all-elements (cdr lower) (cdr upper))))
                             (local-iota (car lower) (car upper))))))

    ;; define random-interval, random-multi-index

    (define (random-multi-index interval)
      (apply values
             (apply map
                    random
                    (map (lambda (bounds)
                           (bounds interval))
                         (list interval-lower-bounds->list
                               interval-upper-bounds->list)))))

    (define use-bignum-intervals #f)

    (define (random-interval . o)
      ;; a random interval with min <= dimension < max
      ;; positive and negative lower bounds
      (let* ((min (if (pair? o) (car o) 1))
             (max (if (and (pair? o) (pair? (cdr o))) (cadr o) 4))
             (lower
              (map (lambda (x)
                     (if use-bignum-intervals
                         (random (- (expt 2 90)) (expt 2 90))
                         (random -10 10)))
                   (vector->list (make-vector (random min max)))))
             (upper
              (map (lambda (x)
                     (+ (random 1 8) x))
                   lower)))
        (make-interval (list->vector lower)
                       (list->vector upper))))

    (define (random-subinterval interval)
      (let* ((lowers (interval-lower-bounds->vector interval))
             (uppers (interval-upper-bounds->vector interval))
             (new-lowers (vector-map random lowers uppers))
             (new-uppers (vector-map (lambda (x) (+ x 1))
                                     (vector-map random new-lowers uppers)))
             (subinterval (make-interval new-lowers new-uppers)))
        subinterval))

    (define (random-nonnegative-interval . o)
      ;; a random interval with min <= dimension < max
      (let* ((min (if (pair? o) (car o) 1))
             (max (if (and (pair? o) (pair? (cdr o))) (cadr o) 6))
             (lower
              (make-vector (random min max) 0))
             (upper
              (vector-map (lambda (x) (random 1 7)) lower)))
        (make-interval lower upper)))

    (define (random-positive-vector n . o)
      (let ((max (if (pair? o) (car o) 5)))
        (vector-map (lambda (x)
                      (random 1 max))
                    (make-vector n))))

    (define (random-boolean)
      (zero? (random 2)))

    (define (array-display A)
  
      (define (display-item x)
        (display x) (display "\t"))
  
      (newline)
      (case (array-dimension A)
        ((1) (array-for-each display-item A) (newline))
        ((2) (array-for-each (lambda (row)
                               (array-for-each display-item row)
                               (newline))
                             (array-curry A 1)))
        (else
         (error "array-display can't handle > 2 dimensions: " A))))

    (define (myindexer= indexer1 indexer2 interval)
      (array-fold (lambda (x y) (and x y))
                  #t
                  (make-array interval
                              (lambda args
                                (= (apply indexer1 args)
                                   (apply indexer2 args))))))

    (define (my-indexer base lower-bounds increments)
      (lambda indices
        (apply + base (map * increments (map - indices lower-bounds)))))

    (define (random-sign)
      (- 1 (* 2 (random 2))))

    (define (myarray= array1 array2)
      (and (interval= (array-domain array1)
                      (array-domain array2))
           (array-fold (lambda (vs result)
                         (and (equal? (car vs)
                                      (cadr vs))
                              result))
                       #t
                       (array-map list array1 array2))))

    (define random-storage-class-and-initializer
      (let* ((storage-classes
              (vector
               ;; generic
               (list generic-storage-class
                     (lambda args (random-permutation (length args))))
               ;; signed integer
               (list s8-storage-class
                     (lambda args (random (- (expt 2 7)) (- (expt 2 7) 1))))
               (list s16-storage-class
                     (lambda args (random (- (expt 2 15)) (- (expt 2 15) 1))))
               (list s32-storage-class
                     (lambda args (random (- (expt 2 31)) (- (expt 2 31) 1))))
               (list s64-storage-class
                     (lambda args (random (- (expt 2 63)) (- (expt 2 63) 1))))
               ;; unsigned integer
               (list u1-storage-class
                     (lambda args (random (expt 2 1))))
               (list u8-storage-class
                     (lambda args (random (expt 2 8))))
               (list u16-storage-class
                     (lambda args (random (expt 2 16))))
               (list u32-storage-class
                     (lambda args (random (expt 2 32))))
               (list u64-storage-class
                     (lambda args (random (expt 2 64))))
               ;; float
               (list f32-storage-class
                     (lambda args (random-real)))
               (list f64-storage-class
                     (lambda args (random-real)))
               ;; complex-float
               (list c64-storage-class
                     (lambda args (make-rectangular (random-real) (random-real))))
               (list c128-storage-class
                     (lambda args (make-rectangular (random-real) (random-real))))))
             (n
              (vector-length storage-classes)))
        (lambda ()
          (vector-ref storage-classes (random n)))))

    ;; Elements of extracted arrays of newly created specialized
    ;; arrays are not in order unless
    ;; (1) the differences in the upper and lower bounds of the
    ;;     first dimensions all equal 1 *and*
    ;; (2) the next dimension doesn't matter *and*
    ;; (3) the upper and lower bounds of the latter dimensions
    ;;     of the original and extracted arrays are the same
    ;; Whew!

    (define (extracted-array-elements-in-order? base extracted)
      (let ((base-domain (array-domain base))
            (extracted-domain (array-domain extracted))
            (dim (array-dimension base)))
        (let loop-1 ((i 0))
          (or (= i (- dim 1))
              (or (and (= 1 (- (interval-upper-bound extracted-domain i)
                               (interval-lower-bound extracted-domain i)))
                       (loop-1 (+ i 1)))
                  (let loop-2 ((i (+ i 1)))
                    (or (= i dim)
                        (and (= (interval-upper-bound extracted-domain i)
                                (interval-upper-bound base-domain i))
                             (= (interval-lower-bound extracted-domain i)
                                (interval-lower-bound base-domain i))
                             (loop-2 (+ i 1))))))))))


    ;; A permuted array has elements in order iff all the dimensions with
    ;; sidelength > 1 are in the same order.
    (define (permuted-array-elements-in-order? array permutation)
      (let* ((domain
              (array-domain array))
             (axes-and-limits
              (vector-map list
                          (list->vector (iota (vector-length permutation)))
                          (interval-lower-bounds->vector domain)
                          (interval-upper-bounds->vector domain)))
             (permuted-axes-and-limits
              (vector->list (vector-permute axes-and-limits permutation))))
        (in-order (lambda (x y)
                    (< (car x) (car y)))
                  (filter (lambda (l)
                            (let ((i (car l))
                                  (l (cadr l))
                                  (u (caddr l)))
                              (< 1 (- u l))))
                          permuted-axes-and-limits))))

    ;; a sampled array has elements in order iff after a string of
    ;; dimensions with side-length 1 at the beginning, all the rest
    ;; of the dimensions have sidelengths the same as the original
    (define (sampled-array-elements-in-order? base scales)
      (let* ((domain
              (array-domain base))
             (sampled-base
              (array-sample base scales))
             (scaled-domain
              (array-domain sampled-base))
             (base-sidelengths
              (vector->list
               (vector-map -
                           (interval-upper-bounds->vector domain)
                           (interval-lower-bounds->vector domain))))
             (scaled-sidelengths
              (vector->list
               (vector-map -
                           (interval-upper-bounds->vector scaled-domain)
                           (interval-lower-bounds->vector scaled-domain)))))
        (let loop-1 ((base-lengths   base-sidelengths)
                     (scaled-lengths scaled-sidelengths))
          (or (null? base-lengths)
              (if (= (car scaled-lengths) 1)
                  (loop-1 (cdr base-lengths)
                          (cdr scaled-lengths))
                  (let loop-2 ((base-lengths   base-lengths)
                               (scaled-lengths scaled-lengths))
                    (or (null? base-lengths)
                        (and (= (car base-lengths) (car scaled-lengths))
                             (loop-2 (cdr base-lengths)
                                     (cdr scaled-lengths))))))))))

    (define (multi-index< ind1 ind2)
      (and (not (null? ind1))
           (not (null? ind2))
           (or (< (car ind1)
                  (car ind2))
               (and (= (car ind1)
                       (car ind2))
                    (multi-index< (cdr ind1)
                                  (cdr ind2))))))

    (define (indices-in-proper-order l)
      (or (null? l)
          (null? (cdr l))
          (and (multi-index< (car l)
                             (cadr l))
               (indices-in-proper-order (cdr l)))))

    ;; OK, how to test array-reduce?

    ;; Well, we take an associative, non-commutative operation,
    ;; multiplying 2x2 matrices, with data such that doing operations
    ;; in the opposite order gives the wrong answer, doing it for the
    ;; wrong interval (e.g., swapping axes) gives the wrong answer.

    ;; This is not in the same style as the other tests, which use random
    ;; data to a great extent, but I couldn't see how to choose random
    ;; data that would satisfy the constraints.

    (define matrix vector)

    (define (x2x2-multiply A B)
      (let ((a_11 (vector-ref A 0)) (a_12 (vector-ref A 1))
            (a_21 (vector-ref A 2)) (a_22 (vector-ref A 3))
            (b_11 (vector-ref B 0)) (b_12 (vector-ref B 1))
            (b_21 (vector-ref B 2)) (b_22 (vector-ref B 3)))
        (vector (+ (* a_11 b_11) (* a_12 b_21))
                (+ (* a_11 b_12) (* a_12 b_22))
                (+ (* a_21 b_11) (* a_22 b_21))
                (+ (* a_21 b_12) (* a_22 b_22)))))

    (define (my-array-translate Array translation)
      (let* ((array-copy (array-copy Array))
             (getter (array-getter array-copy))
             (setter (array-setter array-copy)))
        (make-array (interval-translate (array-domain Array)
                                        translation)
                    (lambda args
                      (apply getter
                             (map - args (vector->list translation))))
                    (lambda (v . args)
                      (apply setter
                             v
                             (map - args (vector->list translation)))))))

    (define (my-array-permute Array permutation)
      (let* ((array-copy (array-copy Array))
             (getter (array-getter array-copy))
             (setter (array-setter array-copy))
             (permutation-inverse (inverse-permutation permutation)))
        (make-array (interval-permute (array-domain Array)
                                      permutation)
                    (lambda args
                      (apply getter
                             (vector->list
                              (vector-permute (list->vector args)
                                              permutation-inverse))))
                    (lambda (v . args)
                      (apply setter
                             v
                             (vector->list
                              (vector-permute (list->vector args)
                                              permutation-inverse)))))))

    (define (my-interval-intersect . args)
      (define (fold-left operator ;; called with (op result-so-far (car list))
                         initial-value
                         list)
        (if (null? list)
            initial-value
            (fold-left operator
                       (operator initial-value (car list))
                       (cdr list))))
      (let ((new-uppers
             (let ((uppers (map interval-upper-bounds->vector args)))
               (fold-left (lambda (arg result)
                            (vector-map min arg result))
                          (car uppers)
                          uppers)))
            (new-lowers (let ((lowers (map interval-lower-bounds->vector args)))
                          (fold-left (lambda (arg result)
                                       (vector-map max arg result))
                                     (car lowers)
                                     lowers))))
        (and (vector-every < new-lowers new-uppers)
             (make-interval new-lowers new-uppers))))

    (define (my-interval-scale interval scales)
      (make-interval (interval-lower-bounds->vector interval)
                     (vector-map (lambda (u s)
                                   (quotient (+ u s -1) s))
                                 (interval-upper-bounds->vector interval)
                                 scales)))

    (define sparse-array
      (let ((domain (make-interval '#(1000000 1000000)))
            (sparse-rows (make-vector 1000000 '())))
        (make-array
         domain
         (lambda (i j)
           (cond ((assv j (vector-ref sparse-rows i))
                  => cdr)
                 (else
                  0.0)))
         (lambda (v i j)
           (cond
            ((assv j (vector-ref sparse-rows i))
             => (lambda (pair)
                  (set-cdr! pair v)))
            (else
             (vector-set! sparse-rows i (cons (cons j v)
                                              (vector-ref sparse-rows i)))))))))

    (define (myarray-sample array scales)
      (let ((scales-list (vector->list scales)))
        (cond ((specialized-array? array)
               (specialized-array-share
                array
                (interval-scale (array-domain array) scales)
                (lambda multi-index
                  (apply values (map * multi-index scales-list)))))
              ((mutable-array? array)
               (let ((getter (array-getter array))
                     (setter (array-setter array)))
                 (make-array
                  (interval-scale (array-domain array) scales)
                  (lambda multi-index
                    (apply getter (map * multi-index scales-list)))
                  (lambda (v . multi-index)
                    (apply setter v (map * multi-index scales-list))))))
              (else
               (let ((getter (array-getter array)))
                 (make-array
                  (interval-scale (array-domain array) scales)
                  (lambda multi-index
                    (apply getter (map * multi-index scales-list)))))))))

    (define (ceiling-quotient x d)
      ;; assumes x and d are positive
      (quotient (+ x d -1) d))

    (define (my-array-tile array sidelengths)
      ;; an alternate definition more-or-less from the srfi document
      (let* ((domain
              (array-domain array))
             (lowers
              (interval-lower-bounds->vector domain))
             (uppers
              (interval-upper-bounds->vector domain))
             (result-lowers
              (vector-map (lambda (x)
                            0)
                          lowers))
             (result-uppers
              (vector-map (lambda (l u s)
                            (ceiling-quotient (- u l) s))
                          lowers uppers sidelengths)))
        (make-array
         (make-interval result-lowers result-uppers)
         (lambda i
           (let* ((vec-i
                   (list->vector i))
                  (result-lowers
                   (vector-map (lambda (l i s)
                                 (+ l (* i s)))
                               lowers vec-i sidelengths))
                  (result-uppers
                   (vector-map (lambda (l u i s)
                                 (min u (+ l (* (+ i 1) s))))
                               lowers uppers vec-i sidelengths)))
             (array-extract array
                            (make-interval result-lowers result-uppers)))))))

    (define (myarray-reverse array flip?)
      (let* ((flips (vector->list flip?))
             (domain (array-domain array))
             (lowers (interval-lower-bounds->list domain))
             (uppers (interval-upper-bounds->list domain))
             (transform
              (lambda (multi-index)
                (map (lambda (i_k l_k u_k f_k?)
                       (if f_k?
                           (- (+ u_k l_k -1) i_k)
                           i_k))
                     multi-index lowers uppers flips))))
        (cond ((specialized-array? array)
               (specialized-array-share
                array
                domain
                (lambda multi-index
                  (apply values (transform multi-index)))))
              ((mutable-array? array)
               (let ((getter (array-getter array))
                     (setter (array-setter array)))
                 (make-array domain
                             (lambda multi-index
                               (apply getter (transform multi-index)))
                             (lambda (v . multi-index)
                               (apply setter v (transform multi-index))))))
              (else
               (let ((getter (array-getter array)))
                 (make-array domain
                             (lambda multi-index
                               (apply getter (transform multi-index)))))))))

    (define (my-interval-cartesian-product . args)
      (make-interval
       (list->vector (apply append (map interval-lower-bounds->list args)))
       (list->vector (apply append (map interval-upper-bounds->list args)))))

    (define make-pgm   cons)
    (define pgm-greys  car)
    (define pgm-pixels cdr)

    (define (read-pgm file)
      (define (read-pgm-object port)
        (skip-white-space port)
        (let ((o (read port)))
          (read-char port)    ; to skip the newline or next whitespace
          (if (eof-object? o)
              (error "reached end of pgm file")
              o)))
      (define (skip-to-end-of-line port)
        (let loop ((ch (read-char port)))
          (if (not (eq? ch #\newline))
              (loop (read-char port)))))
      (define (white-space? ch)
        (case ch
          ((#\newline #\space #\tab) #t)
          (else #f)))
      (define (skip-white-space port)
        (let ((ch (peek-char port)))
          (cond ((white-space? ch) (read-char port) (skip-white-space port))
                ((eq? ch #\#) (skip-to-end-of-line port)(skip-white-space port))
                (else #f))))
      (call-with-input-file file
        (lambda (port)
          ;; We're going to read text for a while,
          ;; then switch to binary.
          ;; So we need to turn off buffering until
          ;; we switch to binary.
          ;;(port-settings-set! port '(buffering: #f))
          (let* ((header (read-pgm-object port))
                 (columns (read-pgm-object port))
                 (rows (read-pgm-object port))
                 (greys (read-pgm-object port)))

            ;; now we switch back to buffering
            ;; to speed things up
            ;; (port-settings-set! port '(buffering: #t))
            (make-pgm greys
                      (array-copy
                       (make-array
                        (make-interval (vector rows columns))
                        (cond ((or (eq? header 'p5) ;; pgm binary
                                   (eq? header 'P5))
                               (if (< greys 256)
                                   (lambda (i j) ;; one byte/pixel
                                     (char->integer (read-char port)))
                                   (lambda (i j) ;; two bytes/pixel, little-endian
                                     (let* ((first-byte
                                             (char->integer (read-char port)))
                                            (second-byte
                                             (char->integer (read-char port))))
                                       (+ (* second-byte 256) first-byte)))))
                              ((or (eq? header 'p2) ;; pgm ascii
                                   (eq? header 'P2))
                               (lambda (i j)
                                 (read port)))
                              (else
                               (error "read-pgm: not a pgm file"))))))))))

    (define (write-pgm pgm-data file . force-ascii)
      (call-with-output-file file
        (lambda (port)
          (let* ((greys
                  (pgm-greys pgm-data))
                 (pgm-array
                  (pgm-pixels pgm-data))
                 (domain
                  (array-domain pgm-array))
                 (rows
                  (fx- (interval-upper-bound domain 0)
                       (interval-lower-bound domain 0)))
                 (columns
                  (fx- (interval-upper-bound domain 1)
                       (interval-lower-bound domain 1))))
            (if (and (pair? force-ascii) (car force-ascii))
                (display "P2" port)
                (display "P5" port))
            (newline port)
            (display columns port) (display " " port)
            (display rows port) (newline port)
            (display greys port) (newline port)
            (array-for-each
             (if (and (pair? force-ascii) (car force-ascii))
                 (let ((next-pixel-in-line 1))
                   (lambda (p)
                     (write p port)
                     (if (fxzero? (fxand next-pixel-in-line 15))
                         (begin
                           (newline port)
                           (set! next-pixel-in-line 1))
                         (begin
                           (display " " port)
                           (set! next-pixel-in-line
                                 (fx+ 1 next-pixel-in-line))))))
                 (if (fx<? greys 256)
                     (lambda (p)
                       (write-u8 p port))
                     (lambda (p)
                       (write-u8 (fxand p 255) port)
                       (write-u8 (fxarithmetic-shift-right p 8) port))))
             pgm-array)))))

    ;;(define test-pgm (read-pgm "girl.pgm"))

    (define (array-dot-product a b)
      (array-fold (lambda (x y)
                    (+ x y))
                  0
                  (array-map
                   (lambda (x y)
                     (* x y))
                   a b)))

    (define (array-convolve source filter)
      (let* ((source-domain
              (array-domain source))
             (S_
              (array-getter source))
             (filter-domain
              (array-domain filter))
             (F_
              (array-getter filter))
             (result-domain
              (interval-dilate
               source-domain
               ;; left bound of an interval is an equality,
               ;; right bound is an inequality, hence the
               ;; the difference in the following two expressions
               (vector-map -
                           (interval-lower-bounds->vector filter-domain))
               (vector-map (lambda (x)
                             (- 1 x))
                           (interval-upper-bounds->vector filter-domain)))))
        (make-array
         result-domain
         (lambda (i j)
           (array-fold
            (lambda (p q)
              (+ p q))
            0
            (make-array
             filter-domain
             (lambda (k l)
               (* (S_ (+ i k)
                      (+ j l))
                  (F_ k l)))))))))

    ;; (define sharpen-filter
    ;;   (list->array
    ;;    '(0 -1  0
    ;;        -1  5 -1
    ;;        0 -1  0)
    ;;    (make-interval '#(-1 -1) '#(2 2))))

    ;; (define edge-filter
    ;;   (list->array
    ;;    '(0 -1  0
    ;;        -1  4 -1
    ;;        0 -1  0)
    ;;    (make-interval '#(-1 -1) '#(2 2))))

    (define (round-and-clip pixel max-grey)
      (max 0 (min (exact (round pixel)) max-grey)))

    (define (array-sum a)
      (array-fold + 0 a))
    (define (array-max a)
      (array-fold max -inf.0 a))

    (define (max-norm a)
      (array-max (array-map abs a)))
    (define (one-norm a)
      (array-sum (array-map abs a)))

    (define (operator-max-norm a)
      (max-norm (array-map one-norm (array-curry (array-permute a '#(1 0)) 1))))
    (define (operator-one-norm a)
      ;; The "permutation" to apply here is the identity, so we omit it.
      (max-norm (array-map one-norm (array-curry a 1))))

    (define (make-separable-transform ~1D-transform)
      (lambda (a)
        (let ((n (array-dimension a)))
          (do ((d 0 (fx+ d 1)))
              ((fx=? d n))
            (array-for-each
             ~1D-transform
             (array-curry (array-rotate a d) 1))))))

    (define (recursively-apply-transform-and-downsample transform)
      (lambda (a)
        (let ((sample-vector (make-vector (array-dimension a) 2)))
          (define (helper a)
            (if (fx<? 1 (interval-upper-bound (array-domain a) 0))
                (begin
                  (transform a)
                  (helper (array-sample a sample-vector)))))
          (helper a))))

    (define (recursively-downsample-and-apply-transform transform)
      (lambda (a)
        (let ((sample-vector (make-vector (array-dimension a) 2)))
          (define (helper a)
            (if (fx<? 1 (interval-upper-bound (array-domain a) 0))
                (begin
                  (helper (array-sample a sample-vector))
                  (transform a))))
          (helper a))))

    (define (~1D-Haar-loop a)
      (let ((a_ (array-getter a))
            (a! (array-setter a))
            (n (interval-upper-bound (array-domain a) 0)))
        (do ((i 0 (fx+ i 2)))
            ((fx=? i n))
          (let* ((a_i               (a_ i))
                 (a_i+1             (a_ (fx+ i 1)))
                 (scaled-sum        (fl/ (fl+ a_i a_i+1) (flsqrt 2.0)))
                 (scaled-difference (fl/ (fl- a_i a_i+1) (flsqrt 2.0))))
            (a! scaled-sum i)
            (a! scaled-difference (fx+ i 1))))))

    (define ~1D-Haar-transform
      (recursively-apply-transform-and-downsample ~1D-Haar-loop))

    (define ~1D-Haar-inverse-transform
      (recursively-downsample-and-apply-transform ~1D-Haar-loop))

    (define hyperbolic-Haar-transform
      (make-separable-transform ~1D-Haar-transform))

    (define hyperbolic-Haar-inverse-transform
      (make-separable-transform ~1D-Haar-inverse-transform))

    (define Haar-transform
      (recursively-apply-transform-and-downsample
       (make-separable-transform ~1D-Haar-loop)))

    (define Haar-inverse-transform
      (recursively-downsample-and-apply-transform
       (make-separable-transform ~1D-Haar-loop)))

    (define (LU-decomposition A)
      ;; Assumes the domain of A is [0,n)\\times [0,n)
      ;; and that Gaussian elimination can be applied
      ;; without pivoting.
      (let ((n
             (interval-upper-bound (array-domain A) 0))
            (A_
             (array-getter A)))
        (do ((i 0 (fx+ i 1)))
            ((= i (fx- n 1)) A)
          (let* ((pivot
                  (A_ i i))
                 (column/row-domain
                  ;; both will be one-dimensional
                  (make-interval (vector (+ i 1))
                                 (vector n)))
                 (column
                  ;; the column below the (i,i) entry
                  (specialized-array-share A
                                           column/row-domain
                                           (lambda (k)
                                             (values k i))))
                 (row
                  ;; the row to the right of the (i,i) entry
                  (specialized-array-share A
                                           column/row-domain
                                           (lambda (k)
                                             (values i k))))
                 ;; the subarray to the right and
                 ;;below the (i,i) entry
                 (subarray
                  (array-extract
                   A (make-interval
                      (vector (fx+ i 1) (fx+ i 1))
                      (vector n         n)))))
            ;; compute multipliers
            (array-assign!
             column
             (array-map (lambda (x)
                          (/ x pivot))
                        column))
            ;; subtract the outer product of i'th
            ;; row and column from the subarray
            (array-assign!
             subarray
             (array-map -
                        subarray
                        (array-outer-product * column row)))))))

    ;; We'll define a brief, not-very-efficient matrix multiply routine.
    (define (matrix-multiply a b)
      (let ((a-rows
             ;; We copy this array because its elements are accessed
             ;; multiple times.
             (array-copy (array-curry a 1)))
            (b-columns
             ;; We copy this array because its elements are accessed
             ;; multiple times.
             (array-copy (array-curry (array-rotate b 1) 1))))
        (array-outer-product array-dot-product a-rows b-columns)))

    (define (inner-product A f g B)
      (array-outer-product
       (lambda (a b)
         (array-reduce f (array-map g a b)))
       (array-copy (array-curry A 1))
       (array-copy (array-curry (array-rotate B 1) 1))))

    (define (x2x2-matrix-multiply-into! A B C)
      (let ((C! (array-setter C))
            (A_ (array-getter A))
            (B_ (array-getter B)))
        (C! (+ (* (A_ 0 0) (B_ 0 0))
               (* (A_ 0 1) (B_ 1 0)))
            0 0)
        (C! (+ (* (A_ 0 0) (B_ 0 1))
               (* (A_ 0 1) (B_ 1 1)))
            0 1)
        (C! (+ (* (A_ 1 0) (B_ 0 0))
               (* (A_ 1 1) (B_ 1 0)))
            1 0)
        (C! (+ (* (A_ 1 0) (B_ 0 1))
               (* (A_ 1 1) (B_ 1 1)))
            1 1)))

    (define (run-tests)

      (random-source-pseudo-randomize! default-random-source 7 23)

      (test-begin "srfi-179: nonempty intervals and generalized arrays")

      (test-group "interval tests"
        (test-error (make-interval 1 '#(3 4)))
        (test-error (make-interval '#(1 1)  3))
        (test-error (make-interval '#(1 1)  '#(3)))
        (test-error (make-interval '#()  '#()))
        (test-error (make-interval '#(1.)  '#(1)))
        (test-error (make-interval '#(1 #f)  '#(1 2)))
        (test-error (make-interval '#(1)  '#(1.)))
        (test-error (make-interval '#(1 1)  '#(1 #f)))
        (test-error (make-interval '#(1)  '#(1)))
        (test-error (make-interval '#(1 2 3)  '#(4 2 6)))
        (test-error (make-interval 1))
        (test-error (make-interval '#()))
        (test-error (make-interval '#(1.)))
        (test-error (make-interval '#(-1)))

        (test (make-interval '#(11111)  '#(11112))
            (make-interval '#(11111) '#(11112)))

        (test (make-interval '#(1 2 3)  '#(4 5 6))
            (make-interval '#(1 2 3) '#(4 5 6)))

        (test-not (interval? #t))

        (test-assert (interval? (make-interval '#(1 2 3) '#(4 5 6))))

        (test-error (interval-dimension 1))

        (test 3
            (interval-dimension (make-interval '#(1 2 3) '#(4 5 6))))

        (test-error
         (interval-lower-bound 1 0))
        (test-error
         (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) #f))
        (test-error
         (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) 1.))
        (test-error
         (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) -1))
        (test-error
         (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) 3))
        (test-error
         (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) 4))
        (test-error
         (interval-upper-bound 1 0))
        (test-error
         (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) #f))
        (test-error
         (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) 1.))
        (test-error
         (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) -1))
        (test-error
         (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) 3))
        (test-error
         (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) 4))
        (test-error
         (interval-lower-bounds->list 1))
        (test-error
         (interval-upper-bounds->list #f))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((lower (map (lambda (x) (random 10))
                             (vector->list (make-vector (random 1 11)))))
                 (upper (map (lambda (x) (+ (random 1 11) x))
                             lower)))
            (let ((interval (make-interval (list->vector lower)
                                           (list->vector upper)))
                  (offset (random (length lower))))
              (test (list-ref lower offset)
                  (interval-lower-bound interval offset))
              (test (list-ref upper offset)
                  (interval-upper-bound interval offset))
              (test lower
                  (interval-lower-bounds->list interval))
              (test upper
                  (interval-upper-bounds->list interval)))))

        (test-error (interval-lower-bounds->vector 1))
        (test-error (interval-upper-bounds->vector #f))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((lower (map (lambda (x) (random 10))
                             (vector->list (make-vector (random 1 11)))))
                 (upper (map (lambda (x) (+ (random 1 11) x))
                             lower)))
            (let ((interval (make-interval (list->vector lower)
                                           (list->vector upper)))
                  (offset (random (length lower))))
              (test (list-ref lower offset)
                  (interval-lower-bound interval offset))
              (test (list-ref upper offset)
                  (interval-upper-bound interval offset))
              (test (list->vector lower)
                  (interval-lower-bounds->vector interval))
              (test (list->vector upper)
                  (interval-upper-bounds->vector interval)))))

        (test-error (interval-projections 1 1))
        (test-error (interval-projections (make-interval '#(0) '#(1)) #t))
        (test-error (interval-projections (make-interval '#(0 0) '#(1 1)) 1/2))
        (test-error (interval-projections (make-interval '#(0 0) '#(1 1)) 1.))
        (test-error (interval-projections (make-interval '#(0 0) '#(1 1)) 0))
        (test-error (interval-projections (make-interval '#(0 0) '#(1 1)) 2))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((lower (map (lambda (x) (random 10))
                             (vector->list (make-vector (random 3 11)))))
                 (upper (map (lambda (x) (+ (random 1 11) x))
                             lower))
                 (left-dimension (random 1 (- (length lower) 1)))
                 (right-dimension (- (length lower) left-dimension)))
            (test-values
             (interval-projections (make-interval (list->vector lower)
                                                  (list->vector upper))
                                   right-dimension)
             (values
              (make-interval (list->vector (take lower right-dimension))
                             (list->vector (take upper right-dimension)))
              (make-interval (list->vector (drop lower right-dimension))
                             (list->vector (drop upper right-dimension))))
             )))

        (test-error (interval-volume #f))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((lower (map (lambda (x) (random 10))
                             (vector->list (make-vector (random 1 11)))))
                 (upper (map (lambda (x) (+ (random 1 11) x))
                             lower)))
            (test (apply * (map - upper lower))
                (interval-volume (make-interval (list->vector lower)
                                                (list->vector upper))))))

        (test-error (interval= #f (make-interval '#(1 2 3) '#(4 5 6))))
        (test-error (interval= (make-interval '#(1 2 3) '#(4 5 6)) #f))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((lower1 (map (lambda (x) (random 2))
                              (vector->list (make-vector (random 1 6)))))
                 (upper1 (map (lambda (x) (+ (random 1 3) x)) lower1))
                 (lower2 (map (lambda (x) (random 2)) lower1))
                 (upper2 (map (lambda (x) (+ 1 (random 1 3) x)) lower2)))
            (test (and (equal? lower1 lower2) ; prob ~1/16
                       (equal? upper1 upper2))
                (interval= (make-interval (list->vector lower1)
                                          (list->vector upper1))
                           (make-interval (list->vector lower2)
                                          (list->vector upper2))))))

        (test-error (interval-subset? #f (make-interval '#(1 2 3) '#(4 5 6))))
        (test-error (interval-subset? (make-interval '#(1 2 3) '#(4 5 6)) #f))
        (test-error (interval-subset? (make-interval '#(1) '#(2))
                                      (make-interval '#(0 0) '#(1 2))))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((lower1 (map (lambda (x) (random 2))
                              (vector->list (make-vector (random 1 6)))))
                 (upper1 (map (lambda (x) (+ (random 1 3) x)) lower1))
                 (lower2 (map (lambda (x) (random 2)) lower1))
                 (upper2 (map (lambda (x) (+ (random 1 3) x)) lower2)))
            (test (and (every (lambda (x) (>= (car x) (cdr x)))
                              (map cons lower1 lower2))
                       (every (lambda (x) (<= (car x) (cdr x)))
                              (map cons upper1 upper2)))
                (interval-subset? (make-interval (list->vector lower1)
                                                 (list->vector upper1))
                                  (make-interval (list->vector lower2)
                                                 (list->vector upper2))))))

        (test-error (interval-contains-multi-index? 1 1))
        (test-error (interval-contains-multi-index?
                     (make-interval '#(1 2 3) '#(4 5 6)) 1))
        (test-error (interval-contains-multi-index?
                     (make-interval '#(1 2 3) '#(4 5 6)) 1 1/2 0.1))

        (let ((interval   (make-interval '#(1 2 3) '#(4 5 6)))
              (interval-2 (make-interval '#(10 11 12) '#(13 14 15))))
          (test-assert
              (array-every
               (lambda (x)
                 (apply interval-contains-multi-index? interval x))
               (make-array interval list)))
          (test-assert
              (array-every
               (lambda (x)
                 (not (apply interval-contains-multi-index? interval x)))
               (make-array interval-2 list))))

        (test-error (interval-for-each (lambda (x) x) 1))
        (test-error (interval-for-each 1 (make-interval '#(3) '#(4))))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((lower (map (lambda (x) (random 10))
                             (vector->list (make-vector (random 1 7)))))
                 (upper (map (lambda (x) (+ (random 1 4) x))
                             lower)))
            (let ((result '()))
              (define (f . args)
                (set! result (cons args result)))
              (test (reverse (all-elements lower upper))
                  (begin
                    (interval-for-each f
                                       (make-interval (list->vector lower)
                                                      (list->vector upper)))
                    result)))))

        (let ((interval (make-interval '#(0 0) '#(100 100))))
          (test-error (interval-dilate interval 'a '#(-10 10)))
          (test-error (interval-dilate 'a '#(10 10) '#(-10 -10)))
          (test-error (interval-dilate interval '#(10 10) 'a))
          (test-error (interval-dilate interval '#(10) '#(-10 -10)))
          (test-error (interval-dilate interval '#(10 10) '#( -10)))
          (test-error (interval-dilate interval '#(100 100) '#(-100 -100))))
        )

      (test-group "basic"
        (test-error (make-array 1 values))
        (test-error (make-array (make-interval '#(3) '#(4)) 1))

        ;; (let ((getter (lambda args 1.)))
        ;;   (test (make-array (make-interval '#(3) '#(4)) getter)
        ;;       (make-%%array (make-interval '#(3) '#(4))
        ;;                     getter
        ;;                     #f
        ;;                     #f
        ;;                     #f
        ;;                     #f
        ;;                     #f
        ;;                     %%order-unknown)))

        (test-error (array-domain #f))
        (test-error (array-getter #f))

        (let* ((getter (lambda args 1.))
               (array    (make-array (make-interval '#(3) '#(4)) getter)))
          (test-not (array? #f))
          (test-assert (array? array))
          (test (make-interval '#(3) '#(4))
              (array-domain array))
          (test getter
              (array-getter array)))

        ;; (let ((result #f))
        ;;   (let ((getter (lambda (i) result))
        ;;         (setter   (lambda (v i) (set! result v)))
        ;;         (domain   (make-interval '#(3) '#(4))))
        ;;     (test (make-array domain
        ;;                       getter
        ;;                       setter)
        ;;         (make-%%array domain
        ;;                       getter
        ;;                       setter
        ;;                       #f
        ;;                       #f
        ;;                       #f
        ;;                       #f
        ;;                       %%order-unknown))))

        (test-error (array-setter #f))

        (let ((result (cons #f #f)))
          (let ((getter (lambda (i) (car result)))
                (setter   (lambda (v i) (set-car! result v)))
                (domain   (make-interval '#(3) '#(4))))
            (let ((array (make-array domain
                                     getter
                                     setter)))
              (test-assert (array? array))
              (test-assert (mutable-array? array))
              (test-not (mutable-array? 1))
              (test setter
                  (array-setter array))
              (test getter
                  (array-getter array))
              (test domain
                  (array-domain array)))))

        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let* ((lower-bounds
        ;;           (map (lambda (x) (random 2))
        ;;                (vector->list (make-vector (random 1 7)))))
        ;;          (upper-bounds
        ;;           (map (lambda (x) (+ x (random 1 3)))
        ;;                lower-bounds))
        ;;          (new-domain
        ;;           (make-interval (list->vector lower-bounds)
        ;;                          (list->vector upper-bounds)))
        ;;          (new-domain-dimension
        ;;           (interval-dimension new-domain))
        ;;          (old-domain-dimension
        ;;           (random 1 7))
        ;;          (base
        ;;           (random 100))
        ;;          (coefficients
        ;;           (map (lambda (x) (* (random-sign)
        ;;                           (random 20)))
        ;;                (local-iota 0 old-domain-dimension)))
        ;;          (old-indexer
        ;;           (lambda args
        ;;             (apply + base (map * args coefficients))))
        ;;          (new-domain->old-domain-coefficients
        ;;           (map (lambda (x)
        ;;                  (map (lambda (x) (* (random-sign) (random 10)))
        ;;                       (local-iota 0 new-domain-dimension)))
        ;;                (local-iota 0 old-domain-dimension)))
        ;;          (new-domain->old-domain
        ;;           (lambda args
        ;;             (apply values (map (lambda (row)
        ;;                                  (apply + (map * row args)))
        ;;                                new-domain->old-domain-coefficients)))))
        ;;     (if (not (and (myindexer=
        ;;                    (lambda args
        ;;                      (call-with-values
        ;;                          (lambda () (apply new-domain->old-domain args))
        ;;                        old-indexer))
        ;;                    (%%compose-indexers old-indexer new-domain
        ;;                                        new-domain->old-domain)
        ;;                    new-domain)))
        ;;         (error (list new-domain
        ;;                      old-domain-dimension
        ;;                      base
        ;;                      coefficients
        ;;                      new-domain->old-domain-coefficients))
        ;;         )))

        ;; errors are not required to signal
        ;; (let ((a (make-array (make-interval '#(0 0) '#(1 1)) ;; not valid
        ;;                      values
        ;;                      values)))
        ;;   (test-error (array-body a))
        ;;   (test-error (array-indexer a))
        ;;   (test-error (array-storage-class a))
        ;;   (test-error (array-safe? a)))

        (test-error (make-specialized-array  'a))
        (test-error (make-specialized-array (make-interval '#(0) '#(10)) 'a))
        (test-error (make-specialized-array
                     (make-interval '#(0) '#(10))
                     generic-storage-class
                     'a))

        ;; We'll use specialized arrays with u1-storage-class---we never
        ;; use the array contents, just the indexers, and it saves storage.

        (test-error (array-elements-in-order? 1))
        (test-error (array-elements-in-order?
                     (make-array (make-interval '#(1 2)) list)))
        (test-error (array-elements-in-order?
                     (make-array (make-interval '#(1 2)) list list)))

        ;; all these are true, we'll have to see how to screw it up later.
        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let ((array
        ;;          (make-specialized-array (random-interval)
        ;;                                  u1-storage-class)))
        ;;     (test-assert (array-elements-in-order? array))))

        (let ((array
               (make-specialized-array (make-interval '#(0 0) '#(2 3)))))
          (test 2 (array-dimension array))
          (test 6 (interval-volume (array-domain array)))
          (do ((i 0 (+ i 1)))
              ((= i 2))
            (do ((j 0 (+ j 1)))
                ((= j 3))
              (array-set! array (+ j (* i 3)) i j)
              (test (+ j (* i 3))
                  (array-ref array i j)))))

        (let ((array
               (make-specialized-array (make-interval '#(-2 -1 3) '#(0 2 5)))))
          (test 3 (array-dimension array))
          (test 12 (interval-volume (array-domain array)))
          (do ((i -2 (+ i 1)))
              ((= i 0))
            (do ((j -1 (+ j 1)))
                ((= j 2))
              (do ((k 3 (+ k 1)))
                  ((= k 5))
                (let ((cell (+ k (* 2 (+ j (* i 3))))))
                  (array-set! array cell i j k)
                  (test cell (array-ref array i j k)))))))

        ;; the elements of curried arrays are in order
        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((base
                  (make-specialized-array (random-interval 2 5)
                                          u1-storage-class))
                 (curried
                  (array-curry base (random 1 (array-dimension base)))))
            (test-assert (array-every array-elements-in-order? curried))))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((base
                  (make-specialized-array (random-interval 2 6)
                                          u1-storage-class))
                 (extracted
                  (array-extract base (random-subinterval (array-domain base)))))
            (test (array-elements-in-order? extracted)
                (extracted-array-elements-in-order? base extracted))))

        ;; Should we do reversed now?
        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((base
                  (make-specialized-array (random-interval)
                                          u1-storage-class))
                 (domain
                  (array-domain base))
                 (reversed-dimensions
                  (vector-map (lambda args (random-boolean))
                              (make-vector (array-dimension base))))
                 (reversed
                  (array-reverse base reversed-dimensions)))
            (test (vector-every
                   (lambda (lower upper reversed?)
                     (or (= (+ 1 lower) upper) ;; side-length 1
                         (not reversed?))) ;; dimension not reversed
                   (interval-lower-bounds->vector domain)
                   (interval-upper-bounds->vector domain)
                   reversed-dimensions)
                (array-elements-in-order? reversed))))

        ;; permutations

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((base
                  (make-specialized-array (random-interval)
                                          u1-storage-class))
                 (domain
                  (array-domain base))
                 (permutation
                  (random-permutation (array-dimension base)))
                 (permuted
                  (array-permute base permutation)))
            (test (permuted-array-elements-in-order? base permutation)
                (array-elements-in-order? permuted))))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((base
                  (make-specialized-array (random-nonnegative-interval 1 6)
                                          u1-storage-class))
                 (scales
                  (random-positive-vector (array-dimension base) 4))
                 (sampled
                  (array-sample base scales)))
            (test (sampled-array-elements-in-order? base scales)
                (array-elements-in-order? sampled))))

        ;; Now we need to test the precomputation and caching of
        ;; array-elements-in-order?
        ;; The only places we precompute are
        ;; 1.  after creating a new specialized array
        ;; 2.  in %%specialized-array-translate
        ;; 3.  in %%specialized-array-curry
        ;; 4.  reshaping a specialized array in place.
        ;; So we need to check these situations.

        ;; (let ((array (array-copy (make-array (make-interval '#(3 5)) list))))
        ;;   (test-assert (and (array-elements-in-order? array)
        ;;                     (%%compute-array-elements-in-order?
        ;;                      (%%array-domain array) (%%array-indexer array)))))

        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let* ((array
        ;;           (make-specialized-array (random-nonnegative-interval)
        ;;                                   u8-storage-class))
        ;;          (ignore ;; compute and cache the results
        ;;           (array-elements-in-order? array))
        ;;          (sampled-array
        ;;           (array-sample array (random-sample (array-dimension array))))
        ;;          (ignore ;; compute and cache the results
        ;;           ;; possibly not in order
        ;;           (array-elements-in-order? sampled-array))
        ;;          (translated-array
        ;;           (array-translate array
        ;;                            (vector-map (lambda (x) (random 10))
        ;;                                        (make-vector
        ;;                                         (array-dimension array)))))
        ;;          (translated-sampled-array
        ;;           (array-translate sampled-array
        ;;                            (vector-map (lambda (x) (random 10))
        ;;                                        (make-vector
        ;;                                         (array-dimension array))))))
        ;;     (test (%%compute-array-elements-in-order?
        ;;            (%%array-domain translated-array)
        ;;            (%%array-indexer translated-array))
        ;;         (array-elements-in-order? translated-array))
        ;;     (test (%%compute-array-elements-in-order?
        ;;            (%%array-domain translated-sampled-array)
        ;;            (%%array-indexer translated-sampled-array))
        ;;         (array-elements-in-order? translated-sampled-array))))

        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let* ((array
        ;;           (make-specialized-array (random-nonnegative-interval 2 4)
        ;;                                   u8-storage-class))
        ;;          (d-1
        ;;           (- (array-dimension array) 1))
        ;;          (ignore
        ;;           ;; compute and cache the result, in order
        ;;           (array-elements-in-order? array))
        ;;          (rotated-array
        ;;           (array-rotate array 1))
        ;;          (ignore ;; compute and cache the results
        ;;           ;; possibly not in order
        ;;           (array-elements-in-order? rotated-array))
        ;;          (sampled-array
        ;;           (array-sample array (list->vector (cons 2 (make-list d-1 1)))))
        ;;          (ignore
        ;;           ;; almost definitely not in order, but if we curry it
        ;;           ;; with dimension 1 the subarrays are in order.
        ;;           (array-elements-in-order? sampled-array))
        ;;          (curried-array
        ;;           (array-ref (array-curry array d-1)
        ;;                      (interval-lower-bound (array-domain array) 0)))
        ;;          (curried-rotated-array
        ;;           (array-ref
        ;;            (array-curry rotated-array d-1)
        ;;            (interval-lower-bound (array-domain rotated-array) 0)))
        ;;          (curried-sampled-array
        ;;           (array-ref
        ;;            (array-curry sampled-array d-1)
        ;;            (interval-lower-bound (array-domain sampled-array) 0))))
        ;;     (test (%%compute-array-elements-in-order?
        ;;            (%%array-domain curried-array)
        ;;            (%%array-indexer curried-array))
        ;;         (array-elements-in-order? curried-array))
        ;;     (test (%%compute-array-elements-in-order?
        ;;            (%%array-domain curried-rotated-array)
        ;;            (%%array-indexer curried-rotated-array))
        ;;         (array-elements-in-order? curried-rotated-array))
        ;;     (test (%%compute-array-elements-in-order?
        ;;            (%%array-domain curried-sampled-array)
        ;;            (%%array-indexer curried-sampled-array))
        ;;         (array-elements-in-order? curried-sampled-array))))
         
        ;; FIXME: array-reshape tests.

        ;; error tests

        ;; (test-error
        ;;  (%%move-array-elements
        ;;   (array-reverse (make-specialized-array (make-interval '#(2 2))))
        ;;   (make-array (make-interval '#(1 4)) list)
        ;;   ""))

        ;; (test-error
        ;;  (%%move-array-elements
        ;;   (make-specialized-array (make-interval '#(2 2)))
        ;;   (make-array (make-interval '#(1 5)) list)
        ;;   ""))

        ;; (test-error
        ;;  (%%move-array-elements
        ;;   (make-array (make-interval '#(2 2)) list list) ;; not a valid setter
        ;;   (make-array (make-interval '#(1 4)) list)
        ;;   ""))

        ;; (do ((d 1 (fx+ d 1)))
        ;;     ((= d 6))
        ;;   (let* ((uppers-list
        ;;           (iota d 2))
        ;;          (domain
        ;;           (make-interval (list->vector uppers-list)))
        ;;          (reversed-domain
        ;;           (make-interval (list->vector (reverse uppers-list)))))
        ;;     (do ((i 0 (fx+ i 1)))
        ;;         ;; distribute "tests" results over five dimensions
        ;;         ((= i (quotient tests 5)))
        ;;       (let* ((storage-class-and-initializer
        ;;               (random-storage-class-and-initializer))
        ;;              (storage-class
        ;;               (car storage-class-and-initializer))
        ;;              (initializer
        ;;               (cadr storage-class-and-initializer))
        ;;              (specialized-source
        ;;               (array-copy
        ;;                (make-array domain
        ;;                            (lambda args
        ;;                              (initializer)))
        ;;                storage-class))
        ;;              (rotated-specialized-source
        ;;               (array-rotate specialized-source (- d 1)))
        ;;              (specialized-reversed-source
        ;;               (array-copy
        ;;                (make-array reversed-domain
        ;;                            (lambda args
        ;;                              (initializer)))
        ;;                storage-class))
        ;;              (specialized-destination
        ;;               (make-specialized-array domain
        ;;                                       storage-class))
        ;;              (specialized-reversed-destination
        ;;               (make-specialized-array reversed-domain
        ;;                                       storage-class))
        ;;              (source
        ;;               (make-array domain
        ;;                           (array-getter
        ;;                            (array-reverse specialized-source))))
        ;;              (destination
        ;;               (make-array (array-domain specialized-destination)
        ;;                           (array-getter specialized-destination)
        ;;                           (array-setter specialized-destination)))
        ;;              (rotated-specialized-source
        ;;               (array-rotate specialized-source (- d 1)))
        ;;              (rotated-source
        ;;               (array-rotate source (- d 1)))
        ;;              (reversed-source
        ;;               (make-array reversed-domain
        ;;                           (array-getter specialized-reversed-source)))
        ;;              (reversed-destination
        ;;               (make-array reversed-domain
        ;;                           (array-getter specialized-reversed-source)
        ;;                           (array-setter specialized-reversed-source))))
        ;;         ;; specialized-to-specialized, use fast copy
        ;;         (test-error (%%move-array-elements specialized-destination
        ;;                                            specialized-source "test: "))
        ;;         (test-assert
        ;;             (myarray= specialized-source specialized-destination))
        ;;         ;; fast copying between specialized of the same volume
        ;;         (test-error (%%move-array-elements specialized-destination
        ;;                                            specialized-reversed-source
        ;;                                            "test: "))
        ;;         ;; copy to adjacent elements of destination, checking needed
        ;;         (test-error
        ;;          (%%move-array-elements specialized-destination source "test: "))
        ;;         (test-assert (myarray= source specialized-destination))
        ;;         ;; copy to adjacent elements of destination, no checking needed
        ;;         ;; arrays of different shapes
        ;;         (test-error (%%move-array-elements specialized-destination
        ;;                                            rotated-specialized-source
        ;;                                            "test: "))
        ;;         (test (array->list rotated-specialized-source)
        ;;             (array->list specialized-destination))
        ;;         ;; copy to adjacent elements of destination, checking needed
        ;;         ;; arrays of different shapes
        ;;         (test-error (%%move-array-elements specialized-destination
        ;;                                            rotated-source "test: "))
        ;;         (test (array->list rotated-source)
        ;;             (array->list specialized-destination))
        ;;         ;; copy to non-adjacent elements of destination, no
        ;;         ;; checking needed
        ;;         (test-error (%%move-array-elements
        ;;                      (array-reverse specialized-destination)
        ;;                      specialized-source "test: "))
        ;;         (test-assert (myarray= specialized-source
        ;;                                (array-reverse specialized-destination)))
        ;;         ;; copy to non-specialized array
        ;;         (test-error (%%move-array-elements destination source "test: "))
        ;;         (test-assert (myarray= destination source))
        ;;         ))))

        (test-error (array-copy #f generic-storage-class))
        (test-error (array-copy (make-array (make-interval '#(1) '#(2))
                                            list)
                                #f))
        (test-error (array-copy (make-array (make-interval '#(1) '#(2))
                                            list)
                                generic-storage-class
                                'a))
        (test-error (array-copy (make-array (make-interval '#(1) '#(2))
                                            list)
                                generic-storage-class
                                (make-interval '#(10))))
        (test-error (array-copy (make-array (make-interval '#(1) '#(2))
                                            list)
                                generic-storage-class
                                #f
                                'a))

        (test-error (array-copy (make-array (make-interval '#(1) '#(2))
                                            list)
                                generic-storage-class
                                #f
                                #f
                                'a))

        ;; We gotta make sure than the error checks work in all dimensions ...
        (test-error (array-copy (make-array (make-interval '#(1) '#(2))
                                            list)
                                u16-storage-class))
        (test-error (array-copy (make-array (make-interval '#(1 1) '#(2 2))
                                            list)
                                u16-storage-class))
        (test-error (array-copy (make-array (make-interval '#(1 1 1) '#(2 2 2))
                                            list)
                                u16-storage-class))
        (test-error (array-copy (make-array (make-interval '#(1 1 1 1)
                                                           '#(2 2 2 2))
                                            list)
                                u16-storage-class))
        (test-error (array-copy (make-array (make-interval '#(1 1 1 1 1)
                                                           '#(2 2 2 2 2))
                                            list)
                                u16-storage-class))
        (test-error (specialized-array-default-safe? 'a))
        (test-error (specialized-array-default-mutable? 'a))

        ;; (let ((mutable-default (specialized-array-default-mutable?)))
        ;;   (specialized-array-default-mutable? #f)
        ;;   (do ((i 1 (+ i 1)))
        ;;       ((= i 6))
        ;;     (let ((A (array-copy (make-array (make-interval (make-vector i 2))
        ;;                                      (lambda args 10)))))
        ;;       (test-error (apply array-set! A 0 (make-list i 0)))
        ;;       (test-error (array-assign! A A))))
        ;;   (specialized-array-default-mutable? mutable-default))

        (specialized-array-default-safe? #t)

        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let* ((domain
        ;;           (random-interval))
        ;;          (lower-bounds
        ;;           (interval-lower-bounds->list domain))
        ;;          (upper-bounds
        ;;           (interval-upper-bounds->list domain))
        ;;          (array1
        ;;           (let ((alist '()))
        ;;             (make-array
        ;;              domain
        ;;              (lambda indices
        ;;                (cond ((assoc indices alist)
        ;;                       => cdr)
        ;;                      (else
        ;;                       indices)))
        ;;              (lambda (value . indices)
        ;;                (cond ((assoc indices alist)
        ;;                       =>(lambda (entry)
        ;;                           (set-cdr! entry value)))
        ;;                      (else
        ;;                       (set! alist (cons (cons indices value)
        ;;                                         alist))))))))
        ;;          (array2
        ;;           (array-copy array1 generic-storage-class))
        ;;          (setter1
        ;;           (array-setter array1))
        ;;          (setter2
        ;;           (array-setter array2)))
        ;;     (do ((j 0 (+ j 1)))
        ;;         ((= j 25))
        ;;       (let ((v (random 1000))
        ;;             (indices (map random lower-bounds upper-bounds)))
        ;;         (apply setter1 v indices)
        ;;         (apply setter2 v indices)))
        ;;     (test-assert (myarray= array1 array2))
        ;;     (test-assert (myarray= (array-copy array1 generic-storage-class)
        ;;                            array2))
        ;;     ))

        (specialized-array-default-safe? #f)

        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let* ((domain
        ;;           (random-interval))
        ;;          (lower-bounds
        ;;           (interval-lower-bounds->list domain))
        ;;          (upper-bounds
        ;;           (interval-upper-bounds->list domain))
        ;;          (array1
        ;;           (let ((alist '()))
        ;;             (make-array
        ;;              domain
        ;;              (lambda indices
        ;;                (cond ((assoc indices alist)
        ;;                       => cdr)
        ;;                      (else
        ;;                       indices)))
        ;;              (lambda (value . indices)
        ;;                (cond ((assoc indices alist)
        ;;                       =>(lambda (entry)
        ;;                           (set-cdr! entry value)))
        ;;                      (else
        ;;                       (set! alist (cons (cons indices value)
        ;;                                         alist))))))))
        ;;          (array2
        ;;           (array-copy array1 generic-storage-class ))
        ;;          (setter1
        ;;           (array-setter array1))
        ;;          (setter2
        ;;           (array-setter array2)))
        ;;     (do ((j 0 (+ j 1)))
        ;;         ((= j 25))
        ;;       (let ((v (random 1000))
        ;;             (indices (map random lower-bounds upper-bounds)))
        ;;         (apply setter1 v indices)
        ;;         (apply setter2 v indices)))
        ;;     (test-assert (myarray= array1 array2))
        ;;     (test-assert (myarray= (array-copy array1 generic-storage-class)
        ;;                            array2))
        ;;     ))

        (test-error (array-map 1 #f))
        (test-error (array-map list 1 (make-array (make-interval '#(3) '#(4))
                                                  list)))
        (test-error (array-map list (make-array (make-interval '#(3) '#(4))
                                                list) 1))
        (test-error (array-map list
                               (make-array (make-interval '#(3) '#(4))
                                           list)
                               (make-array (make-interval '#(3 4) '#(4 5))
                                           list)))
        (test-error (array-every 1 2))
        (test-error (array-every list 1))
        (test-error (array-every list
                                 (make-array (make-interval '#(3) '#(4))
                                             list)
                                 1))
        (test-error (array-every list
                                 (make-array (make-interval '#(3) '#(4))
                                             list)
                                 (make-array (make-interval '#(3 4) '#(4 5))
                                             list)))
        (test-error (array-any 1 2))
        (test-error (array-any list 1))
        (test-error (array-any list
                               (make-array (make-interval '#(3) '#(4))
                                           list)
                               1))
        (test-error (array-any list
                               (make-array (make-interval '#(3) '#(4))
                                           list)
                               (make-array (make-interval '#(3 4) '#(4 5))
                                           list)))

        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let* ((interval
        ;;           (random-nonnegative-interval 1 6))
        ;;          (n
        ;;           (interval-volume interval))
        ;;          (separator
        ;;           ;; I want to make sure that the last item is chosen at least
        ;;           ;; once for each random
        ;;           (random (max 0 (- n 10)) n))
        ;;          (indexer
        ;;           (%%interval->basic-indexer interval))
        ;;          (arguments-1
        ;;           '())
        ;;          (array-1
        ;;           (make-array
        ;;            interval
        ;;            (lambda args
        ;;              (set! arguments-1 (cons args
        ;;                                      arguments-1))
        ;;              (let ((index (apply indexer args)))
        ;;                (cond
        ;;                 ((< index separator)
        ;;                  #f)
        ;;                 ((= index separator)
        ;;                  1)
        ;;                 (else
        ;;                  (error "The array should never be called with these args"
        ;;                         interval
        ;;                         separator
        ;;                         args
        ;;                         index)))))))
        ;;          (arguments-2
        ;;           '())
        ;;          (array-2
        ;;           (make-array
        ;;            interval
        ;;            (lambda args
        ;;              (set! arguments-2 (cons args
        ;;                                      arguments-2))
        ;;              (let ((index (apply indexer args)))
        ;;                (cond
        ;;                 ((< index separator)
        ;;                  #t)
        ;;                 ((= index separator)
        ;;                  #f)
        ;;                 (else
        ;;                  (error "The array should never be called with these args"
        ;;                         interval
        ;;                         separator
        ;;                         args
        ;;                         index))))))))
        ;;     (test 1
        ;;         (array-any values array-1))
        ;;     (test-not (array-every values array-2))
        ;;     (test-assert (indices-in-proper-order (reverse arguments-1)))
        ;;     (test-assert (indices-in-proper-order (reverse arguments-2)))
        ;;     ))

        (test-error (array-fold 1 1 1))
        (test-error (array-fold list 1 1))
        (test-error (array-fold-right 1 1 1))
        (test-error (array-fold-right list 1 1))
        (test-error (array-for-each 1 #f))
        (test-error (array-for-each list 1 (make-array (make-interval '#(3) '#(4))
                                                       list)))
        (test-error (array-for-each list (make-array (make-interval '#(3) '#(4))
                                                     list) 1))
        (test-error (array-for-each list
                                    (make-array (make-interval '#(3) '#(4))
                                                list)
                                    (make-array (make-interval '#(3 4) '#(4 5))
                                                list)))

        (specialized-array-default-safe? #t)

        ;; (let ((array-builders
        ;;        (vector
        ;;         (list u1-storage-class      (lambda indices (random 0 (expt 2 1))))
        ;;         (list u8-storage-class      (lambda indices (random 0 (expt 2 8))))
        ;;         (list u16-storage-class     (lambda indices (random 0 (expt 2 16))))
        ;;         (list u32-storage-class     (lambda indices (random 0 (expt 2 32))))
        ;;         (list u64-storage-class     (lambda indices (random 0 (expt 2 64))))
        ;;         (list s8-storage-class
        ;;               (lambda indices (random (- (expt 2 7))  (expt 2 7))))
        ;;         (list s16-storage-class
        ;;               (lambda indices (random (- (expt 2 15)) (expt 2 15))))
        ;;         (list s32-storage-class
        ;;               (lambda indices (random (- (expt 2 31)) (expt 2 31))))
        ;;         (list s64-storage-class
        ;;               (lambda indices (random (- (expt 2 63)) (expt 2 63))))
        ;;         (list f32-storage-class
        ;;               (lambda indices (random-real)))
        ;;         (list f64-storage-class
        ;;               (lambda indices (random-real)))
        ;;         (list c64-storage-class
        ;;               (lambda indices (make-rectangular (random-real) (random-real))))
        ;;         (list c128-storage-class
        ;;               (lambda indices (make-rectangular (random-real) (random-real))))
        ;;         (list generic-storage-class
        ;;               (lambda indices indices)))))
        ;;   (do ((i 0 (+ i 1)))
        ;;       ((= i tests))
        ;;     (let* ((domain
        ;;             (random-interval))
        ;;            (lower-bounds
        ;;             (interval-lower-bounds->list domain))
        ;;            (upper-bounds
        ;;             (interval-upper-bounds->list domain))
        ;;            (array-length
        ;;             (lambda (a)
        ;;               (let ((upper-bounds
        ;;                      (interval-upper-bounds->list (array-domain a)))
        ;;                     (lower-bounds
        ;;                      (interval-lower-bounds->list (array-domain a))))
        ;;                 (apply * (map - upper-bounds lower-bounds)))))
        ;;            (arrays
        ;;             (map (lambda (ignore)
        ;;                    (let ((array-builder
        ;;                           (vector-ref array-builders
        ;;                                       (random
        ;;                                        (vector-length array-builders)))))
        ;;                      (array-copy (make-array domain
        ;;                                              (cadr array-builder))
        ;;                                  (car array-builder))))
        ;;                  (local-iota 0 (random 1 7))))
        ;;            (result-array-1
        ;;             (apply array-map
        ;;                    list
        ;;                    arrays))
        ;;            (result-array-2
        ;;             (array-copy
        ;;              (apply array-map
        ;;                     list
        ;;                     arrays)))
        ;;            (getters
        ;;             (map array-getter arrays))
        ;;            (result-array-3
        ;;             (make-array domain
        ;;                         (lambda indices
        ;;                           (map (lambda (g) (apply g indices)) getters)))))
        ;;       (test-assert
        ;;           (and (myarray= result-array-1 result-array-2)
        ;;                (myarray= result-array-2 result-array-3)
        ;;                (equal? (vector->list (array-body result-array-2))
        ;;                        (reverse (array-fold (lambda (x y) (cons x y))
        ;;                                             '()
        ;;                                             result-array-2)))
        ;;                (equal? (vector->list (array-body result-array-2))
        ;;                        (reverse (let ((result '()))
        ;;                                   (array-for-each
        ;;                                    (lambda (f)
        ;;                                      (set! result (cons f result)))
        ;;                                    result-array-2)
        ;;                                   result)))
        ;;                (equal?  (map array-length arrays)
        ;;                         (map (lambda (array)
        ;;                                ((storage-class-length
        ;;                                  (array-storage-class array))
        ;;                                 (array-body array)))
        ;;                              arrays))))
        ;;       )))

        (specialized-array-default-safe? #f)

        ;; (let ((array-builders
        ;;        (vector
        ;;         (list u1-storage-class      (lambda indices (random (expt 2 1))))
        ;;         (list u8-storage-class      (lambda indices (random (expt 2 8))))
        ;;         (list u16-storage-class     (lambda indices (random (expt 2 16))))
        ;;         (list u32-storage-class     (lambda indices (random (expt 2 32))))
        ;;         (list u64-storage-class     (lambda indices (random (expt 2 64))))
        ;;         (list s8-storage-class
        ;;               (lambda indices (random (- (expt 2 7))  (expt 2 7))))
        ;;         (list s16-storage-class
        ;;               (lambda indices (random (- (expt 2 15)) (expt 2 15))))
        ;;         (list s32-storage-class
        ;;               (lambda indices (random (- (expt 2 31)) (expt 2 31))))
        ;;         (list s64-storage-class
        ;;               (lambda indices (random (- (expt 2 63)) (expt 2 63))))
        ;;         (list f32-storage-class
        ;;               (lambda indices (random-real)))
        ;;         (list f64-storage-class
        ;;               (lambda indices (random-real)))
        ;;         (list c64-storage-class
        ;;               (lambda indices (make-rectangular (random-real) (random-real))))
        ;;         (list c128-storage-class
        ;;               (lambda indices (make-rectangular (random-real) (random-real))))
        ;;         (list generic-storage-class (lambda indices indices)))))
        ;;   (do ((i 0 (+ i 1)))
        ;;       ((= i tests))
        ;;     (let* ((domain
        ;;             (random-interval))
        ;;            ;;(_ (begin (write `(domain: ,domain)) (newline)))
        ;;            (lower-bounds
        ;;             (interval-lower-bounds->list domain))
        ;;            (upper-bounds
        ;;             (interval-upper-bounds->list domain))
        ;;            (arrays
        ;;             (map (lambda (ignore)
        ;;                    (let ((array-builder
        ;;                           (vector-ref array-builders
        ;;                                       (random
        ;;                                        (vector-length array-builders)))))
        ;;                      (array-copy (make-array domain
        ;;                                              (cadr array-builder))
        ;;                                  (car array-builder))))
        ;;                  (local-iota 0 (random 1 7))))
        ;;            (result-array-1
        ;;             (apply array-map
        ;;                    list
        ;;                    arrays))
        ;;            (result-array-2
        ;;             (array-copy
        ;;              (apply array-map
        ;;                     list
        ;;                     arrays)))
        ;;            (getters
        ;;             (map array-getter arrays))
        ;;            (result-array-3
        ;;             (make-array domain
        ;;                         (lambda indices
        ;;                           (map (lambda (g) (apply g indices)) getters)))))
        ;;       (test-assert
        ;;           (and (myarray= result-array-1 result-array-2)
        ;;                (myarray= result-array-2 result-array-3)
        ;;                (equal? (vector->list (array-body result-array-2))
        ;;                        (reverse (array-fold cons
        ;;                                             '()
        ;;                                             result-array-2)))
        ;;                (equal? (vector->list (array-body result-array-2))
        ;;                        (reverse (let ((result '()))
        ;;                                   (array-for-each
        ;;                                    (lambda (f)
        ;;                                      (set! result (cons f result)))
        ;;                                    result-array-2)
        ;;                                   result))))))))

        (test-error (array-reduce 'a 'a))
        (test-error (array-reduce 'a (make-array (make-interval '#(1) '#(3))
                                                 list)))

        (let ((A (make-array (make-interval '#(1) '#(11))
                             (lambda (i)
                               (if (even? i)
                                   (matrix 1 i
                                           0 1)
                                   (matrix 1 0
                                           i 1))))))
          (test (array-fold-right x2x2-multiply (matrix 1 0 0 1) A)
              (array-reduce x2x2-multiply A))
          (test-not (equal? (array-reduce x2x2-multiply A)
                            (array-fold x2x2-multiply (matrix 1 0 0 1) A))))

        (let ((A_2 (make-array (make-interval '#(1 1) '#(3 7))
                               (lambda (i j)
                                 (if (and (even? i) (even? j))
                                     (matrix 1 i
                                             j 1)
                                     (matrix 1 j
                                             i -1))))))
          (test (array-fold-right x2x2-multiply (matrix 1 0 0 1) A_2)
              (array-reduce x2x2-multiply A_2))
          (test-not (equal? (array-reduce x2x2-multiply A_2)
                            (array-fold x2x2-multiply (matrix 1 0 0 1) A_2)))
          (test-not (equal? (array-reduce x2x2-multiply A_2)
                            (array-reduce x2x2-multiply (array-rotate A_2 1)))))

        (let ((A_3 (make-array (make-interval '#(1 1 1) '#(3 5 4))
                               (lambda (i j k)
                                 (if (and (even? i) (even? j))
                                     (matrix 1 i
                                             j k)
                                     (matrix k j
                                             i -1))))))
          (test (array-fold-right x2x2-multiply (matrix 1 0 0 1) A_3)
              (array-reduce x2x2-multiply A_3))
          (test-not (equal? (array-reduce x2x2-multiply A_3)
                            (array-fold x2x2-multiply (matrix 1 0 0 1) A_3)))
          (test-not (equal? (array-reduce x2x2-multiply A_3)
                            (array-reduce x2x2-multiply (array-rotate A_3 1)))))

        (let ((A_4 (make-array (make-interval '#(1 1 1 1) '#(3 2 4 3))
                               (lambda (i j k l)
                                 (if (and (even? i) (even? j))
                                     (matrix l i
                                             j k)
                                     (matrix l k
                                             i j))))))
          (test (array-fold-right x2x2-multiply (matrix 1 0 0 1) A_4)
              (array-reduce x2x2-multiply A_4))
          (test-not (equal? (array-reduce x2x2-multiply A_4)
                            (array-fold x2x2-multiply (matrix 1 0 0 1) A_4)))
          (test-not (equal? (array-reduce x2x2-multiply A_4)
                            (array-reduce x2x2-multiply (array-rotate A_4 1)))))

        (let ((A_5 (make-array (make-interval '#(1 1 1 1 1) '#(3 2 4 3 3))
                               (lambda (i j k l m)
                                 (if (even? m)
                                     (matrix (+ m l) i
                                             j k)
                                     (matrix (- l m) k
                                             i j))))))
          (test (array-fold-right x2x2-multiply (matrix 1 0 0 1) A_5)
              (array-reduce x2x2-multiply A_5))
          (test-not (equal? (array-reduce x2x2-multiply A_5)
                            (array-fold x2x2-multiply (matrix 1 0 0 1) A_5)))
          (test-not (equal? (array-reduce x2x2-multiply A_5)
                            (array-reduce x2x2-multiply (array-rotate A_5 1)))))

        (test-error (array-curry 'a 1))
        (test-error
         (array-curry (make-array (make-interval '#(0) '#(1)) list)  'a))
        (test-error
         (array-curry (make-array (make-interval '#(0 0) '#(1 1)) list)  0))
        (test-error
         (array-curry (make-array (make-interval '#(0 0) '#(1 1)) list)  2))

        ;; (let ((array-builders
        ;;        (vector
        ;;         (list u1-storage-class      (lambda indices (random (expt 2 1))))
        ;;         (list u8-storage-class      (lambda indices (random (expt 2 8))))
        ;;         (list u16-storage-class     (lambda indices (random (expt 2 16))))
        ;;         (list u32-storage-class     (lambda indices (random (expt 2 32))))
        ;;         (list u64-storage-class     (lambda indices (random (expt 2 64))))
        ;;         (list s8-storage-class
        ;;               (lambda indices (random (- (expt 2 7))  (expt 2 7))))
        ;;         (list s16-storage-class
        ;;               (lambda indices (random (- (expt 2 15)) (expt 2 15))))
        ;;         (list s32-storage-class
        ;;               (lambda indices (random (- (expt 2 31)) (expt 2 31))))
        ;;         (list s64-storage-class
        ;;               (lambda indices (random (- (expt 2 63)) (expt 2 63))))
        ;;         (list f32-storage-class
        ;;               (lambda indices (random-real)))
        ;;         (list f64-storage-class
        ;;               (lambda indices (random-real)))
        ;;         (list c64-storage-class
        ;;               (lambda indices (make-rectangular (random-real) (random-real))))
        ;;         (list c128-storage-class
        ;;               (lambda indices (make-rectangular (random-real) (random-real))))
        ;;         (list generic-storage-class
        ;;               (lambda indices indices)))))
        ;;   (do ((i 0 (+ i 1)))
        ;;       ((= i tests))
        ;;     (let* ((domain
        ;;             (random-interval 2 7))
        ;;            (lower-bounds
        ;;             (interval-lower-bounds->list domain))
        ;;            (upper-bounds
        ;;             (interval-upper-bounds->list domain))
        ;;            (array-builder
        ;;             (vector-ref array-builders
        ;;                         (random (vector-length array-builders))))
        ;;            (random-array-element
        ;;             (cadr array-builder))
        ;;            (storage-class
        ;;             (car array-builder))
        ;;            (Array
        ;;             (array-copy (make-array domain
        ;;                                     random-array-element)
        ;;                         storage-class))
        ;;            (copied-array
        ;;             (array-copy Array
        ;;                         storage-class))
        ;;            (inner-dimension
        ;;             (random 1 (interval-dimension domain)))
        ;;            (domains
        ;;             (call-with-values
        ;;                 (lambda () (interval-projections domain inner-dimension))
        ;;               list))
        ;;            (outer-domain
        ;;             (car domains))
        ;;            (inner-domain
        ;;             (cadr domains))
        ;;            (immutable-curry
        ;;             (array-curry (make-array (array-domain Array)
        ;;                                      (array-getter Array))
        ;;                          inner-dimension))
        ;;            (mutable-curry
        ;;             (array-curry (make-array (array-domain Array)
        ;;                                      (array-getter Array)
        ;;                                      (array-setter Array))
        ;;                          inner-dimension))
        ;;            (specialized-curry
        ;;             (array-curry Array inner-dimension))
        ;;            (immutable-curry-from-definition
        ;;             (call-with-values
        ;;                 (lambda () (interval-projections (array-domain Array)
        ;;                                              inner-dimension))
        ;;               (lambda (outer-interval inner-interval)
        ;;                 (make-array
        ;;                  outer-interval
        ;;                  (lambda outer-multi-index
        ;;                    (make-array
        ;;                     inner-interval
        ;;                     (lambda inner-multi-index
        ;;                       (apply (array-getter Array)
        ;;                              (append outer-multi-index
        ;;                                      inner-multi-index)))))))))
        ;;            (mutable-curry-from-definition
        ;;             (call-with-values
        ;;                 (lambda () (interval-projections (array-domain Array)
        ;;                                              inner-dimension))
        ;;               (lambda (outer-interval inner-interval)
        ;;                 (make-array
        ;;                  outer-interval
        ;;                  (lambda outer-multi-index
        ;;                    (make-array
        ;;                     inner-interval
        ;;                     (lambda inner-multi-index
        ;;                       (apply (array-getter Array)
        ;;                              (append outer-multi-index
        ;;                                      inner-multi-index)))
        ;;                     (lambda (v . inner-multi-index)
        ;;                       (apply (array-setter Array) v
        ;;                              (append outer-multi-index
        ;;                                      inner-multi-index)))))))))
        ;;            (specialized-curry-from-definition
        ;;             (call-with-values
        ;;                 (lambda () (interval-projections (array-domain Array)
        ;;                                              inner-dimension))
        ;;               (lambda (outer-interval inner-interval)
        ;;                 (make-array
        ;;                  outer-interval
        ;;                  (lambda outer-multi-index
        ;;                    (specialized-array-share
        ;;                     Array
        ;;                     inner-interval
        ;;                     (lambda inner-multi-index
        ;;                       (apply values
        ;;                              (append outer-multi-index
        ;;                                      inner-multi-index))))))))))
        ;;       ;; mutate the curried array
        ;;       (for-each
        ;;        (lambda (curried-array)
        ;;          (let ((outer-getter
        ;;                 (array-getter curried-array)))
        ;;            (do ((i 0 (+ i 1)))
        ;;                ((= i 50)) ;; used to be tests, not 50, but 50 will do fine
        ;;              (call-with-values
        ;;                  (lambda ()
        ;;                    (random-multi-index outer-domain))
        ;;                (lambda outer-multi-index
        ;;                  (let ((inner-setter
        ;;                         (array-setter (apply outer-getter
        ;;                                              outer-multi-index))))
        ;;                    (call-with-values
        ;;                        (lambda ()
        ;;                          (random-multi-index inner-domain))
        ;;                      (lambda inner-multi-index
        ;;                        (let ((new-element
        ;;                               (random-array-element)))
        ;;                          (apply inner-setter
        ;;                                 new-element
        ;;                                 inner-multi-index)
        ;;                          ;; mutate the copied array without currying
        ;;                          (apply (array-setter copied-array)
        ;;                                 new-element
        ;;                                 (append outer-multi-index
        ;;                                         inner-multi-index)))))))))))
        ;;        (list mutable-curry
        ;;              specialized-curry
        ;;              mutable-curry-from-definition
        ;;              specialized-curry-from-definition
        ;;              ))

        ;;       (and (or (myarray= Array copied-array) (error "Arggh"))
        ;;            (or (array-every array? immutable-curry) (error "Arggh"))
        ;;            (or (array-every (lambda (a) (not (mutable-array? a)))
        ;;                             immutable-curry)
        ;;                (error "Arggh"))
        ;;            (or (array-every mutable-array? mutable-curry) (error "Arggh"))
        ;;            (or (array-every (lambda (a) (not (specialized-array? a)))
        ;;                             mutable-curry)
        ;;                (error "Arggh"))
        ;;            (or (array-every specialized-array? specialized-curry)
        ;;                (error "Arggh"))
        ;;            (or (array-every
        ;;                 (lambda (xy) (apply myarray= xy))
        ;;                 (array-map list immutable-curry
        ;;                            immutable-curry-from-definition))
        ;;                (error "Arggh"))
        ;;            (or (array-every
        ;;                 (lambda (xy) (apply myarray= xy))
        ;;                 (array-map list mutable-curry
        ;;                            mutable-curry-from-definition))
        ;;                (error "Arggh"))
        ;;            (or (array-every
        ;;                 (lambda (xy) (apply myarray= xy))
        ;;                 (array-map list specialized-curry
        ;;                            specialized-curry-from-definition))
        ;;                (error "Arggh"))))))

        (test-error (specialized-array-share 1 1 1))
        (test-error (specialized-array-share
                     (make-specialized-array (make-interval '#(1) '#(2)))
                     1 1))
        ;; (test-error (specialized-array-share
        ;;              (make-specialized-array (make-interval '#(1) '#(2)))
        ;;              (make-interval '#(0) '#(1))
        ;;              1))

        (test-assert
            (myarray= (list->array (reverse (local-iota 0 10))
                                   (make-interval '#(0) '#(10)))
                      (specialized-array-share
                       (list->array (local-iota 0 10)
                                    (make-interval '#(0) '#(10)))
                       (make-interval '#(0) '#(10))
                       (lambda (i)
                         (- 9 i)))))

        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let* ((n (random 1 11))
        ;;          (permutation (random-permutation n))
        ;;          (input-vec
        ;;           (list->vector (f64vector->list (random-f64vector n)))))
        ;;     (test (%%vector-permute input-vec permutation)
        ;;         (vector-permute input-vec permutation))
        ;;     (test (vector-permute input-vec permutation)
        ;;         (list->vector (%%vector-permute->list input-vec permutation)))))

        (specialized-array-default-safe? #t)

        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let* ((interval (random-interval))
        ;;          (axes (local-iota 0 (interval-dimension interval)))
        ;;          (lower-bounds (interval-lower-bounds->vector interval))
        ;;          (upper-bounds (interval-upper-bounds->vector interval))
        ;;          (a (array-copy (make-array interval list)))
        ;;          (new-axis-order
        ;;           (vector-permute (list->vector axes)
        ;;                           (random-permutation (length axes))))
        ;;          (reverse-order?
        ;;           (list->vector (map (lambda (x) (zero? (random 2))) axes))))
        ;;     (let ((b (make-array
        ;;               (make-interval (vector-permute lower-bounds new-axis-order)
        ;;                              (vector-permute upper-bounds new-axis-order))
        ;;               (lambda multi-index
        ;;                 (apply
        ;;                  (array-getter a)
        ;;                  (let* ((n (vector-length new-axis-order))
        ;;                         (multi-index-vector
        ;;                          (list->vector multi-index))
        ;;                         (result (make-vector n)))
        ;;                    (do ((i 0 (+ i 1)))
        ;;                        ((= i n) (vector->list result))
        ;;                      (vector-set!
        ;;                       result
        ;;                       (vector-ref new-axis-order i)
        ;;                       (if (vector-ref reverse-order?
        ;;                                       (vector-ref new-axis-order i))
        ;;                           (+ (vector-ref lower-bounds
        ;;                                          (vector-ref new-axis-order i))
        ;;                              (- (vector-ref upper-bounds
        ;;                                             (vector-ref new-axis-order i))
        ;;                                 (vector-ref multi-index-vector i)
        ;;                                 1))
        ;;                           (vector-ref multi-index-vector i)))))))))
        ;;           (c (specialized-array-share
        ;;               a
        ;;               (make-interval (vector-permute lower-bounds new-axis-order)
        ;;                              (vector-permute upper-bounds new-axis-order))
        ;;               (lambda multi-index
        ;;                 (apply
        ;;                  values
        ;;                  (let* ((n (vector-length new-axis-order))
        ;;                         (multi-index-vector (list->vector multi-index))
        ;;                         (result (make-vector n)))
        ;;                    (do ((i 0 (+ i 1)))
        ;;                        ((= i n) (vector->list result))
        ;;                      (vector-set!
        ;;                       result
        ;;                       (vector-ref new-axis-order i)
        ;;                       (if (vector-ref reverse-order?
        ;;                                       (vector-ref new-axis-order i))
        ;;                           (+ (vector-ref lower-bounds
        ;;                                          (vector-ref new-axis-order i))
        ;;                              (- (vector-ref upper-bounds
        ;;                                             (vector-ref new-axis-order i))
        ;;                                 (vector-ref multi-index-vector i)
        ;;                                 1))
        ;;                           (vector-ref multi-index-vector i))))))))))
        ;;       (test-assert (myarray= b c)))))

        (specialized-array-default-safe? #f)

        ;; (do ((i 0 (+ i 1)))
        ;;     ((= i tests))
        ;;   (let* ((interval (random-interval))
        ;;          (axes (local-iota 0 (interval-dimension interval)))
        ;;          (lower-bounds (interval-lower-bounds->vector interval))
        ;;          (upper-bounds (interval-upper-bounds->vector interval))
        ;;          (a (array-copy (make-array interval list)))
        ;;          (new-axis-order
        ;;           (vector-permute (list->vector axes)
        ;;                           (random-permutation (length axes))))
        ;;          (reverse-order?
        ;;           (list->vector (map (lambda (x) (zero? (random 2))) axes))))
        ;;     (let ((b (make-array
        ;;               (make-interval (vector-permute lower-bounds new-axis-order)
        ;;                              (vector-permute upper-bounds new-axis-order))
        ;;               (lambda multi-index
        ;;                 (apply
        ;;                  (array-getter a)
        ;;                  (let* ((n (vector-length new-axis-order))
        ;;                         (multi-index-vector (list->vector multi-index))
        ;;                         (result (make-vector n)))
        ;;                    (do ((i 0 (+ i 1)))
        ;;                        ((= i n) (vector->list result))
        ;;                      (vector-set!
        ;;                       result
        ;;                       (vector-ref new-axis-order i)
        ;;                       (if (vector-ref reverse-order?
        ;;                                       (vector-ref new-axis-order i))
        ;;                           (+ (vector-ref lower-bounds
        ;;                                          (vector-ref new-axis-order i))
        ;;                              (- (vector-ref upper-bounds
        ;;                                             (vector-ref new-axis-order i))
        ;;                                 (vector-ref multi-index-vector i)
        ;;                                 1))
        ;;                           (vector-ref multi-index-vector i)))))))))
        ;;           (c (specialized-array-share
        ;;               a
        ;;               (make-interval
        ;;                (vector-permute lower-bounds new-axis-order)
        ;;                (vector-permute upper-bounds new-axis-order))
        ;;               (lambda multi-index
        ;;                 (apply
        ;;                  values
        ;;                  (let* ((n (vector-length new-axis-order))
        ;;                         (multi-index-vector (list->vector multi-index))
        ;;                         (result (make-vector n)))
        ;;                    (do ((i 0 (+ i 1)))
        ;;                        ((= i n) (vector->list result))
        ;;                      (vector-set!
        ;;                       result
        ;;                       (vector-ref new-axis-order i)
        ;;                       (if (vector-ref reverse-order?
        ;;                                       (vector-ref new-axis-order i))
        ;;                           (+ (vector-ref lower-bounds
        ;;                                          (vector-ref new-axis-order i))
        ;;                              (- (vector-ref upper-bounds
        ;;                                             (vector-ref new-axis-order i))
        ;;                                 (vector-ref multi-index-vector i)
        ;;                                 1))
        ;;                           (vector-ref multi-index-vector i))))))))))
        ;;       (test-assert (myarray= b c)))))

        (let ((int (make-interval '#(0 0) '#(10 10)))
              (translation '#(10 -2)))
          (test-error (interval-translate 'a 10))
          (test-error (interval-translate int 10))
          (test-error (interval-translate int '#(a b)))
          (test-error (interval-translate int '#(1. 2.)))
          (test-error (interval-translate int '#(1)))
          (do ((i 0 (+ i 1)))
              ((= i tests))
            (let* ((int (random-interval))
                   (lower-bounds (interval-lower-bounds->vector int))
                   (upper-bounds (interval-upper-bounds->vector int))
                   (translation
                    (list->vector
                     (map (lambda (x)
                            (random -10 10))
                          (local-iota 0 (vector-length lower-bounds))))))
              (interval= (interval-translate int translation)
                         (make-interval
                          (vector-map + lower-bounds translation)
                          (vector-map + upper-bounds translation)))))
          )

        (let* ((specialized-array
                (array-copy (make-array (make-interval '#(0 0) '#(10 12))
                                        list)))
               (mutable-array (let ((temp (array-copy specialized-array)))
                                (make-array (array-domain temp)
                                            (array-getter temp)
                                            (array-setter temp))))
               (immutable-array (make-array (array-domain mutable-array)
                                            (array-getter mutable-array)))
               (translation '#(10 -2)))

          (test-error (array-translate 'a 1))
          (test-error (array-translate immutable-array '#(1.)))
          (test-error (array-translate immutable-array '#(0 2 3)))
          (let ((specialized-result
                 (array-translate specialized-array translation)))
            (test-assert (specialized-array? specialized-result)))
          (let ((mutable-result (array-translate mutable-array translation)))
            (test-assert (and (mutable-array? mutable-array)
                              (not (specialized-array? mutable-array))
                              (mutable-array? mutable-result)
                              (not (specialized-array? mutable-result)))))
          (let ((immutable-result (array-translate immutable-array translation)))
            (test-assert (and (array? immutable-array)
                              (not (mutable-array? immutable-array))
                              (array? immutable-result)
                              (not (mutable-array? immutable-result)))))

          ;; (do ((i 0 (+ i 1)))
          ;;     ((= i tests))
          ;;   (let* ((domain (random-interval))
          ;;          (Array (let ((temp (make-array domain list)))
          ;;                   (case (random-integer 3)
          ;;                     ((0) temp)
          ;;                     ((1) (array-copy temp))
          ;;                     ((2) (let ((temp (array-copy temp)))
          ;;                            (make-array (array-domain temp)
          ;;                                        (array-getter temp)
          ;;                                        (array-setter temp)))))))
          ;;          (translation
          ;;           (list->vector
          ;;            (map (lambda (x) (random -10 10))
          ;;                 (vector->list (%%interval-lower-bounds domain))))))
          ;;     (let ((translated-array       (array-translate Array translation))
          ;;           (my-translated-array (my-array-translate Array translation)))
          ;;       (if (mutable-array? Array)
          ;;           (let ((translated-domain
          ;;                  (interval-translate domain translation)))
          ;;             (do ((j 0 (+ j 1)))
          ;;                 ((= j 50))
          ;;               (call-with-values
          ;;                   (lambda ()
          ;;                     (random-multi-index translated-domain))
          ;;                 (lambda multi-index
          ;;                   (let ((value (random-integer 10000)))
          ;;                     (apply (array-setter translated-array)
          ;;                            value multi-index)
          ;;                     (apply (array-setter my-translated-array)
          ;;                            value multi-index)))))))
          ;;       (test-assert
          ;;           (myarray= (array-translate Array translation)
          ;;                     (my-array-translate Array translation))))))
          )
        )

      (test-group "permutation tests"
        (let* ((specialized
                (make-specialized-array (make-interval '#(0 0 0 0 0)
                                                       '#(1 1 1 1 1))))
               (mutable (make-array (array-domain specialized)
                                    (array-getter specialized)
                                    (array-setter specialized)))
               (A (array-translate  mutable '#(0 0 0 0 0))))
          (test-error ((array-getter A) 0 0))
          (test-error ((array-setter A) 'a 0 0)))

        (let ((int (make-interval '#(0 0) '#(10 10)))
              (permutation '#(1 0)))
          (test-error (interval-permute 'a 10))
          (test-error (interval-permute int 10))
          (test-error (interval-permute int '#(a b)))
          (test-error (interval-permute int '#(1. 2.)))
          (test-error (interval-permute int '#(10 -2)))
          (test-error (interval-permute int '#(0)))
          (do ((i 0 (+ i 1)))
              ((= i tests))
            (let* ((int (random-interval))
                   (lower-bounds (interval-lower-bounds->vector int))
                   (upper-bounds (interval-upper-bounds->vector int))
                   (permutation
                    (random-permutation (vector-length lower-bounds))))
              (interval=
               (interval-permute int permutation)
               (make-interval (vector-permute lower-bounds permutation)
                              (vector-permute upper-bounds permutation))))))

        (let* ((specialized-array
                (array-copy (make-array (make-interval '#(0 0) '#(10 12))
                                        list)))
               (mutable-array (let ((temp (array-copy specialized-array)))
                                (make-array (array-domain temp)
                                            (array-getter temp)
                                            (array-setter temp))))
               (immutable-array (make-array (array-domain mutable-array)
                                            (array-getter mutable-array)))
               (permutation '#(1 0)))

          (test-error (array-permute 'a 1))
          (test-error (array-permute immutable-array '#(1.)))
          (test-error (array-permute immutable-array '#(2)))
          (test-error (array-permute immutable-array '#(0 1 2)))
          (let ((specialized-result
                 (array-permute specialized-array permutation)))
            (test-assert (specialized-array? specialized-result)))
          (let ((mutable-result (array-permute mutable-array permutation)))
            (test-assert (and (mutable-array? mutable-array)
                              (not (specialized-array? mutable-array))
                              (mutable-array? mutable-result)
                              (not (specialized-array? mutable-result)))))
          (let ((immutable-result (array-permute immutable-array permutation)))
            (test-assert (and (array? immutable-array)
                              (not (mutable-array? immutable-array))
                              (array? immutable-result)
                              (not (mutable-array? immutable-result)))))

          (specialized-array-default-safe? #t)

          (do ((i 0 (+ i 1)))
              ((= i tests))
            (let* ((domain (random-interval))
                   (Array (let ((temp (make-array domain list)))
                            (case (random-integer 3)
                              ((0) temp)
                              ((1) (array-copy temp))
                              ((2) (let ((temp (array-copy temp)))
                                     (make-array (array-domain temp)
                                                 (array-getter temp)
                                                 (array-setter temp)))))))
                   (permutation
                    (random-permutation (interval-dimension domain))))
              (let* ((permuted-array       (array-permute Array permutation))
                     (my-permuted-array (my-array-permute Array permutation)))
                (let ((permuted-domain (interval-permute domain permutation)))
                  (do ((j 0 (+ j 1)))
                      ((= j 50))
                    (call-with-values
                        (lambda ()
                          (random-multi-index permuted-domain))
                      (lambda multi-index
                        (test (apply (array-getter my-permuted-array)
                                     multi-index)
                            (apply (array-getter permuted-array)
                                   multi-index))))))
                (if (mutable-array? Array)
                    (let ((permuted-domain
                           (interval-permute domain permutation)))
                      (do ((j 0 (+ j 1)))
                          ((= j 50))
                        (call-with-values
                            (lambda ()
                              (random-multi-index permuted-domain))
                          (lambda multi-index
                            (let ((value (random-integer 10000)))
                              (apply (array-setter permuted-array) value
                                     multi-index)
                              (apply (array-setter my-permuted-array) value
                                     multi-index)))))))
                (test-assert (myarray= permuted-array
                                       my-permuted-array)))))

          (specialized-array-default-safe? #f)

          (do ((i 0 (+ i 1)))
              ((= i tests))
            (let* ((domain (random-interval))
                   (Array (let ((temp (make-array domain list)))
                            (case (random-integer 3)
                              ((0) temp)
                              ((1) (array-copy temp))
                              ((2) (let ((temp (array-copy temp)))
                                     (make-array (array-domain temp)
                                                 (array-getter temp)
                                                 (array-setter temp)))))))
                   (permutation
                    (random-permutation (interval-dimension domain)))
                   (permuted-array       (array-permute Array permutation))
                   (my-permuted-array (my-array-permute Array permutation))
                   (permuted-domain (interval-permute domain permutation)))
              ;;(write `(permuted: ,permuted-array my-permuted: ,my-permuted-array)) (newline)
              (do ((j 0 (+ j 1)))
                  ((= j 50))
                (call-with-values
                    (lambda () (random-multi-index permuted-domain))
                  (lambda multi-index
                    ;;(write `(multi-index: ,multi-index domain: ,permuted-domain)) (newline)
                    (test (apply (array-getter my-permuted-array) multi-index)
                        (apply (array-getter permuted-array) multi-index)))))
              (if (mutable-array? Array)
                  (let ((permuted-domain
                         (interval-permute domain permutation)))
                    (do ((j 0 (+ j 1)))
                        ((= j 50))
                      (call-with-values
                          (lambda ()
                            (random-multi-index permuted-domain))
                        (lambda multi-index
                          (let ((value (random-integer 10000)))
                            (apply (array-setter permuted-array) value
                                   multi-index)
                            (apply (array-setter my-permuted-array) value
                                   multi-index)))))))
              (test-assert (myarray= permuted-array
                                     my-permuted-array))))
          )

        ;; because array-rotate is built using the array-permute
        ;; infrastructure, we won't test as much

        (test-error (array-rotate 1 1))
        (test-error
         (array-rotate (make-array (make-interval '#(0 0) '#(2 3)) list) 'a))
        (test-error
         (array-rotate (make-array (make-interval '#(0 0) '#(2 3)) list) 1.))
        (test-error
         (array-rotate (make-array (make-interval '#(0 0) '#(2 3)) list) 1/2))
        (test-error
         (array-rotate (make-array (make-interval '#(0 0) '#(2 3)) list) -1))
        (test-error
         (array-rotate (make-array (make-interval '#(0 0) '#(2 3)) list) 4))
        (test-error (interval-rotate 1 1))
        (test-error (interval-rotate (make-interval '#(0 0) '#(2 3)) 'a))
        (test-error (interval-rotate (make-interval '#(0 0) '#(2 3)) 1.))
        (test-error (interval-rotate (make-interval '#(0 0) '#(2 3)) 37))

        (for-each
         (lambda (n)
           (let* ((upper-bounds (make-vector n 2))
                  (lower-bounds (make-vector n 0))
                  (domain (make-interval lower-bounds upper-bounds))
                  (A (array-copy (make-array domain list)))
                  (immutable-A
                   (let ((A (array-copy A))) ;; copy A
                     (make-array domain
                                 (array-getter A))))
                  (mutable-A
                   (let ((A (array-copy A))) ;; copy A
                     (make-array domain
                                 (array-getter A)
                                 (array-setter A)))))
             (for-each (lambda (dim)
                         (let ((permutation
                                (list->vector
                                 (append
                                  (local-iota dim n)
                                  (local-iota 0 dim)))))
                           (let ((rA
                                  (array-rotate A dim))
                                 (pA
                                  (array-permute A permutation)))
                             (if (not (and (specialized-array? rA)
                                           (specialized-array? pA)
                                           (myarray= rA pA)))
                                 (error "blah rotate specialized")))
                           (let ((rA
                                  (array-rotate immutable-A dim))
                                 (pA
                                  (array-permute immutable-A permutation)))
                             (if (not (and (array? rA)
                                           (array? pA)
                                           (myarray= rA pA)))
                                 (error "blah rotate immutable")))
                           (let ((rA
                                  (array-rotate mutable-A dim))
                                 (pA
                                  (array-permute mutable-A permutation)))
                             (if (not (and (mutable-array? rA)
                                           (mutable-array? pA)
                                           (myarray= rA pA)))
                                 (error "blah rotate mutable")))
                           (test (array-domain (array-rotate mutable-A dim))
                               (interval-rotate (array-domain A) dim))))
                       (iota n))))
         (iota 5 1))
        )

      (test-group "intersect/scale/sample"
        (let ((a (make-interval '#(0 0) '#(10 10)))
              (b (make-interval '#(0) '#(10)))
              (c (make-interval '#(10 10) '#(20 20))))
          (test-error (interval-intersect 'a))
          (test-error (interval-intersect  a 'a))
          (test-error (interval-intersect a b)))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((dimension (random 1 6))
                 (number-of-intervals (random 1 4))
                 (intervals (map (lambda (x)
                                   (random-interval dimension (+ dimension 1)))
                                 (local-iota 0 number-of-intervals))))
            (test (apply interval-intersect intervals)
                (apply my-interval-intersect intervals))))

        (test-error (interval-scale 1 'a))
        (test-error (interval-scale (make-interval '#(1) '#(2)) 'a))
        (test-error (interval-scale (make-interval '#(0) '#(1)) 'a))
        (test-error (interval-scale (make-interval '#(0) '#(1)) '#(a)))
        (test-error (interval-scale (make-interval '#(0) '#(1)) '#(0)))
        (test-error (interval-scale (make-interval '#(0) '#(1)) '#(1.)))
        (test-error (interval-scale (make-interval '#(0) '#(1)) '#(1 2)))

        (do ((i 0 (fx+ i 1)))
            ((fx=? i tests))
          (let* ((interval (random-nonnegative-interval))
                 (scales (random-positive-vector (interval-dimension interval))))
            (test (my-interval-scale interval scales)
                (interval-scale interval scales))))

        (test-error (array-sample 'a 'a))
        (test-error
         (array-sample (make-array (make-interval '#(1) '#(2)) list) 'a))
        (test-error
         (array-sample (make-array (make-interval '#(0) '#(2)) list) 'a))
        (test-error
         (array-sample (make-array (make-interval '#(0) '#(2)) list) '#(1.)))
        (test-error
         (array-sample (make-array (make-interval '#(0) '#(2)) list) '#(0)))
        (test-error
         (array-sample (make-array (make-interval '#(0) '#(2)) list) '#(2 1)))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((domain (random-nonnegative-interval 1 6))
                 (Array (let ((temp (make-array domain list)))
                          (case (random-integer 3)
                            ((0) temp)
                            ((1) (array-copy temp))
                            ((2) (let ((temp (array-copy temp)))
                                   (make-array (array-domain temp)
                                               (array-getter temp)
                                               (array-setter temp)))))))
                 (scales (random-positive-vector (interval-dimension domain)))
                 (sampled-array (array-sample Array scales))
                 (my-sampled-array (myarray-sample Array scales)))

            (if (mutable-array? Array)
                (let ((scaled-domain (interval-scale domain scales)))
                  (do ((j 0 (+ j 1)))
                      ((= j 50))
                    (call-with-values
                        (lambda ()
                          (random-multi-index scaled-domain))
                      (lambda multi-index
                        (let ((value (random-integer 10000)))
                          (apply (array-setter sampled-array) value multi-index)
                          (apply (array-setter my-sampled-array) value
                                 multi-index)))))))
            (test-assert (myarray= sampled-array
                                   my-sampled-array))))

        (test-error
         (array-extract (make-array (make-interval '#(0 0) '#(1 1)) list)
                        'a))
        (test-error (array-extract 'a (make-interval '#(0 0) '#(1 1))))
        (test-error
         (array-extract (make-array (make-interval '#(0 0) '#(1 1)) list)
                        (make-interval '#(0) '#(1))))
        (test-error
         (array-extract (make-array (make-interval '#(0 0) '#(1 1)) list)
                        (make-interval '#(0 0) '#(1 3))))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i tests))
          (let* ((domain (random-interval))
                 (subdomain (random-subinterval domain))
                 (spec-A (array-copy (make-array domain list)))
                 (spec-A-extract (array-extract spec-A subdomain))
                 (mut-A (let ((A-prime (array-copy spec-A)))
                          (make-array domain
                                      (array-getter A-prime)
                                      (array-setter A-prime))))
                 (mut-A-extract (array-extract mut-A subdomain))
                 (immutable-A (let ((A-prime (array-copy spec-A)))
                                (make-array domain
                                            (array-getter A-prime))))
                 (immutable-A-extract (array-extract immutable-A subdomain))
                 (spec-B (array-copy (make-array domain list)))
                 (spec-B-extract (array-extract spec-B subdomain))
                 (mut-B (let ((B-prime (array-copy spec-B)))
                          (make-array domain
                                      (array-getter B-prime)
                                      (array-setter B-prime))))
                 (mut-B-extract (array-extract mut-B subdomain)))
            ;; test that the extracts are the same kind of arrays as the original
            (if (not (and (specialized-array? spec-A)
                          (specialized-array? spec-A-extract)
                          (mutable-array? mut-A)
                          (mutable-array? mut-A-extract)
                          (not (specialized-array? mut-A))
                          (not (specialized-array? mut-A-extract))
                          (array? immutable-A)
                          (array? immutable-A-extract)
                          (not (mutable-array? immutable-A))
                          (not (mutable-array? immutable-A-extract))
                          (equal? (array-domain spec-A-extract) subdomain)
                          (equal? (array-domain mut-A-extract) subdomain)
                          (equal? (array-domain immutable-A-extract) subdomain)))
                (error "extract: Aargh!"))
            ;; test that applying the original setter to arguments in
            ;; the subdomain gives the same answer as applying the
            ;; setter of the extracted array to the same arguments.
            (for-each (lambda (A B A-extract B-extract)
                        (let ((A-setter (array-setter A))
                              (B-extract-setter (array-setter B-extract)))
                          (do ((i 0 (fx+ i 1)))
                              ((fx=? i 100)
                               (test-assert (myarray= spec-A spec-B))
                               (test-assert
                                   (myarray= spec-A-extract spec-B-extract)))
                            (call-with-values
                                (lambda ()
                                  (random-multi-index subdomain))
                              (lambda multi-index
                                (let ((val (random-real)))
                                  (apply A-setter val multi-index)
                                  (apply B-extract-setter val multi-index)))))))
                      (list spec-A mut-A)
                      (list spec-B mut-B)
                      (list spec-A-extract mut-A-extract)
                      (list spec-B-extract mut-B-extract))))
        )

      (test-group "tile/reverse/flip"
        (test-error (array-tile 'a '#(10)))
        (test-error
         (array-tile (make-array (make-interval '#(0 0) '#(10 10)) list)
                     'a))
        (test-error
         (array-tile (make-array (make-interval '#(0 0) '#(10 10)) list)
                     '#(a a)))
        (test-error
         (array-tile (make-array (make-interval '#(0 0) '#(10 10)) list)
                     '#(-1 1)))
        (test-error
         (array-tile (make-array (make-interval '#(0 0) '#(10 10)) list)
                     '#(10)))

        (do ((d 1 (fx+ d 1)))
            ((fx=? d 6))
          (let* ((A (make-array (make-interval (make-vector d 100)) list))
                 (B (array-tile A (make-vector d 10)))
                 (index (make-list d 12)))
            (test-error (apply (array-getter B) index))))

        ;; (do ((i 0 (fx+ i 1)))
        ;;     ((fx=? i tests))
        ;;   (let* ((domain
        ;;           (random-interval))
        ;;          (array
        ;;           (let ((res (make-array domain list)))
        ;;             (case (random-integer 3)
        ;;               ;; immutable
        ;;               ((0) res)
        ;;               ;; specialized
        ;;               ((1) (array-copy res))
        ;;               (else
        ;;                ;; mutable, but not specialized
        ;;                (let ((res (array-copy res)))
        ;;                  (make-array domain
        ;;                              (array-getter res)
        ;;                              (array-setter res)))))))
        ;;          (lowers
        ;;           (%%interval-lower-bounds domain))
        ;;          (uppers
        ;;           (%%interval-upper-bounds domain))
        ;;          (sidelengths
        ;;           (vector-map (lambda (l u)
        ;;                         (let ((dim (- u l)))
        ;;                           (random 1 (ceiling-quotient (* dim 7) 5))))
        ;;                       lowers uppers))
        ;;          (result
        ;;           (array-tile array sidelengths))
        ;;          (test-result
        ;;           (my-array-tile array sidelengths)))

        ;;     ;; extract-array is tested independently, so we just make a few tests.

        ;;     ;; test all the subdomain tiles are the same
        ;;     (test-assert
        ;;         (array-every (lambda (r t)
        ;;                        (equal? (array-domain r) (array-domain t)))
        ;;                      result test-result))
        ;;     ;; test that the subarrays are the same type
        ;;     (test-assert
        ;;         (array-every
        ;;          (lambda (r t)
        ;;            (and
        ;;             (eq? (mutable-array? r) (mutable-array? t))
        ;;             (eq? (mutable-array? r) (mutable-array? array))
        ;;             (eq? (specialized-array? r) (specialized-array? t))
        ;;             (eq? (specialized-array? r) (specialized-array? array))))
        ;;          result test-result))
        ;;     ;; test that the first tile has the right values
        ;;     (test-assert
        ;;         (myarray= (apply (array-getter result)
        ;;                          (make-list (vector-length lowers) 0))
        ;;                   (apply (array-getter test-result)
        ;;                          (make-list (vector-length lowers) 0))))
        ;;     ))

        (test-error (array-reverse 'a 'a))
        (test-error
         (array-reverse (make-array (make-interval '#(0 0) '#(2 2)) list)
                        'a))
        (test-error
         (array-reverse (make-array (make-interval '#(0 0) '#(2 2)) list)
                        '#(1 0)))
        (test-error
         (array-reverse (make-array (make-interval '#(0 0) '#(2 2)) list)
                        '#(#t)))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((domain (random-interval))
                 (Array (let ((temp (make-array domain list)))
                          (case (random-integer 3)
                            ((0) temp)
                            ((1) (array-copy temp))
                            ((2) (let ((temp (array-copy temp)))
                                   (make-array (array-domain temp)
                                               (array-getter temp)
                                               (array-setter temp)))))))
                 (flips (vector-map (lambda (x) (random-boolean))
                                    (make-vector (interval-dimension domain))))
                 (reversed-array (array-reverse Array flips))
                 (my-reversed-array (myarray-reverse Array flips)))

            (if (mutable-array? Array)
                (do ((j 0 (+ j 1)))
                    ((= j 50))
                  (call-with-values
                      (lambda ()
                        (random-multi-index domain))
                    (lambda multi-index
                      (let ((value (random-integer 10000)))
                        (apply (array-setter reversed-array) value multi-index)
                        (apply (array-setter my-reversed-array) value
                               multi-index))))))
            (test-assert (myarray= reversed-array
                                   my-reversed-array))))

        ;; next test that the optional flip? argument is computed correctly.

        (for-each
         (lambda (n)
           (let* ((upper-bounds (make-vector n 2))
                  (lower-bounds (make-vector n 0))
                  (domain (make-interval lower-bounds upper-bounds))
                  (A (array-copy (make-array domain list)))
                  (immutable-A
                   (let ((A (array-copy A))) ;; copy A
                     (make-array domain
                                 (array-getter A))))
                  (mutable-A
                   (let ((A (array-copy A))) ;; copy A
                     (make-array domain
                                 (array-getter A)
                                 (array-setter A))))
                  (flip? (make-vector n #t)))
             (let ((r1 (array-reverse A))
                   (r2 (array-reverse A flip?)))
               (test-assert (and (specialized-array? r1)
                                 (specialized-array? r2)
                                 (myarray= r1 r2))))
             (let ((r1 (array-reverse mutable-A))
                   (r2 (array-reverse mutable-A flip?)))
               (test-assert (and (mutable-array? r1)
                                 (mutable-array? r2)
                                 (myarray= r1 r2))))
             (let ((r1 (array-reverse immutable-A))
                   (r2 (array-reverse immutable-A flip?)))
               (test-assert (and (array? r1)
                                 (array? r2)
                                 (myarray= r1 r2))))))
         (iota 5 1))

        (test-error (array-assign! 'a 'a))
        (test-error
         (array-assign! (make-array (make-interval '#(0 0) '#(1 1)) values) 'a))
        (test-error
         (array-assign! (array-copy (make-array (make-interval '#(0 0) '#(1 1))
                                                values))
                        'a))
        (test-error
         (array-assign! (array-copy (make-array (make-interval '#(0 0) '#(1 1))
                                                values))
                        (make-array (make-interval '#(0 0) '#(2 1)) values)))
        (test-error
         (array-assign! (make-array (make-interval '#(1 2)) list list) ; invalid
                        (make-array (make-interval '#(0 0) '#(2 1)) values)))
        (test-error
         (array-assign! (array-rotate
                         (array-copy (make-array (make-interval '#(2 3))
                                                 list ))
                         1)
                        (make-array (make-interval '#(2 3)) list)))

        (let ( ;; elements in order
              (destination (make-specialized-array (make-interval '#(3 2))))
              ;; not the same interval, but same volume
              (source (array-rotate (make-array (make-interval '#(3 2)) list)
                                    1)))
          (array-assign! destination source)
          (test (array->list destination)
              (array->list source)))
        )

      (test-group "assign/product"
        (do ((d 1 (fx+ d 1)))
            ((= d 6))
          (let* ((unsafe-specialized-destination
                  (make-specialized-array (make-interval (make-vector d 10))
                                          u1-storage-class))
                 (safe-specialized-destination
                  (make-specialized-array (make-interval (make-vector d 10))
                                          u1-storage-class
                                          #t))
                 (mutable-destination
                  (make-array (array-domain safe-specialized-destination)
                              (array-getter safe-specialized-destination)
                              (array-setter safe-specialized-destination)))
                 (source
                  (make-array (array-domain safe-specialized-destination)
                              (lambda args 100)))) ;; not 0 or 1
            (test-error (array-assign! unsafe-specialized-destination source))
            (test-error (array-assign! safe-specialized-destination source))
            (test-error (array-assign! mutable-destination source))))

        (do ((i 0 (fx+ i 1)))
            ((fx=? i tests))
          (let* ((interval
                  (random-interval))
                 (subinterval
                  (random-subinterval interval))
                 (storage-class-and-initializer
                  (random-storage-class-and-initializer))
                 (storage-class
                  (car storage-class-and-initializer))
                 (initializer
                  (cadr storage-class-and-initializer))
                 (specialized-array
                  (array-copy
                   (make-array interval initializer)
                   storage-class))
                 (mutable-array
                  (let ((specialized-array
                         (array-copy
                          (make-array interval initializer)
                          storage-class)))
                    (make-array interval
                                (array-getter specialized-array)
                                (array-setter specialized-array))))
                 (specialized-subarray
                  (array-extract specialized-array subinterval))
                 (mutable-subarray
                  (array-extract mutable-array subinterval))
                 (new-subarray
                  (array-copy
                   (make-array subinterval initializer)
                   storage-class)))
            (array-assign! specialized-subarray new-subarray)
            (array-assign! mutable-subarray new-subarray)
            (test-assert
                (myarray=
                 specialized-array
                 (make-array
                  interval
                  (lambda multi-index
                    (if (apply interval-contains-multi-index? subinterval
                               multi-index)
                        (apply (array-getter new-subarray) multi-index)
                        (apply (array-getter specialized-array) multi-index))))))
            (test-assert
                (myarray=
                 mutable-array
                 (make-array
                  interval
                  (lambda multi-index
                    (if (apply interval-contains-multi-index? subinterval
                               multi-index)
                        (apply (array-getter new-subarray) multi-index)
                        (apply (array-getter mutable-array) multi-index)))))
              )))

        (test-error (make-array (make-interval '#(0 0) '#(10 10)) list 'a))
        (test-error (array-dimension 'a))
        (test-assert
            (array-safe?
             (array-copy (make-array (make-interval '#(0 0) '#(10 10)) list)
                         generic-storage-class
                         #f
                         #t
                         #t)))
        (test-not
         (array-safe?
          (array-copy (make-array (make-interval '#(0 0) '#(10 10)) list)
                      generic-storage-class
                      #f
                      #t
                      #f)))

        (let ((array-builders
               (vector
                (list u1-storage-class
                      (lambda indices (let ((res (random (expt 2 1)))) res))
                      '(a -1))
                (list u8-storage-class
                      (lambda indices (random (expt 2 8)))
                      '(a -1))
                (list u16-storage-class
                      (lambda indices (random (expt 2 16)))
                      '(a -1))
                (list u32-storage-class
                      (lambda indices (random (expt 2 32)))
                      '(a -1))
                (list u64-storage-class
                      (lambda indices (random (expt 2 64)))
                      '(a -1))
                (list s8-storage-class
                      (lambda indices (random (- (expt 2 7))  (expt 2 7)))
                      `(a ,(expt 2 8)))
                (list s16-storage-class
                      (lambda indices (random (- (expt 2 15)) (expt 2 15)))
                      `(a ,(expt 2 16)))
                (list s32-storage-class
                      (lambda indices (random (- (expt 2 31)) (expt 2 31)))
                      `(a ,(expt 2 32)))
                (list s64-storage-class
                      (lambda indices (random (- (expt 2 63)) (expt 2 63)))
                      `(a ,(expt 2 64)))
                (list f32-storage-class
                      (lambda indices (random-real))
                      `(a 1))
                (list f64-storage-class
                      (lambda indices (random-real))
                      `(a 1))
                (list c64-storage-class
                      (lambda indices (make-rectangular (random-real) (random-real)))
                      `(a 1))
                (list c128-storage-class
                      (lambda indices (make-rectangular (random-real) (random-real)))
                      `(a 1))
                )))
          (do ((i 0 (+ i 1)))
              ((= i tests))
            (let* ((domain (random-interval))
                   (builders (vector-ref array-builders
                                         0
                                         #; (random-integer
                                         (vector-length array-builders))
                                         ))
                   (storage-class (car builders))
                   (random-entry (cadr builders))
                   (invalid-entry (list-ref (caddr builders) (random 2)))
                   (Array (array-copy (make-array domain random-entry)
                                      storage-class
                                      #f
                                      #t   ; mutable
                                      #t)) ; safe
                   (getter (array-getter Array))
                   (setter (array-setter Array))
                   (dimension (interval-dimension domain))
                   (valid-args (call-with-values
                                   (lambda ()
                                     (random-multi-index domain))
                                 list)))
              (test-error (apply setter invalid-entry valid-args))
              (set-car! valid-args 'a)
              (test-error (apply getter valid-args))
              (test-error (apply setter 10 valid-args))
              ;; outside the range of any random-interval
              (set-car! valid-args 10000)
              (test-error (apply getter valid-args))
              (test-error (apply setter 10 valid-args))
              (if (< 4 dimension)
                  (begin
                    (set! valid-args (cons 1 valid-args))
                    (test-error (apply getter valid-args))
                    (test-error (apply setter 10 valid-args)))))))

        (test-error (array->list 'a))
        (test-error (list->array 'a 'b))
        (test-error (list->array '(0) 'b))
        (test-error (list->array '(0) (make-interval '#(0) '#(1)) 'a))
        (test-error (list->array '(0) (make-interval '#(0) '#(1))
                                 generic-storage-class 'a))
        (test-error (list->array '(0) (make-interval '#(0) '#(1))
                                 generic-storage-class #t 'a))

        ;; (list->array '(0) (make-interval '#(0) '#(10)))

        (test-error (list->array '(0) (make-interval '#(0) '#(10))))
        (test-error
         (list->array '(a) (make-interval '#(0) '#(1)) u1-storage-class))
        (test-error (list->array '(a) (make-interval '#(10))))

        (let ((array-builders
               (vector
                (list u1-storage-class      (lambda indices (random 0 (expt 2 1))))
                (list u8-storage-class      (lambda indices (random 0 (expt 2 8))))
                (list u16-storage-class     (lambda indices (random 0 (expt 2 16))))
                (list u32-storage-class     (lambda indices (random 0 (expt 2 32))))
                (list u64-storage-class     (lambda indices (random 0 (expt 2 64))))
                (list s8-storage-class
                      (lambda indices (random (- (expt 2 7))  (expt 2 7))))
                (list s16-storage-class
                      (lambda indices (random (- (expt 2 15)) (expt 2 15))))
                (list s32-storage-class
                      (lambda indices (random (- (expt 2 31)) (expt 2 31))))
                (list s64-storage-class
                      (lambda indices (random (- (expt 2 63)) (expt 2 63))))
                (list f32-storage-class
                      (lambda indices (random-real)))
                (list f64-storage-class
                      (lambda indices (random-real)))
                (list c64-storage-class
                      (lambda indices (make-rectangular (random-real) (random-real))))
                (list c128-storage-class
                      (lambda indices (make-rectangular (random-real) (random-real))))
                (list generic-storage-class (lambda indices indices)))))
          (do ((i 0 (+ i 1)))
              ((= i tests))
            (let* ((domain (random-interval))
                   (builders
                    (vector-ref array-builders
                                (random-integer (vector-length array-builders))))
                   (storage-class (car builders))
                   (random-entry (cadr builders))
                   (Array (array-copy (make-array domain random-entry)
                                      storage-class
                                      #f
                                      #t)) ; safe
                   (l (array->list Array))
                   (new-array
                    (list->array l domain storage-class
                                 (zero? (random-integer 2)))))
              (test-assert (myarray= Array new-array)))))

        (test-error (interval-cartesian-product 'a))
        (test-error (interval-cartesian-product (make-interval '#(0) '#(1)) 'a))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((intervals
                  (map (lambda (ignore)
                         (random-interval 1 4))
                       (make-list (random 1 3)))))
            (test (apply my-interval-cartesian-product intervals)
                (apply interval-cartesian-product intervals))))

        (let ((test-array (make-array  (make-interval '#(0) '#(1)) list)))
          (test-error (array-outer-product 'a test-array test-array))
          (test-error (array-outer-product append 'a test-array))
          (test-error (array-outer-product append test-array 'a)))

        (do ((i 0 (+ i 1)))
            ((= i tests))
          (let* ((arrays
                  (map (lambda (ignore)
                         (make-array (random-interval 1 5) list))
                       (make-list 2))))
            (test-assert
                (myarray= (apply array-outer-product append arrays)
                          (make-array (apply my-interval-cartesian-product
                                             (map array-domain arrays))
                                      list))))))

      (test-group "reshape tests"
        (specialized-array-default-safe? #t)
        (let ((A-ref
               (array-copy
                (make-array (make-interval '#(10 10))
                            (lambda (i j) (if (= i j) 1 0)))))
              (B-set!
               (array-copy
                (make-array (make-interval '#(10 10))
                            (lambda (i j) (if (= i j) 1 0)))
                u1-storage-class)))
          (do ((i 1 (+ i 1)))
              ((= i 6))
            (test-error (apply array-ref 1 (make-list i 0))))
          (test-error (array-ref A-ref 1))
          (test-error (array-ref A-ref 1 1001))
          (test 1 (array-ref A-ref 4 4))
          (test 0 (array-ref A-ref 4 5))

          (test-error (array-set! 1 1 1))
          (test-error (array-set! B-set!))
          (test-error (array-set! B-set! 2))
          (test-error (array-set! B-set! 2 1))
          (test-error (array-set! B-set! 2 1 1))
          (array-set! B-set! 1 1 2)
          (array-set! B-set! 0 2 2)
          ;;(array-display B-set!)

          (test-error (specialized-array-reshape 'a 1))
          (test-error (specialized-array-reshape A-ref 'a))
          (test-error (specialized-array-reshape A-ref (make-interval '#(5))))
          )

        (let ((array (array-copy (make-array (make-interval '#(2 1 3 1)) list))))
          (test (array->list array)
              (array->list
               (specialized-array-reshape array (make-interval '#(6))))))

        (let ((array (array-copy (make-array (make-interval '#(2 1 3 1)) list))))
          (test (array->list array)
              (array->list
               (specialized-array-reshape array (make-interval '#(3 2))))))

        (let ((array
               (array-reverse
                (array-copy (make-array (make-interval '#(2 1 3 1)) list)))))
          (test (array->list array)
              (array->list
               (specialized-array-reshape array (make-interval '#(6))))))

        (let ((array
               (array-reverse
                (array-copy (make-array (make-interval '#(2 1 3 1)) list)))))
          (test (array->list
                 (specialized-array-reshape array (make-interval '#(3 2))))
              (array->list array)))

        (let ((array
               (array-reverse
                (array-copy (make-array (make-interval '#(2 1 3 1)) list))
                '#(#f #f #f #t))))
          (test (array->list
                 (specialized-array-reshape array (make-interval '#(3 2))))
              (array->list array)))

        (let ((array (array-reverse
                      (array-copy (make-array (make-interval '#(2 1 3 1)) list))
                      '#(#f #f #f #t))))
          (test (array->list
                 (specialized-array-reshape array (make-interval '#(3 1 2))))
              (array->list array)))

        (let ((array (array-reverse
                      (array-copy (make-array (make-interval '#(2 1 3 1)) list))
                      '#(#f #f #f #t))))
          (test (array->list
                 (specialized-array-reshape array (make-interval '#(1 1 1 3 2))))
              (array->list array)))

        (let ((array (array-reverse
                      (array-copy (make-array (make-interval '#(2 1 3 1)) list))
                      '#(#f #f #f #t))))
          (test (array->list
                 (specialized-array-reshape array (make-interval '#(3 2 1 1 1))))
              (array->list array)))

        (let ((array (array-reverse
                      (array-copy (make-array (make-interval '#(2 1 3 1)) list))
                      '#(#f #f #f #t))))
          (test (array->list
                 (specialized-array-reshape array (make-interval '#(3 1 1 2))))
              (array->list array)))

        (let ((array (array-reverse
                      (array-copy (make-array (make-interval '#(2 1 3 1)) list))
                      '#(#f #f #f #t))))
          (test (array->list
                 (specialized-array-reshape array (make-interval '#(3 1 2 1))))
              (array->list array)))

        (let ((array
               (array-sample
                (array-reverse
                 (array-copy (make-array (make-interval '#(2 1 4 1)) list))
                 '#(#f #f #f #t))
                '#(1 1 2 1))))
          (test (array->list
                 (specialized-array-reshape array (make-interval '#(4))))
              (array->list array)))

        (let ((array
               (array-sample
                (array-reverse
                 (array-copy (make-array (make-interval '#(2 1 4 1)) list))
                 '#(#t #f #t #t))
                '#(1 1 2 1))))
          (test (array->list
                 (specialized-array-reshape array (make-interval '#(4))))
              (array->list array)))

        (test-error
         (specialized-array-reshape
          (array-reverse
           (array-copy (make-array (make-interval '#(2 1 3 1)) list))
           '#(#t #f #f #f))
          (make-interval '#(6))))

        (test-error
         (specialized-array-reshape
          (array-reverse
           (array-copy (make-array (make-interval '#(2 1 3 1)) list))
           '#(#t #f #f #f))
          (make-interval '#(3 2))))

        (test-error
         (specialized-array-reshape
          (array-reverse
           (array-copy (make-array (make-interval '#(2 1 3 1)) list))
           '#(#f #f #t #f))
          (make-interval '#(6))))

        (test-error
         (specialized-array-reshape
          (array-reverse
           (array-copy (make-array (make-interval '#(2 1 3 1)) list))
           '#(#f #f #t #t))
          (make-interval '#(3 2))))

        (test-error
         (specialized-array-reshape
          (array-sample
           (array-reverse
            (array-copy (make-array (make-interval '#(2 1 3 1)) list))
            '#(#f #f #f #t))
           '#(1 1 2 1))
          (make-interval '#(4))))

        (test-error
         (specialized-array-reshape
          (array-sample
           (array-reverse
            (array-copy (make-array (make-interval '#(2 1 4 1)) list))
            '#(#f #f #t #t))
           '#(1 1 2 1))
          (make-interval '#(4))))
        )

      (test-group "curry tests"
        (test-assert
            (interval=
             (interval-dilate (make-interval '#(100 100)) '#(1 1) '#(1 1))
             (make-interval '#(1 1) '#(101 101))))

        (test-assert
            (interval=
             (interval-dilate (make-interval '#(100 100)) '#(-1 -1) '#(1 1))
             (make-interval '#(-1 -1) '#(101 101))))

        (test-assert
            (interval=
             (interval-dilate (make-interval '#(100 100))  '#(0 0) '#(-50 -50))
             (make-interval '#(50 50))))

        (test-error
         (interval-dilate (make-interval '#(100 100)) '#(0 0) '#(-500 -50)))

        (let ((a (make-array (make-interval '#(1 1) '#(11 11))
                             (lambda (i j)
                               (if (= i j)
                                   1
                                   0)))))
          (test 1
              ((array-getter a) 3 3))
          (test 0
              ((array-getter a) 2 3)))

        (let ((a (make-array (make-interval '#(0 0) '#(10 10))
                             list)))
          (test '(3 4)
              ((array-getter a) 3 4))
          (let ((curried-a (array-curry a 1)))
            (test '(3 4)
                ((array-getter ((array-getter curried-a) 3)) 4))))

        (test 0.
            ((array-getter sparse-array) 12345 6789))

        (test 0.
            ((array-getter sparse-array) 0 0))

        ((array-setter sparse-array) 1.0 0 0)

        (test 0.
            ((array-getter sparse-array) 12345 6789))

        (test 1.
            ((array-getter sparse-array) 0 0))
        )

      (test-group "misc"
        (let ()
          (define a
            (array-copy
             (make-array (make-interval '#(5 10))
                         list)))
          (define b
            (specialized-array-share
             a
             (make-interval '#(5 5))
             (lambda (i j)
               (values i (+ i j)))))
          ;; Print the \"rows\" of b
          ;; (array-for-each (lambda (row)
          ;;                   (pretty-print (array->list row)))
          ;;                 (array-curry b 1))
  
          ;; which prints
          ;; ((0 0) (0 1) (0 2) (0 3) (0 4))
          ;; ((1 1) (1 2) (1 3) (1 4) (1 5))
          ;; ((2 2) (2 3) (2 4) (2 5) (2 6))
          ;; ((3 3) (3 4) (3 5) (3 6) (3 7))
          ;; ((4 4) (4 5) (4 6) (4 7) (4 8))
          )

        '(let ()
           (define (palindrome? s)
             (let ((n (string-length s)))
               (or (< n 2)
                   (let* ((a
                           ;; an array accessing the characters of s
                           (make-array (make-interval (vector n))
                                       (lambda (i)
                                         (string-ref s i))))
                          (ra
                           ;; the array in reverse order
                           (array-reverse a))
                          (half-domain
                           (make-interval (vector (quotient n 2)))))
                     (array-every
                      char=?
                      ;; the first half of s
                      (array-extract a half-domain)
                      ;; the second half of s
                      (array-extract ra half-domain))))))
           (for-each
            (lambda (s)
              (for-each display
                        (list "(palindrome? \""
                              s
                              "\") => "
                              (palindrome? s)
                              #\newline)))
            '("" "a" "aa" "ab" "aba" "abc" "abba" "abca" "abbc")))

        ;; (let ((greys (pgm-greys test-pgm)))
        ;;   (write-pgm
        ;;    (make-pgm
        ;;     greys
        ;;     (array-map (lambda (p)
        ;;                  (round-and-clip p greys))
        ;;                (array-convolve
        ;;                 (pgm-pixels test-pgm)
        ;;                 sharpen-filter)))
        ;;    "sharper-test.pgm"))

        ;; (let* ((greys (pgm-greys test-pgm))
        ;;        (edge-array
        ;;         (array-copy
        ;;          (array-map
        ;;           abs
        ;;           (array-convolve
        ;;            (pgm-pixels test-pgm)
        ;;            edge-filter))))
        ;;        (max-pixel
        ;;         (array-fold max 0 edge-array))
        ;;        (normalizer
        ;;         (inexact (/ greys max-pixel))))
        ;;   (write-pgm
        ;;    (make-pgm
        ;;     greys
        ;;     (array-map (lambda (p)
        ;;                  (- greys
        ;;                     (round-and-clip (* p normalizer) greys)))
        ;;                edge-array))
        ;;    "edge-test.pgm"))

      
        (let ((m
               (array-copy (make-array (make-interval '#(0 0) '#(40 30))
                                       (lambda (i j) (inexact (+ i j)))))))
          (test 1940. (operator-max-norm m))
          (test 1605. (operator-one-norm m)))

        (let ((image
               (array-copy
                (make-array (make-interval '#(4 4))
                            (lambda (i j)
                              (case i
                                ((0) 1.)
                                ((1) -1.)
                                (else 0.)))))))
          ;; (display "\nInitial image: \n")
          ;; (pretty-print (list (array-domain image)
          ;;                     (array->list image)))
          ;; (hyperbolic-Haar-transform image)
          ;; (display "\nArray of hyperbolic Haar wavelet coefficients: \n")
          ;; (pretty-print (list (array-domain image)
          ;;                     (array->list image)))
          ;; (hyperbolic-Haar-inverse-transform image)
          ;; (display "\nReconstructed image: \n")
          ;; (pretty-print (list (array-domain image)
          ;;                     (array->list image)))
          #f)


        ;; (let ((image
        ;;        (array-copy
        ;;         (make-array (make-interval '#(4 4))
        ;;                     (lambda (i j)
        ;;                       (case i
        ;;                         ((0) 1.)
        ;;                         ((1) -1.)
        ;;                         (else 0.)))))))
        ;;   (display "\nInitial image: \n")
        ;;   (pretty-print (list (array-domain image)
        ;;                       (array->list image)))
        ;;   (Haar-transform image)
        ;;   (display "\nArray of Haar wavelet coefficients: \n")
        ;;   (pretty-print (list (array-domain image)
        ;;                       (array->list image)))
        ;;   (Haar-inverse-transform image)
        ;;   (display "\nReconstructed image: \n")
        ;;   (pretty-print (list (array-domain image)
        ;;                       (array->list image))))

        ;; (define A
        ;;   ;; A Hilbert matrix
        ;;   (array-copy
        ;;    (make-array (make-interval '#(4 4))
        ;;                (lambda (i j)
        ;;                  (/ (+ 1 i j))))))

        ;; (display "\nHilbert matrix:\n\n")
        ;; (array-display A)

        ;; (LU-decomposition A)

        ;; (display "\nLU decomposition of Hilbert matrix:\n\n")

        ;; (array-display A)

        ;; Functions to extract the lower- and upper-triangular
        ;; matrices of the LU decomposition of A.

        ;; (define (L a)
        ;;   (let ((a_ (array-getter a))
        ;;         (d  (array-domain a)))
        ;;     (make-array
        ;;      d
        ;;      (lambda (i j)
        ;;        (cond ((= i j) 1)        ;; diagonal
        ;;              ((> i j) (a_ i j)) ;; below diagonal
        ;;              (else 0))))))      ;; above diagonal

        ;; (define (U a)
        ;;   (let ((a_ (array-getter a))
        ;;         (d  (array-domain a)))
        ;;     (make-array
        ;;      d
        ;;      (lambda (i j)
        ;;        (cond ((<= i j) (a_ i j)) ;; diagonal and above
        ;;              (else 0))))))       ;; below diagonal

        ;; ;; Lower triangular matrix of decomposition of Hilbert matrix
        ;; (array-display (L A))

        ;; ;; Upper triangular matrix of decomposition of Hilbert matrix
        ;; (array-display (U A))

        ;; We'll check that the product of the result of LU
        ;; decomposition of A is again A.

        ;; (define product (matrix-multiply (L A) (U A)))

        ;; (display "\nProduct of lower and upper triangular matrices ")
        ;; (display "of LU decomposition of Hilbert matrix:\n\n")
        ;; (array-display product)

        ;; Examples from
        ;; http://microapl.com/apl_help/ch_020_020_880.htm

        (let ((TABLE1
               (list->array
                '(1 2
                    5 4
                    3 0)
                (make-interval '#(3 2))))
              (TABLE2
               (list->array
                '(6 2 3 4
                    7 0 1 8)
                (make-interval '#(2 4)))))
          (test '(20 2 5 20
                     58 10 19 52
                     18 6 9 12)
              (array->list (inner-product TABLE1 + * TABLE2))))

        (let ((X ;; a "row vector"
               (list->array '(1 3 5 7) (make-interval '#(1 4))))
              (Y ;; a "column vector"
               (list->array '(2 3 6 7) (make-interval '#(4 1)))))
          (test '(2)
              (array->list (inner-product X + (lambda (x y) (if (= x y) 1 0)) Y))))

        ;; (let* ((A (array-copy (make-array (make-interval '#(3 4)) list)))
        ;;        (B (array-sample A '#(2 1))))
        ;;   (test-error
        ;;    (specialized-array-reshape B (make-interval '#(8)))))

        '(let* ((interval-flat (make-interval '#(100 100 4)))
                (interval-2x2  (make-interval '#(100 100 2 2)))
                (A (array-copy (make-array interval-flat
                                           (lambda args (random-integer 5)))))
                (B (array-copy (make-array interval-flat
                                           (lambda args (random-integer 5)))))
                (C (array-copy (make-array interval-flat
                                           (lambda args 0)))))
           (array-for-each
            x2x2-matrix-multiply-into!
            (array-curry (specialized-array-reshape A interval-2x2) 2)
            (array-curry (specialized-array-reshape B interval-2x2) 2)
            (array-curry (specialized-array-reshape C interval-2x2) 2))
           (array-for-each
            (lambda (A B C)
              (array-assign! C (matrix-multiply A B)))
            (array-curry (specialized-array-reshape A interval-2x2) 2)
            (array-curry (specialized-array-reshape B interval-2x2) 2)
            (array-curry (specialized-array-reshape C interval-2x2) 2))
           '(array-display ((array-getter
                             (array-curry
                              (specialized-array-reshape A interval-2x2)
                              2))
                            0 0))
           '(array-display ((array-getter
                             (array-curry
                              (specialized-array-reshape B interval-2x2)
                              2))
                            0 0))
           '(array-display ((array-getter
                             (array-curry
                              (specialized-array-reshape C interval-2x2)
                              2))
                            0 0))

           (let ((a2x2 (make-interval '#(2 2))))
             (array-for-each (lambda (A B C)
                               (x2x2-matrix-multiply-into!
                                (specialized-array-reshape A a2x2)
                                (specialized-array-reshape B a2x2)
                                (specialized-array-reshape C a2x2)))
                             (array-curry A 1)
                             (array-curry B 1)
                             (array-curry C 1))
             (array-for-each (lambda (A B C)
                               (array-assign!
                                (specialized-array-reshape C a2x2)
                                (matrix-multiply
                                 (specialized-array-reshape A a2x2)
                                 (specialized-array-reshape B a2x2))))
                             (array-curry A 1)
                             (array-curry B 1)
                             (array-curry C 1)))

           '(array-display ((array-getter
                             (array-curry
                              (specialized-array-reshape A interval-2x2)
                              2))
                            0 0))
           '(array-display ((array-getter
                             (array-curry
                              (specialized-array-reshape B interval-2x2)
                              2))
                            0 0))
           '(array-display ((array-getter
                             (array-curry
                              (specialized-array-reshape C interval-2x2)
                              2))
                            0 0))
           )
        )

      (test-end)
      )))
