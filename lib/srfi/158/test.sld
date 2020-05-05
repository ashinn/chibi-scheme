(define-library (srfi 158 test)
  (import (scheme base)
	  (scheme read)
	  (srfi 1)
	  (srfi 158)
	  (chibi test))
  (export run-tests)
  (begin
    (define (with-input-from-string str thunk)
      (parameterize ((current-input-port (open-input-string str)))
	(thunk)))
    (define g
      (make-coroutine-generator
       (lambda (yield) (let loop ((i 0))
			 (when (< i 3) (yield i) (loop (+ i 1)))))))
    (define (for-each-digit proc n)
      (when (> n 0)
	(let-values (((div rem) (truncate/ n 10)))
	  (proc rem)
	  (for-each-digit proc div))))
    (define g1 (generator 1 2 3))
    (define g2 (generator 4 5 6 7))
    (define (proc . args) (values (apply + args) (apply + args)))
    (define (small? x) (< x 3))
    (define n 0)
    (define (run-tests)
      (test-group "generators"
		  (test-group "generators/constructors"
			      (test '() (generator->list (generator)))
			      (test '(1 2 3) (generator->list (generator 1 2 3)))
			      (test '(1 2 3 1 2) (generator->list (circular-generator 1 2 3) 5))
			      (test '(8 9 10) (generator->list (make-iota-generator 3 8)))
			      (test '(8 10 12) (generator->list (make-iota-generator 3 8 2)))
			      (test '(3 4 5 6) (generator->list (make-range-generator 3) 4))
			      (test '(3 4 5 6 7) (generator->list (make-range-generator 3 8)))
			      (test '(3 5 7) (generator->list (make-range-generator 3 8 2)))

			      (test '(0 1 2) (generator->list g))
			      (test '(1 2 3 4 5) (generator->list (list->generator '(1 2 3 4 5))))
			      (test '(1 2 3 4 5) (generator->list (vector->generator '#(1 2 3 4 5))))
			      (test '#(0 0 1 2 4)
				    (let ((v (make-vector 5 0)))
				      (generator->vector! v 2 (generator 1 2 4))
				      v))
			      (test '(5 4 3 2 1) (generator->list (reverse-vector->generator '#(1 2 3 4 5))))
			      (test '(#\a #\b #\c #\d #\e) (generator->list (string->generator "abcde")))
			      (test '(10 20 30) (generator->list (bytevector->generator (bytevector 10 20 30))))
			      (test '(5 4 3 2 1) (generator->list
						  (make-for-each-generator for-each-digit
									   12345)))
			      (test '(0 2 4 6 8 10) (generator->list
						     (make-unfold-generator
						      (lambda (s) (> s 5))
						      (lambda (s) (* s 2))
						      (lambda (s) (+ s 1))
						      0)))
			      ) ; end "generators/constructors"

		  (test-group "generators/operators"
			      (test '(a b 0 1) (generator->list (gcons* 'a 'b (make-range-generator 0 2))))
			      (test '(0 1 2 0 1) (generator->list (gappend (make-range-generator 0 3)
									   (make-range-generator 0 2))))
			      (test '() (generator->list (gappend)))
			      (test '(15 22 31) (generator->list (gcombine proc 10 g1 g2)))
			      (test '(1 3 5 7 9) (generator->list (gfilter
								   odd?
								   (make-range-generator 1 11))))
			      (test '(2 4 6 8 10) (generator->list (gremove
								    odd?
								    (make-range-generator 1 11))))
			      (set! g (make-range-generator 1 5))
			      (test '(1 2 3) (generator->list (gtake g 3)))
			      (test '(4) (generator->list g))
			      (test '(1 2) (generator->list (gtake (make-range-generator 1 3) 3)))
			      (test '(1 2 0) (generator->list (gtake (make-range-generator 1 3) 3 0)))
			      (test '(3 4) (generator->list (gdrop (make-range-generator 1 5) 2)))
			      (set! g (make-range-generator 1 5))
			      (test '(1 2) (generator->list (gtake-while small? g)))
			      (set! g (make-range-generator 1 5))
			      (test '(3 4) (generator->list (gdrop-while small? g)))
			      (test '() (generator->list (gdrop-while (lambda args #t) (generator 1 2 3))))
			      (test '(0.0 1.0 0 2) (generator->list (gdelete 1
									     (generator 0.0 1.0 0 1 2))))
			      (test '(0.0 0 2) (generator->list (gdelete 1
									 (generator 0.0 1.0 0 1 2)
									 =)))
			      (test '(a c e) (generator->list (gindex (list->generator '(a b c d e f))
								      (list->generator '(0 2 4)))))
			      (test '(a d e) (generator->list (gselect (list->generator '(a b c d e f))
								       (list->generator '(#t #f #f #t #t #f)))))
			      (test '(1 2 3) (generator->list (gdelete-neighbor-dups
							       (generator 1 1 2 3 3 3)
							       =)))
			      (test '(1) (generator->list (gdelete-neighbor-dups
							   (generator 1 2 3)
							   (lambda args #t))))
			      (test '(1 2 3 a b c)
				    (generator->list
				     (gflatten (generator '(1 2 3) '(a b c)))))
			      (test '((1 2 3) (4 5 6) (7 8))
				    (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3)))
			      (test '((1 2 3) (4 5 6) (7 8 0))
				    (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3 0)))
			      (test '(1 2 3)
				    (generator->list (gmerge < (generator 1 2 3))))
			      (test '(1 2 3 4 5 6)
				    (generator->list (gmerge < (generator 1 2 3) (generator 4 5 6))))
			      (test '(1 2 3 4 4 5 6)
				    (generator->list (gmerge <
							     (generator 1 2 4 6)
							     (generator)
							     (generator 3 4 5))))
			      (test '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
				    (generator->list (gmerge <
							     (generator 1 10 11)
							     (generator 2 9 12)
							     (generator 3 8 13)
							     (generator 4 7 14)
							     (generator 5 6 15))))
			      ;; check the tie-break rule
			      (test '((1 a) (1 e) (1 b) (1 c) (1 d))
				    (generator->list (gmerge (lambda (x y) (< (car x) (car y)))
							     (generator '(1 a) '(1 e))
							     (generator '(1 b))
							     (generator '(1 c) '(1 d)))))

			      (test '(-1 -2 -3 -4 -5)
				    (generator->list (gmap - (generator 1 2 3 4 5))))
			      (test '(7 9 11 13)
				    (generator->list (gmap +
							   (generator 1 2 3 4 5)
							   (generator 6 7 8 9))))
			      (test '(54 140 264)
				    (generator->list (gmap *
							   (generator 1 2 3 4 5)
							   (generator 6 7 8)
							   (generator 9 10 11 12 13))))
			      (test '(a c e g i)
				    (generator->list
				     (gstate-filter
				      (lambda (item state) (values (even? state) (+ 1 state)))
				      0
				      (generator 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j))))
			      ) ; end "generators/operators"


		  (test-group "generators/consumers"
			      ;; no test for plain generator->list (used throughout)
			      (test '(1 2 3) (generator->list (generator 1 2 3 4 5) 3))
			      (test '(5 4 3 2 1) (generator->reverse-list (generator 1 2 3 4 5)))
			      (test '#(1 2 3 4 5) (generator->vector (generator 1 2 3 4 5)))
			      (test '#(1 2 3) (generator->vector (generator 1 2 3 4 5) 3))
			      (test "abc" (generator->string (generator #\a #\b #\c)))
			      (test '(e d c b a . z) (with-input-from-string "a b c d e"
						       (lambda () (generator-fold cons 'z read))))

			      (generator-for-each (lambda values (set! n (apply + values)))
						  (generator 1) (generator 2) (generator 3))
			      (test 6 n)
			      (test '(6 15)
				    (generator-map->list (lambda values (apply + values))
							 (generator 1 4) (generator 2 5) (generator 3 6)))
			      (test 3 (generator-find (lambda (x) (> x 2)) (make-range-generator 1 5)))
			      (test 2 (generator-count odd? (make-range-generator 1 5)))
			      (set! g (make-range-generator 2 5))
			      (test #t (generator-any odd? g))
			      (test '(4) (generator->list g))
			      (set! g (make-range-generator 2 5))
			      (test #f (generator-every odd? g))
			      (test '(3 4) (generator->list g))
			      (test '(#\a #\b #\c) (generator-unfold (make-for-each-generator string-for-each "abc") unfold))

			      ) ; end "generators/consumers"

		  ) ; end "generators"


      (test-group "accumulators"
		  (test -8
			(let ((a (make-accumulator * 1 -)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test 3
			(let ((a (count-accumulator)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test '(1 2 4)
			(let ((a (list-accumulator)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test '(4 2 1)
			(let ((a (reverse-list-accumulator)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test '#(1 2 4)
			(let ((a (vector-accumulator)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test '#(0 0 1 2 4)
			(let* ((v (vector 0 0 0 0 0))
			       (a (vector-accumulator! v 2)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test '#u8(0 0 1 2 4)
			(let* ((v (bytevector 0 0 0 0 0))
			       (a (bytevector-accumulator! v 2)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test '#(4 2 1)
			(let ((a (reverse-vector-accumulator)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test "abc"
			(let ((a (string-accumulator)))
			  (a #\a)
			  (a #\b)
			  (a #\c)
			  (a (eof-object))))

		  (test #u8(1 2 4)
			(let ((a (bytevector-accumulator)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test 7
			(let ((a (sum-accumulator)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  (test 8
			(let ((a (product-accumulator)))
			  (a 1)
			  (a 2)
			  (a 4)
			  (a (eof-object))))

		  ) ; end "accumulators"

      )))
