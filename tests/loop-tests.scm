
(import (chibi loop))

(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test name expr expect)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((str (call-with-output-string (lambda (out) (display name out))))
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
 "stepping"
 '(0 1 2)
 (loop lp ((with i 0 (+ i 1))
           (with res '() (cons i res)))
   (if (= i 3)
     (reverse res)
     (lp))))

(test
 "basic in-list"
 '(c b a)
 (let ((res '()))
   (loop ((for x (in-list '(a b c))))
     (set! res (cons x res)))
   res))

(test
 "in-list with result"
 '(c b a)
 (loop ((for x (in-list '(a b c)))
        (with res '() (cons x res)))
   => res))

(test
 "in-list with listing"
 '(a b c)
 (loop ((for x (in-list '(a b c))) (for res (listing x))) => res))

(test
 "in-list with listing-reverse"
 '(c b a)
 (loop ((for x (in-list '(a b c))) (for res (listing-reverse x))) => res))

(test
 "uneven length in-list's"
 '((a . 1) (b . 2) (c . 3))
 (loop ((for x (in-list '(a b c)))
        (for y (in-list '(1 2 3 4)))
        (for res (listing (cons x y))))
    => res))

(test
 "in-lists"
 '((a 1) (b 2) (c 3))
 (loop ((for ls (in-lists '((a b c) (1 2 3))))
        (for res (listing ls)))
   => res))

(define (flatten ls)
  (reverse
   (loop lp ((for x ls (in-list ls)) (with res '()))
     => res
     (if (pair? x)
         (lp (=> res (lp (=> ls x))))
         (lp (=> res (cons x res)))))))

(test
 "flatten (recursion test)"
 '(1 2 3 4 5 6 7)
 (flatten '(1 (2) (3 (4 (5)) 6) 7)))

(test
 "in-string"
 '(#\h #\e #\l #\l #\o)
 (loop ((for c (in-string "hello")) (for res (listing c))) => res))

(test
 "in-string with start"
 '(#\l #\o)
 (loop ((for c (in-string "hello" 3)) (for res (listing c))) => res))

(test
 "in-string with start and end"
 '(#\h #\e #\l #\l)
 (loop ((for c (in-string "hello" 0 4)) (for res (listing c))) => res))

(test
 "in-string with start, end and step"
 '(#\e #\l)
 (loop ((for c (in-string "hello" 1 4 2)) (for res (listing c))) => res))

(test
 "in-string-reverse"
 '(#\o #\l #\l #\e #\h)
 (loop ((for c (in-string-reverse "hello")) (for res (listing c))) => res))

(test
 "in-vector"
 '(1 2 3)
 (loop ((for x (in-vector '#(1 2 3))) (for res (listing x))) => res))

(test "up-from" '(5 6 7)
  (loop ((for i (up-from 5 (to 8)))
         (for res (listing i)))
    => res))

(test "up-from by" '(5 10 15)
  (loop ((for i (up-from 5 (to 20) (by 5)))
         (for res (listing i)))
    => res))

(test "up-from listing if" '(10 12 14 16 18)
  (loop ((for i (up-from 10 (to 20)))
         (for res (listing i (if (even? i)))))
    => res))

(test "down-from" '(7 6 5)
  (loop ((for i (down-from 8 (to 5)))
         (for res (listing i)))
    => res))

(test "down-from by" '(15 10 5)
  (loop ((for i (down-from 20 (to 5) (by 5)))
         (for res (listing i)))
    => res))

(test "down-from listing if" '(18 16 14 12 10)
  (loop ((for i (down-from 20 (to 10)))
         (for res (listing i (if (even? i)))))
    => res))

(test "appending" '(1 2 3 4 5 6 7 8 9)
  (loop ((for ls (in-list '((1 2 3) (4 5 6) (7 8 9))))
         (for res (appending ls)))
    => res))

(test "appending-reverse" '(9 8 7 6 5 4 3 2 1)
  (loop ((for ls (in-list '((1 2 3) (4 5 6) (7 8 9))))
         (for res (appending-reverse ls)))
    => res))

(test "while + up-from" '(5 6 7)
      (loop ((for i (up-from 5 (to 10)))
             (while (< i 8))
             (for res (listing i)))
        => res))

(test "up-from by, open-ended" '(5 7 9)
  (loop ((for i (up-from 5 (by 2)))
         (while (< i 10))
         (for res (listing i)))
    => res))

(test "up-from open-ended" '(5 6 7)
  (loop ((for i (up-from 5))
         (while (< i 8))
         (for res (listing i)))
    => res))

(test "down-from by, open-ended" '(5 3 1)
  (loop ((for i (down-from 7 (by 2)))
         (until (< i 1))
         (for res (listing i)))
    => res))

(test "down-from open-ended" '(4 3 2)
  (loop ((for i (down-from 5))
         (until (< i 2))
         (for res (listing i)))
    => res))

(test-report)

