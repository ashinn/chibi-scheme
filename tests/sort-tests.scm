
(cond-expand
 (modules (import (srfi 95) (only (chibi test) test-begin test test-end)))
 (else #f))

(test-begin "sorting")

(test "sort null" '() (sort '()))
(test "sort null <" '() (sort '() <))
(test "sort null < car" '() (sort '() < car))
(test "sort equal list" '(0 0 0 0 0 0 0 0 0) (sort '(0 0 0 0 0 0 0 0 0)))
(test "sort equal list cmp" '(0 0 0 0 0 0 0 0 0)
  (sort '(0 0 0 0 0 0 0 0 0) (lambda (a b) (< a b))))
(test "sort ordered list" '(1 2 3 4 5 6 7 8 9) (sort '(1 2 3 4 5 6 7 8 9)))
(test "sort reversed list" '(1 2 3 4 5 6 7 8 9) (sort '(9 8 7 6 5 4 3 2 1)))
(test "sort random list 1" '(1 2 3 4 5 6 7 8 9) (sort '(7 5 2 8 1 6 4 9 3)))
(test "sort random list 2" '(1 2 3 4 5 6 7 8) (sort '(5 3 4 1 7 6 8 2)))
(test "sort random list 3" '(1 2 3 4 5 6 7 8 9) (sort '(5 3 4 1 7 9 6 8 2)))
(test "sort short equal list" '(0 0 0) (sort '(0 0 0)))
(test "sort short random list" '(1 2 3) (sort '(2 1 3)))
(test "sort short random list cmp" '(1 2 3) (sort '(2 1 3) (lambda (a b) (< a b))))
(test "sort numeric list <" '(1 2 3 4 5 6 7 8 9)
  (sort '(7 5 2 8 1 6 4 9 3) <))
(test "sort numeric list < car" '((1) (2) (3) (4) (5) (6) (7) (8) (9))
  (sort '((7) (5) (2) (8) (1) (6) (4) (9) (3)) < car))
(test "sort list (lambda (a b) (< (car a) (car b)))"
    '((1) (2) (3) (4) (5) (6) (7) (8) (9))
  (sort '((7) (5) (2) (8) (1) (6) (4) (9) (3))
        (lambda (a b) (< (car a) (car b)))))
(test "sort 1-char symbols" '(a b c d e f g h i j k)
  (sort '(h b k d a c j i e g f)))
(test "sort short symbols" '(a aa b c d e ee f g h i j k)
  (sort '(h b aa k d a ee c j i e g f)))
(test "sort long symbol"
    '(a aa b bzzzzzzzzzzzzzzzzzzzzzzz c d e ee f g h i j k)
  (sort '(h b aa k d a ee c j i bzzzzzzzzzzzzzzzzzzzzzzz e g f)))
(test "sort long symbols"
    '(a aa b bzzzzzzzzzzzzzzzzzzzzzzz czzzzzzzzzzzzz dzzzzzzzz e ee f g h i j k)
  (sort '(h b aa k dzzzzzzzz a ee czzzzzzzzzzzzz j i bzzzzzzzzzzzzzzzzzzzzzzz e g f)))
(test "sort strings"
    '("ape" "bear" "cat" "dog" "elephant" "fox" "goat" "hawk")
  (sort '("elephant" "cat" "dog" "ape" "goat" "fox" "hawk" "bear")))
(test "sort strings string-ci<?"
    '("ape" "Bear" "CaT" "DOG" "elephant" "Fox" "GoAt" "HAWK")
  (sort '("elephant" "CaT" "DOG" "ape" "GoAt" "Fox" "HAWK" "Bear")
          string-ci<?))

(test "sort numeric inexact vector <" '#(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9)
  (sort '#(7.7 5.5 2.2 8.8 1.1 6.6 4.4 9.9 3.3) <))
(test "sort numeric signed inexact vector <"
    '#(-9.9 -7.7 -5.5 -3.3 -1.1 2.2 4.4 6.6 8.8)
  (sort '#(-7.7 -5.5 2.2 8.8 -1.1 6.6 4.4 -9.9 -3.3) <))
(test "sort numeric same whole number inexact vector"
    '#(-5.2155
       -4.3817
       -4.3055
       -4.0415
       -3.5883
       -3.5714
       -3.4059
       -2.7829
       -2.6406
       -2.4985
       -2.4607
       -1.2487
       -0.537800000000001
       -0.481999999999999
       -0.469100000000001
       -0.0932999999999993
       0.0066999999999986)
  (sort '#(-5.2155
           -3.5714
           -4.3817
           -3.5883
           -4.3055
           -2.4985
           -4.0415
           -3.4059
           -0.0932999999999993
           -0.537800000000001
           -2.6406
           -0.481999999999999
           -2.7829
           -2.4607
           -1.2487
           -0.469100000000001
           0.0066999999999986)
        <))

(test "sort watson no dups"
    '#(-0.3096 -0.307000000000002 -0.303800000000003 -0.301600000000001
       -0.300599999999999 -0.3003 -0.3002 -0.2942)
  (sort '#(-0.3096 -0.307000000000002 -0.303800000000003 -0.301600000000001
           -0.300599999999999 -0.2942 -0.3003 -0.3002)))

(test "sort watson"
    '#(-0.3096 -0.307000000000002 -0.303800000000003 -0.301600000000001
       -0.300599999999999 -0.3003 -0.3003 -0.3002 -0.2942)
  (sort '#(-0.3096 -0.307000000000002 -0.303800000000003 -0.301600000000001
           -0.300599999999999 -0.2942 -0.3003 -0.3003 -0.3002)))

(test "sort ratios" '(1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5)
  (sort '(1/2 1/3 1/4 1/5 2/3 3/4 2/5 3/5 4/5)))

(test "sort complex" '(1+1i 1+2i 1+3i 2+2i 3+3i 4+4i 5+5i 6+6i 7+7i 8+8i 9+9i)
  (sort '(7+7i 1+2i 5+5i 2+2i 8+8i 1+1i 6+6i 4+4i 9+9i 1+3i 3+3i)))

(test-end)
