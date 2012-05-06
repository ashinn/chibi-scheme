
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

(test-end)
