
(define-library (chibi diff-test)
  (import (scheme base) (chibi diff) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "diff")
      (test '((#\A 1 0) (#\C 2 2))
          (lcs-with-positions '(#\G #\A #\C) '(#\A #\G #\C #\A #\T)))
      (test '(#\A #\C)
          (lcs '(#\G #\A #\C) '(#\A #\G #\C #\A #\T)))
      (test '((#\G #\A #\C) (#\A #\G #\C #\A #\T) ((#\A 1 0) (#\C 2 2)))
          (diff "GAC" "AGCAT" read-char))
      (let ((d (diff "GAC" "AGCAT" read-char)))
        (test " »G« AC"
            (edits->string (car d) (car (cddr d)) 1))
        (test "A «G» C «AT» "
            (edits->string (cadr d) (car (cddr d)) 2))
        (test "\x1b;[31mG\x1b;[39mAC"
            (edits->string/color (car d) (car (cddr d)) 1))
        (test "A\x1b;[32mG\x1b;[39mC\x1b;[32mAT\x1b;[39m"
            (edits->string/color (cadr d) (car (cddr d)) 2)))
      (test-end))))
