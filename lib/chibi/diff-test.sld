
(define-library (chibi diff-test)
  (import (scheme base) (chibi diff))
  (export run-tests)
  (cond-expand
   (chibi (import (chibi test)))
   (else
    (import (scheme write))
    ;; inline (chibi test) to avoid circular dependencies in snow
    ;; installations
    (begin
      (define-syntax test
        (syntax-rules ()
          ((test expect expr)
           (test 'expr expect expr))
          ((test name expect expr)
           (guard (exn (else (display "!\nERROR: ") (write name) (newline)
                             (write exn) (newline)))
             (let* ((res expr)
                    (pass? (equal? expect expr)))
               (display (if pass? "." "x"))
               (cond
                ((not pass?)
                 (display "\nFAIL: ") (write name) (newline))))))))
      (define (test-begin name)
        (display name))
      (define (test-end)
        (newline)))))
  (begin
    (define (run-tests)
      (test-begin "diff")
      (test '((#\A 1 0) (#\C 2 2))
          (lcs-with-positions '(#\G #\A #\C) '(#\A #\G #\C #\A #\T)))
      (test '(#\A #\C)
          (lcs '(#\G #\A #\C) '(#\A #\G #\C #\A #\T)))
      (test '((#\G #\A #\C) (#\A #\G #\C #\A #\T) ((#\A 1 0) (#\C 2 2)))
          (diff "GAC" "AGCAT" read-char))
      (test '((#\A #\G #\C #\A #\T) (#\A #\G #\C #\A #\T)
              ((#\A 0 0) (#\G 1 1) (#\C 2 2) (#\A 3 3) (#\T 4 4)))
          (diff "AGCAT" "AGCAT" read-char))
      (test '((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.
               #\G #\A #\C #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
              (#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.
               #\A #\G #\C #\A #\T #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
              ((#\0 0 0) (#\1 1 1) (#\2 2 2) (#\3 3 3) (#\4 4 4) (#\5 5 5)
               (#\6 6 6) (#\7 7 7) (#\8 8 8) (#\9 9 9) (#\. 10 10)
               (#\A 12 11) (#\C 13 13)
               (#\. 14 16) (#\0 15 17) (#\1 16 18) (#\2 17 19) (#\3 18 20)
               (#\4 19 21) (#\5 20 22) (#\6 21 23) (#\7 22 24) (#\8 23 25)
               (#\9 24 26)))
          (diff "0123456789.GAC.0123456789"
                "0123456789.AGCAT.0123456789"
                read-char))
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
