
(define-library (chibi shell-test)
  (import (scheme base) (chibi shell) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "(chibi shell)")
      (test "hello\n"
          (shell->string (echo "hello")))
      (test "world\n"
          (shell->string (echo "world")))
      (test "HELLO\n"
          (shell->string
           ,(shell-pipe
             '(echo "hello")
             '(tr "a-z" "A-Z"))))
      (test "OLLEH\n"
          (shell->string
           ,(shell-pipe
             '(echo "hello")
             '(tr "a-z" "A-Z")
             'rev)))
      (test "OLLEH\n"
          (shell->string (echo "hello") (tr "a-z" "A-Z") rev))
      (test "pass\n"
          (shell->string ,(shell-if 'true '(echo "pass") '(echo "fail"))))
      (test "fail\n"
          (shell->string ,(shell-if 'false '(echo "pass") '(echo "fail"))))
      (test "hello\nworld\n"
          (shell->string ,(shell-do '(echo "hello") '(echo "world"))))
      (test "hello\n"
          (shell->string
           ,(shell-and 'true '(echo "hello") 'false '(echo "world"))))
      (test "hello\n"
          (shell->string
           ,(shell-or 'false '(echo "hello") '(echo "world"))))
      (test "hello\n"
          (shell->string (or false (echo "hello") (echo "world"))))
      (test '("hello" "world")
          (shell->string-list (do (echo "hello") (echo "world"))))
      (test '(hello world)
          (shell->sexp-list (do (echo "hello") (echo "world"))))
      (test "HELLO"
          (shell->string (cat) (<< hello) (tr "a-z" "A-Z")))
      (test "HELLO"
          (shell->string (>< (cat) (tr "a-z" "A-Z")) (<< hello)))
      (test-end))))
