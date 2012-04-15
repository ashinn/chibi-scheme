
(cond-expand
 (modules (import (chibi io) (only (chibi test) test-begin test test-end)))
 (else #f))

(test-begin "io")

(define long-string (make-string 2000 #\a))

(test "input-string-port" 1025
  (call-with-input-string (substring long-string 0 1025)
    (lambda (in)
      (let loop ((c (read-char in)) (i 0))
        (cond ((eof-object? c) i)
              ((> i 1025) (error "read past eof"))
              (else (loop (read-char in) (+ i 1))))))))

(test "read-line" '("abc" "def")
  (call-with-input-string "abc\ndef\n"
    (lambda (in) (let ((line (read-line in))) (list line (read-line in))))))

(test "read-line-to-eof" '("abc" "def")
  (call-with-input-string "abc\ndef"
    (lambda (in) (let ((line (read-line in))) (list line (read-line in))))))

;; Custom ports are only supported with string streams (i.e. either
;; GNU fopencookie or BSD funopen).

(cond-expand
 (string-streams

  (test "null-output-port" #t
    (let ((out (make-null-output-port)))
      (write 1 out)
      (close-output-port out)
      #t))

  (test "null-input-port" #t
    (let ((in (make-concatenated-port)))
      (let ((res (eof-object? (read-char in))))
        (close-input-port in)
        res)))

  (define (string-upcase str)
    (list->string (map char-upcase (string->list str))))

  (test "upcase-input-port" "ABC"
    (call-with-input-string "abc"
      (lambda (in)
        (let ((in (make-filtered-input-port string-upcase in)))
          (let ((res (read-line in)))
            (close-input-port in)
            res)))))

  (test "upcase-output-port" "ABC"
    (call-with-output-string
      (lambda (out)
        (let ((out (make-filtered-output-port string-upcase out)))
          (display "abc" out)
          (close-output-port out)))))))

(test-end)
