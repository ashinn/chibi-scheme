
(cond-expand
 (modules
  (import (chibi io)
          (only (scheme base) read-bytevector write-bytevector)
          (only (chibi test) test-begin test test-end)))
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

(test "read-line" '("abc" "def" "ghi")
  (call-with-input-string "abcdef\nghi\n"
    (lambda (in)
      (let* ((line1 (read-line in 3))
             (line2 (read-line in 3)))
        (list line1 line2 (read-line in 3))))))

(test "read-line-to-eof" '("abc" "def")
  (call-with-input-string "abc\ndef"
    (lambda (in) (let ((line (read-line in))) (list line (read-line in))))))

(test "read-string" '("abc" "def")
  (call-with-input-string "abcdef"
    (lambda (in) (let ((str (read-string 3 in))) (list str (read-string 3 in))))))

(test "read-string-to-eof" '("abc" "de")
  (call-with-input-string "abcde"
    (lambda (in) (let ((str (read-string 3 in))) (list str (read-string 3 in))))))

(test "read-string" '("ab日" "本語f")
  (call-with-input-string "ab日本語f"
    (lambda (in) (let ((str (read-string 3 in))) (list str (read-string 3 in))))))

(test "read-string!" '("abc" "def")
  (call-with-input-string "abcdef"
    (lambda (in)
      (let* ((str1 (make-string 3))
             (str2 (make-string 3)))
        (read-string! str1 3 in)
        (read-string! str2 3 in)
        (list str1 str2)))))

(test "read-string!-to-eof" '("abc" "de ")
  (call-with-input-string "abcde"
    (lambda (in)
      (let* ((str1 (make-string 3))
             (str2 (make-string 3 #\space)))
        (read-string! str1 3 in)
        (read-string! str2 3 in)
        (list str1 str2)))))

(test "null-output-port" #t
  (let ((out (make-null-output-port)))
    (write 1 out)
    (close-output-port out)
    #t))

(test "null-input-port" #t
  (let ((in (make-null-input-port)))
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
        (close-output-port out)))))

(define (strings->input-port str-ls)
  (make-generated-input-port
   (lambda ()
     (and (pair? str-ls)
          (let ((res (car str-ls)))
            (set! str-ls (cdr str-ls))
            res)))))

(test "abcdef" (read-line (strings->input-port '("abcdef"))))
(test "abcdef" (read-line (strings->input-port '("abc" "def"))))
(test "abcdef" (read-line (strings->input-port '("a" "b" "c" "d" "e" "f"))))
(test "日本語" (read-line (strings->input-port '("日本語"))))
(test "日本語" (read-line (strings->input-port '("日" "本" "語"))))
(test "abc"
    (let ((in (strings->input-port
               (list "日本語" (make-string 4087 #\-) "abc"))))
      (read-string 4090 in)
      (read-line in)))
(test "abc"
    (let ((in (strings->input-port
               (list "日本語" (make-string 4087 #\本) "abc"))))
      (read-string 4090 in)
      (read-line in)))
(test "abc"
    (let ((in (strings->input-port
               (list "日本語" (make-string 4093 #\-) "abc"))))
      (read-string 4096 in)
      (read-line in)))

(let ((in (make-custom-binary-input-port
           (let ((i 0))
             (lambda (bv start end)
               (do ((j start (+ j 1)))
                   ((= j end))
                 (bytevector-u8-set! bv j (modulo (+ j i) 256)))
               (if (> end 0)
                   (set! i (bytevector-u8-ref bv (- end 1))))
               (- end start))))))
  (test #u8(0 1 2 3) (read-bytevector 4 in))
  (test #u8(4 5 6 7) (read-bytevector 4 in))
  (test 7 (bytevector-u8-ref (read-bytevector 256 in) 255))
  (test 6 (bytevector-u8-ref (read-bytevector 1024 in) 1022)))

(let* ((sum 0)
       (out (make-custom-binary-output-port
             (lambda (bv start end)
               (do ((i start (+ i 1))
                    (x 0 (+ x (bytevector-u8-ref bv i))))
                   ((= i end) (set! sum x)))))))
  (write-bytevector #u8(0 1 2 3) out)
  (flush-output out)
  (test 6 sum)
  (write-bytevector #u8(100) out)
  (flush-output out)
  (test 106 sum))

(test "file-position"
    '(0 1 2)
  (let* ((p (open-input-file "tests/io-tests.scm"))
         (t0 (file-position p)))
    (read-char p)
    (let ((t1 (file-position p)))
      (read-char p)
      (let ((t2 (file-position p)))
        (close-input-port p)
        (list t0 t1 t2)))))

(test-end)
