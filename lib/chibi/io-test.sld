(define-library (chibi io-test)
  (export run-tests)
  (import (chibi)
          (chibi io)
          (only (scheme base) read-bytevector write-bytevector)
          (only (chibi test) test-begin test test-end))
  (begin
    (define (run-tests)
      (define long-string (make-string 2000 #\a))

      (define (string-upcase str)
        (list->string (map char-upcase (string->list str))))

      (define (strings->input-port str-ls)
        (make-generated-input-port
         (lambda ()
           (and (pair? str-ls)
                (let ((res (car str-ls)))
                  (set! str-ls (cdr str-ls))
                  res)))))

      (define (bytevectors->input-port bv-ls)
        (make-generated-binary-input-port
         (lambda ()
           (and (pair? bv-ls)
                (let ((res (car bv-ls)))
                  (set! bv-ls (cdr bv-ls))
                  res)))))

      (test-begin "io")

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
          (lambda (in)
            (let ((str (read-string 3 in))) (list str (read-string 3 in))))))

      (test "read-string-to-eof" '("abc" "de")
        (call-with-input-string "abcde"
          (lambda (in)
            (let ((str (read-string 3 in))) (list str (read-string 3 in))))))

      (test "read-string" '("ab日" "本語f")
        (call-with-input-string "ab日本語f"
          (lambda (in)
            (let ((str (read-string 3 in))) (list str (read-string 3 in))))))

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

      (test "abcdef" (read-line (strings->input-port '("abcdef"))))
      (test "abcdef" (read-line (strings->input-port '("abc" "def"))))
      (test "abcdef"
          (read-line (strings->input-port '("a" "b" "c" "d" "e" "f"))))
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

      (test #u8(0 1 2)
        (let ((in (bytevectors->input-port (list #u8(0 1 2)))))
          (read-bytevector 3 in)))

      (test #u8(0 1 2 3 4 5)
        (let ((in (bytevectors->input-port (list #u8(0 1 2) #u8(3 4 5)))))
          (read-bytevector 6 in)))

      (test #u8(3 4 5)
        (let ((in (bytevectors->input-port
                   (list #u8(0 1 2) (make-bytevector 4087 7) #u8(3 4 5)))))
          (read-bytevector 4090 in)
          (read-bytevector 3 in)))

      (test #u8(3 4 5)
        (let ((in (bytevectors->input-port
                   (list #u8(0 1 2) (make-bytevector 4093 7) #u8(3 4 5)))))
          (read-bytevector 4096 in)
          (read-bytevector 3 in)))

      (test #u8(3 4 5)
        (let ((in (bytevectors->input-port
                   (list #u8(0 1 2) (make-bytevector 5000 7) #u8(3 4 5)))))
          (read-bytevector 5003 in)
          (read-bytevector 3 in)))

      (let ((in (make-custom-binary-input-port
                 (let ((i 0))
                   (lambda (bv start end)
                     (do ((j start (+ j 1)))
                         ((= j end))
                       (bytevector-u8-set! bv j (modulo (- (+ j i) start) 256)))
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
                         ((= i end) (set! sum (+ sum x))))))))
        (write-bytevector #u8(0 1 2 3) out)
        (flush-output out)
        (test 6 sum)
        (write-bytevector #u8(100) out)
        (flush-output out)
        (test 106 sum))

      (let* ((ls '())
             (out (make-custom-output-port
                   (lambda (str start end)
                     (set! ls (cons (substring str start end) ls))
                     (- end start)))))
        (display "Test1\n" out)
        (flush-output out)
        (display "Test2\n" out)
        (flush-output out)
        (display "Test3\n" out)
        (flush-output out)
        (test "Test1\nTest2\nTest3\n" (string-concatenate (reverse ls))))

      (test "file-position"
          '(0 1 2)
        (let* ((p (open-input-file "/etc/passwd"))
               (t0 (file-position p)))
          (read-char p)
          (let ((t1 (file-position p)))
            (read-char p)
            (let ((t2 (file-position p)))
              (close-input-port p)
              (list t0 t1 t2)))))

      (test-end))))
