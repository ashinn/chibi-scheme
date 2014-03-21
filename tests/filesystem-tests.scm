
(import (chibi) (chibi io) (chibi filesystem) (chibi test) (srfi 33))

(test-begin "filesystem")

(define tmp-file "/tmp/chibi-fs-test-0123456789")
(define tmp-file2 "/tmp/chibi-fs-test-0123456789-2")
(define tmp-link "/tmp/chibi-fs-test-0123456789-link")
(define tmp-dir "/tmp/chibi-fs-test-0123456789-dir")

(call-with-output-file tmp-file
  (lambda (out) (display "0123456789" out)))

(test-assert (file-exists? tmp-file))
(test "0123456789" (call-with-input-file tmp-file port->string))

;; call-with-output-file truncates
(call-with-output-file tmp-file
  (lambda (out) (display "xxxxx" out)))
(test "xxxxx" (call-with-input-file tmp-file port->string))

(call-with-output-file tmp-file
  (lambda (out) (display "0123456789" out)))
(test "0123456789" (call-with-input-file tmp-file port->string))

;; open without open/truncate writes in place
(let* ((fd (open tmp-file open/write))
       (out (open-output-file-descriptor fd)))
  (display "xxxxx" out)
  (close-output-port out))
(test "xxxxx56789" (call-with-input-file tmp-file port->string))

;; file-truncate can explicitly truncate
(let* ((fd (open tmp-file open/write))
       (out (open-output-file-descriptor fd)))
  (display "01234" out)
  (file-truncate out 7)
  (close-output-port out))
(test "0123456" (call-with-input-file tmp-file port->string))

;; symbolic links
(test-assert (symbolic-link-file tmp-file tmp-link))
(test-assert (file-exists? tmp-link))
(test-assert (file-link? tmp-link))
(test tmp-file (read-link tmp-link))

;; rename
(test-assert (rename-file tmp-file tmp-file2))
(test-not (file-exists? tmp-file))
(test-not (file-exists? tmp-link))
(test-assert (file-link? tmp-link))
(test-assert (delete-file tmp-link))
(test-not (file-exists? tmp-link))

;; cleanup
(test-assert (delete-file tmp-file2))
(test-not (file-exists? tmp-file2))

;; directories
(test-assert (file-directory? "."))
(test-assert (file-directory? ".."))
(test-assert (file-directory? "/"))
(test-not (file-regular? "."))
(test-assert (create-directory tmp-dir))
(test-assert (file-directory? tmp-dir))
(test-not (file-regular? tmp-dir))
(test-assert
  (let ((files (directory-files tmp-dir)))
    (or (equal? files '("." ".."))
        (equal? files '(".." ".")))))
(test-assert (delete-directory tmp-dir))
(test-not (file-directory? tmp-dir))

(test-end)
