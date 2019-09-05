
(define-library (chibi pty-test)
  (import (scheme base) (scheme file) (scheme write)
          (chibi io) (chibi pty) (chibi stty) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests . o)
      (when (file-exists? "/usr/bin/units")
        (test-begin "pty")
        (test '("\t* 3.2808399" "\t/ 0.3048")
            (call-with-pty-process-io
             '("/usr/bin/units" "-q")
             (lambda (pid in out name)
               (with-raw-io
                out
                (lambda ()
                  ;; input with tab completion
                  (display "mete\t" out) (newline out)
                  (display "fee\t" out) (newline out)
                  (display (integer->char 4) out)
                  (flush-output-port out)
                  ;; result
                  (let* ((l1 (read-line in))
                         (l2 (read-line in)))
                    (list l1 l2)))))))
        (test-end)))))
