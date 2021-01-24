#! /usr/bin/env chibi-scheme

(import (scheme base) (scheme write) (srfi 193))

(define-syntax pp
  (syntax-rules ()
    ((_ expr) (begin (write 'expr) (display " => ") (write expr) (newline)))))

(pp (command-line))
(pp (command-name))
(pp (command-args))
(pp (script-file))
(pp (script-directory))
