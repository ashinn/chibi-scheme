#!/usr/bin/env chibi-scheme

(import (scheme base) (scheme read) (scheme write) (scheme eval)
        (chibi net) (chibi net server))

(define (repl-handler in out sock addr)
  (let ((env (environment '(scheme base)
                          '(only (chibi) import))))
    (let lp ()
      (let ((expr (read in)))
        (cond
         ((not (eof-object? expr))
          (let ((result (guard (exn (else
                                     (display "ERROR: " out)
                                     (write exn out)
                                     (newline out)
                                     (if #f #f)))
                          (eval expr env))))
            (cond
             ((not (eq? result (if #f #f)))
              (write result out)
              (newline out)))
            (flush-output-port out)
            (lp))))))))

(run-net-server 5556 repl-handler)
