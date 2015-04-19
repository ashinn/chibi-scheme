;; sha2-native.scm -- SHA-2 digest algorithms native interface
;; Copyright (c) 2015 Alexei Lozovsky.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (process-sha-data! context src)
  (cond ((or (bytevector? src) (string? src))
         (add-sha-data! context src))
        ((input-port? src)
         (let lp ((chunk (read-bytevector 1024 src)))
           (unless (eof-object? chunk)
             (add-sha-data! context chunk)
             (lp (read-bytevector 1024 src)))))
        (else
         (error "unknown digest source: " src))))

(define (sha-224 src)
  (let ((context (start-sha type-sha-224)))
    (process-sha-data! context src)
    (get-sha context)))

(define (sha-256 src)
  (let ((context (start-sha type-sha-256)))
    (process-sha-data! context src)
    (get-sha context)))
