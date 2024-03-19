
(define (utf8-initial-byte-length bv offset)
  (let ((ch (bytevector-u8-ref bv offset)))
    (cond
     ((< ch #xC0) 1)
     ((< ch #xE0) 2)
     (else (+ 3 (bitwise-and 1 (arithmetic-shift ch -4)))))))

(define (utf8-ref bv offset)
  (let ((end (min (+ 4 offset) (bytevector-length bv))))
    ;; TODO: this is unsafe, read directly
    (string-ref (utf8->string! bv offset end) 0)))

(define (utf8-next bv offset end)
  (min end (+ offset (utf8-initial-byte-length bv offset))))

(define (utf8-prev bv offset start)
  (let lp ((i (- offset 1)))
    (and (>= i start)
         (if (= #b10 (arithmetic-shift (bytevector-u8-ref bv i) -6))
             (lp (- i 1))
             i))))
