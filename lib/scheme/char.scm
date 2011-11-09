
;; Simple ASCII definitions, need to make Unicode aware.

(define char-foldcase char-downcase)

(define (numeric-digit ch)
  (let ((n (- (char->integer ch) (char->integer #\0))))
    (and (<= 0 n 9) n)))

(define (string-upcase str)
  (string-map char-upcase str))

(define (string-downcase str)
  (string-map char-downcase str))

(define (string-foldcase str)
  (string-map char-foldcase str))
