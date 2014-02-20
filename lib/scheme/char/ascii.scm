
(define char-foldcase char-downcase)

(define (string-downcase str)
  (string-map char-downcase str))

(define (string-upcase str)
  (string-map char-upcase str))

(define (string-foldcase str)
  (string-map char-foldcase str))
