
(define char-set:letter+digit
  (immutable-char-set (char-set-union char-set:letter char-set:digit)))

(define char-set:hex-digit
  (immutable-char-set
   (char-set-union (string->char-set "0123456789abcdefABCDEF"))))

(define char-set:iso-control
  (immutable-char-set
   (char-set-union (ucs-range->char-set 0 #x20)
                   (ucs-range->char-set #x7F #xA0))))

(define char-set:graphic
  (immutable-char-set
   (char-set-union
    char-set:letter char-set:digit char-set:punctuation char-set:symbol)))

(define char-set:printing
  (immutable-char-set (char-set-union char-set:whitespace char-set:graphic)))
