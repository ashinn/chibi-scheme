;; char-set:lower-case
(define char-set:lower-case (immutable-char-set (%make-iset 97 127 67108863 #f #f)))

;; char-set:upper-case
(define char-set:upper-case (immutable-char-set (%make-iset 65 127 67108863 #f #f)))

;; char-set:title-case
(define char-set:title-case (immutable-char-set (%make-iset 0 0 0 #f #f)))

;; char-set:letter
(define char-set:letter (immutable-char-set (%make-iset 65 127 288230371923853311 #f #f)))

;; char-set:punctuation
(define char-set:punctuation (immutable-char-set (%make-iset 33 127 6189700203056200029306911735 #f #f)))

;; char-set:symbol
(define char-set:symbol (immutable-char-set (%make-iset 36 127 1547425050547877224499904641 #f #f)))

;; char-set:blank
(define char-set:blank (immutable-char-set (%make-iset 9 32 8388609 #f #f)))

;; char-set:whitespace
(define char-set:whitespace (immutable-char-set (%make-iset 9 127 8388639 #f #f)))

;; char-set:digit
(define char-set:digit (immutable-char-set (%make-iset 48 57 #f #f #f)))

;; char-set:letter+digit
(define char-set:letter+digit (immutable-char-set (%make-iset 48 127 37778931308803301180415 #f #f)))

;; char-set:hex-digit
(define char-set:hex-digit (immutable-char-set (%make-iset 48 102 35465847073801215 #f #f)))

;; char-set:iso-control
(define char-set:iso-control (immutable-char-set (%make-iset 0 127 170141183460469231731687303720179073023 #f #f)))

;; char-set:graphic
(define char-set:graphic (immutable-char-set (%make-iset 33 127 19807040628566084398385987583 #f #f)))

;; char-set:printing
(define char-set:printing (immutable-char-set (%make-iset 9 127 332306998946228968225951765061697567 #f #f)))

