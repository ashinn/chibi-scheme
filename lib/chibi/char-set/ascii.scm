;; char-set:lower-case
(define char-set:lower-case (immutable-char-set (%make-iset 97 122 #f #f #f)))

;; char-set:upper-case
(define char-set:upper-case (immutable-char-set (%make-iset 65 90 #f #f #f)))

;; char-set:title-case
(define char-set:title-case (immutable-char-set (%make-iset 0 0 0 #f #f)))

;; char-set:letter
(define char-set:letter (immutable-char-set (%make-iset 97 122 #f (%make-iset 65 90 #f #f #f) #f)))

;; char-set:punctuation
(define char-set:punctuation (immutable-char-set (%make-iset 63 64 #f (%make-iset 44 47 #f (%make-iset 37 42 #f (%make-iset 33 35 #f #f #f) #f) (%make-iset 58 59 #f #f #f)) (%make-iset 123 123 #f (%make-iset 95 95 #f (%make-iset 91 93 #f #f #f) #f) (%make-iset 125 125 #f #f #f)))))

;; char-set:symbol
(define char-set:symbol (immutable-char-set (%make-iset 94 94 #f (%make-iset 43 43 #f (%make-iset 36 36 #f #f #f) (%make-iset 60 62 #f #f #f)) (%make-iset 124 124 #f (%make-iset 96 96 #f #f #f) (%make-iset 126 126 #f #f #f)))))

;; char-set:blank
(define char-set:blank (immutable-char-set (%make-iset 32 32 #f (%make-iset 9 9 #f #f #f) #f)))

;; char-set:whitespace
(define char-set:whitespace (immutable-char-set (%make-iset 32 32 #f (%make-iset 9 13 #f #f #f) #f)))

;; char-set:digit
(define char-set:digit (immutable-char-set (%make-iset 48 57 #f #f #f)))

;; char-set:letter+digit
(define char-set:letter+digit (immutable-char-set (%make-iset 65 90 #f (%make-iset 48 57 #f #f #f) (%make-iset 97 122 #f #f #f))))

;; char-set:hex-digit
(define char-set:hex-digit (immutable-char-set (%make-iset 65 70 #f (%make-iset 48 57 #f #f #f) (%make-iset 97 102 #f #f #f))))

;; char-set:iso-control
(define char-set:iso-control (immutable-char-set (%make-iset 127 127 #f (%make-iset 0 31 #f #f #f) #f)))

;; char-set:graphic
(define char-set:graphic (immutable-char-set (%make-iset 33 126 #f #f #f)))

;; char-set:printing
(define char-set:printing (immutable-char-set (%make-iset 32 126 #f (%make-iset 9 13 #f #f #f) #f)))

