
(cond-expand
 (full-unicode
  (define zeros
    '#(#\x0030                ;DIGIT ZERO
       #\x0660                ;ARABIC-INDIC DIGIT ZERO
       #\x06F0                ;EXTENDED ARABIC-INDIC DIGIT ZERO
       #\x07C0                ;NKO DIGIT ZERO
       #\x0966                ;DEVANAGARI DIGIT ZERO
       #\x09E6                ;BENGALI DIGIT ZERO
       #\x0A66                ;GURMUKHI DIGIT ZERO
       #\x0AE6                ;GUJARATI DIGIT ZERO
       #\x0B66                ;ORIYA DIGIT ZERO
       #\x0BE6                ;TAMIL DIGIT ZERO
       #\x0C66                ;TELUGU DIGIT ZERO
       #\x0CE6                ;KANNADA DIGIT ZERO
       #\x0D66                ;MALAYALAM DIGIT ZERO
       #\x0E50                ;THAI DIGIT ZERO
       #\x0ED0                ;LAO DIGIT ZERO
       #\x0F20                ;TIBETAN DIGIT ZERO
       #\x1040                ;MYANMAR DIGIT ZERO
       #\x1090                ;MYANMAR SHAN DIGIT ZERO
       #\x17E0                ;KHMER DIGIT ZERO
       #\x1810                ;MONGOLIAN DIGIT ZERO
       #\x1946                ;LIMBU DIGIT ZERO
       #\x19D0                ;NEW TAI LUE DIGIT ZERO
       #\x1A80                ;TAI THAM HORA DIGIT ZERO
       #\x1A90                ;TAI THAM THAM DIGIT ZERO
       #\x1B50                ;BALINESE DIGIT ZERO
       #\x1BB0                ;SUNDANESE DIGIT ZERO
       #\x1C40                ;LEPCHA DIGIT ZERO
       #\x1C50                ;OL CHIKI DIGIT ZERO
       #\xA620                ;VAI DIGIT ZERO
       #\xA8D0                ;SAURASHTRA DIGIT ZERO
       #\xA900                ;KAYAH LI DIGIT ZERO
       #\xA9D0                ;JAVANESE DIGIT ZERO
       #\xAA50                ;CHAM DIGIT ZERO
       #\xABF0                ;MEETEI MAYEK DIGIT ZERO
       #\xFF10                ;FULLWIDTH DIGIT ZERO
       #\x104A0               ;OSMANYA DIGIT ZERO
       #\x11066               ;BRAHMI DIGIT ZERO
       #\x1D7CE               ;MATHEMATICAL BOLD DIGIT ZERO
       #\x1D7D8               ;MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
       #\x1D7E2               ;MATHEMATICAL SANS-SERIF DIGIT ZERO
       #\x1D7EC               ;MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
       #\x1D7F6               ;MATHEMATICAL MONOSPACE DIGIT ZERO
       )))
 (else
  (define zeros #(#\0))))

(define (digit-value ch)
  (let ((n (char->integer ch)))
    (let lp ((lo 0) (hi (- (vector-length zeros) 1)))
      (if (> lo hi)
          #f
          (let* ((mid (+ lo (quotient (- hi lo) 2)))
                 (mid-zero (char->integer (vector-ref zeros mid))))
            (cond
             ((<= mid-zero n (+ mid-zero 9))
              (- n mid-zero))
             ((< n mid-zero)
              (lp lo (- mid 1)))
             (else
              (lp (+ mid 1) hi))))))))
