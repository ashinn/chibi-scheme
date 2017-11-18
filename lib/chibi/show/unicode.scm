;; unicode.scm -- Unicode character width and ANSI escape support
;; Copyright (c) 2006-2017 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; a condensed non-spacing mark range from UnicodeData.txt (chars with
;; the Mn property) - generated partially by hand, should automate
;; this better

(define low-non-spacing-chars
  (bytevector
#xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#x78    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0 #xfe #xff #xff #xff #xff #xff #x1f    0    0    0    0    0    0    0
   0    0 #x3f    0    0    0    0    0    0 #xf8 #xff #x01    0    0 #x01    0
   0    0    0    0    0    0    0    0    0    0 #xc0 #xff #xff #x3f    0    0
   0    0 #x02    0    0    0 #xff #xff #xff #x07    0    0    0    0    0    0
   0    0    0    0 #xc0 #xff #x01    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#x06    0    0    0    0    0    0 #x10 #xfe #x21 #x1e    0 #x0c    0    0    0
#x02    0    0    0    0    0    0 #x10 #x1e #x20    0    0 #x0c    0    0    0
#x06    0    0    0    0    0    0 #x10 #xfe #x3f    0    0    0    0 #x03    0
#x06    0    0    0    0    0    0 #x30 #xfe #x21    0    0 #x0c    0    0    0
#x02    0    0    0    0    0    0 #x90 #x0e #x20 #x40    0    0    0    0    0
#x04    0    0    0    0    0    0    0    0 #x20    0    0    0    0    0    0
   0    0    0    0    0    0    0 #xc0 #xc1 #xff #x7f    0    0    0    0    0
   0    0    0    0    0    0    0 #x10 #x40 #x30    0    0    0    0    0    0
   0    0    0    0    0    0    0    0 #x0e #x20    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0 #x04 #x7c    0    0    0    0    0
   0    0    0    0    0    0 #xf2 #x07 #x80 #x7f    0    0    0    0    0    0
   0    0    0    0    0    0 #xf2 #x1f    0 #x3f    0    0    0    0    0    0
   0    0    0 #x03    0    0 #xa0 #x02    0    0    0    0    0    0 #xfe #x7f
#xdf    0 #xff #xff #xff #xff #xff #x1f #x40    0    0    0    0    0    0    0
   0    0    0    0    0 #xe0 #xfd #x02    0    0    0 #x03    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0 #x1c    0    0    0 #x1c    0    0    0 #x0c    0    0    0 #x0c    0
   0    0    0    0    0    0 #x80 #x3f #x40 #xfe #x0f #x20    0    0    0    0
   0 #x38    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0 #x02    0    0    0    0    0    0    0    0    0    0
   0    0    0    0 #x87 #x01 #x04 #x0e    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
   0    0    0    0    0    0    0    0    0    0 #xff #x1f #xe2 #x07
       ))

(define (unicode-char-width c)
  (let ((ci (char->integer c)))
    (cond
      ;; hand-checked ranges from EastAsianWidth.txt
      ((<= #x1100 ci #x115F) 2) ; Hangul
      ((<= #x2E80 ci #x4DB5) 2) ; CJK
      ((<= #x4E00 ci #xA4C6) 2)
      ((<= #xAC00 ci #xD7A3) 2) ; Hangul
      ((<= #xF900 ci #xFAD9) 2) ; CJK compat
      ((<= #xFE10 ci #xFE6B) 2)
      ((<= #xFF01 ci #xFF60) 2)
      ((<= #xFFE0 ci #xFFE6) 2)
      ((<= #x20000 ci #x30000) 2)
      ;; non-spacing mark (Mn) ranges from UnicodeData.txt
      ((<= #x0300 ci #x3029)
       ;; inlined bit-vector-ref for portability
       (let* ((i (- ci #x0300))
              (byte (quotient i 8))
              (off (remainder i 8)))
         (if (zero? (bitwise-and (bytevector-u8-ref low-non-spacing-chars byte)
                                 (arithmetic-shift 1 off)))
             1
             0)))
      ((<= #x302A ci #x302F) 0)
      ((<= #x3099 ci #x309A) 0)
      ((= #xFB1E ci) 0)
      ((<= #xFE00 ci #xFE23) 0)
      ((<= #x1D167 ci #x1D169) 0)
      ((<= #x1D17B ci #x1D182) 0)
      ((<= #x1D185 ci #x1D18B) 0)
      ((<= #x1D1AA ci #x1D1AD) 0)
      ((<= #xE0100 ci #xE01EF) 0)
      (else 1))))

(define (unicode-terminal-width str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (string-length str))))
    (let lp1 ((i start) (width 0))
      (if (>= i end)
          width
          (let ((c (string-ref str i)))
            (cond
              ;; ANSI escapes
              ((and (= 27 (char->integer c)) ; esc
                    (< (+ i 1) end)
                    (eqv? #\[ (string-ref str (+ i 1))))
               (let lp2 ((i (+ i 2)))
                 (cond ((>= i end) width)
                       ((memv (string-ref str i) '(#\m #\newline))
                        (lp1 (+ i 1) width))
                       (else (lp2 (+ i 1))))))
              ;; unicode characters
              ((>= (char->integer c) #x80)
               (lp1 (+ i 1) (+ width (unicode-char-width c))))
              ;; normal ASCII
              (else (lp1 (+ i 1) (+ width 1)))))))))

(define (as-unicode . args)
  (with ((string-width unicode-terminal-width))
    (each-in-list args)))
