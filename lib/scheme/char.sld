
(define-library (scheme char)
  (import (scheme))
  (export
   char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
   char-downcase char-foldcase char-lower-case? char-numeric?
   char-upcase char-upper-case? char-whitespace? numeric-digit
   string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
   string-downcase string-foldcase string-upcase)
  (include "char.scm"))
