
(define-library (chibi char-set ascii)
  (import (chibi) (chibi iset base))
  (export char-set-contains?
          char-set:lower-case  char-set:upper-case  char-set:title-case
          char-set:letter      char-set:digit       char-set:letter+digit
          char-set:graphic     char-set:printing    char-set:whitespace
          char-set:iso-control char-set:punctuation char-set:symbol
          char-set:hex-digit   char-set:blank       char-set:ascii
          char-set:empty       char-set:full)
  (include "ascii.scm"))
