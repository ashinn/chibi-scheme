
(define-library (chibi string)
  (export
   string-cursor-start string-cursor-end string-cursor-ref
   string-cursor<? string-cursor<=? string-cursor>? string-cursor>=?
   string-cursor=? string-cursor-next substring-cursor
   string-null? string-every string-any
   string-join string-split string-count
   string-trim string-trim-left string-trim-right
   string-mismatch string-mismatch-right
   string-prefix? string-suffix?
   string-find string-find-right string-find? string-skip string-skip-right
   string-fold string-fold-right string-map string-for-each
   string-contains make-string-searcher
   string-downcase-ascii string-upcase-ascii)
  (import (chibi) (chibi ast) (chibi char-set base))
  (include "string.scm"))
