
(define-library (chibi strings)
  (export
   string-cursor-start string-cursor-end string-cursor-ref
   string-cursor<? string-cursor<=? string-cursor>? string-cursor>=?
   string-cursor=? string-null? string-every string-any
   string-join string-split string-count
   string-trim string-trim-left string-trim-right
   string-mismatch string-mismatch-right
   string-prefix? string-suffix?
   string-index string-index-right string-skip string-skip-right
   string-fold string-fold-right string-map string-for-each
   string-contains make-string-searcher)
  (import (scheme) (chibi ast))
  (include "strings.scm"))
