;; This library is deprecated, occurring in early R7RS drafts before
;; being removed.

(define-library (scheme char normalization)
  (import (rename (chibi)
                  (string=? string-ni=?)
                  (string<? string-ni<?)
                  (string>? string-ni>?)
                  (string<=? string-ni<=?)
                  (string>=? string-ni>=?)))
  (export string-ni=? string-ni<? string-ni>? string-ni<=? string-ni>=?))
