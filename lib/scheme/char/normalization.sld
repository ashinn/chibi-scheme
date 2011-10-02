
(define-library (scheme char normalization)
  (import (rename (scheme)
                  (string=? string-ni=?)
                  (string<? string-ni<?)
                  (string>? string-ni>?)
                  (string<=? string-ni<=?)
                  (string>=? string-ni>=?)))
  (export string-ni=? string-ni<? string-ni>? string-ni<=? string-ni>=?))
