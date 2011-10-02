
(define-library (scheme division)
  (import (scheme))
  (export ceiling-quotient ceiling-remainder ceiling/
          centered-quotient centered-remainder centered/
          euclidean-quotient euclidean-remainder euclidean/
          floor-quotient floor-remainder floor/
          round-quotient round-remainder round/
          truncate-quotient truncate-remainder truncate/)
  (include "division.scm"))
