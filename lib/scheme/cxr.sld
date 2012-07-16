
(define-library (scheme cxr)
  (import (scheme))
  (export
   caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
  (include "cxr.scm"))
