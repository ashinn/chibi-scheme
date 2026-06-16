(define-library (srfi 235)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1))
  (export constantly
          complement
          swap
          flip
          on-left
          on-right
          conjoin
          disjoin
          each-of
          all-of
          any-of
          on
          left-section
          right-section
          apply-chain
          arguments-drop
          arguments-drop-right
          arguments-take
          arguments-take-right
          group-by

          begin-procedure
          if-procedure
          when-procedure
          unless-procedure
          value-procedure
          case-procedure
          and-procedure
          eager-and-procedure
          or-procedure
          eager-or-procedure
          funcall-procedure
          loop-procedure
          while-procedure
          until-procedure

          always
          never
          boolean)
  (include "235/impl.scm"))
