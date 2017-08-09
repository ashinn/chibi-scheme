(define-library (srfi 144)
  (import (chibi)
          (rename (srfi 141)
                  (floor/ flfloor/)
                  (floor-quotient flfloor-quotient)
                  (floor-remainder flfloor-remainder)
                  (ceiling/ flceiling/)
                  (ceiling-quotient flceiling-quotient)
                  (ceiling-remainder flceiling-remainder)
                  (truncate/ fltruncate/)
                  (truncate-quotient fltruncate-quotient)
                  (truncate-remainder fltruncate-remainder)
                  (round/ flround/)
                  (round-quotient flround-quotient)
                  (round-remainder flround-remainder)
                  (euclidean/ fleuclidean/)
                  (euclidean-quotient fleuclidean-quotient)
                  (euclidean-remainder fleuclidean-remainder)
                  (balanced/ flbalanced/)
                  (balanced-quotient flbalanced-quotient)
                  (balanced-remainder flbalanced-remainder)))
  (export
   fl-e fl-e-2 fl-log2-e fl-log10-e fl-log-2 fl-log-10 fl-pi
   fl-1/pi fl-2/pi fl-pi/2 fl-pi/4 fl-sqrt-pi fl-2/sqrt-pi
   fl-sqrt-2 fl-sqrt-3 fl-sqrt-5 fl-sqrt-10 fl-1/sqrt-2
   fl-cbrt-2 fl-cbrt-3 fl-4thrt-2 fl-phi fl-log-phi fl-1/log-phi
   fl-euler fl-e-euler fl-sin-1 fl-cos-1 fl-greatest fl-least
   fl-integer-exponent-zero fl-integer-exponent-nan fl-fast-+*

   flonum flonum? fl= fl< fl> fl<= fl>= flodd? fleven?
   flunordered? flinteger? flzero? flpositive? flnegative?

   fl+ fl- fl* fl/ fl+* flmax flmin flabsdiff
   flnumerator fldenominator

   fladjacent flcopysign flsgn make-flonum flinteger-fraction
   flexponent flinteger-exponent flnormalized-fraction-exponent
   sign-bit flfinite? flinfinite? flnan? flnormalized? fldenormalized?
   flabs flposdiff flfloor flceiling flround fltruncate

   flexp flexp2 flexp-1 flsquare flsqrt flcbrt flhypot flexpt fllog fllog1+
   fllog2 fllog10 flsin flcos fltan flasin flacos flatan
   flsinh flcosh fltanh flasinh flacosh flatanh flremquo
   flgamma flloggamma flfirst-bessel flsecond-bessel flerf flerfc

   flfloor/ flfloor-quotient flfloor-remainder
   flceiling/ flceiling-quotient flceiling-remainder
   fltruncate/ fltruncate-quotient fltruncate-remainder
   flround/ flround-quotient flround-remainder
   fleuclidean/ fleuclidean-quotient fleuclidean-remainder
   flbalanced/ flbalanced-quotient flbalanced-remainder)
  (include-shared "144/math")
  (include "144/flonum.scm"))
