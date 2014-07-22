(define-library (chibi term ansi)
  (export
   black red yellow green 
   blue cyan magenta white
   background-black background-red background-yellow background-green 
   background-blue background-cyan background-magenta background-white
   bold 
   underline
   negative
   ansi-escapes-enabled?)
  (import (scheme base)
          (scheme write)
          (scheme process-context))
  (include "ansi.scm"))
