(define-library (chibi term ansi)
  (export

   black-escape red-escape yellow-escape green-escape
   blue-escape cyan-escape magenta-escape white-escape
   rgb-escape 
   gray-escape
   rgb24-escape 
   reset-color-escape

   black-background-escape red-background-escape
   yellow-background-escape green-background-escape
   blue-background-escape cyan-background-escape
   magenta-background-escape white-background-escape
   rgb-background-escape
   gray-background-escape
   rgb24-background-escape
   reset-background-color-escape

   black red yellow green
   blue cyan magenta white
   black-background red-background yellow-background green-background
   blue-background cyan-background magenta-background white-background
   bold
   underline
   negative
   rgb rgb-background
   gray gray-background
   rgb24 rgb24-background
   bold-escape reset-bold-escape
   underline-escape reset-underline-escape
   negative-escape reset-negative-escape

   ansi-escapes-enabled?)
  (import (scheme base)
          (scheme write)
          (scheme process-context))
  (include "ansi.scm"))
