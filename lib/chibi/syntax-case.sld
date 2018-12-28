(define-library (chibi syntax-case)
  (export ... _ free-identifier=? bound-identifier=? identifier?
	  syntax-case syntax quasisyntax unsyntax unsyntax-splicing
	  datum->syntax syntax->datum
	  generate-temporaries with-syntax syntax-violation
	  with-ellipsis ellipsis-identifier?)
  (import (chibi)
	  (chibi ast)
	  (meta)
	  (srfi 1)
	  (srfi 11))
  (include "syntax-case.scm"))
