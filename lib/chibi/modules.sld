
(define-library (chibi modules)
  (export module-name module-dir module-includes module-shared-includes
          module-include-library-declarations
          module-ast module-ast-set! module-ref module-contains?
          analyze-module containing-module load-module module-exports
          module-name->file procedure-analysis find-module
          available-modules-in-directory available-modules
          modules-exporting-identifier file->sexp-list)
  (import (chibi) (meta) (srfi 1) (chibi ast) (chibi filesystem))
  (include "modules.scm"))
