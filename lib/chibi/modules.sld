
(define-library (chibi modules)
  (export module-name module-dir module-includes module-shared-includes
          module-ast module-ast-set! module-ref module-contains?
          analyze-module containing-module load-module module-exports
          module-name->file procedure-analysis find-module
          available-modules-in-directory available-modules
          modules-exporting-identifier)
  (import (chibi) (meta) (chibi ast) (chibi filesystem))
  (include "modules.scm"))
