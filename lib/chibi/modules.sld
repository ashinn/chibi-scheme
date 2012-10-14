
(define-library (chibi modules)
  (export module-name module-dir module-includes module-shared-includes
          module-ast module-ast-set! module-ref module-contains?
          analyze-module containing-module load-module module-exports
          module-name->file procedure-analysis)
  (import (chibi) (meta) (chibi ast))
  (include "modules.scm"))
