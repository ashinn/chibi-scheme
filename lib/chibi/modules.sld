
(define-library (chibi modules)
  (export module? module-name module-dir module-includes module-shared-includes
          module-include-library-declarations
          module-ast module-ast-set! module-ref module-contains?
          analyze-module containing-module load-module module-exports
          module-name->file procedure-analysis find-module
          available-modules-in-directory available-modules
          modules-exporting-identifier file->sexp-list)
  (import (chibi) (srfi 1) (chibi ast) (chibi filesystem)
          (only (meta)
                module-env module-meta-data module-exports
                make-module load-module find-module resolve-import
                module-name-prefix module-name->file *modules*))
  (include "modules.scm"))
