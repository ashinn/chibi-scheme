
(define-library (chibi modules)
  (export module? module-name module-dir module-includes module-shared-includes
          module-include-library-declarations module-meta-data
          module-ast module-ast-set! module-ref module-contains?
          analyze-module containing-module load-module module-exports
          module-name->file module-lib-dir procedure-analysis find-module
          available-modules-in-directory available-modules
          modules-exporting-identifier file->sexp-list)
  (import (chibi)
          (srfi 1)
          (chibi ast)
          (chibi pathname)
          (chibi filesystem)
          (chibi string)
          (only (meta)
                module-env module-meta-data module-exports
                make-module load-module find-module resolve-import
                resolve-module-imports
                module-name-prefix
                module-name->file
                *modules*))
  (include "modules.scm"))
