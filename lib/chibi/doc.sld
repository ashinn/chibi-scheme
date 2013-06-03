
(define-library (chibi doc)
  (import
   (chibi) (scheme eval) (srfi 1)
   (chibi modules) (chibi ast) (chibi io) (chibi match)
   (chibi time) (chibi filesystem) (chibi process)
   (chibi scribble) (chibi sxml) (chibi highlight)
   (chibi type-inference))
  (export print-module-docs print-module-binding-docs
          generate-docs expand-docs fixup-docs
          extract-module-docs extract-file-docs
          make-default-doc-env make-module-doc-env)
  (include "doc.scm"))
