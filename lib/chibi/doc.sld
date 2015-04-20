
(define-library (chibi doc)
  (import
   (except (chibi) eval) (scheme eval) (srfi 1) (srfi 95)
   (chibi modules) (chibi ast) (chibi io) (chibi match)
   (chibi time) (chibi filesystem) (chibi process)
   (chibi string) (chibi scribble) (chibi sxml) (chibi highlight)
   (chibi type-inference))
  (export procedure-docs print-procedure-docs
          print-module-docs print-module-binding-docs
          generate-docs expand-docs fixup-docs
          extract-module-docs extract-module-file-docs extract-file-docs
          make-default-doc-env make-module-doc-env)
  (include "doc.scm"))
