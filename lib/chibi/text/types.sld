
(define-library (chibi text types)
  (import (scheme base))
  (export
   make-text text?
   text-bytes text-bytes-set!
   text-start text-start-set!
   text-end text-end-set!
   text-prev text-prev-set!
   text-next text-next-set!
   text-marks text-marks-set!
   text-source text-source-set!
   text-first text-last
   make-mark mark?
   mark-text mark-text-set!
   mark-offset mark-offset-set!
   mark-data mark-data-set!
   ;; loading
   make-text-source text-source?
   text-source-loader text-source-loader-set!
   text-source-path text-source-path-set!
   text-source-data text-source-data-set!
   make-text-loader text-loader?
   text-loader-load text-loader-load-set!
   text-loader-reload text-loader-reload-set!
   text-loader-write text-loader-write-set!
   text-loader-modified? text-loader-modified?-set!)
  (include "types.scm"))
