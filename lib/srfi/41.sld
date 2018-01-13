
(define-library (srfi 41)
  (import (scheme base) (srfi 1))
  (export
   stream-null stream-cons stream? stream-null? stream-pair?
   stream-car stream-cdr stream-lambda)
  (export
   define-stream list->stream port->stream stream stream->list
   stream-append stream-concat stream-constant stream-drop
   stream-drop-while stream-filter stream-fold stream-for-each stream-from
   stream-iterate stream-length stream-let stream-map stream-match _
   stream-of stream-range stream-ref stream-reverse stream-scan stream-take
   stream-take-while stream-unfold stream-unfolds stream-zip)
  (include "41.scm"))
