
(define-library (chibi json)
  (import (scheme base)
          (except (srfi 99 records) define-record-type)
          (only (chibi ast) type-name)
          (only (chibi) make-constructor))
  (export string->json json->string json-read json-write
          make-json-reader)
  (include-shared "json")
  (include "json.scm"))
