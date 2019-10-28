(define-library (gleckler hamt-map-test)
  (import (scheme base)
	  (chibi test)
	  (gleckler hamt)
	  (gleckler hamt-map)
	  (gleckler hamt-misc)
	  (only (srfi 1) alist-delete fold)
	  (only (srfi 27) random-integer)
	  (only (srfi 113)
                set
		set-adjoin!
		set-delete!
		set-for-each)
	  (only (srfi 125)
		hash-table->alist
		hash-table-keys
		hash-table-delete!
		hash-table-for-each
		hash-table-set!
		hash-table-size
		string-hash)
          (only (srfi 128) make-comparator)
	  (only (srfi 132) list-sort)
	  (only (srfi 151) bit-count))
  (export run-hamt-map-tests)
  (include "hamt-map-test.scm"))