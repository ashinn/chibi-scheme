;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (srfi 146 hash)
  (export hashmap hashmap-unfold
	  hashmap? hashmap-contains? hashmap-empty? hashmap-disjoint?
	  hashmap-ref hashmap-ref/default hashmap-key-comparator
	  hashmap-adjoin hashmap-adjoin!
	  hashmap-set hashmap-set!
	  hashmap-replace hashmap-replace!
	  hashmap-delete hashmap-delete! hashmap-delete-all hashmap-delete-all!
	  hashmap-intern hashmap-intern!
	  hashmap-update hashmap-update! hashmap-update/default hashmap-update!/default
	  hashmap-pop hashmap-pop!
	  hashmap-search hashmap-search!
	  hashmap-size hashmap-find hashmap-count hashmap-any? hashmap-every?
	  hashmap-keys hashmap-values hashmap-entries
	  hashmap-map hashmap-map->list hashmap-for-each hashmap-fold
	  hashmap-filter hashmap-filter!
	  hashmap-remove hashmap-remove!
	  hashmap-partition hashmap-partition!
	  hashmap-copy hashmap->alist alist->hashmap alist->hashmap!
	  hashmap=? hashmap<? hashmap>? hashmap<=? hashmap>=?
	  hashmap-union hashmap-intersection hashmap-difference hashmap-xor
	  hashmap-union! hashmap-intersection! hashmap-difference! hashmap-xor!
	  make-hashmap-comparator
	  hashmap-comparator
	  comparator?)
  (import (scheme base)
	  (scheme case-lambda)
	  (srfi 1)
	  (srfi 8)
	  (srfi 121)
	  (srfi 128)
	  (srfi 145)
	  (gleckler hamt-map))
  (include "hash.scm"))
