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

(define-library (srfi 146)
  (export mapping mapping-unfold
          mapping/ordered mapping-unfold/ordered
          mapping? mapping-contains? mapping-empty? mapping-disjoint?
          mapping-ref mapping-ref/default mapping-key-comparator
          mapping-adjoin mapping-adjoin!
          mapping-set mapping-set!
          mapping-replace mapping-replace!
          mapping-delete mapping-delete! mapping-delete-all mapping-delete-all!
          mapping-intern mapping-intern!
          mapping-update mapping-update! mapping-update/default mapping-update!/default
          mapping-pop mapping-pop!
          mapping-search mapping-search!
          mapping-size mapping-find mapping-count mapping-any? mapping-every?
          mapping-keys mapping-values mapping-entries
          mapping-map mapping-map->list mapping-for-each mapping-fold
          mapping-filter mapping-filter!
          mapping-remove mapping-remove!
          mapping-partition mapping-partition!
          mapping-copy mapping->alist alist->mapping alist->mapping!
          alist->mapping/ordered alist->mapping/ordered!
          mapping=? mapping<? mapping>? mapping<=? mapping>=?
          mapping-union mapping-intersection mapping-difference mapping-xor
          mapping-union! mapping-intersection! mapping-difference! mapping-xor!
          make-mapping-comparator
          mapping-comparator
          mapping-min-key mapping-max-key
          mapping-min-value mapping-max-value
          mapping-key-predecessor mapping-key-successor
          mapping-range= mapping-range< mapping-range> mapping-range<= mapping-range>=
          mapping-range=! mapping-range<! mapping-range>! mapping-range<=! mapping-range>=!
          mapping-split
          mapping-catenate mapping-catenate!
          mapping-map/monotone mapping-map/monotone!
          mapping-fold/reverse
          comparator?)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          (srfi 2)
          (srfi 8)
          (srfi 121)
          (srfi 128)
          (srfi 145))
  (include "146/rbtree.scm"
           "146/mapping.scm"))
