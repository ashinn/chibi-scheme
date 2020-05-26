;; Copyright (C) Marc Nieper-Wißkirchen (2016, 2017).  All Rights
;; Reserved.

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

(define-library (srfi 146 test)
  (export run-tests)
  (import (scheme base)
          (srfi 1)
          (srfi 8)
          (srfi 128)
          (srfi 146)
          (chibi test))
  (begin
    (define (run-tests)
      (test-begin "SRFI 146")

      (test-group "Predicates"
        (define mapping0 (mapping comparator))
        (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
        (define mapping2 (mapping comparator 'c 1 'd 2 'e 3))
        (define mapping3 (mapping comparator 'd 1 'e 2 'f 3))
        
        (test-assert "mapping?: a mapping"
          (mapping? (mapping comparator)))

        (test-assert "mapping?: not a mapping"
          (not (mapping? (list 1 2 3))))
        
        (test-assert "mapping-empty?: empty mapping"
          (mapping-empty? mapping0))
        
        (test-assert "mapping-empty?: non-empty mapping"
          (not (mapping-empty? mapping1)))

        (test-assert "mapping-contains?: containing"
          (mapping-contains? mapping1 'b))   

        (test-assert "mapping-contains?: not containing"
          (not (mapping-contains? mapping1 '2)))

        (test-assert "mapping-disjoint?: disjoint"
          (mapping-disjoint? mapping1 mapping3))

        (test-assert "mapping-disjoint?: not disjoint"
          (not (mapping-disjoint? mapping1 mapping2))))

      (test-group "Accessors"
        (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))

        (test "mapping-ref: key found"
                    2
                    (mapping-ref mapping1 'b))

        (test "mapping-ref: key not found/with failure"
                    42
                    (mapping-ref mapping1 'd (lambda () 42)))

        (test-error "mapping-ref: key not found/without failure"
                    (mapping-ref mapping1 'd))

        (test "mapping-ref: with success procedure"
                    (* 2 2)
                    (mapping-ref mapping1 'b (lambda () #f) (lambda (x) (* x x))))

        (test "mapping-ref/default: key found"
                    3
                    (mapping-ref/default mapping1 'c 42))

        (test "mapping-ref/default: key not found"
                    42
                    (mapping-ref/default mapping1 'd 42))

        (test "mapping-key-comparator"
                    comparator
                    (mapping-key-comparator mapping1)))

      (test-group "Updaters"
        (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
        (define mapping2 (mapping-set mapping1 'c 4 'd 4 'd 5))
        (define mapping3 (mapping-update mapping1 'b (lambda (x) (* x x))))
        (define mapping4 (mapping-update/default mapping1 'd (lambda (x) (* x x)) 4))
        (define mapping5 (mapping-adjoin mapping1 'c 4 'd 4 'd 5))
        (define mapping0 (mapping comparator))

        (test "mapping-adjoin: key already in mapping"
                    3
                    (mapping-ref mapping5 'c))

        (test "mapping-adjoin: key set earlier"
                    4
                    (mapping-ref mapping5 'd))
        
        (test "mapping-set: key already in mapping"
                    4
                    (mapping-ref mapping2 'c))

        (test "mapping-set: key set earlier"
                    5
                    (mapping-ref mapping2 'd))

        (test "mapping-replace: key not in mapping"
                    #f
                    (mapping-ref/default (mapping-replace mapping1 'd 4) 'd #f))

        (test "mapping-replace: key in mapping"
                    6
                    (mapping-ref (mapping-replace mapping1 'c 6) 'c))

        (test "mapping-delete"
                    42
                    (mapping-ref/default (mapping-delete mapping1 'b) 'b 42))

        (test "mapping-delete-all"
                    42
                    (mapping-ref/default (mapping-delete-all mapping1 '(a b)) 'b 42))
        
        (test "mapping-intern: key in mapping"
                    (list mapping1 2)
                    (receive result
                        (mapping-intern mapping1 'b (lambda () (error "should not have been invoked")))
                      result))

        (test "mapping-intern: key not in mapping"
                    (list 42 42)
                    (receive (mapping value)
                        (mapping-intern mapping1 'd (lambda () 42))
                      (list value (mapping-ref mapping 'd))))
	    
        (test "mapping-update"
                    4
                    (mapping-ref mapping3 'b))

        (test "mapping-update/default"
                    16
                    (mapping-ref mapping4 'd))

        (test "mapping-pop: empty mapping"
                    'empty
                    (mapping-pop mapping0 (lambda () 'empty)))

        (test "mapping-pop: non-empty mapping"
                    (list 2 'a 1)
                    (receive (mapping key value)
                        (mapping-pop mapping1)
                      (list (mapping-size mapping) key value))))

      (test-group "The whole mapping"
        (define mapping0 (mapping comparator))
        (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))

        (test "mapping-size: empty mapping"
                    0
                    (mapping-size mapping0))

        (test "mapping-size: non-empty mapping"
                    3
                    (mapping-size mapping1))

        (test "mapping-find: found in mapping"
                    (list 'b 2)
                    (receive result
                        (mapping-find (lambda (key value)
                                        (and (eq? key 'b)
                                             (= value 2)))
                                      mapping1
                                      (lambda () (error "should not have been called")))
                      result))

        (test "mapping-find: not found in mapping"
                    (list 42)
                    (receive result
                        (mapping-find (lambda (key value)
                                        (eq? key 'd))
                                      mapping1
                                      (lambda ()
                                        42))
                      result))

        (test "mapping-count"
                    2
                    (mapping-count (lambda (key value)
                                     (>= value 2))
                                   mapping1))
        
        (test-assert "mapping-any?: found"
          (mapping-any? (lambda (key value)
                          (= value 3))
                        mapping1))

        (test-assert "mapping-any?: not found"
          (not (mapping-any? (lambda (key value)
                               (= value 4))
                             mapping1)))

        (test-assert "mapping-every?: true"
          (mapping-every? (lambda (key value)
                            (<= value 3))
                          mapping1))

        (test-assert "mapping-every?: false"
          (not (mapping-every? (lambda (key value)
                                 (<= value 2))
                               mapping1)))

        (test "mapping-keys"
                    3
                    (length (mapping-keys mapping1)))

        (test "mapping-values"
                    6
                    (fold + 0 (mapping-values mapping1)))

        (test "mapping-entries"
                    (list 3 6)
                    (receive (keys values)
                        (mapping-entries mapping1)
                      (list (length keys) (fold + 0 values)))))

      (test-group "Mapping and folding"
        (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
        (define mapping2 (mapping-map (lambda (key value)
                                        (values (symbol->string key)
                                                (* 10 value)))
                                      comparator
                                      mapping1))

        (test "mapping-map"
                    20
                    (mapping-ref mapping2 "b"))

        (test "mapping-for-each"
                    6
                    (let ((counter 0))
                      (mapping-for-each (lambda (key value)
                                          (set! counter (+ counter value)))
                                        mapping1)
                      counter))

        (test "mapping-fold"
                    6
                    (mapping-fold (lambda (key value acc)
                                    (+ value acc))
                                  0
                                  mapping1))

        (test "mapping-map->list"
                    (+ (* 1 1) (* 2 2) (* 3 3))
                    (fold + 0 (mapping-map->list (lambda (key value)
                                                   (* value value))
                                                 mapping1)))

        (test "mapping-filter"
                    2
                    (mapping-size (mapping-filter (lambda (key value)
                                                    (<= value 2))
                                                  mapping1)))

        (test "mapping-remove"
                    1
                    (mapping-size (mapping-remove (lambda (key value)
                                                    (<= value 2))
                                                  mapping1)))

        (test "mapping-partition"
                    (list 1 2)
                    (receive result
                        (mapping-partition (lambda (key value)
                                             (eq? 'b key))
                                           mapping1)
                      (map mapping-size result)))

        (test-group "Copying and conversion"
          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
          (define mapping2 (alist->mapping comparator '((a . 1) (b . 2) (c . 3))))
          (define mapping3 (alist->mapping! (mapping-copy mapping1) '((d . 4) '(c . 5))))
          
          (test "mapping-copy: same size"
                      3
                      (mapping-size (mapping-copy mapping1)))

          (test "mapping-copy: same comparator"
                      comparator
                      (mapping-key-comparator (mapping-copy mapping1)))

          (test "mapping->alist"
                      (cons 'b 2)
                      (assq 'b (mapping->alist mapping1)))
          
          (test "alist->mapping"
                      2
                      (mapping-ref mapping2 'b)
                      )

          (test "alist->mapping!: new key"
                      4
                      (mapping-ref mapping3 'd))

          (test "alist->mapping!: existing key"
                      3
                      (mapping-ref mapping3 'c)))

        (test-group "Submappings"
          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
          (define mapping2 (mapping comparator 'a 1 'b 2 'c 3))
          (define mapping3 (mapping comparator 'a 1 'c 3))
          (define mapping4 (mapping comparator 'a 1 'c 3 'd 4))
          (define mapping5 (mapping comparator 'a 1 'b 2 'c 6))
          (define mapping6 (mapping (make-comparator (comparator-type-test-predicate comparator)
                                                     (comparator-equality-predicate comparator)
                                                     (comparator-ordering-predicate comparator)
                                                     (comparator-hash-function comparator))
                                    'a 1 'b 2 'c 3))
          

          (test-assert "mapping=?: equal mappings"
            (mapping=? comparator mapping1 mapping2))
          
          (test-assert "mapping=?: unequal mappings"
            (not (mapping=? comparator mapping1 mapping4)))

          (test-assert "mapping=?: different comparators"
            (not (mapping=? comparator mapping1 mapping6)))
          
          (test-assert "mapping<?: proper subset"
            (mapping<? comparator mapping3 mapping1))

          (test-assert "mapping<?: improper subset"
            (not (mapping<? comparator mapping3 mapping1 mapping2)))
          
          (test-assert "mapping>?: proper superset"
            (mapping>? comparator mapping2 mapping3))

          (test-assert "mapping>?: improper superset"
            (not (mapping>? comparator mapping1 mapping2 mapping3)))

          (test-assert "mapping<=?: subset"
            (mapping<=? comparator mapping3 mapping2 mapping1))

          (test-assert "mapping<=?: non-matching values"
            (not (mapping<=? comparator mapping3 mapping5)))

          (test-assert "mapping<=?: not a subset"
            (not (mapping<=? comparator mapping2 mapping4)))

          (test-assert "mapping>=?: superset"
            (mapping>=? comparator mapping4 mapping3))

          (test-assert "mapping>=?: not a superset"
            (not (mapping>=? comparator mapping5 mapping3))))

        (test-group "Set theory operations"
          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
          (define mapping2 (mapping comparator 'a 1 'b 2 'd 4))
          (define mapping3 (mapping comparator 'a 1 'b 2))
          (define mapping4 (mapping comparator 'a 1 'b 2 'c 4))
          (define mapping5 (mapping comparator 'a 1 'c 3))
          (define mapping6 (mapping comparator 'd 4 'e 5 'f 6))
          
          (test "mapping-union: new association"
                      4
                      (mapping-ref (mapping-union mapping1 mapping2) 'd))

          (test "mapping-union: existing association"
                      3
                      (mapping-ref (mapping-union mapping1 mapping4) 'c))
          
          (test "mapping-union: three mappings"
                      6
                      (mapping-size (mapping-union mapping1 mapping2 mapping6)))
          
          (test "mapping-intersection: existing association"
                      3
                      (mapping-ref (mapping-intersection mapping1 mapping4) 'c))

          (test "mapping-intersection: removed association"
                      42
                      (mapping-ref/default (mapping-intersection mapping1 mapping5) 'b 42))

          (test "mapping-difference"
                      2
                      (mapping-size (mapping-difference mapping2 mapping6)))

          (test "mapping-xor"
                      4
                      (mapping-size (mapping-xor mapping2 mapping6))))

        (test-group "Additional procedures for mappings with ordered keys"
          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
          (define mapping2 (mapping comparator 'a 1 'b 2 'c 3 'd 4))
          (define mapping3 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5))
          (define mapping4 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6))
          (define mapping5 (mapping comparator 'f 6 'g 7 'h 8))

          (test "mapping-min-key"
                      '(a a a a)
                      (map mapping-min-key (list mapping1 mapping2 mapping3 mapping4)))

          (test "mapping-max-key"
                      '(c d e f)
                      (map mapping-max-key (list mapping1 mapping2 mapping3 mapping4)))
          
          (test "mapping-min-value"
                      '(1 1 1 1)
                      (map mapping-min-value (list mapping1 mapping2 mapping3 mapping4)))

          (test "mapping-max-value"
                      '(3 4 5 6)
                      (map mapping-max-value (list mapping1 mapping2 mapping3 mapping4)))

          (test "mapping-key-predecessor"
                      '(c d d d)
                      (map (lambda (mapping)
                             (mapping-key-predecessor mapping 'e (lambda () #f)))
                           (list mapping1 mapping2 mapping3 mapping4)))

          (test "mapping-key-successor"
                      '(#f #f e e)
                      (map (lambda (mapping)
                             (mapping-key-successor mapping 'd (lambda () #f)))
                           (list mapping1 mapping2 mapping3 mapping4)))

          (test "mapping-range=: contained"
                      '(4)
                      (mapping-values (mapping-range= mapping4 'd)))

          (test "mapping-range=: not contained"
                      '()
                      (mapping-values (mapping-range= mapping4 'z)))

          (test "mapping-range<"
                      '(1 2 3)
                      (mapping-values (mapping-range< mapping4 'd)))

          (test "mapping-range<="
                      '(1 2 3 4)
                      (mapping-values (mapping-range<= mapping4 'd)))

          (test "mapping-range>"
                      '(5 6)
                      (mapping-values (mapping-range> mapping4 'd)))

          (test "mapping-range>="
                      '(4 5 6)
                      (mapping-values (mapping-range>= mapping4 'd)))

          (test "mapping-split"
                      '((1 2 3) (1 2 3 4) (4) (4 5 6) (5 6))
                      (receive mappings
                          (mapping-split mapping4 'd)
                        (map mapping-values mappings)))
          
          (test "mapping-catenate"
                      '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8))
                      (mapping->alist (mapping-catenate comparator mapping2 'e 5 mapping5)))
          
          (test "mapping-map/monotone"
                      '((1 . 1) (2 . 4) (3 . 9))
                      (mapping->alist
                       (mapping-map/monotone (lambda (key value)
                                               (values value (* value value)))
                                             comparator
                                             mapping1)))

          (test "mapping-fold/reverse"
                      '(1 2 3)
                      (mapping-fold/reverse (lambda (key value acc)
                                              (cons value acc))
                                            '() mapping1)))
        
        (test-group "Comparators"
          (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
          (define mapping2 (mapping comparator 'a 1 'b 2 'c 3))
          (define mapping3 (mapping comparator 'a 1 'b 2))
          (define mapping4 (mapping comparator 'a 1 'b 2 'c 4))
          (define mapping5 (mapping comparator 'a 1 'c 3))
          (define mapping0 (mapping comparator mapping1 "a" mapping2 "b" mapping3 "c" mapping4 "d" mapping5 "e"))

          (test-assert "mapping-comparator"
            (comparator? mapping-comparator))
          
          (test "mapping-keyed mapping"
                      (list "a" "a" "c" "d" "e")
                      (list (mapping-ref mapping0 mapping1)
                            (mapping-ref mapping0 mapping2)
                            (mapping-ref mapping0 mapping3)
                            (mapping-ref mapping0 mapping4)
                            (mapping-ref mapping0 mapping5)))
          
          (test-group "Ordering comparators"
            (test-assert "=?: equal mappings"
              (=? comparator mapping1 mapping2))

            (test-assert "=?: unequal mappings"
              (not (=? comparator mapping1 mapping4)))

            (test-assert "<?: case 1"
              (<? comparator mapping3 mapping4))

            (test-assert "<?: case 2"
              (<? comparator mapping1 mapping4))

            (test-assert "<?: case 3"
              (<? comparator mapping1 mapping5)))))
      
      (test-end "SRFI 146"))

    (define comparator (make-default-comparator))))
