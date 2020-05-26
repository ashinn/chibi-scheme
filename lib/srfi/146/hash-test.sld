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

(define-library (srfi 146 hash-test)
  (export run-tests)
  (import (scheme base)
          (srfi 1)
          (srfi 8)
          (srfi 146 hash)
          (srfi 128)
          (chibi test))
  (begin
    (define comparator (make-default-comparator))

    (define (run-tests)
      (test-begin "SRFI 146: Hashmaps")

      (test-group "Predicates"
        (define hashmap0 (hashmap comparator))
        (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
        (define hashmap2 (hashmap comparator 'c 1 'd 2 'e 3))
        (define hashmap3 (hashmap comparator 'd 1 'e 2 'f 3))

        (test-assert "hashmap?: a hashmap"
          (hashmap? (hashmap comparator)))

        (test-assert "hashmap?: not a hashmap"
          (not (hashmap? (list 1 2 3))))

        (test-assert "hashmap-empty?: empty hashmap"
          (hashmap-empty? hashmap0))

        (test-assert "hashmap-empty?: non-empty hashmap"
          (not (hashmap-empty? hashmap1)))

        (test-assert "hashmap-contains?: containing"
          (hashmap-contains? hashmap1 'b))

        (test-assert "hashmap-contains?: not containing"
          (not (hashmap-contains? hashmap1 '2)))

        (test-assert "hashmap-disjoint?: disjoint"
          (hashmap-disjoint? hashmap1 hashmap3))

        (test-assert "hashmap-disjoint?: not disjoint"
          (not (hashmap-disjoint? hashmap1 hashmap2))))

      (test-group "Accessors"
        (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))

        (test "hashmap-ref: key found"
                    2
                    (hashmap-ref hashmap1 'b))

        (test "hashmap-ref: key not found/with failure"
                    42
                    (hashmap-ref hashmap1 'd (lambda () 42)))

        (test-error "hashmap-ref: key not found/without failure"
                    (hashmap-ref hashmap1 'd))

        (test "hashmap-ref: with success procedure"
                    (* 2 2)
                    (hashmap-ref hashmap1 'b (lambda () #f) (lambda (x) (* x x))))

        (test "hashmap-ref/default: key found"
                    3
                    (hashmap-ref/default hashmap1 'c 42))

        (test "hashmap-ref/default: key not found"
                    42
                    (hashmap-ref/default hashmap1 'd 42))

        (test "hashmap-key-comparator"
                    comparator
                    (hashmap-key-comparator hashmap1)))

      (test-group "Updaters"
        (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
        (define hashmap2 (hashmap-set hashmap1 'c 4 'd 4 'd 5))
        (define hashmap3 (hashmap-update hashmap1 'b (lambda (x) (* x x))))
        (define hashmap4 (hashmap-update/default hashmap1 'd (lambda (x) (* x x)) 4))
        (define hashmap5 (hashmap-adjoin hashmap1 'c 4 'd 4 'd 5))
        (define hashmap0 (hashmap comparator))

        (test "hashmap-adjoin: key already in hashmap"
                    3
                    (hashmap-ref hashmap5 'c))

        (test "hashmap-adjoin: key set earlier"
                    4
                    (hashmap-ref hashmap5 'd))

        (test "hashmap-set: key already in hashmap"
                    4
                    (hashmap-ref hashmap2 'c))

        (test "hashmap-set: key set earlier"
                    5
                    (hashmap-ref hashmap2 'd))

        (test "hashmap-replace: key not in hashmap"
                    #f
                    (hashmap-ref/default (hashmap-replace hashmap1 'd 4) 'd #f))

        (test "hashmap-replace: key in hashmap"
                    6
                    (hashmap-ref (hashmap-replace hashmap1 'c 6) 'c))

        (test "hashmap-delete"
                    42
                    (hashmap-ref/default (hashmap-delete hashmap1 'b) 'b 42))

        (test "hashmap-delete-all"
                    42
                    (hashmap-ref/default (hashmap-delete-all hashmap1 '(a b)) 'b 42))

        (test "hashmap-intern: key in hashmap"
                    (list hashmap1 2)
                    (receive result
                        (hashmap-intern hashmap1 'b (lambda () (error "should not have been invoked")))
                      result))

        (test "hashmap-intern: key not in hashmap"
                    (list 42 42)
                    (receive (hashmap value)
                        (hashmap-intern hashmap1 'd (lambda () 42))
                      (list value (hashmap-ref hashmap 'd))))

        (test "hashmap-update"
                    4
                    (hashmap-ref hashmap3 'b))

        (test "hashmap-update/default"
                    16
                    (hashmap-ref hashmap4 'd))

        (test "hashmap-pop: empty hashmap"
                    'empty
                    (hashmap-pop hashmap0 (lambda () 'empty)))

        (test-assert "hashmap-pop: non-empty hashmap"
          (member
           (receive (hashmap key value)
               (hashmap-pop hashmap1)
             (list (hashmap-size hashmap) key value))
           '((2 a 1) (2 b 2) (2 c 3)))))

      (test-group "The whole hashmap"
        (define hashmap0 (hashmap comparator))
        (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))

        (test "hashmap-size: empty hashmap"
                    0
                    (hashmap-size hashmap0))

        (test "hashmap-size: non-empty hashmap"
                    3
                    (hashmap-size hashmap1))

        (test "hashmap-find: found in hashmap"
                    (list 'b 2)
                    (receive result
                        (hashmap-find (lambda (key value)
                                        (and (eq? key 'b)
                                             (= value 2)))
                                      hashmap1
                                      (lambda () (error "should not have been called")))
                      result))

        (test "hashmap-find: not found in hashmap"
                    (list 42)
                    (receive result
                        (hashmap-find (lambda (key value)
                                        (eq? key 'd))
                                      hashmap1
                                      (lambda ()
                                        42))
                      result))

        (test "hashmap-count"
                    2
                    (hashmap-count (lambda (key value)
                                     (>= value 2))
                                   hashmap1))

        (test-assert "hashmap-any?: found"
          (hashmap-any? (lambda (key value)
                          (= value 3))
                        hashmap1))

        (test-assert "hashmap-any?: not found"
          (not (hashmap-any? (lambda (key value)
                               (= value 4))
                             hashmap1)))

        (test-assert "hashmap-every?: true"
          (hashmap-every? (lambda (key value)
                            (<= value 3))
                          hashmap1))

        (test-assert "hashmap-every?: false"
          (not (hashmap-every? (lambda (key value)
                                 (<= value 2))
                               hashmap1)))

        (test "hashmap-keys"
                    3
                    (length (hashmap-keys hashmap1)))

        (test "hashmap-values"
                    6
                    (fold + 0 (hashmap-values hashmap1)))

        (test "hashmap-entries"
                    (list 3 6)
                    (receive (keys values)
                        (hashmap-entries hashmap1)
                      (list (length keys) (fold + 0 values)))))

      (test-group "Hashmap and folding"
        (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
        (define hashmap2 (hashmap-map (lambda (key value)
                                        (values (symbol->string key)
                                                (* 10 value)))
                                      comparator
                                      hashmap1))

        (test "hashmap-map"
                    20
                    (hashmap-ref hashmap2 "b"))

        (test "hashmap-for-each"
                    6
                    (let ((counter 0))
                      (hashmap-for-each (lambda (key value)
                                          (set! counter (+ counter value)))
                                        hashmap1)
                      counter))

        (test "hashmap-fold"
                    6
                    (hashmap-fold (lambda (key value acc)
                                    (+ value acc))
                                  0
                                  hashmap1))

        (test "hashmap-map->list"
                    (+ (* 1 1) (* 2 2) (* 3 3))
                    (fold + 0 (hashmap-map->list (lambda (key value)
                                                   (* value value))
                                                 hashmap1)))

        (test "hashmap-filter"
                    2
                    (hashmap-size (hashmap-filter (lambda (key value)
                                                    (<= value 2))
                                                  hashmap1)))

        (test "hashmap-remove"
                    1
                    (hashmap-size (hashmap-remove (lambda (key value)
                                                    (<= value 2))
                                                  hashmap1)))

        (test "hashmap-partition"
                    (list 1 2)
                    (receive result
                        (hashmap-partition (lambda (key value)
                                             (eq? 'b key))
                                           hashmap1)
                      (map hashmap-size result)))

        (test-group "Copying and conversion"
          (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
          (define hashmap2 (alist->hashmap comparator '((a . 1) (b . 2) (c . 3))))
          (define hashmap3 (alist->hashmap! (hashmap-copy hashmap1) '((d . 4) '(c . 5))))

          (test "hashmap-copy: same size"
                      3
                      (hashmap-size (hashmap-copy hashmap1)))

          (test "hashmap-copy: same comparator"
                      comparator
                      (hashmap-key-comparator (hashmap-copy hashmap1)))

          (test "hashmap->alist"
                      (cons 'b 2)
                      (assq 'b (hashmap->alist hashmap1)))

          (test "alist->hashmap"
                      2
                      (hashmap-ref hashmap2 'b)
                      )

          (test "alist->hashmap!: new key"
                      4
                      (hashmap-ref hashmap3 'd))

          (test "alist->hashmap!: existing key"
                      3
                      (hashmap-ref hashmap3 'c)))

        (test-group "Subhashmaps"
          (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
          (define hashmap2 (hashmap comparator 'a 1 'b 2 'c 3))
          (define hashmap3 (hashmap comparator 'a 1 'c 3))
          (define hashmap4 (hashmap comparator 'a 1 'c 3 'd 4))
          (define hashmap5 (hashmap comparator 'a 1 'b 2 'c 6))
          (define hashmap6 (hashmap (make-comparator (comparator-type-test-predicate comparator)
                                                     (comparator-equality-predicate comparator)
                                                     (comparator-ordering-predicate comparator)
                                                     (comparator-hash-function comparator))
                                    'a 1 'b 2 'c 3))


          (test-assert "hashmap=?: equal hashmaps"
            (hashmap=? comparator hashmap1 hashmap2))

          (test-assert "hashmap=?: unequal hashmaps"
            (not (hashmap=? comparator hashmap1 hashmap4)))

          (test-assert "hashmap=?: different comparators"
            (not (hashmap=? comparator hashmap1 hashmap6)))

          (test-assert "hashmap<?: proper subset"
            (hashmap<? comparator hashmap3 hashmap1))

          (test-assert "hashmap<?: improper subset"
            (not (hashmap<? comparator hashmap3 hashmap1 hashmap2)))

          (test-assert "hashmap>?: proper superset"
            (hashmap>? comparator hashmap2 hashmap3))

          (test-assert "hashmap>?: improper superset"
            (not (hashmap>? comparator hashmap1 hashmap2 hashmap3)))

          (test-assert "hashmap<=?: subset"
            (hashmap<=? comparator hashmap3 hashmap2 hashmap1))

          (test-assert "hashmap<=?: non-matching values"
            (not (hashmap<=? comparator hashmap3 hashmap5)))

          (test-assert "hashmap<=?: not a subset"
            (not (hashmap<=? comparator hashmap2 hashmap4)))

          (test-assert "hashmap>=?: superset"
            (hashmap>=? comparator hashmap4 hashmap3))

          (test-assert "hashmap>=?: not a superset"
            (not (hashmap>=? comparator hashmap5 hashmap3))))

        (test-group "Set theory operations"
          (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
          (define hashmap2 (hashmap comparator 'a 1 'b 2 'd 4))
          (define hashmap3 (hashmap comparator 'a 1 'b 2))
          (define hashmap4 (hashmap comparator 'a 1 'b 2 'c 4))
          (define hashmap5 (hashmap comparator 'a 1 'c 3))
          (define hashmap6 (hashmap comparator 'd 4 'e 5 'f 6))

          (test "hashmap-union: new association"
                      4
                      (hashmap-ref (hashmap-union hashmap1 hashmap2) 'd))

          (test "hashmap-union: existing association"
                      3
                      (hashmap-ref (hashmap-union hashmap1 hashmap4) 'c))

          (test "hashmap-union: three hashmaps"
                      6
                      (hashmap-size (hashmap-union hashmap1 hashmap2 hashmap6)))

          (test "hashmap-intersection: existing association"
                      3
                      (hashmap-ref (hashmap-intersection hashmap1 hashmap4) 'c))

          (test "hashmap-intersection: removed association"
                      42
                      (hashmap-ref/default (hashmap-intersection hashmap1 hashmap5) 'b 42))

          (test "hashmap-difference"
                      2
                      (hashmap-size (hashmap-difference hashmap2 hashmap6)))

          (test "hashmap-xor"
                      4
                      (hashmap-size (hashmap-xor hashmap2 hashmap6))))

        (test-group "Comparators"
          (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
          (define hashmap2 (hashmap comparator 'a 1 'b 2 'c 3))
          (define hashmap3 (hashmap comparator 'a 1 'b 2))
          (define hashmap4 (hashmap comparator 'a 1 'b 2 'c 4))
          (define hashmap5 (hashmap comparator 'a 1 'c 3))
          (define hashmap0 (hashmap comparator
                                    hashmap1 "a"
                                    hashmap2 "b"
                                    hashmap3 "c"
                                    hashmap4 "d"
                                    hashmap5 "e"))

          (test-assert "hashmap-comparator"
            (comparator? hashmap-comparator))

          (test "hashmap-keyed hashmap"
                      (list "a" "a" "c" "d" "e")
                      (list (hashmap-ref hashmap0 hashmap1)
                            (hashmap-ref hashmap0 hashmap2)
                            (hashmap-ref hashmap0 hashmap3)
                            (hashmap-ref hashmap0 hashmap4)
                            (hashmap-ref hashmap0 hashmap5)
                            ))

          (test-group "Ordering comparators"
            (test-assert "=?: equal hashmaps"
              (=? comparator hashmap1 hashmap2))

            (test-assert "=?: unequal hashmaps"
              (not (=? comparator hashmap1 hashmap4))))))

      (test-end "SRFI 146: Hashmaps"))))
