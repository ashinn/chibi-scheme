;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
(define-library (srfi 168 test)

  (export run-tests)

  (import (scheme base))
  (import (srfi 158))
  (import (chibi test))
  (import (srfi 128))
  (import (srfi 146 hash))

  (import (srfi 167 memory))
  (import (srfi 167 engine))
  (import (srfi 168))
  (import (srfi 173))

  (begin

    (define (run-tests)
      (define engine (make-default-engine))

      (define (triplestore)
        (nstore engine (list 42 1337) '(uid key value)))

      (test "ask empty triplestore"
        #f
        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))
          ;; ask
          (let ((out (engine-in-transaction
                      engine okvs
                      (lambda (transaction)
                        (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
            (engine-close engine okvs)
            out)))

      (test "add and ask triplestore"
        #t
        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))
          ;; add
          (engine-in-transaction
           engine okvs
           (lambda (transaction)
             (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))))
          ;; ask
          (let ((out
                 (engine-in-transaction
                  engine okvs
                  (lambda (transaction)
                    (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
            (engine-close engine okvs)
            out)))

      (test "add, rm and ask triplestore"
        #f
        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))

          (let ((out
                 (engine-in-transaction
                  engine okvs
                  (lambda (transaction)
                    ;; add!
                    (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))
                    ;; remove!
                    (nstore-delete! transaction triplestore '("P4X432" blog/title "hyper.dev"))
                    ;; ask
                    (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
            (engine-close engine okvs)
            out)))

      (test "blog query post titles"
        '("DIY a database" "DIY a full-text search engine")

        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))
          (engine-in-transaction
           engine okvs
           (lambda (transaction)
             ;; add hyper.dev blog posts
             (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))
             (nstore-add! transaction triplestore '("123456" post/title "DIY a database"))
             (nstore-add! transaction triplestore '("123456" post/blog "P4X432"))
             (nstore-add! transaction triplestore '("654321" post/title "DIY a full-text search engine"))
             (nstore-add! transaction triplestore '("654321" post/blog "P4X432"))
             ;; add dthompson.us blog posts
             (nstore-add! transaction triplestore '("1" blog/title "dthompson.us"))
             (nstore-add! transaction triplestore '("2" post/title "Haunt 0.2.4 released"))
             (nstore-add! transaction triplestore '("2" post/blog "1"))
             (nstore-add! transaction triplestore '("3" post/title "Haunt 0.2.3 released"))
             (nstore-add! transaction triplestore '("3" post/blog "1"))))
          ;; query
          (let ()
            (define query
              (lambda (transaction blog/title)
                (generator->list (nstore-query
                                  (nstore-from transaction triplestore
                                               (list (nstore-var 'blog/uid)
                                                     'blog/title
                                                     blog/title))
                                  (nstore-where transaction triplestore
                                                (list (nstore-var 'post/uid)
                                                      'post/blog
                                                      (nstore-var 'blog/uid)))
                                  (nstore-where transaction triplestore
                                                (list (nstore-var 'post/uid)
                                                      'post/title
                                                      (nstore-var 'post/title)))))))
            (let* ((out (engine-in-transaction engine okvs (lambda (transaction) (query transaction "hyper.dev"))))
                   (out (map (lambda (x) (hashmap-ref x 'post/title)) out)))
              (engine-close engine okvs)
              out))))

      (test "nstore-from limit and offset"
        '("hyperdev.fr")
        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))
          ;; add!
          (nstore-add! okvs triplestore '("P4X432" blog/title "hyper.dev"))
          (nstore-add! okvs triplestore '("P4X433" blog/title "hyperdev.fr"))
          (nstore-add! okvs triplestore '("P4X434" blog/title "hypermove.net"))
          (let ((out (engine-in-transaction
                      engine okvs
                      (lambda (transaction)
                        (generator-map->list
                         (lambda (item) (hashmap-ref item 'title))
                         (nstore-from transaction triplestore (list (nstore-var 'uid)
                                                                    'blog/title
                                                                    (nstore-var 'title))
                                      `((limit . 1) (offset . 1))))))))
            (engine-close engine okvs)
           out)))

      (test "nstore validation add via hooks"
            #t
            (let* ((okvs (engine-open engine #f))
                   (triplestore (triplestore))
                   (hook (nstore-hook-on-add triplestore)))
              (hook-add! hook (lambda (nstore items)
                                (when (string=? (car items) "private")
                                  (error 'nstore-hook "private is private" items))))
              (guard (ex (else #t))
                (nstore-add! okvs triplestore '("private" private "private"))
                #f)))

      (test "nstore validation delete via hooks"
            #t
            (let* ((okvs (engine-open engine #f))
                   (triplestore (triplestore))
                   (hook (nstore-hook-on-delete triplestore)))
              (hook-add! hook (lambda (nstore items)
                                (when (string=? (car items) "private")
                                  (error 'nstore-hook "private is private" items))))
              (guard (ex (else #t))
                (nstore-delete! okvs triplestore '("private" private "private"))
                #f)))

      )))
