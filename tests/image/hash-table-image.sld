;; Identity hash tables built before saving an image must still find
;; their keys after the image is loaded at a different address.

(define-library (hash-table-image)
  (import (scheme base) (scheme write) (scheme hash-table)
          (scheme process-context))
  (export check run)
  (begin
    (define-record-type Point (make-point x) point? (x point-x))
    (define keys
      (let lp ((i 0) (acc '()))
        (if (= i 500) acc (lp (+ i 1) (cons (make-point i) acc)))))
    ;; large enough to have been resized
    (define table (make-hash-table eq?))
    (for-each (lambda (k) (hash-table-set! table k (point-x k))) keys)
    ;; record types as keys, as in a registry of types
    (define types (make-hash-table eq?))
    (hash-table-set! types Point 'point)
    (define (check)
      (let ((ok (and (= 500 (hash-table-size table))
                     (let lp ((ls keys))
                       (or (null? ls)
                           (and (eqv? (point-x (car ls))
                                      (hash-table-ref/default table (car ls) #f))
                                (lp (cdr ls)))))
                     (eq? 'point (hash-table-ref/default types Point #f))
                     (begin
                       (hash-table-set! table (car keys) 'updated)
                       (and (= 500 (hash-table-size table))
                            (eq? 'updated
                                 (hash-table-ref/default table (car keys) #f)))))))
        (display (if ok "[PASS] identity hash tables in images\n"
                     "[FAIL] identity hash tables in images\n"))
        ok))
    ;; emergency-exit since exit can't unwind to a continuation
    ;; captured before the image was saved on the native backend
    (define (run)
      (let ((ok (check)))
        (flush-output-port)
        (emergency-exit ok)))))
