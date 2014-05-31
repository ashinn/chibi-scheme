
(import (chibi weak) (chibi ast) (only (chibi test) test-begin test test-end))

(test-begin "weak pointers")

(test "preserved key and value" '("key1" "value1" #f)
  (let ((key (string-append "key" "1"))
        (value (string-append "value" "1")))
    (let ((eph (make-ephemeron key value)))
      (gc)
      (list key (ephemeron-value eph) (ephemeron-broken? eph)))))

(test "unpreserved key and value" '(#f #f #t)
  (let ((eph (make-ephemeron (string-append "key" "2")
                             (string-append "value" "2"))))
    (gc)
    (list (ephemeron-key eph) (ephemeron-value eph) (ephemeron-broken? eph))))

(test "unpreserved key and preserved value" '(#f "value3" #t)
  (let ((value (string-append "value" "3")))
    (let ((eph (make-ephemeron (string-append "key" "3") value)))
      (gc)
      (list (ephemeron-key eph) value (ephemeron-broken? eph)))))

(test "unpreserved value references unpreserved key" '(#f #f #t)
  (let ((key (string-append "key")))
    (let ((eph (make-ephemeron key (cons (string-append "value") key))))
      (gc)
      (list (ephemeron-key eph) (ephemeron-value eph) (ephemeron-broken? eph)))))

;; disabled - we support weak keys, but not proper ephemerons

'(test "preserved key and unpreserved value" '("key" "value" #f)
  (let ((key (string-append "key")))
    (let ((eph (make-ephemeron key (string-append "value"))))
      (gc)
      (list key (ephemeron-value eph) (ephemeron-broken? eph)))))

'(test "preserved value references unpreserved key" '(#f #f #t)
  (let* ((key (string-append "key"))
         (value (cons (string-append "value") key)))
    (let ((eph (make-ephemeron key value)))
      (gc)
      (list (ephemeron-key eph) value (ephemeron-broken? eph)))))

(test-end)
