
(import (chibi weak) (chibi ast) (only (chibi test) test-begin test test-end))

(test-begin "weak pointers")

(test "preserved key and value" '("key" "value" #f)
  (let ((key (string-append "key"))
        (value (string-append "value")))
    (let ((eph (make-ephemeron key value)))
      (gc)
      (list key (ephemeron-value eph) (ephemeron-broken? eph)))))

(test "unpreserved key and value" '(#f #f #t)
  (let ((eph (make-ephemeron (string-append "key") (string-append "value"))))
    (gc)
    (list (ephemeron-key eph) (ephemeron-value eph) (ephemeron-broken? eph))))

(test "unpreserved key and preserved value" '(#f "value" #t)
  (let ((value (string-append "value")))
    (let ((eph (make-ephemeron (string-append "key") value)))
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
