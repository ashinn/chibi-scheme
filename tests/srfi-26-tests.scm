
(import (scheme base) (srfi 26) (chibi test))

(test-begin "srfi-26")

(let ((x 'orig))
  (let ((f (cute list x)))
    (set! x 'wrong)
    (test '(orig) (f))))

(let ((x 'wrong))
  (let ((f (cut list x)))
    (set! x 'right)
    (test '(right) (f))))

(test-end)
