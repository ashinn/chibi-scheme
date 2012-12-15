
(import (scheme base) (srfi 2) (chibi test))

(test-begin "srfi-2")

(test 1 (and-let* () 1))
(test 2 (and-let* () 1 2))
(test #t (and-let* () ))

(test #f (let ((x #f)) (and-let* (x))))
(test 1 (let ((x 1)) (and-let* (x))))
(test #f (and-let* ((x #f)) ))
(test 1 (and-let* ((x 1)) ))
;; (test-syntax-error (and-let* ( #f (x 1))))
(test #f (and-let* ( (#f) (x 1)) ))
;; (test-syntax-error (and-let* (2 (x 1))))
(test 1 (and-let* ( (2) (x 1)) ))
(test 2 (and-let* ( (x 1) (2)) ))
(test #f (let ((x #f)) (and-let* (x) x)))
(test "" (let ((x "")) (and-let* (x) x)))
(test "" (let ((x "")) (and-let* (x)  )))
(test 2 (let ((x 1)) (and-let* (x) (+ x 1))))
(test #f (let ((x #f)) (and-let* (x) (+ x 1))))
(test 2 (let ((x 1)) (and-let* (((positive? x))) (+ x 1))))
(test #t (let ((x 1)) (and-let* (((positive? x))) )))
(test #f (let ((x 0)) (and-let* (((positive? x))) (+ x 1))))
(test 3  (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1))))
(test 4
    (let ((x 1))
      (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1))))

(test 2 (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))))
(test 2 (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))))
(test #f (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))))
(test #f (let ((x #f)) (and-let* (x ((positive? x))) (+ x 1))))
(test #f (let ((x #f)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))))

(test  #f (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test  #f (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test  #f (let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test  3/2 (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))

(test-end)
