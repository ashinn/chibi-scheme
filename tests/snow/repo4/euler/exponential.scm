
;;> Returns e^\var{x}.

(define e
  (let ((iterations 10))
    (lambda (x)
      (let lp ((i 1) (num 1) (den 1) (res 0))
        (if (> i iterations)
            res
            (lp (+ i 1) (* num x) (* den i) (+ res (/ num den))))))))
