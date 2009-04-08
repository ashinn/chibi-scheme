
(define foo
  (lambda (a b c d e f g h)
    (+ (+ (* a b) (* c d)) (+ (* e f) (* g h)))))

(define (writeln x)
  (write x)
  (newline))

(writeln (length (reverse (list 1 2 3 4 5 6 7 8 9 10 11))))
(writeln (reverse (list 1 2 3 4 5 6 7 8 9 10 11)))
(writeln (append (list 1 2) (list 3 4)))
(writeln (foo 1 2 3 4 5 6 7 8))
(writeln (apply foo (list 1 2 3 4 5 6 7 8)))
(writeln (apply foo 1 (list 2 3 4 5 6 7 8)))
(writeln (apply foo 1 2 3 4 (list 5 6 7 8)))
(writeln (apply foo 1 2 3 4 5 (list 6 7 8)))

