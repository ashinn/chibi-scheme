
;; A tail call with more args than fit in the trampoline's fixed buffer,
;; after a variadic call consing a rest list from many args.

(define (g a b c d e f g h i j k l m n o p) (list a p))
(define (f . xs) (g (length xs) 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
(display (f 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(newline)
