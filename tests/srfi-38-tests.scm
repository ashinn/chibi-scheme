
(cond-expand
 (chibi (import (chibi) (chibi test) (srfi 38)))
 (chicken (use chicken test srfi-38)))

(test-begin "read/write")

(define (read-from-string str)
  (call-with-input-string str
    (lambda (in) (read/ss in))))

(define (write-to-string x . o)
  (call-with-output-string
    (lambda (out) (apply write/ss x out o))))

(define-syntax test-io
  (syntax-rules ()
    ((test-io str-expr expr)
     (let ((str str-expr)
           (value expr))
       (test str (write-to-string value))
       (test str (write-to-string (read-from-string str)))))))

(define-syntax test-cyclic-io
  (syntax-rules ()
    ((test-io str-expr expr)
     (let ((str str-expr)
           (value expr))
       (test str (write-to-string value #t))
       (test str (write-to-string (read-from-string str) #t))))))

(define (circular-list . args)
  (let ((res (map (lambda (x) x) args)))
    (set-cdr! (list-tail res (- (length res) 1)) res)
    res))

(test-io "(1)" (list 1))
(test-io "(1 2)" (list 1 2))
(test-io "(1 . 2)" (cons 1 2))

(test-io "#0=(1 . #0#)" (circular-list 1))
(test-io "#0=(1 2 . #0#)" (circular-list 1 2))
(test-io "(1 . #0=(2 . #0#))" (cons 1 (circular-list 2)))
(test-io "#0=(1 #0# 3)"
         (let ((x (list 1 2 3))) (set-car! (cdr x) x) x))
(test-io "(#0=(1 #0# 3))"
         (let ((x (list 1 2 3))) (set-car! (cdr x) x) (list x)))
(test-io "(#0=(1 #0# 3) #0#)"
         (let ((x (list 1 2 3))) (set-car! (cdr x) x) (list x x)))
(test-io "(#0=(1 . #0#) #1=(1 . #1#))"
         (list (circular-list 1) (circular-list 1)))

(test-io "(#0=(1 . 2) #1=(1 . 2) #2=(3 . 4) #0# #1# #2#)"
         (let ((a (cons 1 2)) (b (cons 1 2)) (c (cons 3 4)))
           (list a b c a b c)))
(test-cyclic-io "((1 . 2) (1 . 2) (3 . 4) (1 . 2) (1 . 2) (3 . 4))"
                (let ((a (cons 1 2)) (b (cons 1 2)) (c (cons 3 4)))
                  (list a b c a b c)))
(test-cyclic-io "#0=((1 . 2) (1 . 2) (3 . 4) . #0#)"
                (let* ((a (cons 1 2))
                       (b (cons 1 2))
                       (c (cons 3 4))
                       (ls (list a b c)))
                  (set-cdr! (cddr ls) ls)
                  ls))

(test-io "#0=#(#0#)"
         (let ((x (vector 1))) (vector-set! x 0 x) x))
(test-io "#0=#(1 #0#)"
         (let ((x (vector 1 2))) (vector-set! x 1 x) x))
(test-io "#0=#(1 #0# 3)"
         (let ((x (vector 1 2 3))) (vector-set! x 1 x) x))
(test-io "(#0=#(1 #0# 3))"
         (let ((x (vector 1 2 3))) (vector-set! x 1 x) (list x)))
(test-io "#0=#(#0# 2 #0#)"
         (let ((x (vector 1 2 3)))
           (vector-set! x 0 x)
           (vector-set! x 2 x)
           x))

(test 255 (read-from-string "#xff"))
(test 99 (read-from-string "#d99"))
(test 63 (read-from-string "#o77"))
(test 3 (read-from-string "#b11"))

(cond-expand
 (chicken
  (test-io "(#0=\"abc\" #0# #0#)"
           (let ((str (string #\a #\b #\c))) (list str str str)))
  (test "(\"abc\" \"abc\" \"abc\")"
      (let ((str (string #\a #\b #\c)))
        (call-with-output-string
          (lambda (out)
            (write/ss (list str str str) out ignore-strings: #t))))))
 (else
  ))

(test-end)
