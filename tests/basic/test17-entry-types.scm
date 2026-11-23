
;; Top-level procedures can be called with anything, so the native
;; backend checks the types their bodies demand once on entry.  The
;; errors must be the same as checking at each use.

(define (try thunk)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (e) (k (cons 'error (exception-irritants e))))
      thunk))))

(define (sum-vector v)
  (let lp ((i 0) (acc 0))
    (if (< i (vector-length v))
        (lp (+ i 1) (+ acc (vector-ref v i)))
        acc)))
(write (list (sum-vector (vector 1 2 3)) (sum-vector (vector))
             (try (lambda () (sum-vector "abc")))))
(newline)

;; recursive calls pass a different value, checked again on entry
(define (last-pair* p)
  (if (pair? (cdr p)) (last-pair* (cdr p)) p))
(write (list (last-pair* '(1 2 3)) (last-pair* '(1 2 . 3))
             (try (lambda () (last-pair* 5)))))
(newline)

(define (cdr-loop p) (cdr p) (cdr-loop (cdr p)))
(write (try (lambda () (cdr-loop '(1 2 3)))))
(newline)

;; through a let
(define (double-length s)
  (let ((n (string-length s))) (* n 2)))
(write (list (double-length "abc") (try (lambda () (double-length 'abc)))))
(newline)

;; checked in the order the body makes them
(define (cars a b) (car b) (car a) 'ok)
(write (list (cars '(1) '(2)) (try (lambda () (cars 1 2)))
             (try (lambda () (cars 1 '(2)))) (try (lambda () (cars '(1) 2)))))
(newline)

;; a mutated param isn't known to keep its type
(define (relength v)
  (let ((n (vector-length v)))
    (set! v "abcd")
    (+ n (string-length v))))
(write (list (relength (vector 1 2)) (try (lambda () (relength "ab")))))
(newline)

;; nothing is demanded past a possible side effect or other error
(define (after-display v)
  (display "") (vector-length v))
(define (after-other-error x v)
  (+ (car x) (vector-length v)))
(define (in-one-branch v)
  (if (vector? v) (vector-length v) 'not-a-vector))
(write (list (try (lambda () (after-display 'a)))
             (try (lambda () (after-other-error 'x (vector))))
             (in-one-branch (vector 1)) (in-one-branch "a")))
(newline)

;; ops that can fail in other ways end the walk
(define (ref-then-length v w)
  (vector-ref v 5)
  (vector-length w))
(write (list (try (lambda () (ref-then-length (make-vector 6 0) 'w)))
             (try (lambda () (ref-then-length 'v 'w)))))
(newline)
