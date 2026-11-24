
;; Lets are compiled inline, binding their params in the enclosing
;; frame, so check what depends on fresh bindings and frame layout.

;; a fresh binding per iteration, captured by closures
(define (make-thunks n)
  (let lp ((i 0) (res '()))
    (if (< i n)
        (let ((x (* i 10)))
          (lp (+ i 1) (cons (lambda () x) res)))
        (reverse res))))
(write (map (lambda (f) (f)) (make-thunks 3)))
(newline)

;; mutated let params get a fresh box per iteration
(define (make-counters n)
  (let lp ((i 0) (res '()))
    (if (< i n)
        (let ((c i))
          (lp (+ i 1) (cons (lambda () (set! c (+ c 1)) c) res)))
        (reverse res))))
(let* ((cs (make-counters 2))
       (a ((car cs)))
       (b ((car cs)))
       (c ((cadr cs))))
  (write (list a b c)))
(newline)

;; set! of a boxed let param from a let inside a nested lambda
(define (make-renamer)
  ((lambda (renames)
     (lambda (id)
       ((lambda (cell)
          (if cell
              (cdr cell)
              ((lambda (name)
                 (set! renames (cons (cons id name) renames))
                 name)
               (list id))))
        (assq id renames))))
   '()))
(let* ((r (make-renamer))
       (a (r 'a))
       (b (r 'b)))
  (write (list a b (eq? a (r 'a)))))
(newline)

;; nested lets, shadowing, and a let in an argument
(define (shadow x)
  (let ((x (+ x 1)))
    (let ((y (let ((x (* x 2))) (+ x 1))))
      (list x y))))
(write (shadow 1))
(newline)

;; a loop through a let in tail position, and a non-tail let
(define (sum-squares n)
  (let lp ((i 0) (acc 0))
    (if (< i n)
        (let ((sq (* i i)))
          (lp (+ i 1) (+ acc sq)))
        (+ acc (let ((z 1)) z)))))
(write (sum-squares 10))
(newline)

;; the types a let's body relies on are still checked
(define (try thunk)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (e) (k (cons 'error (exception-irritants e))))
      thunk))))
(define (first-elt v)
  (let ((x v))
    (vector-ref x 0)))
(write (list (first-elt (vector 1 2)) (try (lambda () (first-elt "abc")))))
(newline)

;; more nested lets than the code generator's initial let stack
(define (deep a)
  (let ((x1 a)) (let ((x2 (+ x1 1))) (let ((x3 (+ x2 1))) (let ((x4 (+ x3 1))) (let ((x5 (+ x4 1))) (let ((x6 (+ x5 1))) (let ((x7 (+ x6 1))) (let ((x8 (+ x7 1))) (let ((x9 (+ x8 1))) (let ((x10 (+ x9 1))) (let ((x11 (+ x10 1))) (let ((x12 (+ x11 1))) (let ((x13 (+ x12 1))) (let ((x14 (+ x13 1))) (let ((x15 (+ x14 1))) (let ((x16 (+ x15 1))) (let ((x17 (+ x16 1))) (let ((x18 (+ x17 1))) (let ((x19 (+ x18 1))) (let ((x20 (+ x19 1))) (let ((x21 (+ x20 1))) (let ((x22 (+ x21 1))) (let ((x23 (+ x22 1))) (let ((x24 (+ x23 1))) (let ((x25 (+ x24 1))) (let ((x26 (+ x25 1))) (let ((x27 (+ x26 1))) (let ((x28 (+ x27 1))) (let ((x29 (+ x28 1))) (let ((x30 (+ x29 1))) (let ((x31 (+ x30 1))) (let ((x32 (+ x31 1))) (let ((x33 (+ x32 1))) (let ((x34 (+ x33 1))) (let ((x35 (+ x34 1))) (let ((x36 (+ x35 1))) (let ((x37 (+ x36 1))) (let ((x38 (+ x37 1))) (let ((x39 (+ x38 1))) (let ((x40 (+ x39 1))) (let ((x41 (+ x40 1))) (let ((x42 (+ x41 1))) (let ((x43 (+ x42 1))) (let ((x44 (+ x43 1))) (let ((x45 (+ x44 1))) (let ((x46 (+ x45 1))) (let ((x47 (+ x46 1))) (let ((x48 (+ x47 1))) (let ((x49 (+ x48 1))) (let ((x50 (+ x49 1))) (let ((x51 (+ x50 1))) (let ((x52 (+ x51 1))) (let ((x53 (+ x52 1))) (let ((x54 (+ x53 1))) (let ((x55 (+ x54 1))) (let ((x56 (+ x55 1))) (let ((x57 (+ x56 1))) (let ((x58 (+ x57 1))) (let ((x59 (+ x58 1))) (let ((x60 (+ x59 1))) (let ((x61 (+ x60 1))) (let ((x62 (+ x61 1))) (let ((x63 (+ x62 1))) (let ((x64 (+ x63 1))) (let ((x65 (+ x64 1))) (let ((x66 (+ x65 1))) (let ((x67 (+ x66 1))) (let ((x68 (+ x67 1))) (let ((x69 (+ x68 1))) (let ((x70 (+ x69 1))) (lambda () (+ x1 x70)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
(write ((deep 1)))
(newline)
