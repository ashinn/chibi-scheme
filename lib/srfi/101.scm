;; Adapted for R7RS by Alex Shinn 2018.

;; SRFI 101: Purely Functional Random-Access Pairs and Lists
;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. REMEMBER, THERE IS NO SCHEME UNDERGROUND. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
;; THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-record-type kons
  (make-kons size tree rest)
  kons?
  (size kons-size)
  (tree kons-tree)
  (rest kons-rest))

(define-record-type node
  (make-node val left right)
  node?
  (val node-val)
  (left node-left)
  (right node-right))

(define-syntax assert
  (syntax-rules ()
    ((assert expr ...)
     (begin (unless expr (error "assertion failed" 'expr)) ...))))

;; Nat -> Nat
(define (sub1 n) (- n 1))
(define (add1 n) (+ n 1))

;; [Tree X] -> X
(define (tree-val t)
  (if (node? t) 
      (node-val t)
      t))

;; [X -> Y] [Tree X] -> [Tree Y]
(define (tree-map f t)
  (if (node? t)
      (make-node (f (node-val t))
                 (tree-map f (node-left t))
                 (tree-map f (node-right t)))
      (f t)))

;; [X -> Y] [Tree X] -> unspecified
(define (tree-for-each f t)
  (if (node? t)
      (begin (f (node-val t))
             (tree-for-each f (node-left t))
             (tree-for-each f (node-right t)))
      (f t)))

;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> [Tree R]
(define (tree-map/n f ts)
  (let recr ((ts ts))
    (if (and (pair? ts)
             (node? (car ts)))
        (make-node (apply f (map node-val ts))
                   (recr (map node-left ts))
                   (recr (map node-right ts)))
        (apply f ts))))

;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> unspecified
(define (tree-for-each/n f ts)
  (let recr ((ts ts))
    (if (and (pair? ts)
             (node? (car ts)))
        (begin (apply f (map node-val ts))
               (recr (map node-left ts))
               (recr (map node-right ts)))
        (apply f ts))))

;; Nat [Nat -> X] -> [Tree X]
;; like build-list, but for complete binary trees
(define (build-tree i f) ;; i = 2^j-1
  (let rec ((i i) (o 0))
    (if (= 1 i) 
        (f o)
        (let ((i/2 (half i)))
          (make-node (f o)
                     (rec i/2 (add1 o))
                     (rec i/2 (+ 1 o i/2)))))))

;; Consumes n = 2^i-1 and produces 2^(i-1)-1.
;; Nat -> Nat
(define (half n)
  (bitwise-arithmetic-shift n -1))

;; Nat X -> [Tree X]
(define (tr:make-tree i x) ;; i = 2^j-1
  (let recr ((i i))
    (if (= 1 i) 
        x
        (let ((n (recr (half i))))
          (make-node x n n)))))

;; Nat [Tree X] Nat [X -> X] -> X [Tree X]
(define (tree-ref/update mid t i f)
  (cond ((zero? i)
         (if (node? t) 
             (values (node-val t)
                     (make-node (f (node-val t))
                                (node-left t)
                                (node-right t)))
             (values t (f t))))
        ((<= i mid)
         (let-values (((v* t*) (tree-ref/update (half (sub1 mid)) 
                                                (node-left t) 
                                                (sub1 i) 
                                                f)))
           (values v* (make-node (node-val t) t* (node-right t)))))
        (else           
         (let-values (((v* t*) (tree-ref/update (half (sub1 mid)) 
                                                (node-right t) 
                                                (sub1 (- i mid)) 
                                                f)))
           (values v* (make-node (node-val t) (node-left t) t*))))))

;; Special-cased above to avoid logarathmic amount of cons'ing
;; and any multi-values overhead.  Operates in constant space.
;; [Tree X] Nat Nat -> X
;; invariant: (= mid (half (sub1 (tree-count t))))
(define (tree-ref/a t i mid) 
  (cond ((zero? i) (tree-val t))
        ((<= i mid) 
         (tree-ref/a (node-left t) 
                     (sub1 i) 
                     (half (sub1 mid))))
        (else 
         (tree-ref/a (node-right t) 
                     (sub1 (- i mid)) 
                     (half (sub1 mid))))))

;; Nat [Tree X] Nat -> X
;; invariant: (= size (tree-count t))
(define (tree-ref size t i)
  (if (zero? i)
      (tree-val t)
      (tree-ref/a t i (half (sub1 size)))))

;; Nat [Tree X] Nat [X -> X] -> [Tree X]
(define (tree-update size t i f)
  (let recr ((mid (half (sub1 size))) (t t) (i i))
    (cond ((zero? i)
           (if (node? t)
               (make-node (f (node-val t))
                          (node-left t)
                          (node-right t))
               (f t)))
          ((<= i mid)
           (make-node (node-val t) 
                      (recr (half (sub1 mid))
                            (node-left t) 
                            (sub1 i)) 
                      (node-right t)))
          (else
           (make-node (node-val t) 
                      (node-left t) 
                      (recr (half (sub1 mid))
                            (node-right t) 
                            (sub1 (- i mid))))))))

;; ------------------------
;; Random access lists

;; [RaListof X]
(define ra:null (quote ()))

;; [Any -> Boolean]
(define ra:pair? kons?)

;; [Any -> Boolean]
(define ra:null? null?)

;; X [RaListof X] -> [RaListof X]  /\
;; X Y -> [RaPair X Y]
(define (ra:cons x ls)
  (if (kons? ls)
      (let ((s (kons-size ls)))
        (if (and (kons? (kons-rest ls))
                 (= (kons-size (kons-rest ls))
                    s))
            (make-kons (+ 1 s s) 
                       (make-node x 
                                  (kons-tree ls)
                                  (kons-tree (kons-rest ls)))
                       (kons-rest (kons-rest ls)))
            (make-kons 1 x ls)))
      (make-kons 1 x ls)))


;; [RaPair X Y] -> X Y
(define ra:car+cdr 
  (lambda (p)
    (assert (kons? p))
    (if (node? (kons-tree p))
        (let ((s* (half (kons-size p))))
          (values (tree-val (kons-tree p))
                  (make-kons s* 
                             (node-left (kons-tree p))
                             (make-kons s*
                                        (node-right (kons-tree p))
                                        (kons-rest p)))))
        (values (kons-tree p) (kons-rest p)))))

;; [RaPair X Y] -> X
(define (ra:car p)
  (call-with-values (lambda () (ra:car+cdr p))
                    (lambda (car cdr) car)))

;; [RaPair X Y] -> Y
(define (ra:cdr p)
  (call-with-values (lambda () (ra:car+cdr p))
                    (lambda (car cdr) cdr)))

;; [RaListof X] Nat [X -> X] -> X [RaListof X]
(define (ra:list-ref/update ls i f)
  ;(assert (< i (ra:length ls)))
  (let recr ((xs ls) (j i))
    (if (< j (kons-size xs))
        (let-values (((v* t*) 
                      (tree-ref/update (half (sub1 (kons-size xs))) 
                                       (kons-tree xs) j f)))
          (values v* (make-kons (kons-size xs) 
                                t* 
                                (kons-rest xs))))
        (let-values (((v* r*) 
                      (recr (kons-rest xs) 
                            (- j (kons-size xs)))))
          (values v* (make-kons (kons-size xs) 
                                (kons-tree xs) 
                                r*))))))

;; [RaListof X] Nat [X -> X] -> [RaListof X]
(define (ra:list-update ls i f)
  ;(assert (< i (ra:length ls)))
  (let recr ((xs ls) (j i))
    (let ((s (kons-size xs)))
      (if (< j s) 
          (make-kons s (tree-update s (kons-tree xs) j f) (kons-rest xs))
          (make-kons s (kons-tree xs) (recr (kons-rest xs) (- j s)))))))

;; [RaListof X] Nat X -> (values X [RaListof X])
(define (ra:list-ref/set ls i v)
  (ra:list-ref/update ls i (lambda (_) v)))

;; X ... -> [RaListof X]
(define (ra:list . xs)
  (fold-right ra:cons ra:null xs))

;; Nat X -> [RaListof X]
(define ra:make-list
  (case-lambda
   ((k) (ra:make-list k 0))
   ((k obj)
    (let loop ((n k) (a ra:null))
      (cond ((zero? n) a)
            (else 
             (let ((t (largest-skew-binary n)))
               (loop (- n t)
                     (make-kons t (tr:make-tree t obj) a)))))))))

;; A Skew is a Nat 2^k-1 with k > 0.

;; Skew -> Skew
(define (skew-succ t) (add1 (bitwise-arithmetic-shift t 1)))

;; Computes the largest skew binary term t <= n.
;; Nat -> Skew
(define (largest-skew-binary n)
  (if (= 1 n) 
      1
      (let* ((t (largest-skew-binary (half n)))
             (s (skew-succ t)))
        (if (> s n) t s))))  

;; [Any -> Boolean]
;; Is x a PROPER list?
(define (ra:list? x)
  (or (ra:null? x)
      (and (kons? x)
           (ra:list? (kons-rest x)))))

(define ra:caar (lambda (ls) (ra:car (ra:car ls))))
(define ra:cadr (lambda (ls) (ra:car (ra:cdr ls))))
(define ra:cddr (lambda (ls) (ra:cdr (ra:cdr ls))))
(define ra:cdar (lambda (ls) (ra:cdr (ra:car ls))))

(define ra:caaar (lambda (ls) (ra:car (ra:car (ra:car ls)))))
(define ra:caadr (lambda (ls) (ra:car (ra:car (ra:cdr ls)))))
(define ra:caddr (lambda (ls) (ra:car (ra:cdr (ra:cdr ls)))))
(define ra:cadar (lambda (ls) (ra:car (ra:cdr (ra:car ls)))))
(define ra:cdaar (lambda (ls) (ra:cdr (ra:car (ra:car ls)))))
(define ra:cdadr (lambda (ls) (ra:cdr (ra:car (ra:cdr ls)))))
(define ra:cdddr (lambda (ls) (ra:cdr (ra:cdr (ra:cdr ls)))))
(define ra:cddar (lambda (ls) (ra:cdr (ra:cdr (ra:car ls)))))

(define ra:caaaar (lambda (ls) (ra:car (ra:car (ra:car (ra:car ls))))))
(define ra:caaadr (lambda (ls) (ra:car (ra:car (ra:car (ra:cdr ls))))))
(define ra:caaddr (lambda (ls) (ra:car (ra:car (ra:cdr (ra:cdr ls))))))
(define ra:caadar (lambda (ls) (ra:car (ra:car (ra:cdr (ra:car ls))))))
(define ra:cadaar (lambda (ls) (ra:car (ra:cdr (ra:car (ra:car ls))))))
(define ra:cadadr (lambda (ls) (ra:car (ra:cdr (ra:car (ra:cdr ls))))))
(define ra:cadddr (lambda (ls) (ra:car (ra:cdr (ra:cdr (ra:cdr ls))))))
(define ra:caddar (lambda (ls) (ra:car (ra:cdr (ra:cdr (ra:car ls))))))
(define ra:cdaaar (lambda (ls) (ra:cdr (ra:car (ra:car (ra:car ls))))))
(define ra:cdaadr (lambda (ls) (ra:cdr (ra:car (ra:car (ra:cdr ls))))))
(define ra:cdaddr (lambda (ls) (ra:cdr (ra:car (ra:cdr (ra:cdr ls))))))
(define ra:cdadar (lambda (ls) (ra:cdr (ra:car (ra:cdr (ra:car ls))))))
(define ra:cddaar (lambda (ls) (ra:cdr (ra:cdr (ra:car (ra:car ls))))))
(define ra:cddadr (lambda (ls) (ra:cdr (ra:cdr (ra:car (ra:cdr ls))))))
(define ra:cddddr (lambda (ls) (ra:cdr (ra:cdr (ra:cdr (ra:cdr ls))))))
(define ra:cdddar (lambda (ls) (ra:cdr (ra:cdr (ra:cdr (ra:car ls))))))

;; [RaList X] -> Nat
(define (ra:length ls)
  (assert (ra:list? ls))
  (let recr ((ls ls))
    (if (kons? ls)
        (+ (kons-size ls) (recr (kons-rest ls)))
        0)))

(define (make-foldl empty? first rest)
  (letrec ((f (lambda (cons empty ls)
                (if (empty? ls) 
                    empty
                    (f cons
                       (cons (first ls) empty) 
                       (rest ls))))))
    f))

(define (make-foldr empty? first rest)
  (letrec ((f (lambda (cons empty ls)
                (if (empty? ls) 
                    empty
                    (cons (first ls)
                          (f cons empty (rest ls)))))))
    f))

;; [X Y -> Y] Y [RaListof X] -> Y
(define ra:foldl/1 (make-foldl ra:null? ra:car ra:cdr))
(define ra:foldr/1 (make-foldr ra:null? ra:car ra:cdr))

;; [RaListof X] ... -> [RaListof X]
(define (ra:append . lss)
  (cond ((null? lss) ra:null)
        (else (let recr ((lss lss))
                (cond ((null? (cdr lss)) (car lss))
                      (else (ra:foldr/1 ra:cons
                                        (recr (cdr lss))
                                        (car lss))))))))

;; [RaListof X] -> [RaListof X]
(define (ra:reverse ls)
  (ra:foldl/1 ra:cons ra:null ls))

;; [RaListof X] Nat -> [RaListof X]
(define (ra:list-tail ls i)
  (let loop ((xs ls) (j i))
    (cond ((zero? j) xs)
          (else (loop (ra:cdr xs) (sub1 j))))))

;; [RaListof X] Nat -> X
;; Special-cased above to avoid logarathmic amount of cons'ing
;; and any multi-values overhead.  Operates in constant space.
(define (ra:list-ref ls i)
  ;(assert (< i (ra:length ls)))
  (let loop ((xs ls) (j i))
    (if (< j (kons-size xs))
        (tree-ref (kons-size xs) (kons-tree xs) j)
        (loop (kons-rest xs) (- j (kons-size xs))))))

;; [RaListof X] Nat X -> [RaListof X]
(define (ra:list-set ls i v)
  (let-values (((_ l*) (ra:list-ref/set ls i v))) l*))

;; [X ... -> y] [RaListof X] ... -> [RaListof Y]
;; Takes advantage of the fact that map produces a list of equal size.
(define ra:map
  (case-lambda 
    ((f ls)
     (let recr ((ls ls))
       (if (kons? ls)
           (make-kons (kons-size ls) 
                      (tree-map f (kons-tree ls)) 
                      (recr (kons-rest ls)))
           ra:null)))
    ((f . lss)
     ;(check-nary-loop-args 'ra:map (lambda (x) x) f lss)
     (let recr ((lss lss))
       (cond ((ra:null? (car lss)) ra:null)
             (else
              ;; IMPROVE ME: make one pass over lss.
              (make-kons (kons-size (car lss))
                         (tree-map/n f (map kons-tree lss))
                         (recr (map kons-rest lss)))))))))


;; [X ... -> Y] [RaListof X] ... -> unspecified
(define ra:for-each
  (case-lambda 
    ((f ls)
     (when (kons? ls)
       (tree-for-each f (kons-tree ls))
       (ra:for-each f (kons-rest ls))))
    ((f . lss)
     ;(check-nary-loop-args 'ra:map (lambda (x) x) f lss)
     (let recr ((lss lss))
       (when (ra:pair? (car lss))
         (tree-map/n f (map kons-tree lss))
         (recr (map kons-rest lss)))))))

;; [RaListof X] -> [Listof X]
(define (ra:random-access-list->linear-access-list x)
  (ra:foldr/1 cons '() x))

;; [Listof X] -> [RaListof X]
(define (ra:linear-access-list->random-access-list x)
  (fold-right ra:cons '() x))

;; This code based on code written by Abdulaziz Ghuloum
;; http://ikarus-scheme.org/pipermail/ikarus-users/2009-September/000595.html
(define get-cached
  (let ((h (make-hash-table eq?)))
    (lambda (x)
      (define (f x)
        (cond
         ((pair? x) (ra:cons (f (car x)) (f (cdr x))))
         ((vector? x) (vector-map f x))
         (else x)))
      (cond
       ((not (or (pair? x) (vector? x))) x)
       ((hash-table-ref/default h x #f))
       (else
        (let ((v (f x)))
          (hash-table-set! h x v)
          v))))))

(define-syntax ra:quote
  (syntax-rules ()
    ((ra:quote datum) (get-cached 'datum)))) 
