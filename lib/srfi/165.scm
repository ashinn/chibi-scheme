;; Copyright (C) Marc Nieper-Wißkirchen (2019).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice (including
;; the next paragraph) shall be included in all copies or substantial
;; portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-record-type Computation-Environment-Variable
  (make-environment-variable name default immutable? id)
  environment-variable?
  (name environment-variable-name)
  (default environment-variable-default)
  (immutable? environment-variable-immutable?)
  (id environment-variable-id))

(define make-computation-environment-variable
  (let ((count 0))
    (lambda (name default immutable?)
      (set! count (+ count 1))
      (make-environment-variable name default immutable? (- count)))))

(define (predefined? var)
  (not (negative? (environment-variable-id var))))

(define variable-comparator
  (make-comparator environment-variable?
                   eq?
                   (lambda (x y)
                     (< (environment-variable-id x)
                        (environment-variable-id y)))
                   (lambda (x . y)
                     (environment-variable-id x))))

(define default-computation
  (make-computation-environment-variable 'default-computation #f #f))

(define (environment-global env)
  (vector-ref env 0))

(define (environment-local env)
  (vector-ref env 1))

(define (environment-set-global! env global)
  (vector-set! env 0 global))

(define (environment-set-local! env local)
  (vector-set! env 1 local))

(define (environment-cell-set! env var box)
  (vector-set! env (+ 2 (environment-variable-id var)) box))

(define (environment-cell env var)
  (vector-ref env (+ 2 (environment-variable-id var))))

(define-syntax define-computation-type
  (syntax-rules ()
    ((define-computation-type make-environment run var ...)
     (%define-computation-type make-environment run (var ...) 0 ()))))

(define-syntax %define-computation-type
  (syntax-rules ()
    ((_ make-environment run () n ((var default e immutable i) ...))
     (begin
       (define-values (e ...) (values default ...))
       (define var (make-environment-variable 'var e immutable i))
       ...
       (define (make-environment)
         (let ((env (make-vector (+ n 2))))
           (environment-set-global! env (hash-table variable-comparator))
           (environment-set-local! env (mapping variable-comparator))
           (vector-set! env (+ i 2) (box e))
           ...
           env))
       (define (run computation)
         (execute computation (make-environment)))))
    ((_ make-environment run ((v d) . v*) n (p ...))
     (%define-computation-type make-environment run v* (+ n 1) (p ... (v d e #f n))))
    ((_ make-environment run ((v d "immutable") . v*) n (p ...))
     (%define-computation-type make-environment run v* (+ n 1) (p ... (v d e #t n))))
    ((_ make-environment run (v . v*) n (p ...))
     (%define-computation-type make-environment run v* (+ n 1) (p ... (v #f e #f n))))))

(define-computation-type make-computation-environment computation-run)

(define (computation-environment-ref env var)
  (if (predefined? var)
      (unbox (environment-cell env var))
      (mapping-ref
       (environment-local env)
       var
       (lambda ()
         (hash-table-ref/default (environment-global env)
                                 var
                                 (environment-variable-default var)))
       unbox)))

(define (computation-environment-update env . arg*)
  (let ((new-env (vector-copy env)))
    (let loop ((arg* arg*)
               (local (environment-local env)))
      (if (null? arg*)
          (begin
            (environment-set-local! new-env local)
            new-env)
          (let ((var (car arg*))
                (val (cadr arg*)))
            (if (predefined? var)
                (begin
                  (environment-cell-set! new-env var (box val))
                  (loop (cddr arg*) local))
                (loop (cddr arg*) (mapping-set local var (box val)))))))))

(define (computation-environment-update! env var val)
  (if (predefined? var)
      (set-box! (environment-cell env var) val)
      (mapping-ref (environment-local env)
                   var
                   (lambda ()
                     (hash-table-set! (environment-global env) var val))
                   (lambda (cell)
                     (set-box! cell val)))))

(define (computation-environment-copy env)
  (let ((global (hash-table-copy (environment-global env) #t)))
    (mapping-for-each (lambda (var cell)
                        (hash-table-set! global var (unbox cell)))
                      (environment-local env))
    (let ((new-env (make-vector (vector-length env))))
      (environment-set-global! new-env global)
      (environment-set-local! new-env (mapping variable-comparator))
      (do ((i (- (vector-length env) 1) (- i 1)))
          ((< i 2)
           new-env)
        (vector-set! new-env i (box (unbox (vector-ref env i))))))))

(define (execute computation env)
  (let ((coerce (if (procedure? computation)
                    values
                    (or (computation-environment-ref env default-computation)
                        (error "not a computation" computation)))))
    ((coerce computation) env)))

(define (make-computation proc)
  (lambda (env)
    (proc (lambda (c) (execute c env)))))

(define (computation-pure . args)
  (make-computation
   (lambda (compute)
     (apply values args))))

(define (computation-each a . a*)
  (computation-each-in-list (cons a a*)))

(define (computation-each-in-list a*)
  (make-computation
   (lambda (compute)
     (let loop ((a (car a*)) (a* (cdr a*)))
       (if (null? a*)
           (compute a)
           (begin
             (compute a)
             (loop (car a*) (cdr a*))))))))

(define (computation-bind a . f*)
  (make-computation
   (lambda (compute)
     (let loop ((a a) (f* f*))
       (if (null? f*)
           (compute a)
           (loop (call-with-values
                     (lambda () (compute a))
                   (car f*))
                 (cdr f*)))))))

(define (computation-ask)
  (lambda (env)
    env))

(define (computation-local updater computation)
  (lambda (env)
    (computation (updater env))))

(define-syntax computation-fn
  (syntax-rules ()
    ((_ (clause ...) expr ... computation)
     (%fn (clause ...) () expr ... computation))))

(define-syntax %fn
  (syntax-rules ()
    ((_ () ((id var tmp) ...) expr ... computation)
     (let ((tmp var) ...)
       (computation-bind
        (computation-ask)
        (lambda (env)
          (let ((id (computation-environment-ref env tmp)) ...)
            expr ...
            computation)))))
    ((_ ((id var) . rest) (p ...) expr ... computation)
     (%fn rest (p ... (id var tmp)) expr ... computation))
    ((_ (id . rest) (p ...) expr ... computation)
     (%fn rest (p ... (id id tmp)) expr ... computation))))

(define-syntax computation-with
  (syntax-rules ()
    ((_ ((var val) ...) a* ... a)
     (%with ((var val) ...) () () a* ... a))))

(define-syntax %with
  (syntax-rules ()
    ((_ () ((x u) ...) ((a b) ...))
     (let ((u x) ... (b a) ...)
       (computation-local
        (lambda (env)
          (computation-environment-update env u ...) )
        (computation-each b ...))))
    ((_ ((var val) . rest) (p ...) () a* ...)
     (%with rest (p ... (var u) (val v)) () a* ...))
    ((_ () p* (q ...) a . a*)
     (%with () p* (q ... (a b)) . a*))))

(define-syntax computation-with!
  (syntax-rules ()
    ((_ (var val) ...)
     (%with! (var val) ... ()))))

(define-syntax %with!
  (syntax-rules ()
    ((_ ((var u val v) ...))
     (let ((u var) ... (v val) ...)
       (computation-bind
        (computation-ask)
        (lambda (env)
          (computation-environment-update! env u v) ...
          (computation-pure (if #f #f))))))
    ((_ (var val) r ... (p ...))
     (%with! r ... (p ... (var u val v))))))

(define (computation-forked a . a*)
  (make-computation
   (lambda (compute)
     (let loop ((a a) (a* a*))
       (if (null? a*)
           (compute a)
           (begin
             (compute (computation-local
                       (lambda (env)
                         (computation-environment-copy env))
                       a))
             (loop (car a*) (cdr a*))))))))

(define (computation-bind/forked computation . proc*)
  (apply computation-bind
         (computation-local computation-environment-copy computation)
         proc*))

(define (computation-sequence fmt*)
  (fold-right
   (lambda (fmt res)
     (computation-bind
      res
      (lambda (vals)
        (computation-bind
         fmt
         (lambda (val)
           (computation-pure (cons val vals)))))))
   (computation-pure '()) fmt*))
