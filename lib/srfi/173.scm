(define-record-type <hook>
  (%make-hook procs arity)
  hook?
  (procs hook-procs hook-procs!)
  (arity hook-arity hook-arity!))

(define (make-hook arity)
  (%make-hook '() arity))

(define (list->hook arity lst)
  (%make-hook lst arity))

(define (list->hook! hook lst)
  (hook-procs! hook lst))

(define (hook-add! hook proc)
  (let ((procs (hook-procs hook)))
    (hook-procs! hook (cons proc procs))))

(define (hook-delete! hook proc)
  (let loop ((procs (hook-procs hook))
             (out '()))
    (unless (null? procs)
      (if (eq? proc (car procs))
          (hook-procs! hook (append (cdr procs) out))
          (loop (cdr procs) (cons (car procs) out))))))

(define (hook-reset! hook)
  (hook-procs! hook '()))

(define (hook->list hook)
  (hook-procs hook))

(define (hook-run hook . args)
  (assume (= (length args) (hook-arity hook)))
  (for-each (lambda (proc) (apply proc args)) (hook-procs hook)))
