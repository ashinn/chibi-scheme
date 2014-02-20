
;; Table mapping traced procedures to their original untraced values.
(define all-traces
  (make-parameter (make-hash-table eq?)))

;; The current number of traced procedure frames on the stack.
(define active-trace-depth
  (make-parameter 0))

(define (show-trace cell args)
  (let ((out (current-error-port)))
    (do ((i 0 (+ i 1)))
        ((>= i (active-trace-depth)))
      (display "| " out))
    (display "> " out)
    (write/ss (cons (car cell) args) out)
    (newline out)))

(define (show-trace-result cell args res)
  (let ((out (current-error-port)))
    (do ((i 0 (+ i 1)))
        ((>= i (active-trace-depth)))
      (display "| " out))
    (write/ss res out)
    (newline out)))

(define (make-tracer cell)
  (let ((proc (cdr cell)))
    (lambda args
      (show-trace cell args)
      (active-trace-depth (+ (active-trace-depth) 1))
      (let ((res (apply proc args)))
        (active-trace-depth (- (active-trace-depth) 1))
        (show-trace-result cell args res)
        res))))

(define-syntax trace
  (syntax-rules ()
    ((trace id)
     (trace-cell (env-cell (interaction-environment) 'id)))))

(define-syntax untrace
  (syntax-rules ()
    ((untrace id)
     (untrace-cell (env-cell (interaction-environment) 'id)))))

(define (warn . args)
  (let ((out (current-error-port)))
    (display "WARNING: " out)
    (for-each (lambda (x) (display x out)) args)
    (newline out)))

(define (trace-cell cell)
  (let ((tab (all-traces)))
    (cond
     ((not (pair? cell))
      (warn "No such binding."))
     ((hash-table-exists? tab cell)
      (warn "Procedure already being traced: " (car cell)))
     (else
      (hash-table-set! tab cell (cdr cell))
      (set-cdr! cell (make-tracer cell))))))

(define (untrace-cell cell)
  (let ((tab (all-traces)))
    (cond
     ((not (pair? cell))
      (warn "No such binding."))
     ((not (hash-table-exists? tab cell))
      (warn "Procedure not being traced: " (car cell)))
     (else
      (let ((proc (hash-table-ref tab cell)))
        (hash-table-delete! tab cell)
        (set-cdr! cell proc))))))

(define (untrace-all)
  (hash-table-walk (all-traces) (lambda (cell proc) (set-cdr! cell proc)))
  (all-traces (make-hash-table eq?)))
