
(define-syntax let-optionals
  (syntax-rules ()
    ((let-optionals ("step") ls (vars ...) ((v d) . rest) . body)
     (let-optionals ("step") ls (vars ... (v tmp d)) rest . body))
    ((let-optionals ("step") ls ((var tmp default) ...) rest . body)
     (let-optionals* ls ((tmp default) ... . rest)
       (let ((var tmp) ...) . body)))
    ((let-optionals ls vars . body)
     (let-optionals ("step") ls () vars . body))))

(define-syntax opt-lambda
  (syntax-rules ()
    ((opt-lambda vars . body)
     (lambda args (let-optionals args vars . body)))))
