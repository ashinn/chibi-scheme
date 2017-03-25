;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; binary records, simpler version with type-checking on set! removed

(define-syntax defrec
  (syntax-rules (make: pred: read: write: block:)
    ((defrec () n m p r w
       ((field-tmp field-read field-read-expr field-write field-write-expr field-get) ...)
       ((field getter . s) ...))
     (begin
       (define-record-type n (m field ...) p
         (field getter . s) ...)
       (define n 'n)  ; chicken define-record-type doesn't define the rtd
       (define r
         (let ((field-read field-read-expr) ...)
           (lambda (in)
             (let* ((field-tmp (field-read in)) ...)
               (m field ...)))))
       (define w
         (let ((field-write field-write-expr) ...)
           (lambda (x out)
             (field-write (field-get x) out) ...)))))
    ((defrec ((make: x) . rest) n m p r w b f)
     (defrec rest n x p r w b f))
    ((defrec ((pred: x) . rest) n m p r w b f)
     (defrec rest n m x r w b f))
    ((defrec ((read: x) . rest) n m p r w b f)
     (defrec rest n m p x w b f))
    ((defrec ((write: x) . rest) n m p r w b f)
     (defrec rest n m p r x b f))
    ((defrec ((block: (field (type . args) getter . s) . fields) . rest) n m p r w
       (b ...) (f ...))
     (defrec ((block: . fields) . rest) n m p r w
       (b ...
          (field read-tmp (type read: 'args) write-tmp (type write: 'args) getter))
       (f ...
          (field getter . s))))
    ((defrec ((block: (field . x)) . rest) n m p r w b f)
     (syntax-error "invalid field in block" (field . x)))
    ((defrec ((block: data . fields) . rest) n m p r w (b ...) f)
     (defrec ((block: . fields) . rest) n m p r w
       (b ...
          (tmp-data read-tmp (read-literal 'data) write-tmp (write-literal 'data) (lambda (x) x)))
       f))
    ((defrec ((block:) . rest) n m p r w b f)
     (defrec rest n m p r w b f))
    ))

(define-syntax define-binary-record-type
  (syntax-rules ()
    ((define-binary-record-type name x ...)
     (defrec (x ...) name hidden-make hidden-pred hidden-read hidden-write
       () ()))))
