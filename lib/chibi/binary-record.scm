
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; binary records

(define-syntax defrec
  (syntax-rules (make: pred: read: write: block:)
    ((defrec () n m p r w
       ((field-tmp field-read field-read-expr field-write field-write-expr field-get) ...)
       ((field getter . s) ...)
       ((def setter val) ...))
     (begin
       (define-record-type n (m field ...) p
         (field getter . s) ...)
       (define r
         (let ((field-read field-read-expr) ...)
           (lambda (in)
             (let* ((field-tmp (field-read in)) ...)
               (m field ...)))))
       (define w
         (let ((field-write field-write-expr) ...)
           (lambda (x out)
             (field-write (field-get x) out) ...)))
       (def setter val) ...)
     ;; workaround for impls which strip hygiene from top-level defs
     ;; for some reason, works in chicken but not across libraries
     ;;
     ;; (begin
     ;;   (define-values (n m p getter ... setter ...)
     ;;     (let ()
     ;;       (define-record-type n (m field ...) p
     ;;         (field getter . s) ...)
     ;;       (def setter val) ...
     ;;       (values (record-rtd n) m p getter ... setter ...)))
     ;;   (define r
     ;;     (let ((field-read field-read-expr) ...)
     ;;       (lambda (in)
     ;;         (let* ((field-tmp (field-read in)) ...)
     ;;           (m field ...)))))
     ;;   (define w
     ;;     (let ((field-write field-write-expr) ...)
     ;;       (lambda (x out)
     ;;         (field-write (field-get x) out) ...))))
     )
    ((defrec ((make: x) . rest) n m p r w b f s)
     (defrec rest n x p r w b f s))
    ((defrec ((pred: x) . rest) n m p r w b f s)
     (defrec rest n m x r w b f s))
    ((defrec ((read: x) . rest) n m p r w b f s)
     (defrec rest n m p x w b f s))
    ((defrec ((write: x) . rest) n m p r w b f s)
     (defrec rest n m p r x b f s))
    ((defrec ((block: (field (type . args) getter setter) . fields) . rest) n m p r w b f s)
     (defrec ((block: (field (type . args) getter setter tmp-setter) . fields) . rest) n m p r w b f s))
    ((defrec ((block: (field (type . args) getter setter tmp-setter) . fields) . rest) n m p r w
       (b ...) (f ...) (s ...))
     (defrec ((block: . fields) . rest) n m p r w
       (b ...
          (field read-tmp (type read: 'args) write-tmp (type write: 'args) getter))
       (f ...
          (field getter tmp-setter))
       (s ...
          (define setter
            (let ((pred? (type pred: 'args)))
              (lambda (x val)
                (if (not (pred? val))
                    (error "invalid val for" 'field val))
                (tmp-setter x val)))))))
    ((defrec ((block: (field (type . args) getter) . fields) . rest) n m p r w
       (b ...) (f ...) s)
     (defrec ((block: . fields) . rest) n m p r w
       (b ...
          (field read-tmp (type read: 'args) write-tmp (type write: 'args) getter))
       (f ...
          (field getter))
       s))
    ((defrec ((block: (field . x)) . rest) n m p r w b f s)
     (syntax-error "invalid field in block" (field . x)))
    ((defrec ((block: data . fields) . rest) n m p r w (b ...) f s)
     (defrec ((block: . fields) . rest) n m p r w
       (b ...
          (tmp-data read-tmp (read-literal 'data) write-tmp (write-literal 'data) (lambda (x) x)))
       f
       s))
    ((defrec ((block:) . rest) n m p r w b f s)
     (defrec rest n m p r w b f s))
    ))

(define-syntax define-binary-record-type
  (syntax-rules ()
    ((define-binary-record-type name x ...)
     (defrec (x ...) name hidden-make hidden-pred hidden-read hidden-write
       () () ()))))
