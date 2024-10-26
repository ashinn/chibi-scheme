(define-record-type Simple-Condition
  (make-simple-condition)
  simple-condition?)

(define-record-type Compound-Condition
  (%make-compound-condition components)
  compound-condition?
  (components compound-condition-components))

(define (make-condition-type id parent field-names)
  (make-rtd id
            (list->vector
             (map
              (lambda (field-name)
                (list 'immutable field-name))
              field-names))
            parent))

(define (condition? obj)
  (or (simple-condition? obj)
      (compound-condition? obj)))

(define (condition-type? obj)
  (condition-subtype? obj Simple-Condition))

(define (condition-subtype? maybe-child-ct maybe-parent-ct)
  (and (rtd? maybe-child-ct)
       (or (eqv? maybe-child-ct maybe-parent-ct)
           (condition-subtype? (rtd-parent maybe-child-ct)
                               maybe-parent-ct))))

(define (condition-type-ancestors ct)
  (unfold (lambda (a) (not (condition-type? a)))
          (lambda (a) a)
          (lambda (a) (rtd-parent a))
          ct))

(define (condition-type-common-ancestor ct_1 ct_2)
  (let ((ct_1-as (condition-type-ancestors ct_1))
        (ct_2-as (condition-type-ancestors ct_2)))
    (find (lambda (a)
            (memv a ct_2-as))
          ct_1-as)))

(define (make-condition ct . plist)
  (define *undef* (cons '*undef* '()))
  (let* ((field-names (rtd-all-field-names ct))
         (field-values (make-vector (vector-length field-names) *undef*)))
    (let loop ((property plist))
      (if (null? property)
          (cond ((vector-any (lambda (name value)
                               (and (eq? value *undef*) name))
                             field-names
                             field-values)
                 => (lambda (undef-field-name)
                      (error "make-condition: value not given for field"
                             undef-field-name
                             ct)))
                (else
                 (apply (rtd-constructor ct) (vector->list field-values))))
          (let ((idx (vector-index (lambda (x) (eq? x (car property)))
                                   field-names)))
            (if idx
                (begin
                  (vector-set! field-values idx (cadr property))
                  (loop (cddr property)))
                (error "make-condition: unknown field" (car property))))))))

(define (make-compound-condition . cs)
  (if (= (length cs) 1)
      (car cs)
      ;; SRFI 35 requires at least one component, but R6RS doesn’t;
      ;; defer to R6RS’s less strict error checking (!)
      (%make-compound-condition
       (append-map
        (lambda (c)
          (if (simple-condition? c)
              (list c)
              (compound-condition-components c)))
        cs))))

(define (condition-has-type? c ct)
  (if (simple-condition? c)
      (is-a? c ct)
      (any
       (lambda (comp) (condition-has-type? comp ct))
       (compound-condition-components c))))

(define (condition-ref c field-name)
  (if (simple-condition? c)
      ((rtd-accessor (record-rtd c) field-name) c)
      (condition-ref
       (find
        (lambda (comp)
          (find field-name
                (vector->list
                 (rtd-all-field-names (record-rtd c)))))
        (compound-condition-components c))
       field-name)))

(define (simple-conditions c)
  (if (simple-condition? c)
      (list c)
      (compound-condition-components c)))

(define (extract-condition c ct)
  (if (and (simple-condition? c)
           (condition-has-type? c ct))
      c
      (find
       (lambda (comp)
         (condition-has-type? comp ct))
       (compound-condition-components ct))))

(define (condition-predicate ct)
  (lambda (obj)
    (and (condition? obj)
         (condition-has-type? obj ct))))
(define (condition-accessor ct proc)
  (lambda (c)
    (cond ((and (simple-condition? c)
                (condition-has-type? c ct))
           (proc c))
          ((find (lambda (comp) (condition-has-type? comp ct))
                 (compound-condition-components c))
           => (lambda (comp)
                (proc comp)))
          (else (error "condition-accessor: condition does not have the right type"
                       c ct)))))

(define-syntax define-condition-type/constructor
  (er-macro-transformer
   (lambda (expr rename compare)
     (let* ((name (list-ref expr 1))
            (parent (list-ref expr 2))
            (constructor (list-ref expr 3))
            (predicate (list-ref expr 4))
            (field-specs (drop expr 5))
            (field-names (map first field-specs))
            (field-accessors (map second field-specs)))
       (define _begin (rename 'begin))
       (define _define (rename 'define))
       (define _make-condition-type (rename 'make-condition-type))
       (define _compound-condition? (rename 'compound-condition?))
       (define _condition-predicate (rename 'condition-predicate))
       (define _condition-accessor (rename 'condition-accessor))
       (define _rtd-constructor (rename 'rtd-constructor))
       (define _rtd-accessor (rename 'rtd-accessor))
       (define _and (rename 'and))
       (define _if (rename 'if))
       (define _ct (rename 'ct))
       (define _x (rename 'x))
       `(,_begin
         (,_define ,_ct
           (,_make-condition-type ',name
                                  ,parent
                                  ',field-names))
         (,_define ,name ,_ct)
         (,_define ,constructor (,_rtd-constructor ,_ct))
         (,_define ,predicate (,_condition-predicate ,_ct))
         ,@(map
            (lambda (field-name field-accessor)
              `(,_define ,field-accessor
                 (,_condition-accessor
                  ,_ct
                  (,_rtd-accessor ,_ct ',field-name))))
            field-names
            field-accessors))))))

(define-syntax define-condition-type
  (syntax-rules ()
    ((_ name parent predicate (field-name field-accessor) ...)
     (define-condition-type/constructor
       name parent blah-ignored predicate
       (field-name field-accessor) ...))))

(define (%condition . specs)
  (define (find-common-field-spec ct name)
    (let loop ((more-specs specs))
      (if (null? more-specs)
          #f
          (let* ((other-ct (caar more-specs))
                 (field-specs (cdar more-specs))
                 (a (condition-type-common-ancestor ct other-ct)))
            (cond ((and (vector-index
                         (lambda (n)
                           (eq? n name))
                         (rtd-all-field-names a))
                        (assq name field-specs)))
                  (else (loop (cdr more-specs))))))))
  (let loop ((more-specs specs)
             (components '()))
    (if (null? more-specs)
        (apply make-compound-condition (reverse components))
        (let* ((this-spec (car more-specs))
               (ct (car this-spec))
               (field-specs (cdr this-spec))
               (field-names (rtd-all-field-names ct))
               (field-values
                (vector-map
                 (lambda (field-name)
                   (cond ((assq field-name field-specs) => cdr)
                         ((find-common-field-spec ct field-name) => cdr)
                         (else
                          (error "condition: value not given for field"
                                 field-name
                                 ct))))
                 field-names)))
          (loop
           (cdr more-specs)
           (cons
            (apply (rtd-constructor ct) (vector->list field-values))
            components))))))
(define-syntax condition
  (syntax-rules ()
    ((_ (ct (field-name field-value) ...) ...)
     (%condition (list ct (cons 'field-name field-value) ...) ...))))

(define &condition Simple-Condition)

(define-condition-type/constructor &message &condition
  make-message-condition message-condition?
  (message condition-message))

(define-condition-type/constructor &serious &condition
  make-serious-condition serious-condition?)

(define-condition-type/constructor &error &serious
  make-error error?)

