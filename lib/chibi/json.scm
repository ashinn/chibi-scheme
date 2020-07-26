
;;> A library for reading and writing data in JSON format (RFC 8259).

;;> \procedure{(json-read [in])}
;;> Reads a JSON expression from port \var{in}.  Objects are
;;> represented as alists with symbol keys, arrays as Scheme vectors,
;;> null as the symbol \scheme{'null}, and strings, numbers and
;;> booleans as the corresponding Scheme types.

;;> \procedure{(string->json str)}
;;> Returns the JSON representation of \var{str} as from \scheme{json-read}.
;;>
;;> \example{
;;>   (string->json "{\\"mean\\": 2.2, \\"quartiles\\": [1, 2, 3, 4]}")
;;> }
(define (string->json str)
  (let* ((in (open-input-string str))
         (res (json-read in)))
    (close-input-port in)
    res))

;;> \procedure{(json-write json [out])}
;;> Writes a JSON representation of \var{obj} to port \var{out}, where
;;> \var{obj} should follow the same mappings as in \var{json-read}.

;;> \procedure{(json->string json)}
;;> Returns the string representation of \var{json} as from \scheme{json-write}.
(define (json->string json)
  (let ((out (open-output-string)))
    (json-write json out)
    (get-output-string out)))

(define (json-field-mapper rtd name spec strict?)
  (if (symbol? spec)
      (rtd-mutator rtd spec)
      (let ((setter (rtd-mutator rtd name))
            (mapper (make-json-mapper spec strict?)))
        (lambda (rec val)
          (setter rec (mapper val))))))

(define (make-json-mapper spec . o)
  (let ((strict? (and (pair? o) (car o))))
    (cond
     ((vector? spec)
      (if (= 1 (vector-length spec))
          (let ((elt-spec (make-json-mapper (vector-ref spec 0) strict?)))
            (lambda (x)
              (if (vector? x)
                  (vector-map elt-spec x)
                  (error "expected json array" x))))
          (lambda (x)
            (if (vector? x) x (error "expected json array" x)))))
     ((procedure? spec)
      (lambda (x)
        (if (spec x) x (error "json check failed" spec x))))
     ((rtd? spec)
      (make-json-mapper
       (cons spec (map (lambda (f) (cons f f))
                       (vector->list (rtd-all-field-names spec))))
       strict?))
     ((pair? spec)
      (if (rtd? (car spec))
          (let* ((rtd (car spec))
                 (make (make-constructor (type-name rtd) rtd))
                 (fields
                  (map (lambda (f)
                         (cons (car f)
                               (json-field-mapper rtd (car f) (cdr f) strict?)))
                       (cdr spec))))
            (lambda (x)
              (if (not (or (pair? x) (null? x)))
                  (error "expected json object" x)
                  (let ((res (make)))
                    (for-each
                     (lambda (y)
                       (cond
                        ((and (pair? y) (assq (car y) fields))
                         => (lambda (f) ((cdr f) res (cdr y))))
                        (strict?
                         (error "unknown field" (if (pair? y) (car y) y)))
                        (else
                         )))
                     x)
                    res))))
          (error "expected rtd in object spec" spec)))
     (else
      (error "unknown json reader spec" spec)))))

;;> Returns a procedure of one argument, an input port, which reads a
;;> JSON object according to the specification \var{spec}, which can
;;> be one of:
;;>
;;> \itemlist[
;;>   \item{a record type: reads a json object with field names
;;>     corresponding to the record names}
;;>   \item{a predicate: reads an arbitrary json object, and returns
;;>     that object if the predicate succeeds, or an error otherwise}
;;>   \item{a vector of one element: reads a json array of objects as
;;>     described by the vector element}
;;>   \item{a list: the car should be a record type, and the cdr
;;>     an alist of (field-name . spec).  The spec can be a symbol,
;;>     in which case it is the record field name (allowing aliasing),
;;>     otherwise it is a normal spec to read and set the corresponding
;;>     field}
;;> ]
;;>
;;> If \var{strict?} is specified and true, raises an error if any
;;> unknown field names are specified in an object.
;;>
;;> Examples:
;;>
;;> \example{
;;> (begin
;;>  (define-record-type Employee
;;>    (make-employee name id title department)
;;>    employee?
;;>    (name employee-name)
;;>    (id employee-id)
;;>    (title employee-title)
;;>    (department employee-department))
;;>  (define-record-type Team
;;>    (make-team name lead devs)
;;>    team?
;;>    (name team-name)
;;>    (lead team-lead)
;;>    (devs team-devs))
;;>  (define read-team
;;>    (make-json-reader
;;>     `(,Team
;;>       (lead . ,Employee)
;;>       (name . ,string?)
;;>       (devs . #(,Employee)))))
;;>  (define team
;;>   (read-team
;;>    (open-input-string
;;>     "{\\"name\\": \\"A-Team\\",
;;>       \\"lead\\": {\\"name\\": \\"Hannibal\\", \\"id\\": 321},
;;>       \\"devs\\": [{\\"name\\": \\"B.A.\\", \\"id\\": 7},
;;>                  {\\"name\\": \\"Murdock\\", \\"id\\": 13}]}")))
;;>  (cons (team-name team)
;;>        (map employee-name
;;>             (cons (team-lead team) (vector->list (team-devs team))))))
;;> }
(define (make-json-reader spec . o)
  (let* ((strict? (and (pair? o) (car o)))
         (proc (make-json-mapper spec strict?)))
    ;; TODO: update this to read directly without the intermediate
    ;; representation
    (lambda (in) (proc (json-read in)))))
