
(define (char-hex-digit? ch)
  (or (char-numeric? ch)
      (memv (char-downcase ch) '(#\a #\b #\c #\d #\e #\f))))

(define (char-octal-digit? ch)
  (and (char? ch) (char<=? #\0 ch #\7)))

(define (parse-assert-range proc lo hi)
  (if (or lo hi)
      (parse-assert proc (lambda (n)
                          (and (or (not lo) (<= lo n))
                               (or (not hi) (<= n hi)))))
      proc))

(define (parse-unsigned-integer . o)
  (let ((lo (and (pair? o) (car o)))
        (hi (and (pair? o) (pair? (cdr o)) (cadr o))))
    (parse-assert-range
     (parse-map (parse-token char-numeric?) string->number)
     lo hi)))

(define (parse-sign+)
  (parse-or (parse-char #\+) (parse-char #\-)))

(define (parse-sign)
  (parse-or (parse-sign+) parse-epsilon))

(define (parse-integer . o)
  (let ((lo (and (pair? o) (car o)))
        (hi (and (pair? o) (pair? (cdr o)) (cadr o))))
    (parse-assert-range
     (parse-map-substring
      (parse-seq (parse-sign) (parse-token char-numeric?)
                 ;; (parse-not (parse-or (parse-sign) (parse-char #\.)))
                 )
      string->number)
     lo hi)))

(define (parse-c-integer)
  (parse-or
   (parse-map (parse-seq (parse-string "0x") (parse-token char-hex-digit?))
              (lambda (x) (string->number (cadr x) 16)))
   (parse-map (parse-seq (parse-string "0") (parse-token char-octal-digit?))
              (lambda (x) (string->number (cadr x) 8)))
   (parse-integer)))

(define (parse-real)
  (parse-map-substring
   (parse-seq
    (parse-or
     (parse-seq (parse-sign) (parse-repeat+ (parse-char char-numeric?))
                (parse-optional
                 (parse-seq (parse-char #\.)
                            (parse-repeat (parse-char char-numeric?)))))
     (parse-seq (parse-sign) (parse-char #\.)
                (parse-repeat+ (parse-char char-numeric?))))
    (parse-optional
     (parse-seq (parse-char (lambda (ch) (eqv? #\e (char-downcase ch))))
                (parse-sign)
                (parse-repeat+ (parse-char char-numeric?)))))
   string->number))

(define (parse-imag)
  (parse-or (parse-char #\i) (parse-char #\I)))

(define (parse-complex)
  (parse-map-substring
   (parse-or
    (parse-seq (parse-real) (parse-sign+) (parse-real) (parse-imag))
    (parse-seq (parse-real) (parse-imag))
    (parse-real))
   string->number))

(define (parse-identifier . o)
  ;; Slightly more complicated than mapping parse-token because the
  ;; typical identifier syntax has different initial and subsequent
  ;; char-sets.
  (let* ((init?
          (if (pair? o)
              (car o)
              (lambda (ch) (or (eqv? #\_ ch) (char-alphabetic? ch)))))
         (init (parse-char init?))
         (subsequent
          (parse-char
           (if (and (pair? o) (pair? (cdr o)))
               (cadr o)
               (lambda (ch) (or (init? ch) (char-numeric? ch)))))))
    (lambda (source0 index0 sk0 fk0)
      (init
       source0
       index0
       (lambda (res source index fk2)
         (let lp ((s source) (i index))
           (subsequent
            s i (lambda (r s i fk) (lp s i))
            (lambda (s i r)
              (sk0 (string->symbol (parse-stream-substring source0 index0 s i))
                   s i fk0)))))
       fk0))))

(define (parse-delimited . o)
  (let ((delim (if (pair? o) (car o) #\"))
        (esc (if (and (pair? o) (pair? (cdr o))) (cadr o) #\\))
        (parse-esc (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                       (car (cddr o))
                       parse-anything)))
    (parse-map
     (parse-seq
      (parse-char delim)
      (parse-repeat
       (parse-or (parse-char
                  (lambda (ch)
                    (and (not (eqv? ch delim)) (not (eqv? ch esc)))))
                 (parse-map (parse-seq (parse-char esc)
                                       (if (eqv? delim esc)
                                           (parse-char esc)
                                           parse-esc))
                            cadr)))
      (parse-char delim))
     (lambda (res) (list->string (cadr res))))))

(define (parse-separated . o)
  (let* ((sep (if (pair? o) (car o) #\,))
         (o1 (if (pair? o) (cdr o) '()))
         (delim (if (pair? o1) (car o1) #\"))
         (o2 (if (pair? o1) (cdr o1) '()))
         (esc (if (pair? o2) (car o2) delim))
         (o3 (if (pair? o2) (cdr o2) '()))
         (ok?
          (if (pair? o3)
              (let ((pred (car o3)))
                (lambda (ch)
                  (and (not (eqv? ch delim))
                       (not (eqv? ch sep))
                       (pred ch))))
              (lambda (ch) (and (not (eqv? ch delim)) (not (eqv? ch sep))))))
         (parse-field
          (parse-or (parse-delimited delim esc)
                    (parse-map-substring
                     (parse-repeat+ (parse-char ok?))))))
    (parse-map
     (parse-seq parse-field
                (parse-repeat
                 (parse-map (parse-seq (parse-char sep) parse-field) cadr)))
     (lambda (res) (cons (car res) (cadr res))))))

(define (parse-records . o)
  (let* ((terms (if (pair? o) (car o) '("\r\n" "\n")))
         (terms (if (list? terms) terms (list terms)))
         (term-chars (apply append (map string->list terms)))
         (ok? (lambda (ch) (not (memv ch term-chars))))
         (o (if (pair? o) (cdr o) '()))
         (sep (if (pair? o) (car o) #\,))
         (o (if (pair? o) (cdr o) '()))
         (delim (if (pair? o) (car o) #\"))
         (o (if (pair? o) (cdr o) '()))
         (esc (if (pair? o) (car o) delim)))
    (parse-repeat
     (parse-map
      (parse-seq (parse-separated sep delim esc ok?)
                 (apply parse-or parse-end (map parse-string terms)))
      car))))

(define parse-space (parse-char char-whitespace?))

(define (op-value op) (car op))
(define (op-prec op) (cadr op))
(define (op-assoc op)
  (let ((tail (cddr op))) (if (pair? tail) (car tail) 'left)))
(define (op<? op1 op2)
  (or (< (op-prec op1) (op-prec op2))
      (and (= (op-prec op1) (op-prec op2))
           (eq? 'right (op-assoc op1)))))

;; rules are of the form ((op precedence [assoc=left]) ...)
;; ls is of the forms (expr [op expr] ...)
;; returns an sexp representation of the operator chain
(define (resolve-operator-precedence rules ls)
  (define (lookup op rules)
    (or (assoc op rules)
        (list op 0)))
  (define (join exprs ops)
    `((,(op-value (car ops)) ,(cadr exprs) ,(car exprs))
      ,@(cddr exprs)))
  (if (null? ls) (error "empty operator chain"))
  (let lp ((ls (cdr ls)) (exprs (list (car ls))) (ops '((#f -1))))
    ;; ls: trailing operations ([op expr] ...)
    ;; exprs: list of expressions (expr expr ...)
    ;; ops: operator chain, same len as exprs ((op prec [assoc]) ...)
    (cond
     ((and (null? ls) (null? (cdr exprs)))
      (car exprs))
     ((null? ls)
      (lp ls (join exprs ops) (cdr ops)))
     ((null? (cdr ls))
      (error "unbalanced expression" ls))
     (else
      (let ((op (lookup (car ls) rules))
            (expr (cadr ls)))
        (if (or (null? (cdr ops)) (op<? op (car ops)))
            (lp (cddr ls) (cons expr exprs) (cons op ops))
            (lp ls (join exprs ops) (cdr ops))))))))

(define (parse-binary-op op rules expr . o)
  (let* ((ws (if (pair? o) (car o) (parse-repeat parse-space)))
         (ws-right (if (and (pair? o) (pair? (cdr o))) (cadr o) ws)))
    (parse-map
     (parse-seq ws expr (parse-repeat (parse-seq ws-right op ws expr)))
     (lambda (x)
       (resolve-operator-precedence
        rules
        (cons (cadr x)
              (apply append
                     (map (lambda (y) (list (cadr y) (cadr (cddr y))))
                          (car (cddr x))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-ipv4-digit (parse-integer 0 255))

(define parse-ipv4-address
  (parse-map-substring
   (parse-seq parse-ipv4-digit
              (parse-repeat (parse-seq (parse-char #\.) parse-ipv4-digit)
                            3 3))))

(define parse-ipv6-digit
  (parse-repeat (parse-char char-hex-digit?) 0 4))

(define parse-ipv6-address
  (parse-map-substring
   (parse-seq
    parse-ipv6-digit
    (parse-repeat (parse-seq (parse-repeat (parse-char #\:) 1 2)
                             parse-ipv6-digit)
                  1 7))))

(define parse-ip-address
  (parse-or parse-ipv4-address parse-ipv6-address))

(define parse-domain-atom
  (parse-token
   (lambda (ch)
     (or (char-alphabetic? ch) (char-numeric? ch) (memv ch '(#\- #\_))))))

(define (parse-domain)
  (parse-map-substring
   (parse-or
    parse-ip-address
    (parse-seq (parse-repeat (parse-seq parse-domain-atom (parse-char #\.)))
               parse-domain-atom))))

(define parse-top-level-domain
  (apply parse-or
         (parse-repeat (parse-char char-alphabetic?) 2 2)
         (map parse-string
              '("arpa" "com" "gov" "mil" "net" "org" "aero" "biz" "coop"
                "info" "museum" "name" "pro"))))

(define (parse-common-domain)
  (parse-map-substring
   (parse-seq (parse-repeat+ (parse-seq parse-domain-atom (parse-char #\.)))
              parse-top-level-domain)))

(define parse-email-local-part
  (parse-token
   (lambda (ch)
     (or (char-alphabetic? ch)
         (char-numeric? ch)
         (memv ch '(#\- #\_ #\. #\+))))))

(define (parse-email)
  ;; no quoted local parts or bang paths
  (parse-seq parse-email-local-part
             (parse-ignore (parse-char #\@))
             (parse-domain)))

(define (char-url-fragment? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)
      (memv ch '(#\_ #\- #\+ #\\ #\= #\~ #\&))))

(define (char-url? ch)
  (or (char-url-fragment? ch) (memv ch '(#\. #\, #\;))))

(define (parse-url-char pred)
  (parse-or (parse-char pred)
            (parse-seq (parse-char #\%)
                       (parse-repeat (parse-char char-hex-digit?) 2 2))))

(define (parse-uri)
  (parse-seq
   (parse-identifier)
   (parse-ignore
    (parse-seq (parse-char #\:) (parse-repeat (parse-char #\/))))
   (parse-domain)
   (parse-optional (parse-map (parse-seq (parse-char #\:)
                                         (parse-integer 0 65536))
                              cadr))
   (parse-optional
    (parse-map-substring
     (parse-seq (parse-char #\/)
                (parse-repeat (parse-url-char char-url?)))))
   (parse-optional
    (parse-map
     (parse-seq (parse-ignore (parse-char #\?))
                (parse-map-substring
                 (parse-repeat (parse-url-char char-url?))))
     car))
   (parse-optional
    (parse-map
     (parse-seq (parse-ignore (parse-char #\#))
                (parse-map-substring
                 (parse-repeat (parse-url-char char-url-fragment?))))
     car))))
