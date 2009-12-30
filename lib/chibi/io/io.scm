
(define eof
  (call-with-input-string " "
    (lambda (in) (read-char in) (read-char in))))

(define (write-line str . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (display str out)
    (newline out)))

(define (read-line . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (n (if (and (pair? o) (pair? (cdr o))) (car (cdr o)) 8192)))
    (let ((res (%read-line n in)))
      (if (not res) eof res))))

(define (read-string n . o)
  (let ((in (if (pair? o) (car o) (current-input-port))))
    (let ((res (%read-string n in)))
      (if (if (pair? res) (= 0 (car res)) #t)
          eof
          (cadr res)))))

(define (port-fold kons knil . o)
  (let ((read (if (pair? o) (car o) read))
        (in (if (and (pair? o) (pair? (cdr o)))
                (car (cdr o))
                (current-input-port))))
    (let lp ((acc knil))
      (let ((x (read in)))
        (if (eof-object? x) acc (lp (kons x acc)))))))

(define (port-fold-right kons knil . o)
  (let ((read (if (pair? o) (car o) read))
        (in (if (and (pair? o) (pair? (cdr o)))
                (car (cdr o))
                (current-input-port))))
    (let lp ()
      (let ((x (read in)))
        (if (eof-object? x) knil (kons x (lp)))))))

(define (port-map fn . o)
  (reverse (apply port-fold (lambda (x ls) (cons (fn x) ls)) '() o)))

(define (port->list read in)
  (port-map (lambda (x) x) read in))

(define (port->sexp-list in)
  (port->list read in))

(define (port->string-list in)
  (port->list read-line in))

(define (port->string in)
  (string-concatenate (port->list (lambda (in) (read-string 1024 in)) in)))
