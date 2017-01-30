
(define (find-in-path file . o)
  (any (lambda (dir)
         (let ((path (make-path dir file)))
           (and (file-exists? path) path)))
       (if (pair? o)
           (car o)
           (string-split (get-environment-variable "PATH") #\:))))

(define (find-sexp-in-path file dirs . o)
  (let ((pred (if (pair? o) (car o) (lambda (x) #t))))
    (any (lambda (dir)
           (let ((path (make-path dir file)))
             (and (file-exists? path)
                  (guard (exn (else #f))
                    (let ((x (call-with-input-file path read)))
                      (and (pred x) x))))))
         dirs)))

(define (write-to-string x)
  (call-with-output-string (lambda (out) (write x out))))

(define (display-to-string x)
  (call-with-output-string
    (lambda (out)
      (if (bytevector? x) (write-bytevector x out) (display x out)))))

(define (resource->bytevector uri)
  (let ((uri (if (uri? uri) uri (string->path-uri 'http uri))))
    (if (uri-host uri)
        (call-with-input-url uri port->bytevector)
        (file->bytevector (uri-path uri)))))

;; path-normalize either a uri or path, and return the result as a string
(define (uri-normalize x)
  (cond
   ((uri? x)
    (uri->string (uri-with-path x (path-normalize (uri-path x)))))
   ((not (string? x))
    (error "not a uri" x))
   ((string->uri x)
    => uri-normalize)
   (else
    (path-normalize x))))

(define (uri-directory x)
  (cond
   ((uri? x)
    (uri->string (uri-with-path x (path-directory (uri-path x)))))
   ((not (string? x))
    (error "not a uri" x))
   ((string->uri x)
    => uri-directory)
   (else
    (path-directory x))))

(define (version-split str)
  (if str
      (map (lambda (x) (or (string->number x) x))
        (string-split str #\.))
      '()))

(define (version-compare a b)
  (define (less? x y)
    (cond ((number? x) (if (number? y) (< x y) 1))
          ((number? y) -1)
          (else (string<? x y))))
  (let lp ((as (version-split a))
           (bs (version-split b)))
    (cond
     ((null? as) (if (null? bs) 0 -1))
     ((null? bs) 1)
     ((less? (car as) (car bs)) -1)
     ((less? (car bs) (car as)) 1)
     (else (lp (cdr as) (cdr bs))))))

(define (version>? a b) (> (version-compare a b) 0))
(define (version>=? a b) (>= (version-compare a b) 0))

;; graph is a list of ((vertex dep-vertices ...) ...)
(define (topological-sort graph . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (cdr
     (let lp ((ls graph) (seen '()) (res '()))
       (if (null? ls)
           (cons seen res)
           (let ((x (caar ls)))
             (if (member x seen eq)
                 (lp (cdr ls) seen res)
                 (let lp2 ((ls2 (cdar ls))
                           (seen (cons x seen))
                           (res res))
                   (cond
                    ((null? ls2)
                     (lp (cdr ls) seen (cons x res)))
                    ((member (car ls2) seen eq)
                     (lp2 (cdr ls2) seen res))
                    ((assoc (car ls2) graph eq)
                     => (lambda (vertices)
                          (let ((tmp (lp (list vertices) seen res)))
                            (lp2 (cdr ls2) (car tmp) (cdr tmp)))))
                    (else
                     (lp2 (cdr ls2)
                          (cons (car ls2) seen)
                          (cons (car ls2) res))))))))))))
