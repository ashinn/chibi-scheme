(define (write-to-string x)
  (call-with-output-string (lambda (out) (write x out))))

(define known-implementations
  `((chibi "chibi-scheme" (chibi-scheme -V) "0.7.3"
           ,(delay
              (process->sexp
               '(chibi-scheme -p "(features)"))))
    (chicken "chicken" (csi -release) "4.9.0"
             ;; work around Chicken's write() not quoting 64bit and
             ;; 32bit properly
             ,(delay
                (process->sexp
                 `(csi -R r7rs -R srfi-1 -e ,(write-to-string
                                              '(write
                                                (filter (lambda (x)
                                                          (not (or (eq? x '|64bit|)
                                                                   (eq? x '|32bit|))))
                                                        (features))))))))
    (cyclone "cyclone" (icyc -vn) "0.5.3"
             ,(delay
                (process->sexp
                 '(icyc -p "(features)"))))
    (foment "foment" #f #f
            ,(delay
               (process->sexp
                '(foment -e "(write (features))"))))
    (gauche "gosh" (gosh -E "print (gauche-version)") "0.9.4"
            ,(delay
               (process->sexp
                '(gosh -uscheme.base -e "(write (features))"))))
    (kawa "kawa" (kawa --version) "2.0"
          ,(delay
             (process->sexp
              '(kawa -e "(write (features))"))))
    (larceny "larceny" (larceny --version) "v0.98"
             ,(delay '()))
    (sagittarius "sagittarius" #f #f
                 ,(delay
                    (process->sexp
                     '(sagittarius -I "(scheme base)" -e "(write (features))"))))))

(define (impl->version impl cmd)
  (let* ((lines (process->string-list cmd))
         (line (and (pair? lines) (string-split (car lines)))))
    (and (pair? line)
         (if (and (pair? (cdr line))
                  (let ((x (string-downcase (car line)))
                        (name (symbol->string impl)))
                    (or (equal? x name)
                        (equal? x (string-append name "-scheme")))))
             (cadr line)
             (car line)))))

(define (target-is-host? impl)
  (case impl
    ((chibi) (cond-expand (chibi #t) (else #f)))
    ((gauche) (cond-expand (gauche #t) (else #f)))
    ((sagittarius) (cond-expand (sagittarius #t) (else #f)))
    (else #f)))

(define (impl->features impl)
  (cond
   ((target-is-host? impl)
    (features))
   ((assq impl known-implementations)
    => (lambda (impl)
         (force (fifth impl))))
   (else '())))

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

(define (display-to-string x)
  (call-with-output-string
    (lambda (out)
      (if (bytevector? x) (write-bytevector x out) (display x out)))))

(define (resource->bytevector cfg uri)
  (let ((uri (if (uri? uri) uri (string->path-uri 'http uri))))
    (if (uri-host uri)
        (if (conf-get cfg 'use-curl?)
            (process->bytevector `(curl --silent ,(uri->string uri)))
            (call-with-input-url uri port->bytevector))
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
        (string-split str (string->char-set "._")))
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
