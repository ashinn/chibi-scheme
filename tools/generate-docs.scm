(import
 (chibi)
 (chibi io)
 (chibi string)
 (chibi process)
 (chibi filesystem)
 (chibi pathname)
 (srfi 18))

(define (walk-directory path)
  #;(write (cons "visiting: " path))
  (apply append
   (map
     (lambda (filename)
       (define path2 (string-append path "/" filename))
       #;(write `(,path2))
       (cond
         ((equal? #\. (string-ref filename 0)) (list))
         ((file-directory? path2) (walk-directory path2))
         (else (list path2))))
     (directory-files path))))

(define (filter f xs)
 (apply append
  (map
   (lambda (x)
     (if (f x)
       (list x)
       (list)))
   xs)))

(define (filename-filter filename)
 (and
  (string-suffix? ".sld" filename)
  (not (string-contains filename "-test"))))


(define (process x)
  (define outfile
    (string-append
     "doc/"
     (substring x 0 (- (string-length x) 4))
     ".html"))
  (display `("Processing" ,x ,outfile))
  (newline)
  (let ((output (process->string `("./chibi-scheme" "tools/chibi-doc" "--html" ,x))))
    (create-directory* (path-directory outfile))
    (call-with-output-file
     outfile
     (lambda (port)
       (display output port)))))

(define (fork-map f xs)
  (if (pair? xs)
    (let* ((x (car xs))
           (thread (make-thread (lambda () (f x)))))
     (thread-start! thread)
     (fork-map f (cdr xs))
     (thread-join! thread))
    (begin)))

(fork-map
 process
 (filter filename-filter (walk-directory "lib")))

