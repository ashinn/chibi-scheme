(import
 (chibi)
 (chibi io)
 (chibi string)
 (chibi process)
 (chibi filesystem)
 (chibi pathname)
 (srfi 98))

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

(define (char-replacer from to)
  (lambda (c)
    (if (eq? from c) to c)))

(define (process x)
  (define strlen-ext (string-length ".sdl"))
  (define outfile
    (string-append
     "doc/"
     (substring x 0 (- (string-length x) strlen-ext))
     ".html"))
  (define doc-mod-name (string-map (char-replacer #\/ #\.) (substring x (string-length "lib/") (- (string-length x) strlen-ext))))
  (let ((output (process->string `("./chibi-scheme" "tools/chibi-doc" "--html" ,doc-mod-name))))
    (create-directory* (path-directory outfile))
    (call-with-output-file
     outfile
     (lambda (port)
       (display output port))))
  (display `("Processed" ,doc-mod-name ,x ,outfile))
  (newline))

(define (fork-map f xs)
  (if (pair? xs)
    (let ((pid (fork)))
      (if (= 0 pid)
       (begin
        (f (car xs))
        (exit))
       (begin
        (fork-map f (cdr xs))
        (waitpid pid 0))))))

; fork-map is naive
; BUG: some files like chibi.show are broken

(define (main)
  (define CHIBI_MODULE_PATH (get-environment-variable "CHIBI_MODULE_PATH"))
  (cond
   ((not CHIBI_MODULE_PATH) (begin
                             (display "USAGE: CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH=lib ./chibi-scheme tools/generate-docs.scm")
                             (newline)))
   (else
    (fork-map
     process
     (filter filename-filter (walk-directory CHIBI_MODULE_PATH))))))

(main)
