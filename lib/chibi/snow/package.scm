
;; general utils

(define (read-from-string str)
  (call-with-input-string str read))

(define (display-to-string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        (else (call-with-output-string (lambda (out) (display x out))))))

(define (maybe-parse-hex x)
  (if (string? x) (string->number x 16) x))

;; rsa key utils

(define (lookup-digest name)
  (case name
    ((md5) md5)
    ((sha-224) sha-224)
    ((sha-256) sha-256)
    (else (error "unknown digest: " name))))

(define (rsa-identity=? email)
  (lambda (x)
    (cond ((not email) #f)
          ((assoc-get x 'email eq?)
           => (lambda (e) (string-ci=? email e)))
          (else #f))))

(define (extract-rsa-key ls name)
  (define (hex x)
    (if (integer? x) x (string->number x 16)))
  (cond
   ((assq name ls)
    => (lambda (x)
         (let ((bits (assoc-get ls 'bits))
               (modulus (assoc-get (cdr x) 'modulus))
               (exponent (assoc-get (cdr x) 'exponent)))
           (and bits modulus exponent
                (if (eq? name 'private-key)
                    (make-rsa-key (hex bits) (hex modulus) #f (hex exponent))
                    (make-rsa-key (hex bits) (hex modulus)
                                  (hex exponent) #f))))))
   (else #f)))

(define (extract-rsa-private-key ls)
  (extract-rsa-key ls 'private-key))

(define (extract-rsa-public-key ls)
  (extract-rsa-key ls 'public-key))

;; repositories

(define (repo-url repo)
  (and (pair? repo) (assoc-get (cdr repo) 'url eq?)))

(define (repo-find-publisher repo email)
  (find (rsa-identity=? email)
        (filter (lambda (x) (and (pair? x) (eq? 'publisher (car x))))
                (cdr repo))))

;; packages

(define (package? x)
  (and (pair? x) (eq? 'package (car x)) (every pair? (cdr x))))

(define (package-name package)
  (and (pair? package)
       (eq? 'package (car package))
       (or (assoc-get (cdr package) 'name)
           ;; TODO: longest common prefix
           (let ((lib (assq 'library (cdr package))))
             (and lib (library-name lib))))))

(define (package-email pkg)
  (and (pair? pkg)
       (let ((sig (assq 'signature (cdr pkg))))
         (and (pair? sig)
              (assoc-get (cdr sig) 'email eq?)))))

(define (package-url repo pkg)
  (let ((url (and (pair? pkg) (assoc-get (cdr pkg) 'url eq?))))
    (if (and url (uri-has-scheme? url))
        url
        (uri-with-path (string->path-uri 'http (repo-url repo)) url))))

(define (package-version pkg)
  (and (pair? pkg) (assoc-get (cdr pkg) 'version eq?)))

(define (package-digest-mismatches cfg pkg raw)
  (let ((size (assoc-get (cdr pkg) 'size))
        (actual-size (bytevector-length raw)))
    (if (and (integer? size) (not (= size actual-size)))
        `(size: expected: ,size actual: ,actual-size)
        (let* ((digest-name (assoc-get (cdr pkg) 'digest #f 'sha-256))
               (digest (assoc-get (cdr pkg) digest-name))
               (actual-digest ((lookup-digest digest-name) raw)))
          (and digest
               (not (equal? digest actual-digest))
               `(digest: ,digest-name expected: ,digest
                         actual: ,actual-digest))))))

(define (package-digest-ok? cfg pkg raw)
  (not (package-digest-mismatches cfg pkg raw)))

(define (package-signature-mismatches repo cfg pkg raw)
  (let* ((sig-spec (assoc-get-list (cdr pkg) 'signature))
         (digest-name (assoc-get sig-spec 'digest #f 'sha-256))
         (digest (assoc-get sig-spec digest-name))
         (sig (assoc-get sig-spec 'rsa))
         (email (assoc-get sig-spec 'email))
         (rsa-key-sexp (repo-find-publisher repo email))
         (rsa-key (and (pair? rsa-key-sexp)
                       (extract-rsa-public-key (cdr rsa-key-sexp)))))
    (cond
     ((not email)
      `(sign: missing-email ,sig-spec))
     ((not rsa-key)
      `(sign: unknown-publisher: ,email))
     ((not (rsa-verify? rsa-key
                        (maybe-parse-hex digest)
                        (maybe-parse-hex sig)))
      `(sign: rsa-signature-invalid: digest: ,digest sig: ,sig
              actual: ,(rsa-encrypt rsa-key digest)))
     (else #f))))

(define (package-signature-ok? cfg pkg raw)
  (not (package-signature-mismatches cfg pkg raw)))

(define (package-libraries package)
  (and (package? package) (filter library? (cdr package))))

(define (package-provides? package name)
  (and (pair? package)
       (eq? 'package (car package))
       (or (equal? name (assoc-get (cdr package) 'name))
           (find (lambda (x) (equal? name (library-name x)))
                 (package-libraries package)))))

(define (package-dependencies package)
  (append-map library-dependencies
              (package-libraries package)))

(define (package-installed-files pkg)
  (or (and (pair? pkg) (assoc-get-list (cdr pkg) 'installed-files)) '()))

(define (library-name->path name)
  (call-with-output-string
    (lambda (out)
      (let lp ((name name))
        (display (car name) out)
        (cond ((pair? (cdr name))
               (write-char #\/ out)
               (lp (cdr name))))))))

;; map a library to the path name it would be found in (sans extension)
(define (library->path library)
  (library-name->path (library-name library)))

;; find the library declaration file for the given library
(define (get-library-file cfg library)
  (or (assoc-get library 'path)
      (string-append (library->path library) "."
                     (conf-get cfg 'library-extension "sld"))))

(define (package->path pkg)
  (library-name->path (package-name pkg)))

(define (package-name->meta-file cfg name)
  (let ((path (library-name->path name)))
    (string-append (path-directory path) "/."
                   (path-strip-directory path) ".meta")))

(define (get-package-meta-file cfg pkg)
  (package-name->meta-file cfg (package-name pkg)))

(define (get-library-meta-file cfg lib)
  (package-name->meta-file cfg (library-name lib)))

;; libraries

(define (library? x)
  (and (pair? x) (eq? 'library (car x)) (every pair? (cdr x))))

(define (library-name lib)
  (and (pair? lib) (assoc-get (cdr lib) 'name eq?)))

(define (library-url lib)
  (and (pair? lib) (assoc-get (cdr lib) 'url eq?)))

(define (library-dependencies lib)
  (assoc-get-list (cdr lib) 'depends))

(define (parse-library-name str)
  (cond
   ((pair? str) str)
   ((equal? "" str) (error "empty library name"))
   ((eqv? #\( (string-ref str 0)) (read-from-string str))
   (else (map (lambda (x) (or (string->number x) (string->symbol x)))
              (string-split str #\.)))))

(define (library-name->path name)
  (and (pair? name)
       (let lp ((ls (cdr name)) (res (list (car name))))
         (if (null? ls)
             (apply string-append
                    (map display-to-string (reverse (cons ".sld" res))))
             (lp (cdr ls) (cons (car ls) (cons "/" res)))))))

(define (check-cond-expand config test)
  (define (library-installed? config name)
    ;; assume it could be installed for now
    #t)
  (cond
   ((symbol? test)
    (or (eq? 'else test) (memq test (conf-get-list config 'features))))
   ((pair? test)
    (case (car test)
      ((not) (not (check-cond-expand config (cadr test))))
      ((and) (every (lambda (x) (check-cond-expand config x)) (cdr test)))
      ((or) (any (lambda (x) (check-cond-expand config x)) (cdr test)))
      ((library) (every (lambda (x) (library-installed? config x)) (cdr test)))
      (else
       (warn "unknown cond-expand form" test)
       #f)))
   (else #f)))

;; We can't use the native library system introspection since we may
;; be analyzing a library which can't be loaded in the native system.
(define (library-analyze config file)
  (let ((sexp (call-with-input-file file read)))
    (and (list? sexp)
         (memq (car sexp) '(define-library library define-module module))
         (let analyze ((ls (cddr sexp)))
           (cond
            ((null? ls) '())
            (else
             (append
              (case (caar ls)
                ((cond-expand)
                 (cond
                  ((find (lambda (x) (check-cond-expand config (car x))) (cdar ls))
                   => (lambda (x) (analyze (cdr x))))
                  (else (analyze (cdr ls)))))
                (else (list (car ls))))
              (analyze (cdr ls)))))))))

(define (library-include-files config file)
  (let ((lib (library-analyze config file))
        (dir (path-directory file)))
    (append-map
     (lambda (x) (map (lambda (y) (make-path dir y)) (cdr x)))
     (filter (lambda (x) (and (pair? x) (memq (car x) '(include include-ci))))
             lib))))

(define (library-rewrite-includes x rules)
  (define (recurse x) (library-rewrite-includes x rules))
  (define (rewrite x)
    (cond ((any (lambda (r) (and (pair? r) (equal? x (car r)))) rules) => cdr)
          (else x)))
  (cond
   ((pair? x)
    (case (car x)
      ((include include-ci)
       (cons (car x) (map rewrite (cdr x))))
      ((cond-expand)
       (cons (car x)
             (map (lambda (y) (cons (car y) (map recurse (cdr y)))) (cdr x))))
      ((define-library library)
       (cons (car x) (map recurse (cdr x))))
      (else x)))
   (else x)))
