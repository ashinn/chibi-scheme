
;; general utils

(define (read-from-string str)
  (call-with-input-string str read))

(define (display-to-string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        (else (call-with-output-string (lambda (out) (display x out))))))

(define (maybe-parse-hex x)
  (if (string? x) (hex-string->bytevector x) x))

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
       (cond ((assoc-get (cdr package) 'name)
              => (lambda (x) (and (pair? x) x)))
             ((assq 'library (cdr package))
              => (lambda (x) (library-name x)))
             ((assq 'progam (cdr package))
              => (lambda (x) (program-name x)))
             (else #f))))

(define (package-email pkg)
  (and (package? pkg)
       (let ((sig (assq 'signature (cdr pkg))))
         (and (pair? sig)
              (assoc-get (cdr sig) 'email eq?)))))

(define (strip-email str)
  (string-trim (regexp-replace '(: "<" (* (~ (">"))) ">") str "")))

(define (package-author repo pkg . o)
  (let ((show-email? (and (pair? o) (car o))))
    (cond
     ((not (package? pkg))
      #f)
     ((assoc-get (cdr pkg) 'authors)
      => (lambda (authors) (if show-email? authors (strip-email authors))))
     (else
      (let ((email (package-email pkg)))
        (or (cond
             ((repo-find-publisher repo email)
              => (lambda (pub)
                   (let ((name (assoc-get pub 'name)))
                     (if (and name show-email?)
                         (string-append name " <" (or email "") ">")
                         (or name email "")))))
             (else #f))
            email))))))

(define (package-maintainer repo pkg . o)
  (let ((show-email? (and (pair? o) (car o))))
    (cond
     ((not (package? pkg))
      #f)
     ((assoc-get (cdr pkg) 'maintainers)
      => (lambda (maint) (if show-email? maint (strip-email maint))))
     (else
      #f))))

(define (package-url repo pkg)
  (let ((url (and (pair? pkg) (assoc-get (cdr pkg) 'url eq?))))
    (and url
         (uri-resolve url (string->path-uri 'http (or (repo-url repo) ""))))))

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
              actual: ,(rsa-verify rsa-key (maybe-parse-hex digest))))
     (else #f))))

(define (package-signature-ok? cfg pkg raw)
  (not (package-signature-mismatches cfg pkg raw)))

(define (failure str . args)
  (let ((out (open-output-string)))
    (display str out)
    (cond
     ((pair? args)
      (display ":" out)
      (for-each (lambda (x) (display " " out) (write x out)) args)))
    (get-output-string out)))

(define (invalid-library-reason lib)
  (cond
   ((not (list? lib)) "library must be a list")
   ((not (list? (library-name lib)))
    (failure "library name must be a list" (library-name lib)))
   ((not (every (lambda (x) (or (symbol? x) (integer? x))) (library-name lib)))
    (failure "library name must contain only symbols or integers"
             (library-name lib)))
   (else #f)))

(define (valid-library? lib)
  (not (invalid-library-reason lib)))

(define (invalid-program-reason prog)
  (cond
   ((not (list? prog)) "program must be a list")
   ((not (or (assoc-get prog 'path) (assoc-get prog 'name)))
    "program must have a path")
   (else #f)))

(define (valid-program? prog)
  (not (invalid-program-reason prog)))

(define (invalid-package-reason pkg)
  (cond
   ((not (list? pkg))
    "package must be a list")
   ((not (string? (package-version pkg)))
    (failure "package-version is not a string" (package-version pkg)))
   (else
    (let ((libs (package-libraries pkg))
          (progs (package-programs pkg)))
      (cond
       ((and (not (pair? libs)) (not (pair? progs)))
        "package must contain at least one library or program")
       ((any invalid-library-reason libs))
       ((any invalid-program-reason progs))
       (else #f))))))

(define (valid-package? pkg)
  (not (invalid-package-reason pkg)))

(define (package-for-impl impl cfg pkg)
  (append
   pkg
   (append-map
    (lambda (x)
      (or (and (pair? x) (eq? 'cond-expand (car x))
               (cond
                ((find
                  (lambda (clause) (check-cond-expand impl cfg (car clause)))
                  (cdr x))
                 => cdr)
                (else #f)))
          '()))
    (cdr pkg))))

(define (package-libraries package)
  (and (package? package) (filter library? (cdr package))))

(define (package-programs package)
  (and (package? package) (filter program? (cdr package))))

(define (package-data-files package)
  (and (package? package)
       (append-map cdr (filter data-files? (cdr package)))))

(define (package-provides? package name)
  (and (pair? package)
       (eq? 'package (car package))
       (or (equal? name (assoc-get (cdr package) 'name))
           (find (lambda (x) (equal? name (library-name x)))
                 (package-libraries package)))))

(define (package-dependencies impl cfg package)
  (append-map (lambda (lib) (library-dependencies cfg impl lib))
              (append (package-libraries package)
                      (package-programs package))))

(define (package-test-dependencies impl cfg package)
  (let ((pkg (package-for-impl impl cfg package)))
    (if (or (conf-get cfg '(command install skip-tests?))
            (conf-get cfg '(command upgrade skip-tests?)))
        '()
        (or (assoc-get (cdr pkg) 'test-depends)
            '()))))

(define (package-installed-files pkg)
  (or (and (pair? pkg) (assoc-get-list (cdr pkg) 'installed-files)) '()))

(define (library-separator cfg)
  (conf-get cfg 'library-separator "/"))

(define (library-name->path cfg name)
  (if (null? name)
      ""
      (call-with-output-string
        (lambda (out)
          (let lp ((name name))
            (display (car name) out)
            (cond ((pair? (cdr name))
                   (display (library-separator cfg) out)
                   (lp (cdr name)))))))))

;; map a library to the path name it would be found in (sans extension)
(define (library->path cfg library)
  (library-name->path cfg (library-name library)))

;; find the library declaration file for the given library
(define (get-library-file cfg library)
  (or (assoc-get library 'path)
      (string-append (library->path cfg library) "."
                     (conf-get cfg 'library-extension "sld"))))

(define (package->path cfg pkg)
  (library-name->path cfg (package-name pkg)))

(define (package-name->meta-file cfg name)
  (let ((path (library-name->path cfg name)))
    (string-append (path-directory path) "/."
                   (path-strip-directory path) ".meta")))

(define (get-package-meta-file cfg pkg)
  (package-name->meta-file cfg (package-name pkg)))

(define (get-library-meta-file cfg lib)
  (package-name->meta-file cfg (library-name lib)))

(define (library-file-name file)
  (guard (exn (else #f))
    (let ((x (call-with-input-file file read)))
      (and (pair? x)
           (memq (car x) '(define-library library))
           (list? (cadr x))
           (cadr x)))))

(define (find-library-file cfg lib-name . o)
  (let ((base (string-append (library-name->path cfg lib-name)
                             "."
                             (conf-get cfg 'library-extension "sld"))))
    (let lp ((dirs (append (or (and (pair? o) (car o)) '())
                           (cons "." (conf-get-list cfg 'library-path )))))
      (and (pair? dirs)
           (let ((path (make-path (car dirs) base)))
             (or (and (file-exists? path)
                      (equal? lib-name (library-file-name path))
                      path)
                 (lp (cdr dirs))))))))

(define (tar-file? file)
  (or (equal? (path-extension file) "tgz")
      (and (member (path-extension file) '("gz" "bz2"))
           (equal? (path-extension (path-strip-extension file)) "tar"))))

(define (package-file-unzipped file)
  (and (tar-file? file)
       (if (member (path-extension file) '("tgz" "gz"))
           (gunzip (let* ((in (open-binary-input-file file))
                          (res (port->bytevector in)))
                     (close-input-port in)
                     res))
           file)))

(define (package-file-meta file)
  (let* ((unzipped-file (package-file-unzipped file))
         (package-file
          (and unzipped-file
               (find
                (lambda (x)
                  (and (equal? "package.scm" (path-strip-directory x))
                       (equal? "." (path-directory (path-directory x)))))
                (tar-files unzipped-file)))))
    (and package-file
         (guard (exn (else #f))
           (let* ((str (utf8->string
                        (tar-extract-file unzipped-file package-file)))
                  (package (read (open-input-string str))))
             (and (pair? package)
                  (eq? 'package (car package))
                  package))))))

(define (package-file? file)
  (and (package-file-meta file) #t))

(define (package-file-top-directory file)
  (let ((unzipped-file (package-file-unzipped file)))
    (and unzipped-file
         (let lp ((file (car (tar-files unzipped-file))))
           (let ((dir (path-directory file)))
             (if (member dir '("" "." "/"))
                 file
                 (lp dir)))))))

;; libraries

(define (library? x)
  (and (pair? x) (eq? 'library (car x)) (every pair? (cdr x))))

(define (library-name lib)
  (and (pair? lib) (assoc-get (cdr lib) 'name eq?)))

(define (library-url lib)
  (and (pair? lib) (assoc-get (cdr lib) 'url eq?)))

(define (library-for-impl impl cfg lib)
  (append
   lib
   (append-map
    (lambda (x)
      (or (and (pair? x) (eq? 'cond-expand (car x))
               (cond
                ((find
                  (lambda (clause) (check-cond-expand impl cfg (car clause)))
                  (cdr x))
                 => cdr)
                (else #f)))
          '()))
    (cdr lib))))

(define (library-dependencies impl cfg lib)
  (append-map
   (lambda (x) (or (and (pair? x) (eq? 'depends (car x)) (cdr x)) '()))
   (cdr (library-for-impl impl cfg lib))))

(define (parse-library-name str)
  (cond
   ((pair? str) str)
   ((equal? "" str) (error "empty library name"))
   ((eqv? #\( (string-ref str 0)) (read-from-string str))
   (else (map (lambda (x) (or (string->number x) (string->symbol x)))
              (string-split str #\.)))))

(define (check-cond-expand impl config test)
  (define (library-installed? config name)
    ;; assume it could be installed for now... this is effectively a
    ;; "suggested" package rather than a required one
    #t)
  (cond
   ((symbol? test)
    (or (eq? 'else test) (eq? impl test)
        (memq test (conf-get-list config 'features))))
   ((pair? test)
    (case (car test)
      ((not) (not (check-cond-expand impl config (cadr test))))
      ((and) (every (lambda (x) (check-cond-expand impl config x)) (cdr test)))
      ((or) (any (lambda (x) (check-cond-expand impl config x)) (cdr test)))
      ((library) (every (lambda (x) (library-installed? config x)) (cdr test)))
      (else
       (warn "unknown cond-expand form" test)
       #f)))
   (else #f)))

;; We can't use the native library system introspection since we may
;; be analyzing a library which can't be loaded in the native system.
(define (library-analyze impl config file)
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
                  ((find (lambda (x) (check-cond-expand impl config (car x)))
                         (cdar ls))
                   => (lambda (x) (analyze (cdr x))))
                  (else (analyze (cdr ls)))))
                (else (list (car ls))))
              (analyze (cdr ls)))))))))

(define (library-include-files impl config file)
  (let ((lib (library-analyze impl config file))
        (dir (path-directory file)))
    (append-map
     (lambda (x) (map (lambda (y) (make-path dir y)) (cdr x)))
     (filter (lambda (x) (and (pair? x) (memq (car x) '(include include-ci))))
             lib))))

(define (library-shared-include-files impl config file)
  (let ((lib (library-analyze impl config file))
        (dir (path-directory file)))
    (append-map
     (lambda (x) (map (lambda (y) (make-path dir y)) (cdr x)))
     (filter (lambda (x) (and (pair? x) (eq? (car x) 'include-shared)))
             lib))))

(define (library-rewrite-includes x rules)
  (define (recurse x) (library-rewrite-includes x rules))
  (define (rewrite x)
    (cond ((find (lambda (r) (and (pair? r) (equal? x (car r)))) rules) => cadr)
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
      ;; support define-library as well as the package format
      ((path) (cons (car x) (map rewrite (cdr x))))
      (else x)))
   (else x)))

;; programs

(define (program? x)
  (and (pair? x) (eq? 'program (car x)) (every pair? (cdr x))))

(define (program-name prog)
  (and (pair? prog)
       (cond ((assoc-get (cdr prog) 'name eq?))
             ((assoc-get (cdr prog) 'path eq?)
              => (lambda (p) (list (string->symbol (path-strip-directory p)))))
             (else #f))))

(define (get-program-file cfg prog)
  (cond ((assoc-get prog 'path))
        ((assoc-get prog 'name)
         => (lambda (name) (library-name->path cfg (list (last name)))))
        (else (error "program missing path: " prog))))

(define (program-install-name prog)
  (or (assoc-get (cdr prog) 'install-name eq?)
      (path-strip-extension
       (path-strip-directory
        (assoc-get (cdr prog) 'path eq?)))))

;; data files

(define (data-files? x)
  (and (pair? x) (eq? 'data-files (car x))))
