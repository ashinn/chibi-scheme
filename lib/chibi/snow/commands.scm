;; commands.scm -- snow commands
;;
;; This code was written by Alex Shinn in 2014 and placed in the
;; Public Domain.  All warranties are disclaimed.

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

(define (available-implementations cfg)
  (define (find prog name) (if (find-in-path prog) (list name) '()))
  (append (cond-expand
           (chibi (list 'chibi))
           (else (find "chibi-scheme" 'chibi)))
          (find "foment" 'foment)
          (find "gosh" 'gauche)
          (find "guile" 'guile)
          (find "sagittarius" 'sagittarius)))

(define (conf-selected-implementations cfg)
  (let ((requested (conf-get-list cfg 'implementations '(chibi)))
        (available (available-implementations cfg)))
    (if (memq 'all requested)
        available
        (lset-intersection eq? requested available))))

(define (conf-for-implementation cfg impl)
  (conf-specialize cfg 'implementation impl))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (write-to-string x)
  (call-with-output-string (lambda (out) (write x out))))

(define (file->sexp-list file)
  (call-with-input-file file
    (lambda (in)
      (let lp ((res '()))
        (let ((x (read in)))
          (if (eof-object? x)
              (reverse res)
              (lp (cons x res))))))))

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
     ((null? as) (if (null? bs) -1 0))
     ((null? bs) 1)
     ((less? (car as) (car bs)) -1)
     ((less? (car bs) (car as)) 1)
     (else (lp (cdr as) (cdr bs))))))

(define (version>? a b) (> (version-compare a b) 0))
(define (version>=? a b) (>= (version-compare a b) 0))

;; Hack to evaluate an expression in a separate process with a larger
;; default heap.  The expression and result must be serializable with
;; write, and imports should be an argument list for environment.
;; Currently only used when generating keys and signing.
(define (fast-eval expr imports . o)
  (let* ((heap-size (if (pair? o) (car o) 500))
         (cmd
          `("chibi-scheme"
            ,(string-append "-h" (number->string heap-size) "M")
            ,@(map
               (lambda (i)
                 (string-append "-m" (string-join (map write-to-string i) ".")))
               imports)
            "-p" ,(write-to-string expr))))
    (let ((res (process->sexp cmd)))
      (if (eof-object? res)  ; process error
          (eval expr (apply environment imports))
          res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package - generate a package from one or more libraries.

(define (tar-file? file)
  (or (equal? (path-extension file) "tgz")
      (and (member (path-extension file) '("gz" "bz2"))
           (equal? (path-extension (path-strip-extension file)) "tar"))))

(define (package-file-meta file)
  (and
   (tar-file? file)
   (let* ((unzipped-file
           (if (member (path-extension file) '("tgz" "gz"))
               (gunzip (let* ((in (open-binary-input-file file))
                              (res (port->bytevector in)))
                         (close-input-port in)
                         res))
               file))
          (package-file
           (find
            (lambda (x)
              (and (equal? "package.scm" (path-strip-directory x))
                   (equal? "." (path-directory (path-directory x)))))
            (tar-files unzipped-file))))
     (and package-file
          (guard (exn (else #f))
            (let* ((str (utf8->string
                         (tar-extract-file unzipped-file package-file)))
                   (package (read (open-input-string str))))
              (and (pair? package)
                   (eq? 'package (car package))
                   package)))))))

(define (package-file? file)
  (and (package-file-meta file) #t))

(define (x->string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        (else (error "not a valid path component" x))))

(define (library-path-base file name)
  (let lp ((ls (cdr (reverse name))) (dir (path-directory file)))
    (cond
     ((null? ls) dir)
     ((equal? (x->string (car ls)) (path-strip-directory dir))
      (lp (cdr ls) (path-directory dir)))
     (else dir))))

(define (path-relative file dir)
  (let ((file (path-normalize file))
        (dir (string-trim-right (path-normalize dir) #\/)))
    (string-trim-left
     (if (string-prefix? dir file)
         (substring file (string-length dir))
         file)
     #\/)))

(define (extract-library cfg file)
  (let ((lib (read-from-file file)))
    (match lib
      (('define-library (name ...)
         declarations ...)
       (let* ((dir (library-path-base file name))
              (lib-file (path-relative file dir))
              (lib-dir (path-directory lib-file)))
         (define (resolve file)
           (let ((dest-path (make-path lib-dir file)))
             (list 'rename (make-path dir dest-path) dest-path)))
         (define (import-name import)
           (cond
            ((and (pair? import)
                  (memq (car import) '(only except prefix drop-prefix rename))
                  (pair? (cadr import)))
             (import-name (cadr import)))
            (else import)))
         (let lp ((ls declarations)
                  (info `(,@(cond
                             ((conf-get cfg '(command package author))
                              => (lambda (x) (list (list 'author x))))
                             (else '()))
                          (path ,lib-file)
                          (name ,name)))
                  (files `((rename ,file ,lib-file)))
                  (dirs '("")))
           (cond
            ((null? ls)
             (cons `(library ,@(reverse info))
                   (cons `(rename ,dir "")
                         (append (map resolve (delete-duplicates dirs equal?))
                                 files))))
            (else
             (match (car ls)
               (((or 'include 'include-ci) includes ...)
                (lp (cdr ls)
                    info
                    (append (map resolve includes) files)
                    (append (map path-directory includes) dirs)))
               (('include-library-declarations includes ...)
                (lp (append (append-map file->sexp-list includes) (cdr ls))
                    info
                    (append (map resolve includes) files)
                    dirs))
               (('import libs ...)
                (lp (cdr ls)
                    (cons (cons 'depends (map import-name libs)) info)
                    files
                    dirs))
               (('cond-expand clauses ...)
                (lp (append (append-map cdr clauses) (cdr ls)) info files dirs))
               (else
                (lp (cdr ls) info files dirs))))))))
      (else
       (die 2 "not a valid library declaration " lib " in file " file)))))

(define (make-package-name cfg libs . o)
  (let ((name (assq 'name (car libs)))
        (version (and (pair? o) (car o))))
    (cond
     ((not (and (pair? name) (pair? (cdr name))))
      (die 2 "Unnamed library"))
     ((not (and (pair? (cadr name)) (list? (cadr name))))
      (die 2 "Invalid library name" (cadr name)))
     (else
      (let lp ((ls (if version (append (cadr name) (list version)) (cadr name)))
               (res '()))
        (if (null? ls)
            (string-join (reverse (cons ".tgz" res)))
            (lp (cdr ls)
                (cons (x->string (car ls))
                      (if (null? res) res (cons "-" res))))))))))

(define (check-overwrite cfg file type-pred type-name)
  (let ((mode (conf-get cfg '(command package overwrite) 'same-type)))
    (cond
     ((eq? mode 'always))
     ((file-exists? file)
      (case mode
        ((never)
         (die 2 "Destination " file " already exists, not overwriting"))
        ((same-type)
         (if (not (type-pred file))
             (die 2 "Destination " file " doesn't look like a " type-name
                  ", not overwriting")))
        ((confirm)
         (if (not (yes-or-no? cfg "Overwrite existing " file "?"))
             (die 2 "Not overwriting " file))))))))

;; Simplistic pretty printing for package/repository/config declarations.
(define (write-simple-pretty pkg out)
  (let wr ((ls pkg) (indent 0) (tails 0))
    (cond
     ((and (pair? ls)
           (pair? (cdr ls))
           (pair? (cadr ls)))
      (display (make-string indent #\space) out)
      (write-char #\( out)
      (write (car ls) out)
      (newline out)
      (for-each (lambda (x) (wr x (+ indent 2) 0)) (drop-right (cdr ls) 1))
      (wr (last ls) (+ indent 2) (+ tails 1)))
     (else
      (display (make-string indent #\space) out)
      (write ls out)
      (display (make-string tails #\)) out)
      (newline out)))))

;; We want to automatically bundle (foo bar *) when packaging (foo bar)
;; if it's already in the same directory.
(define (submodule->path base file lib dep)
  (and base
       (> (length dep) (length base))
       (equal? base (take dep (length base)))
       ;; TODO: find-library(-relative)
       (let* ((dir (library-path-base file lib))
              (dep-file (make-path dir (string-append
                                        (library-name->path dep)
                                        ".sld"))))
         (and (file-exists? dep-file) dep-file))))

(define (package-docs cfg spec libs)
  (cond
   ((conf-get cfg '(command package doc)) => list)
   ((conf-get cfg '(command package doc-from-scribble))
    (map
     (lambda (lib)
       (let* ((lib+files (extract-library cfg lib))
              (lib-name (library-name (car lib+files))))
         `(inline
           ,(string-append (library-name->path lib-name) ".html")
           ,(call-with-output-string
              (lambda (out)
                (print-module-docs lib-name out sxml-display-as-html))))))
     libs))
   (else '())))

(define (package-description cfg spec libs docs)
  (cond
   ((conf-get cfg '(command package description)))
   ((conf-get cfg '(command upload description)))
   ;; Crazy hack, make this more robust, probably opt-in.
   ((and (pair? docs) (pair? (car docs)) (eq? 'inline (caar docs))
         (regexp-search
          '(: "<p>" (* "\n") (* space) ($ (* (~ ("."))) "."))
          (third (car docs))))
    => (lambda (m)
         (let ((s (regexp-match-submatch m 1)))
           (and s
                (regexp-replace-all
                 '(>= 2 space)
                 (regexp-replace-all
                  "\n"
                  (regexp-replace-all '(: "<" (? "/") (* (~ ("<>"))) ">")
                                      s "")
                  " ")
                 " ")))))
   (else #f)))

(define (package-test cfg)
  (conf-get cfg '(command package test)))

(define (package-output-version cfg)
  (cond ((conf-get cfg '(command package version)))
        ((conf-get cfg '(command package version-file))
         => (lambda (file) (call-with-input-file file read-line)))
        ((conf-get cfg '(command upload version)))
        ((conf-get cfg '(command upload version-file))
         => (lambda (file) (call-with-input-file file read-line)))
        (else #f)))

(define (package-output-path cfg package-spec)
  (or (conf-get cfg 'output)
      (make-package-name
       cfg
       (filter (lambda (x) (and (pair? x) (eq? 'library (car x)))) package-spec)
       (package-output-version cfg))))

(define (package-spec+files cfg spec libs)
  (let* ((recursive? (conf-get cfg '(command package recursive?)))
         (docs (package-docs cfg spec libs))
         (desc (package-description cfg spec libs docs))
         (test (package-test cfg))
         (version (package-output-version cfg)))
    (let lp ((ls (map (lambda (x) (cons x #f)) libs))
             (res
              `(,@(if (pair? docs)
                      `((manual ,@(map
                                   (lambda (x)
                                     (path-strip-leading-parents
                                      (if (pair? x) (cadr x) x)))
                                   docs)))
                      '())
                ,@(if desc `((description ,desc)) '())
                ,@(if test `((test ,(path-strip-leading-parents test))) '())
                ,@(if version `((version ,version)) '())))
             (files
              `(,@docs
                ,@(if test (list test) '()))))
      (cond
       ((and (null? ls) (null? res))
        (die 2 "No packages generated"))
       ((null? ls)
        (cons (cons 'package (reverse res)) files))
       (else
        (let* ((lib+files (extract-library cfg (caar ls)))
               (lib (car lib+files))
               (name (library-name lib))
               (base (or (cdar ls) name))
               (subdeps (if recursive?
                            (filter-map
                             (lambda (x)
                               (submodule->path base (caar ls) name x))
                             (cond ((assq 'depends (cdr lib)) => cdr)
                                   (else '())))
                            '())))
          (lp (append (map (lambda (x) (cons x base)) subdeps) (cdr ls))
              (cons lib res)
              (append (cdr lib+files) files))))))))

(define (create-package spec files path)
  (gzip
   (tar-create #f `(,@files
                    (inline "package.scm"
                            ,(call-with-output-string
                               (lambda (out) (write-simple-pretty spec out)))))
               (let ((dir (path-strip-extension (path-strip-directory path))))
                 (lambda (f) (make-path dir f)))
               #t)))

(define (command/package cfg spec . libs)
  (let* ((spec+files (package-spec+files cfg spec libs))
         (output (package-output-path cfg (car spec+files)))
         (tarball (create-package (car spec+files) (cdr spec+files) output)))
    (check-overwrite cfg output package-file? "package")
    (let ((out (open-binary-output-file output)))
      (write-bytevector tarball out)
      (close-output-port out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gen-key - generate a new RSA key pair.

(define (conf-get-snow-dir cfg)
  (or (conf-get cfg 'snow-dir)
      (string-append (get-environment-variable "HOME") "/.snow")))

(define (rsa-key->sexp key name email)
  `((name ,name)
    (email ,email)
    (bits ,(rsa-key-bits key))
    ,@(cond
       ((rsa-key-e key)
        => (lambda (e)
             `((public-key
                (modulus ,(integer->hex-string (rsa-key-n key)))
                (exponent ,e)))))
       (else '()))
    ,@(cond
       ((rsa-key-d key)
        => (lambda (d)
             `((private-key
                (modulus ,(integer->hex-string (rsa-key-n key)))
                (exponent ,d)))))
       (else '()))))

(define (conf-gen-key cfg bits)
  (show #t "Generating a new key, this may take quite a while...\n")
  (if (conf-get cfg 'gen-key-in-process?)
      (rsa-key-gen bits)
      (let* ((lo (max 3 (expt 2 (- bits 1))))
             (hi (expt 2 bits))
             (p (fast-eval `(random-prime ,lo ,hi)
                           '((chibi math prime))))
             (q (fast-eval `(random-prime-distinct-from ,lo ,hi ,p)
                           '((chibi math prime)))))
        (rsa-key-gen-from-primes bits p q))))

(define (command/gen-key cfg spec)
  (show #t
        "Generate a new RSA key for signing packages.\n"
        "We need a descriptive name, and an email address to "
        "uniquely identify the key.\n")
  (let* ((name (input cfg '(gen-key name) "Name: "))
         (email (input cfg '(gen-key email) "Email: "))
         (bits (input-number cfg '(gen-key bits)
                             "RSA key size in bits: " 512 64 20148))
         (key (conf-gen-key cfg bits))
         (snow-dir (conf-get-snow-dir cfg))
         (key-file (or (conf-get cfg 'key-file)
                       (string-append snow-dir "/priv-key.scm")))
         (old-keys (guard (exn (else '()))
                     (call-with-input-file key-file read)))
         (new-keys
          (cons (rsa-key->sexp key name email) 
                ;; TODO: confirm overwrite, preserve old keys
                (remove (rsa-identity=? email) old-keys))))
    (if (not (file-directory? snow-dir))
        (create-directory snow-dir))
    (let* ((fd (open key-file (bitwise-ior open/write open/create) #o600))
           (out (open-output-file-descriptor fd)))
      (show out "("
            (joined (lambda (x)
                      (if (pair? x)
                          (each "(" (joined written x "\n  ") ")")
                          (written x)))
                    new-keys
                    "\n ")
            ")" nl)
      (close-output-port out)
      (show #t "Saved key to " key-file ".\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reg-key - register an RSA key pair with a repository.

(define (remote-uri cfg name path)
  (or (conf-get cfg name)
      (make-path (or (conf-get cfg 'host) "http://snow-fort.org")
                 path)))

(define (remote-command cfg name path params)
  (let ((uri (remote-uri cfg name path)))
    (sxml-display-as-text (read (http-post uri (cons '(fmt . "sexp") params))))
    (newline)))

(define (command/reg-key cfg spec)
  (let* ((keys (call-with-input-file
                   (or (conf-get cfg 'key-file)
                       (string-append (conf-get-snow-dir cfg) "/priv-key.scm"))
                 read))
         (email (or (conf-get cfg 'email)
                    (assoc-get (car keys) 'email)))
         (rsa-key-sexp (or (find (rsa-identity=? email) keys)
                           (and (not email) (car keys))))
         (name (assoc-get rsa-key-sexp 'name))
         (rsa-pub-key (extract-rsa-public-key rsa-key-sexp))
         (rsa-pub-key-str
          (write-to-string (rsa-key->sexp rsa-pub-key name email))))
    (remote-command cfg
                    '(command reg-key uri)
                    "/pkg/reg"
                    `((u (file . "pub-key.scm")
                         (value . ,rsa-pub-key-str))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sign - sign a package.

(define (generate-signature cfg package)
  (let* ((digest-name (conf-get cfg 'digest 'sha-256))
         (digest-func (lookup-digest digest-name))
         (digest (if (string? package)
                     (call-with-input-file package digest-func)
                     (digest-func package)))
         (keys (call-with-input-file
                   (or (conf-get cfg 'key-file)
                       (string-append (conf-get-snow-dir cfg) "/priv-key.scm"))
                 read))
         (email (or (conf-get cfg 'email)
                    (assoc-get (car keys) 'email)))
         (rsa-key-sexp (find (rsa-identity=? email) keys))
         (rsa-key (extract-rsa-private-key rsa-key-sexp))
         (sig (fast-eval `(rsa-sign (make-rsa-key ,(rsa-key-bits rsa-key)
                                                  ,(rsa-key-n rsa-key)
                                                  #f
                                                  ,(rsa-key-d rsa-key))
                                    ;;,(hex-string->integer digest)
                                    ,(hex-string->bytevector digest))
                         '((chibi crypto rsa))))
         (hex-sig (if (bytevector? sig)
                      (bytevector->hex-string sig)
                      (integer->hex-string sig))))
    `(signature
      (email ,email)
      (digest ,digest-name)
      (,digest-name ,digest)
      (rsa ,hex-sig))))

(define (command/sign cfg spec package)
  (let* ((dst (or (conf-get cfg 'output)
                  (path-replace-extension package "sig")))
         (sig (generate-signature cfg package)))
    (call-with-output-file dst
      (lambda (out) (write sig out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verify - verify a signature.

(define (command/verify cfg spec sig)
  (let* ((sig-spec (cdr (call-with-input-file sig read)))
         (keys (call-with-input-file
                   (or (conf-get cfg 'key-file)
                       (string-append (conf-get-snow-dir cfg) "/priv-key.scm"))
                 read))
         (email (assoc-get sig-spec 'email))
         (digest-name (assoc-get sig-spec 'digest #f 'sha-256))
         (digest (assoc-get sig-spec digest-name))
         (sig (assoc-get sig-spec 'rsa))
         (rsa-key-sexp (or (and (string? email)
                                (find (rsa-identity=? email) keys))
                           (car keys)))
         (rsa-key (extract-rsa-public-key rsa-key-sexp))
         (cipher (rsa-verify rsa-key (hex-string->bytevector sig)))
         (digest-bv (hex-string->bytevector digest)))
    (if (equal? cipher digest-bv)
        (show #t "signature valid " nl)
        (show #t "signature invalid " cipher " != " digest-bv nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upload - upload a package.

(define (upload-package cfg spec package . o)
  (let ((pkg (if (string? package)
                 `(u (file . ,package))
                 `(u (file . ,(if (pair? o) (car o) "package.tgz"))
                     (value . ,package))))
        (sig
         (cond
          ((conf-get cfg 'sig-file)
           => (lambda (sig-file) `(sig (file . ,sig-file))))
          (else
           `(sig (file . "package.sig")
                 (value . ,(write-to-string
                            (generate-signature cfg package))))))))
    (remote-command cfg '(command package uri) "/pkg/put" (list pkg sig))))

(define (command/upload cfg spec . o)
  (define (non-homogeneous)
    (die 1 "upload arguments must all be packages or all be libraries, "
         "but got " o))
  (cond
   ((null? o)
    (die 1 "upload requires at least one input argument"))
   ((package-file? (car o))
    (if (not (every package-file? (cdr o)))
        (non-homogeneous))
    (for-each
     (lambda (package) (upload-package cfg spec package))
     o))
   (else
    (if (any package-file? (cdr o))
        (non-homogeneous))
    (let* ((spec+files (package-spec+files cfg spec o))
           (package-file (package-output-path cfg (car spec+files)))
           (package (create-package (car spec+files)
                                    (cdr spec+files)
                                    package-file)))
      (upload-package cfg spec package package-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove - removes the listed libraries.
;;
;; Provides a summary of the libraries to remove along with any
;; dependencies they have which were not explicitly installed.

(define (warn-delete-file file)
  (guard (exn (else (warn "couldn't delete file: " file)))
    (delete-file file)))

(define (delete-library-files impl cfg pkg lib-name)
  (for-each warn-delete-file (package-installed-files pkg))
  (warn-delete-file (make-path (get-install-source-dir impl cfg)
                               (get-package-meta-file cfg pkg)))
  (let ((dir (make-path (get-install-source-dir impl cfg)
                        (package->path pkg))))
    (if (and (file-directory? dir)
             (= 2 (length (directory-files dir))))
        (delete-directory dir))))

(define (command/remove cfg spec . args)
  (let* ((impls (conf-selected-implementations cfg))
         (impl-cfgs (map (lambda (impl)
                           (conf-for-implementation cfg impl))
                         impls))
         (lib-names (map parse-library-name args)))
    (for-each
     (lambda (impl impl-cfg)
       (for-each (lambda (pkg lib-name)
                   (delete-library-files impl impl-cfg (cdr pkg) lib-name))
                 (lookup-installed-libraries impl impl-cfg lib-names)
                 lib-names))
     impls
     impl-cfgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search - search for libraries matching keywords.
;;
;; Prints a list of libraries whose meta-info contain any of the given
;; keywords.  Returns in sorted order for how well the package matches.

(define (summarize-libraries cfg lib-names+pkgs)
  (for-each describe-library
            (map car lib-names+pkgs)
            (map cdr lib-names+pkgs)))

;; faster than (length (regexp-extract re str))
(define (regexp-count re str)
  (regexp-fold re (lambda (from md str acc) (+ acc 1)) 0 str))

(define (count-in-sexp x keywords)
  (regexp-count `(word (or ,@keywords)) (write-to-string x)))

(define (extract-matching-libraries cfg repo keywords)
  (define (library-score lib)
    (+ (* 10 (count-in-sexp (library-name lib) keywords))
       (count-in-sexp lib keywords)))
  (append-map
   (lambda (x)
     (cond
      ((not (package? x)) '())
      (else
       (let ((pkg-score (count-in-sexp x keywords))
             (libs (package-libraries x)))
         (if (or (zero? pkg-score) (null? libs))
             '()
             (let lp ((libs (cdr libs))
                      (best-score (library-score (car libs)))
                      (best-lib (car libs)))
               (cond
                ((null? libs)
                 (list (cons (+ best-score pkg-score)
                             (cons (library-name best-lib) x))))
                (else
                 (let ((score (library-score (car libs))))
                   (if (> score best-score)
                       (lp (cdr libs) score (car libs))
                       (lp (cdr libs) best-score best-lib)))))))))))
   repo))

(define (extract-sorted-packages cfg repo keywords)
  (let ((ls (extract-matching-libraries cfg repo keywords)))
    (map cdr (sort ls > car))))

(define (command/search cfg spec . keywords)
  (let* ((repo (maybe-update-repository cfg))
         (lib-names+pkgs (extract-sorted-packages cfg repo keywords)))
    (if (pair? lib-names+pkgs)
        (summarize-libraries cfg lib-names+pkgs)
        (display "No libraries matched your query.\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show - show detailed information for the given libraries
;;
;; The typical pattern is to use search to find the names of libraries
;; of interest, and show to see detailed information to decide whether
;; or not to install them.

(define (describe-library library-name pkg)
  (display library-name)
  (display "\t")
  (display (package-version pkg))
  (newline))

(define (command/show cfg spec . args)
  (maybe-update-repository cfg)
  (let* ((impls (conf-selected-implementations cfg))
         (impl-cfgs (map (lambda (impl)
                           (conf-for-implementation cfg impl))
                         impls))
         (lib-names (map parse-library-name args)))
    (for-each
     (lambda (impl impl-cfg)
       (for-each describe-library
                 (lookup-installed-libraries impl impl-cfg lib-names)
                 lib-names))
     impls
     impl-cfgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update - update the repository.

(define (valid-repository? repo)
  (and (pair? repo) (list? repo) (eq? 'repository (car repo))))

(define (repository-dir cfg)
  (cond
   ((zero? (current-user-id))
    (or (conf-get cfg 'local-root-repository)
        "/usr/local/share/snow/repo"))
   (else
    (or (conf-get cfg 'local-user-repository)
        (make-path (conf-get-snow-dir cfg) "repo")))))

(define (update-repository cfg)
  (let* ((local-dir (repository-dir cfg))
         (local-path (make-path local-dir "repo.scm"))
         (local-tmp (string-append local-path ".tmp."
                                   (number->string (current-second))))
         (repo-uri (remote-uri cfg 'repository-uri "/s/repo.scm"))
         (repo-str (call-with-input-url repo-uri port->string))
         (repo (guard (exn (else #f))
                 (let ((repo (read (open-input-string repo-str))))
                   `(,(car repo) (url ,repo-uri) ,@(cdr repo))))))
    (cond
     ((not (valid-repository? repo))
      (die 2 "not a valid repository: " repo-uri))
     ((not (create-directory* local-dir))
      (die 2 "can't create directory: " local-dir ))
     (else
      (guard (exn (else (die 2 "couldn't write repository")))
        (call-with-output-file local-tmp
          (lambda (out) (write repo out)))
        (if (file-exists? local-path)
            (rename-file local-path (string-append local-path ".bak")))
        (rename-file local-tmp local-path)
        repo)))))

(define (repository-stale? cfg)
  (let ((path (make-path (repository-dir cfg) "repo.scm")))
    (guard (exn (else #t))
      (> (current-second)
         (+ (file-modification-time path)
            ;; by default update once every 3 hours
            (conf-get cfg 'update-refresh (* 3 60 60)))))))

(define (should-update-repository? cfg)
  (case (conf-get cfg 'update-strategy 'cache)
    ((always) #t)
    ((never) #f)
    ((cache)
     (repository-stale? cfg))
    ((confirm)
     (and (repository-stale? cfg)
          (yes-or-no? cfg "Update repository info?")))
    (else
     (warn "unknown update-stategy: " (conf-get cfg 'update-strategy))
     #f)))

(define (maybe-update-repository cfg)
  (or (guard (exn (else #f))
        (and (should-update-repository? cfg)
             (update-repository cfg)))
      (guard (exn (else '(repository)))
        (call-with-input-file (make-path (repository-dir cfg) "repo.scm")
          read))))

(define (command/update cfg spec)
  (update-repository cfg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install - install one or more libraries.
;;
;; Installs the listed libraries along with their transitive closure
;; of dependencies.  For each library to install we confirm the
;; current status (skipping if already installed), the signature and
;; trust (optionally updating the trust level), and the default tests.
;; If multiple implementations are targeted, we install separately but
;; use the same confirmations for each.

(define (get-install-dirs impl cfg)
  (define (guile-eval expr)
    (guard (exn (else #f))
      (process->sexp `(guile -c ,(write-to-string `(write ,expr))))))
  (case impl
    ((chibi)
     (let* ((dirs (reverse (fast-eval '(current-module-path) '((chibi)))))
            (share-dir (find (lambda (d) (string-contains d "/share/")) dirs)))
       (if share-dir
           (cons share-dir (delete share-dir dirs))
           dirs)))
    ((gauche)
     (let ((dir (process->string '(gauche-config "--sitelibdir"))))
       (and (string? dir) (> 0 (string-length dir))
            (eqv? #\/ (string-ref dir 0))
            dir)))
    ((guile)
     (let ((path
            (guile-eval
             '(string-append (cdr (assq 'pkgdatadir %guile-build-info))
                             (string (integer->char 47))
                             (effective-version)))))
       (if (string? path)
           path
           "/usr/local/share/guile/")))
    (else (list (make-path "/usr/local/share/snow" impl)))))

(define (get-install-search-dirs impl cfg)
  (let ((install-dir (get-install-source-dir impl cfg))
        (other-dirs (get-install-dirs impl cfg)))
    (cons install-dir (delete install-dir other-dirs))))

(define (find-library-meta impl cfg name)
  (let ((dirs (get-install-search-dirs impl cfg)))
    (let lp ((subname name))
      (or (find-sexp-in-path
           (package-name->meta-file cfg subname)
           dirs
           (lambda (x)
             (and (package? x)
                  (any (lambda (y) (equal? name (library-name y)))
                       (package-libraries x)))))
          (and (pair? (cdr subname))
               (lp (drop-right subname 1)))))))

(define (test-library impl cfg library dir)
  #t)

(define (lookup-installed-libraries impl cfg names)
  (map (lambda (name)
         (cons name
               (or (find-library-meta impl cfg name)
                   `(not-installed ,name))))
       names))

(define (installed-libraries impl cfg)
  (delete-duplicates
   (directory-fold-tree
    (get-install-source-dir impl cfg)
    #f #f
    (lambda (file acc)
      (cond
       ((and (equal? "meta" (path-extension file))
             (guard (exn (else #f))
               (let ((pkg (call-with-input-file file read)))
                 (and (package? pkg) pkg))))
        => (lambda (pkg)
             (append
              (map
               (lambda (lib) (cons (library-name lib) pkg))
               (package-libraries pkg))
              acc)))
       (else acc)))
    '())
   (lambda (a b) (equal? (car a) (car b)))))

(define (get-install-source-dir impl cfg)
  (cond
   ((conf-get cfg 'install-source-dir))
   ((conf-get cfg 'install-prefix)
    => (lambda (prefix) (make-path prefix "share/snow" impl)))
   (else (car (get-install-dirs impl cfg)))))

(define (install-with-sudo? cfg path)
  (case (conf-get cfg '(command install use-sudo?))
    ((always) #t)
    ((never) #f)
    (else
     (let lp ((path path))
       (let ((dir (path-directory path)))
         (and (not (file-is-writable? path))
              (or (file-exists? path)
                  (lp dir))))))))

(define (install-file cfg source dest)
  (if (install-with-sudo? cfg dest)
      (system "sudo" "cp" source dest)
      (copy-file source dest)))

(define (install-sexp-file cfg obj dest)
  (if (install-with-sudo? cfg dest)
      (call-with-temp-file "sexp"
        (lambda (tmp-path out)
          (write-simple-pretty obj out)
          (close-output-port out)
          (system "sudo" "cp" tmp-path dest)))
      (call-with-output-file dest
        (lambda (out) (write-simple-pretty obj out)))))

(define (install-symbolic-link cfg source dest)
  (if (install-with-sudo? cfg dest)
      (system "sudo" "ln" "-s" source dest)
      (symbolic-link-file source dest)))

(define (install-directory cfg dir)
  (cond
   ((file-directory? dir))
   ((install-with-sudo? cfg dir)
    (system "sudo" "mkdir" "-p" dir))
   (else
    (create-directory* dir))))

(define (install-package-meta-info impl cfg pkg)
  (let* ((meta-file (get-package-meta-file cfg pkg))
         (install-dir (get-install-source-dir impl cfg))
         (path (make-path install-dir meta-file)))
    ;; write the package name
    (install-sexp-file cfg pkg path)
    ;; symlink utility libraries for which the package can't be inferred
    (let ((pkg-name (package-name pkg)))
      (for-each
       (lambda (lib)
         (let ((lib-name (library-name lib)))
           (if (not (equal? pkg-name (take lib-name (length pkg-name))))
               (let ((lib-meta (get-library-meta-file cfg lib)))
                 (install-symbolic-link
                  cfg path (make-path install-dir lib-meta))))))
       (package-libraries pkg)))))

;; The default installer just copies the library file and any included
;; source files to an installation directory, optionally mapping
;; extensions to the implementations preferred value.
(define (default-installer impl cfg library dir)
  (let* ((library-file (get-library-file cfg library))
         (ext (conf-get cfg 'library-extension "sld"))
         (dest-library-file (path-replace-extension library-file ext))
         (include-files
          (library-include-files cfg (make-path dir library-file)))
         (rewrite-include-files
          ;; Rewrite if any include has the same path as the library
          ;; declaration file after extension renaming.
          ;; TODO: Also rewrite if multiple libs use same file names?
          (map
           (lambda (x)
             (if (equal? x dest-library-file)
                 (cons x (string-append x "." ext))
                 x))
           include-files))
         (install-dir (get-install-source-dir impl cfg)))
    ;; install the library file
    (let ((path (make-path install-dir dest-library-file)))
      (install-directory cfg (path-directory path))
      (if (any pair? rewrite-include-files)
          (install-sexp-file
           cfg
           (library-rewrite-includes library rewrite-include-files)
           path)
          (install-file cfg (make-path dir library-file) path))
      ;; install any includes
      (cons
       path
       (map
        (lambda (x)
          (let ((dest-file
                 (make-path install-dir
                            (path-relative (if (pair? x) (cdr x) x) dir))))
            (install-directory cfg (path-directory dest-file))
            (install-file cfg (if (pair? x) (car x) x) dest-file)
            dest-file))
        rewrite-include-files)))))

;; installers should return the list of installed files
(define (lookup-installer installer)
  (case installer
    (else default-installer)))

(define (install-library impl cfg library dir)
  (let ((installer (lookup-installer (conf-get cfg 'installer))))
    (installer impl cfg library dir)))

(define (build-library impl cfg library dir)
  ;; the currently supported implementations don't require building
  #t)

(define (fetch-package cfg url)
  (call-with-input-url url port->bytevector))

(define (path-strip-top file)
  (let ((pos (string-find file #\/)))
    (if (string-cursor<? pos (string-cursor-end file))
        (substring-cursor file (string-cursor-next file pos))
        file)))

(define (package-maybe-digest-mismatches impl cfg pkg raw)
  (and (not (conf-get cfg 'ignore-digests?))
       (let ((res (package-digest-mismatches cfg pkg raw)))
         (and res
              (not (yes-or-no? cfg "Package checksum mismatches: " res
                               "\nProceed anyway?"))
              res))))

(define (package-maybe-signature-mismatches repo impl cfg pkg raw)
  (and (not (conf-get cfg 'ignore-signature?))
       (let ((res (package-signature-mismatches repo cfg pkg raw)))
         (and res
              (not (yes-or-no? cfg "Package signature mismatches: " res
                               "\nProceed anyway?"))
              res))))

(define (install-package repo impl cfg pkg)
  (let* ((url (package-url repo pkg))
         (raw (fetch-package cfg url)))
    (cond
     ((package-maybe-digest-mismatches impl cfg pkg raw)
      => (lambda (x) (die 2 "package checksum didn't match: " x)))
     ((package-maybe-signature-mismatches repo impl cfg pkg raw)
      => (lambda (x) (die 2 "package signature didn't match: " x)))
     (else
      (let ((snowball (maybe-gunzip raw)))
        (call-with-temp-dir
         "pkg"
         (lambda (dir)
           (tar-extract snowball (lambda (f) (make-path dir (path-strip-top f))))
           (let ((installed-files
                  (append-map
                   (lambda (lib)
                     (build-library impl cfg lib dir)
                     (test-library impl cfg lib dir)
                     (install-library impl cfg lib dir))
                   (package-libraries pkg))))
             (install-package-meta-info
              impl cfg
              `(,@(remove (lambda (x)
                            (and (pair? x) (eq? 'installed-files (car x))))
                          pkg)
                (installed-files ,@installed-files)))))))))))

(define (install-for-implementation repo impl cfg pkgs)
  (for-each (lambda (pkg) (install-package repo impl cfg pkg)) pkgs))

(define (select-best-candidate impl cfg repo candidates)
  (cond
   ((null? (cdr candidates))
    (car candidates))
   (else
    (display "Select a package:\n")
    (let lp ((ls candidates) (i 1))
      (if (pair? ls)
          (let ((pkg (car ls)))
            (display "  ") (display i)
            (display "  ") (display (package-name pkg))
            (display " ") (display (package-version pkg))
            (display " (") (display (package-author repo pkg #t))
            (display ")\n")
            (lp (cdr ls) (+ i 1)))))
    (let ((n (input-number cfg 'candidate-number "Candidate number: "
                           1 1 (length candidates))))
      (list-ref candidates (- n 1))))))

;; Choose packages for the corresponding libraries, and recursively
;; select uninstalled packages.  Verifies and records preferences for
;; trusting publishers for different library prefixes.
(define (expand-package-dependencies repo impl cfg lib-names)
  (let ((current (installed-libraries impl cfg)))
    (let lp ((ls lib-names) (res '()))
      (cond
       ((null? ls) res)
       ((find (lambda (pkg) (package-provides? pkg (car ls))) res)
        (lp (cdr ls) res))
       (else
        (let* ((current-version
                (cond ((assoc (car ls) current)
                       => (lambda (x) (package-version (cdr x))))
                      (else #f)))
               (candidates
                (filter
                 (lambda (pkg)
                   (and (package-provides? pkg (car ls))
                        (or (not current-version)
                            (version>? (package-version pkg)
                                       current-version))))
                 (cdr repo))))
          (cond
           ((and (null? candidates) (assoc (car ls) current))
            (if (member (car ls) lib-names)
                (warn "skipping already installed library" (car ls)))
            (lp (cdr ls) res))
           ((and (null? candidates) (member (car ls) lib-names))
            (die 2 "Can't find package: " (car ls)))
           ((null? candidates)
            (if (yes-or-no? cfg "Can't find package: " (car ls)
                            ".  Proceed anyway?")
                (lp (cdr ls) res)
                (exit 2)))
           (else
            (let ((pkg (select-best-candidate impl cfg repo candidates)))
              (lp (append (package-dependencies pkg) (cdr ls))
                  (cons pkg res)))))))))))

;; First lookup dependencies for all implementations so we can
;; download in a single batch.  Then perform the installations a
;; single implementation at a time.
(define (command/install cfg spec . args)
  (let* ((repo (maybe-update-repository cfg))
         (impls (conf-selected-implementations cfg))
         (impl-cfgs (map (lambda (impl)
                           (conf-for-implementation cfg impl))
                         impls))
         (lib-names (map parse-library-name args))
         (impl-pkgs
          (map (lambda (impl cfg)
                 (expand-package-dependencies repo impl cfg lib-names))
               impls
               impl-cfgs)))
    (for-each
     (lambda (impl cfg pkgs)
       (install-for-implementation repo impl cfg pkgs))
     impls
     impl-cfgs
     impl-pkgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upgrade - upgrade installed packages.

;; With explicit packages same as install, but by default upgrade all
;; available packages.
(define (command/upgrade cfg spec . args)
  (if (pair? args)
      (apply command/install cfg spec args)
      (let* ((repo (maybe-update-repository cfg))
             (impls (conf-selected-implementations cfg))
             (impl-cfgs (map (lambda (impl)
                               (conf-for-implementation cfg impl))
                             impls)))
        (for-each
         (lambda (impl cfg)
           (let ((pkgs (map cdr (installed-libraries impl cfg))))
             (install-for-implementation repo impl cfg pkgs)))
         impls
         impl-cfgs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status - show the status of installed libraries.

(define (command/status cfg spec . args)
  (let* ((impls (conf-selected-implementations cfg))
         (impl-cfgs (map (lambda (impl)
                           (conf-for-implementation cfg impl))
                         impls)))
    (for-each
     (lambda (impl impl-cfg)
       (cond
        ((pair? (cdr impls))
         (if (not (eq? impl (car impls)))
             (display "\n"))
         (display impl)
         (display ":\n")))
       (summarize-libraries
        impl-cfg
        (if (pair? args)
            (lookup-installed-libraries
             impl impl-cfg (map parse-library-name args))
            (installed-libraries impl impl-cfg))))
     impls
     impl-cfgs)))
