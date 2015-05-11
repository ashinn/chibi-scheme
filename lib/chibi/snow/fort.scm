
(define (static-url cfg path)
  (make-path "/s" path))

(define (static-local-path cfg path)
  (make-path (conf-get cfg 'doc-root ".") "s" path))

(define (maybe-parse-hex x)
  (if (string? x) (hex-string->bytevector x) x))

(define valid-email?
  ;; Conservatively match local parts allowed by hotmail, removing
  ;; the restriction on ".." as allowed by Japanese phone providers.
  (let ((re (regexp
             '(: (+ (or alphanumeric #\_ #\- #\. #\+ #\= #\& #\'))
                 "@" (+ (or alphanumeric #\_ #\-))
                 (+ "." (+ (or alphanumeric #\_ #\-)))))))
    (lambda (str) (regexp-matches? re str))))

(define (extract-snowball-package bv)
  (define (path-top path)
    (substring path 0 (string-find path #\/)))
  (guard (exn
          (else
           (log-error "couldn't extract package.scm: " exn)
           #f))
    (cond
     ((tar-safe? bv)
      (let* ((files (tar-files bv))
             (dir (path-top (car files)))
             (pkg-path (make-path dir "package.scm")))
        (cond
         ((member pkg-path files)
          (read (open-input-bytevector
                 (tar-extract-file bv pkg-path))))
         (else
          (log-error "no package.scm in " dir)
          #f))))
     (else
      (log-error "tar-bomb")
      #f))))

(define escape-path
  (lambda (str)
    (let ((re (regexp '(w/ascii (~ (or alphanumeric #\_ #\- #\.))))))
      (regexp-replace
       re
       str
       (lambda (m)
         (let ((n (char->integer
                   (string-ref (regexp-match-submatch m 0) 0))))
           (string-append
            "%"
            (if (< n 16) "0" "")
            (number->string n 16))))))))

(define (x->string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        (else (error "not stringable" x))))

(define (email->path str)
  (let ((ls (string-split str #\@)))
    (make-path (escape-path (cadr ls)) (escape-path (car ls)))))

(define (repo-publishers cfg)
  (filter (lambda (x) (and (pair? x) (eq? 'publisher (car x))))
          (cdr (current-repo cfg))))

(define (invalid-signature-reason cfg sig-spec snowball)
  (let* ((digest-name (assoc-get (cdr sig-spec) 'digest #f 'sha-256))
         (digest (assoc-get (cdr sig-spec) digest-name))
         (actual-digest ((lookup-digest digest-name) snowball))
         (sig (assoc-get (cdr sig-spec) 'rsa))
         (email (assoc-get (cdr sig-spec) 'email))
         (rsa-key-sexp (find (rsa-identity=? email)
                             (repo-publishers cfg)))
         (verify-rsa? (conf-get cfg 'verify-signatures?))
         (rsa-key (and verify-rsa?
                       (pair? rsa-key-sexp)
                       (extract-rsa-public-key (cdr rsa-key-sexp)))))
    (cond
     ((not (equal? digest actual-digest))
      (string-append "the " digest-name " digest in the signature <" digest
                     "> didn't match the actual value: <" actual-digest ">"))
     ((and rsa-key-sexp (not rsa-key))
      (string-append "unknown publisher: " email))
     ((and verify-rsa?
           (not (rsa-verify? rsa-key
                             (maybe-parse-hex digest)
                             (maybe-parse-hex sig))))
      (log-error "digest: " digest " sig: " (maybe-parse-hex sig)
                 " verify: " (rsa-encrypt rsa-key digest))
      "rsa signature did not match")
     (else
      #f))))

(define (get-user-password cfg email)
  (let* ((user-dir (static-local-path cfg (email->path email)))
         (key-file (make-path user-dir "pub-key"))
         (key (guard (exn (else #f)) (call-with-input-file key-file read))))
    (and (pair? key) (assoc-get key 'password))))

(define (package-dir email pkg)
  (make-path
   (email->path email)
   (string-join (map escape-path (map x->string (package-name pkg))) "/")
   (escape-path (package-version pkg))))

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

(define (file-lock-loop port-or-fd mode)
  (let lp ()
    (cond
     ((file-lock port-or-fd mode))
     ((memv (errno) '(11 35)) (thread-sleep! 0.01) (lp))
     (else (error "couldn't lock file" (integer->error-string))))))

(define (call-with-locked-file path proc . o)
  (let ((fd (open path
                  (+ open/create open/read-write)
                  (if (pair? o) (car o) #o644))))
    (file-lock-loop fd (+ lock/exclusive lock/non-blocking))
    (exception-protect (proc fd) (file-lock fd lock/unlock))))

;; Rewrites file in place with the result of (proc orig-contents),
;; synchronized with file-lock.
(define (synchronized-rewrite-text-file path proc . o)
  (call-with-locked-file
   path
   (lambda (fd)
     (let* ((in (open-input-file-descriptor fd))
            (out (open-output-file-descriptor fd))
            (str (port->string in))
            (res (proc str)))
       (set-file-position! out seek/set 0)
       (display res out)
       (file-truncate out (string-size res))
       (close-output-port out)
       res))))

(define (synchronized-rewrite-sexp-file path proc . o)
  (apply synchronized-rewrite-text-file
         path
         (lambda (str)
           (let ((x (call-with-input-string str read)))
             (call-with-output-string
               (lambda (out) (write-simple-pretty (proc x) out)))))
         o))

(define (current-repo cfg)
  (call-with-input-file (static-local-path cfg "repo.scm") read))

(define (rewrite-repo cfg proc)
  (synchronized-rewrite-sexp-file
   (static-local-path cfg "repo.scm")
   proc
   "(repository)"))

(define (update-repo cfg rem-pred value)
  (rewrite-repo
   cfg
   (lambda (repo)
     (let ((repo (if (pair? repo) repo '(repository))))
       `(,(car repo) ,value ,@(remove rem-pred (cdr repo)))))))

(define (update-repo-object cfg key-field value)
  (let* ((type (car value))
         (key-value (assoc-get (cdr value) key-field eq?))
         (pred
          (lambda (x)
            (and (pair? x)
                 (eq? type (car x))
                 (equal? key-value (assoc-get (cdr x) key-field eq?))))))
    (update-repo cfg pred value)))

(define (update-repo-package cfg pkg)
  (let* ((email (package-email pkg))
         (auth-pred (lambda (x) (equal? email (package-email x))))
         (pkg-pred
          (cond
           ((package-name pkg)
            => (lambda (name)
                 (lambda (x) (equal? name (package-name x)))))
           (else
            (let ((libs (map (lambda (x) (assoc-get (cdr x) 'name eq?))
                             (package-libraries pkg))))
              (lambda (x)
                (every (lambda (y)
                         (member (assoc-get (cdr x) 'name eq?) libs))
                       (package-libraries x)))))))
         (rem-pred
          (lambda (x)
            (and (pair? x) (eq? 'package (car x))
                 (auth-pred x) (pkg-pred x)))))
    (update-repo cfg rem-pred pkg)))

(define (fail msg . args)
  `(span (@ (style . "background:red")) ,msg ,@args))

(define (page body)
  `(html
    (head
     (title "Snow")
     (meta (@ (charset . "utf-8")))
     (link (@ (type . "text/css")
              (rel . "stylesheet")
              (href . "/s/snow.css")))
     (link (@ (rel . "shortcut icon")
              (href . "/s/favicon.ico"))))
    (body
     (div (@ (id . "head"))
          (div (@ (id . "head_pic")) "☃")
          (div (@ (id . "head_name")) (b "Snow")))
     (div (@ (id . "toolbar"))
          (nav (@ (id . "menu"))
               (a (@ (href . "/")) "Home")
               (a (@ (href . "/pkg")) "Libraries")
               (a (@ (href . "/doc")) "Docs")
               (a (@ (href . "/link")) "Resources")
               (a (@ (href . "/faq")) "FAQ"))
          (div (@ (id . "search"))
               (form
                 (@ (action . "http://www.google.com/search"))
                 (input (@ (type . "text") (name . "q")))
                 (input (@ (type . "hidden")
                           (name . "domains")
                           (value . "snow-fort.org")))
                 (input (@ (type . "hidden")
                           (name . "sitesearch")
                           (value . "snow-fort.org")))
                 (input (@ (type . "submit")
                           (name . "search")
                           (value . "Search Libraries"))))))
     ,body)))

(define (respond cfg request proc)
  (let ((sexp? (equal? "sexp" (request-param request "fmt"))))
    (servlet-write
     request
     (cond
      (sexp?
       (call-with-current-continuation proc))
      (else
       (let ((res (sxml->xml (proc (lambda (x) x)))))
         (servlet-respond request 200 "OK"
                          '((Content-Type . "text/html; charset=utf-8")))
         res))))
    (if sexp? (servlet-write request "\n"))))
