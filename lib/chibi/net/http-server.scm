;; http-server.scm -- combinator-based http server
;; Copyright (c) 2013-2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Runs an http server listening at the given address, with the given
;;> servlet.
;;>
;;> An servlet is a procedure which takes three arguments: an
;;> \scheme{Http-Request} record, which contains the I/O ports and
;;> parsed request and headers; a \scheme{next} procedure to call the
;;> next available servlet if any, and a \scheme{restart} procedure to
;;> restart the servlets with a new request.

(define (run-http-server listener-or-addr servlet . o)
  (let ((cfg (if (pair? o) (car o) (make-conf '() #f #f #f))))
    (run-net-server
     listener-or-addr
     (command-handler
      (lambda (command ls in out sock addr)
        (cond
         ((= 2 (length ls))
          (let ((request
                 (make-request command (car ls) (cadr ls) in out sock addr)))
            (log-info `(request: ,command ,(car ls) ,(cadr ls)
                                 ,(request-headers request)))
            (protect (exn
                      (else
                       (log-error "internal error: " exn)
                       (servlet-respond request 500 "Internal server error")))
              (let restart ((request request))
                (servlet cfg request servlet-bad-request restart)))))
         (else
          (let ((request (make-request command #f #f in out sock addr)))
            (servlet-respond request 400 "bad request")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Servlets.

(define (http-chain-servlets f . o)
  (let lp ((f f) (ls o))
    (if (pair? ls)
        (let ((g (lp (car ls) (cdr ls))))
          (lambda (cfg request next restart)
            (let ((next2 (lambda (cfg request) (g cfg request next restart))))
              (f cfg request next2 restart))))
        f)))

(define (http-wrap-default servlet)
  (http-chain-servlets servlet http-default-servlet))

(define (http-guard-servlet cfg request next restart)
  (let ((orig-out (request-out request))
        (tmp-out (open-output-string))
        (request2 (copy-request request)))
    (request-out-set! request2 tmp-out)
    (protect (exn (else (servlet-respond request 500 "Interal server error")))
      (next cfg request2)
      (display (get-output-string tmp-out) orig-out))))

(define (http-parse-body-servlet cfg request next restart)
  (let ((request2 (copy-request request)))
    (servlet-parse-body! request2)
    (next cfg request2)))

(define (http-get*-servlet proc)
  (lambda (cfg request next restart)
    (if (memq (request-method request) '(GET POST))
        (proc cfg request next restart)
        (next cfg request))))

;; Generate a simple page listing the linked files in a directory.
(define (send-directory path out)
  (display "<html><body bgcolor=white><pre>\n" out)
  (for-each
   (lambda (file)
     (display "<a href=\"/" out)
     (display (path-normalize (make-path path file)) out)
     (display "\">" out)
     (display file out)
     (display "</a>\n" out))
   (sort (directory-files path)))
  (display "</pre></body></html>\n" out))

;; TODO: If the index-rx is a short list of fixed strings, check
;; individually to avoid the full directory lookup.
(define (find-index-file dir index-rx)
  (and index-rx
       (any (lambda (f) (and (regexp-matches? index-rx f) (make-path dir f)))
            (directory-files dir))))

(define (http-send-directory request path index-rx restart)
  (cond
   ((find-index-file path index-rx)
    => (lambda (index-file)
         ;; Generate and restart a new request with explicit index file.
         (let* ((uri (request-uri request))
                (path2 (make-path (uri-path uri) index-file)))
           (restart
            (request-with-uri request (uri-with-path uri path2))))))
   (else
    (send-directory path (request-out request)))))

(define (http-send-file request path)
  (cond
   ((file-exists? path)
    (servlet-respond request 200 "OK")
    (send-file path (request-out request)))
   (else
    (servlet-respond request 404 "Not Found"))))

(define (http-file-servlet . o)
  (let ((dir (if (pair? o) (car o) "."))
        (index-rx (and (pair? o) (pair? (cdr o)) (cadr o))))
    (http-get*-servlet
     (lambda (cfg request next restart)
       (let ((path (make-path dir (request-path request))))
         (if (file-directory? path)
             (http-send-directory request path index-rx restart)
             (http-send-file request path)))))))

(define (http-procedure-servlet path proc)
  (http-get*-servlet
   (lambda (cfg request next restart)
     (cond
      ((equal? path (path-normalize (request-path request)))
       (servlet-respond request 200 "OK")
       (proc request))
      (else
       (next cfg request))))))

(define (http-regexp-servlet rules get-field)
  (lambda (cfg request next restart)
    (let ((str (get-field request)))
      (let lp ((request request) (ls rules))
        (cond
         ((null? ls)
          (next cfg request))
         ((not (valid-sre? (caar ls)))
          (log-warn "invalid sre: " (caar ls))
          (lp request (cdr ls)))
         ((regexp-matches? (caar ls) str)
          => (lambda (m)
               (let ((next (lambda (cfg request) (lp request (cdr ls)))))
                 ((cdar ls) cfg request next restart))))
         (else
          (lp request (cdr ls))))))))

(define (http-uri-regexp-servlet rules)
  (http-regexp-servlet rules request-uri-string))

(define (http-path-regexp-servlet rules)
  (http-regexp-servlet
   rules
   (lambda (request) (uri-path (request-uri request)))))

(define (http-host-regexp-servlet rules)
  (http-regexp-servlet rules request-host))

(define (http-regexp-replace-servlet rules helper)
  (lambda (cfg request next restart)
    (let ((uri (uri->string (request-uri request))))
      (let lp ((ls rules))
        (if (null? ls)
            (next cfg request)
            (let ((uri2 (regexp-replace (caar ls) uri (cdar ls))))
              (cond
               ((equal? uri uri2)
                (lp (cdr ls)))
               ((string->path-uri uri2)
                => (lambda (uri)
                     (helper (request-with-uri request uri) next restart)))
               (else
                (log-warn "invalid rewritten uri: " uri2)
                (lp (cdr ls))))))))))

(define (http-redirect-servlet rules)
  (http-regexp-replace-servlet
   rules
   (lambda (cfg request next restart)
     (let ((headers `(Location . ,(uri->string (request-uri request)))))
       (servlet-respond request 302 "Found" headers)))))

(define (http-rewrite-servlet rules)
  (http-regexp-replace-servlet
   rules
   (lambda (cfg request next restart) (restart request))))

(define (index-path-map-servlet from to index-rx servlet)
  (http-get*-servlet
   (lambda (cfg request next restart)
     (let* ((path (path-normalize (uri-path (request-uri request))))
            (rel-path (path-relative-to path from)))
       (cond
        (rel-path
         (let* ((local-path (make-path to rel-path))
                (local-path
                 (if (and index-rx (file-directory? local-path))
                     (find-index-file local-path index-rx)
                     local-path)))
           (if (file-exists? local-path)
               (servlet cfg request local-path next restart)
               (servlet-respond request 404 "Not found"))))
        (else
         (next cfg request)))))))

(define (path-map-servlet from to servlet)
  (index-path-map-servlet from to #f servlet))

(define (http-cgi-bin-servlet request local-path next restart)
  (call-with-temp-file "cgi.out"
    (lambda (temp-file out)
      (let ((pid (fork)))
        (cond
         ((zero? pid)
          (duplicate-file-descriptor-to
           (port-fileno (request-in request)) 0)
          (duplicate-file-descriptor-to (port-fileno out) 1)
          (safe-setenv "HTTP_HOST" (request-host request))
          (safe-setenv "REQUEST_URI" (uri->string (request-uri request)))
          (safe-setenv "REQUEST_METHOD"
                       (symbol->string (request-method request)))
          (safe-setenv "QUERY_STRING"
                       (or (uri-query (request-uri request)) ""))
          (let ((res (execute local-path (list local-path))))
            (display "failed to execute program: " (current-error-port))
            (write local-path (current-error-port))
            (display " => " (current-error-port))
            (write res (current-error-port))
            (newline (current-error-port))
            (exit 1)))
         (else
          (let ((status (waitpid pid 0)))
            (cond
             ((negative? (car status))
              (servlet-respond request 500 "Internal server error"))
             (else
              (display "HTTP/1.0 200 OK\r\n" (request-out request))
              (flush-output (request-out request))
              (send-file temp-file (request-out request))
              (close-output-port out))))))))))

(define (http-cgi-bin-dir-servlet local-dir . o)
  (let ((virtual-dir (if (pair? o) (car o) "/cgi-bin")))
    (path-map-servlet
     virtual-dir local-dir
     (lambda (cfg request prog-path next restart)
       (http-cgi-bin-servlet request prog-path next restart)))))

(define (with-add-to-load-path dir thunk)
  (if dir
      (let* ((orig-path (current-module-path))
             (new-path (cons dir (current-module-path))))
        (dynamic-wind (lambda () (current-module-path new-path))
                      thunk
                      (lambda () (current-module-path orig-path))))
      (thunk)))

(define (make-import-environment)
  (let ((env (make-environment)))
    (%import env (current-environment) '(import) #t)
    env))

(define (load-scheme-script path . o)
  (if (and (file-exists? path) (not (file-directory? path)))
      (let ((env (make-import-environment)) 
            (handle #f))
        (protect (exn (else
                       (log-error "failed to load servlet " exn)))
          (let ((e1 (call-with-input-file path read)))
            (cond
             ((not (and (pair? e1) (eq? 'import (car e1))))
              (log-error "not a scheme program (no import): " path))
             (else
              (parameterize ((servlet-handler (lambda (h) (set! handle h))))
                (with-add-to-load-path (and (pair? o) (car o))
                                       (lambda () (load path env))))))))
        (cond ((not (procedure? handle))
               (log-error "no servlet defined in " path)
               (lambda (cfg request next restart)
                 (servlet-respond request 500 "Internal server error")))
              (else handle)))
      (lambda (cfg request next restart)
        (servlet-respond request 404 "Not found"))))

(define load-scheme-script/memoized
  (memoize-file-loader load-scheme-script))

(define (http-scheme-script-dir-servlet local-dir . o)
  (let ((virtual-dir (or (and (pair? o) (car o)) "/"))
        (index-rx (and (pair? o) (pair? (cdr o)) (cadr o))))
    (index-path-map-servlet
     virtual-dir local-dir index-rx
     (lambda (cfg request script-path next restart)
       (let ((servlet (load-scheme-script/memoized script-path local-dir)))
         (servlet cfg request next restart))))))

(define (http-scheme-script-ext-servlet cfg request local-path next restart)
  ((load-scheme-script/memoized local-path) cfg request next restart))

(define (get-ext-servlet x file)
  (if (procedure? x)
      x
      (case x
        ((scheme) http-scheme-script-ext-servlet)
        ((cgi) http-cgi-bin-servlet)
        (else (error "unknown ext servlet" x)))))

(define (http-ext-servlet rules local-dir . o)
  (let ((virtual-dir (if (pair? o) (car o) "/")))
    (path-map-servlet
     virtual-dir local-dir
     (lambda (cfg request local-path next restart)
       (cond
        ((assoc (path-extension local-path) rules)
         => (lambda (cell)
              (let ((name (if (pair? (cdr cell)) (cadr cell) (cdr cell))))
                ((get-ext-servlet name local-path) cfg request local-path next restart))))
        (else
         (next cfg request)))))))

(define (http-default-servlet cfg request next restart)
  (case (request-method request)
    ((HEAD)
     (call-with-temp-file "get.out"
       (lambda (temp-file out)
         (let ((request2 (copy-request request)))
           (request-method-set! request2 'GET)
           (request-out-set! request2 out)
           (restart request2)
           (close-output-port out)
           (call-with-input-file temp-file
             (lambda (in)
               (let* ((status (read-line in))
                      (headers (mime-headers->list in))
                      (out (request-out request)))
                 (display status out)
                 (display "\r\n" out)
                 (mime-write-headers headers out)
                 (display "\r\n" out))))))))
    ((BREW)
     (servlet-respond request 418 "I'm a teapot"))
    (else
     (servlet-bad-request cfg request next restart))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config-based servlets.

;; Utility to wrap SRE rules.
(define (http-wrap-sre-config cfg clear ls)
  (map
   (lambda (x)
     (cond
      ((valid-sre? (car x))
       `(,(regexp (car x))
         . ,(http-config-servlet
             (make-conf (cdr x) (make-conf `((,clear)) cfg #f #f) #f #f))))
      (else
       (log-error "invalid sre in config: " (car x))
       `(,(regexp '(+ (~ any)))
         . ,(lambda (request) (error "unreachable servlet"))))))
   ls))

;; Utility to wrap servlets which take a local path as the first arg.
(define (http-wrap-doc-root f)
  (lambda (args cfg)
    (let* ((root (conf-get cfg 'doc-root "."))
           (local-dir (if (pair? args) (car args) "/"))
           (virtual-dir (if (and (pair? args) (pair? (cdr args)))
                            (cadr args)
                            (make-path "/" local-dir)))
           (args (append (list (make-path root local-dir) virtual-dir)
                         (if (and (pair? args) (pair? (cdr args)))
                             (cddr args)
                             '()))))
      (f args cfg))))

(define (http-config-file-servlet cfg . o)
  (let* ((root (conf-get cfg 'doc-root "."))
         (dir (make-path root (if (pair? o) (car o) ".")))
         (index-rx (if (and (pair? o) (pair? (cdr o)))
                       (cadr o)
                       (conf-get cfg 'index-regexp "index.html"))))
    (http-file-servlet dir index-rx)))

;; Ordered list of servlets to try for a given config.  Generally
;; gives the intuitive order, but manual ordering can be imposed with
;; regexp rules.
(define ordered-config-servlets
  `((redirect . ,(lambda (rules cfg) (http-redirect-servlet rules)))
    (rewrite . ,(lambda (rules cfg) (http-rewrite-servlet rules)))
    (host . ,(lambda (hosts cfg)
               (http-host-regexp-servlet
                (http-wrap-sre-config cfg 'host hosts))))
    (uri . ,(lambda (rules cfg)
              (http-uri-regexp-servlet
               (http-wrap-sre-config cfg 'uri rules))))
    (path . ,(lambda (rules cfg)
               (http-path-regexp-servlet
                (http-wrap-sre-config cfg 'path rules))))
    (file . ,(lambda (x cfg)
               (apply http-config-file-servlet cfg x)))
    (cgi . ,(http-wrap-doc-root
             (lambda (dirs cfg) (apply http-cgi-bin-dir-servlet dirs))))
    (scheme . ,(http-wrap-doc-root
                (lambda (dirs cfg)
                  (let ((local-dir (car dirs))
                        (virtual-dir (cadr dirs))
                        (index-rx (conf-get cfg 'index-regexp "index.scm")))
                    (http-scheme-script-dir-servlet
                     local-dir virtual-dir index-rx)))))
    (ext . ,(lambda (rules cfg)
              (http-ext-servlet rules (conf-get cfg 'doc-root "."))))
    ))

;; Config servlet for an already constructed config.
(define (http-config-conf-servlet cfg . o)
  (http-chain-servlets
   (lambda (orig-cfg request next restart)
     (next cfg request))
   (let lp ((ls ordered-config-servlets))
     (cond
      ((null? ls)
       (if (pair? o)
           (http-chain-servlets (car o) http-default-servlet)
           http-default-servlet))
      ((conf-get-cdr cfg (caar ls))
       => (lambda (x)
            (let ((rest (lp (cdr ls))))
              (if (or (pair? x) (null? x))
                  (http-chain-servlets ((cdar ls) x cfg) rest) 
                  rest))))
      (else
       (lp (cdr ls)))))))

;; Config servlet to load a config from a file.
(define (http-config-servlet-load file . o)
  (let* ((cfg (conf-load file))
         (cfg (if (and (pair? o) (conf? (car o)))
                  (if (and (pair? (cdr o)) (conf? (cadr o)))
                      (conf-append (car o) (conf-append cfg (cadr o)))
                      (conf-append (car o) cfg))
                  cfg)))
    (http-config-conf-servlet cfg (http-config-file-servlet cfg))))

;; Primary config servlet which dispatches on argument type.
(define (http-config-servlet x)
  (cond
   ((procedure? x)
    x)
   ((list? x)
    (http-config-conf-servlet (make-conf x #f #f (current-second))))
   ((string? x)
    (if (file-directory? x)
        (http-file-servlet x)
        (http-config-servlet (make-conf #f #f x -1))))
   ((not (conf? x))
    (error "unknown type for http-config-servlet" x))
   ((and (string? (conf-source x))
         (file-exists? (conf-source x)))
    (let* ((f (conf-source x))
           (mtime (file-modification-time f)))
      ((memoize-file-loader
        (lambda (f) (http-config-servlet-load f #f (conf-parent x)))
        'reloader?: #true
        'cache: `(((,mtime . ,f) . ,x)))
       f)))
   ((and (conf-parent x)
         (string? (conf-source (conf-parent x)))
         (file-exists? (conf-source (conf-parent x))))
    (let* ((f (conf-source (conf-parent x)))
           (mtime (file-modification-time f)))
      ((memoize-file-loader
        (lambda (f) (http-config-servlet-load f (conf-head x) (conf-parent x)))
        'reloader?: #true
        'cache: `(((,mtime . ,f) . ,x)))
       f)))
   (else
    (http-config-conf-servlet x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample main.  In chibi-scheme you can run:
;;
;; chibi-scheme -Rchibi.net.http-config-server -- [<cfg-file-or-directory>]
;;
;; which defaults to serving the current directory on port 8000.

(define (run-app cfg spec . args)
  (define (run cfg servlet)
    (run-http-server (conf-get cfg 'port 8000) servlet cfg))
  (cond
   ((> (length args) 1)
    (error "usage: httpd [<cfg-file-or-directory>]"))
   ((or (null? args) (file-directory? (car args)))
    (let ((dir (if (null? args) "." (car args))))
      (run cfg (http-wrap-default (http-config-file-servlet cfg dir)))))
   (else
    (let* ((cfg-file (car args))
           (last-cfg
            (make-conf `((doc-root . ,(path-directory cfg-file))) #f #f #f))
           (cfg (conf-append cfg (conf-append (conf-load cfg-file) last-cfg))))
      (run cfg (http-config-servlet cfg))))))

(define app-spec
  `(http-config-server
    "Config-based HTTP server"
    (@
     ((port integer)
      (doc-root string)
      (verbose? boolean (#\v "verbose"))))
    ,run-app))

(define (main args) (run-application app-spec args))
