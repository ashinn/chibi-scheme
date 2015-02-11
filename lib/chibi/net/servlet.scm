;; servlet.scm -- basic web servlets and utilities
;; Copyright (c) 2013-2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Library for http and cgi servlets.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uploads.

(define-record-type Upload
  (make-upload name filename sxml)
  upload?
  (name upload-name upload-name-set!)
  (filename upload-filename upload-filename-set!)
  (sxml upload-sxml upload-sxml-set!))

;; Currently uploads are only represented as inlined strings, but may
;; be saved to temp files in later versions so we provide only this
;; abstract API.

(define (upload-headers upload)
  (cadr (upload-sxml upload)))

(define (upload-content upload)
  (car (cddr (upload-sxml upload))))

(define (upload->string upload)
  (let ((x (upload-content upload)))
    (if (bytevector? x) (utf8->string x) x)))

(define (upload->bytevector upload)
  (let ((x (upload-content upload)))
    (if (string? x) (string->utf8 x) x)))

(define (upload->sexp upload)
  (let* ((in (upload-input-port upload))
         (res (read in)))
    (close-input-port in)
    res))

(define (upload-input-port upload)
  (open-input-string (upload->string upload)))

(define (upload-binary-input-port upload)
  (open-input-bytevector (upload->bytevector upload)))

(define (upload-save upload path)
  (let ((content (upload-content upload)))
    (call-with-output-file path
      (lambda (out)
        (if (string? content)
            (display content out)
            (write-bytevector content out))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requests.

(define-record-type Request
  (%make-request
   method host uri version headers body params uploads in out sock addr status)
  request?
  (method request-method request-method-set!)
  (host request-host request-host-set!)
  (uri request-uri request-uri-set!)
  (version request-version request-version-set!)
  (headers request-headers request-headers-set!)
  (body request-body request-body-set!)
  (params request-params request-params-set!)
  (uploads request-uploads request-uploads-set!)
  (in request-in request-in-set!)
  (out request-out request-out-set!)
  (sock request-sock request-sock-set!)
  (addr request-addr request-addr-set!)
  (status request-status request-status-set!))

(define (request-uri-string request)
  (uri->string (request-uri request)))

(define (request-path request)
  (uri-path (request-uri request)))

(define (copy-request r)
  (%make-request
   (request-method r) (request-host r) (request-uri r) (request-version r)
   (request-headers r) (request-body r) (request-params r) (request-uploads r)
   (request-in r) (request-out r) (request-sock r) (request-addr r)
   (request-status r)))

(define (request-with-uri request uri)
  (let ((request2 (copy-request request))
        (uri (string->path-uri 'http uri)))
    (request-uri-set! request2 uri)
    ;; NOTE: this looses form parameters
    (request-params-set! request2 (uri-query->alist (or (uri-query uri) "")))
    request2))

(define (request-param request name . o)
  (cond ((assoc name (request-params request)) => cdr)
        (else (and (pair? o) (car o)))))

(define (assoc-multi ls key)
  (let lp ((ls ls) (res '()))
    (cond ((not (pair? ls)) (reverse res))
          ((equal? key (caar ls)) (lp (cdr ls) (cons (cdar ls) res)))
          (else (lp (cdr ls)res)))))

(define (request-param-list request name)
  (assoc-multi (request-params request) name))

(define (request-upload request name . o)
  (cond ((assoc name (request-uploads request)) => cdr)
        (else (and (pair? o) (car o)))))

(define (request-upload-list request name)
  (assoc-multi (request-uploads request) name))

(define (make-request method path version in out sock addr)
  (let* ((uri (string->path-uri 'http path))
         (headers (mime-headers->list in))
         (host (get-host uri headers))
         (params (uri-query->alist (or (uri-query uri) ""))))
    (%make-request method host uri version headers #f params '()
                   in out sock addr #f)))

(define (make-cgi-request)
  (let* ((method (or (get-environment-variable "REQUEST_METHOD") "GET"))
         (uri (string->path-uri
               'http (or (get-environment-variable "REQUEST_URI") "")))
         (params (uri-query->alist (or (uri-query uri) "")))
         (headers `((host . ,(or (get-environment-variable "HTTP_HOST")
                                 ""))))
         (host (get-host uri headers))
         (version (or (get-environment-variable "SERVER_PROTOCOL")
                      "HTTP/1.0")))
    (%make-request method host uri version headers #f params '()
                   (current-input-port) (current-output-port) #f #f #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Higher-level utilities.

(define (servlet-write-status out status msg)
  (display "HTTP/1.1 " out)
  (display status out)
  (display " " out)
  (display msg out)
  (display "\r\n" out))

;;> Respond with a numeric status, string message and optional headers.

(define (servlet-respond request status msg . o)
  (cond
   ((not (and (integer? status) (<= 0 status 999)))
    (error "http status must be a 3-digit integer" status))
   ((request-status request)
    (error "can't set servlet status multiple times: "
           (request-status request) status))
   (else
    (request-status-set! request status)
    (let* ((out (request-out request))
           (headers (if (pair? o) (car o) '()))
           (headers
            (cond
             ;; Socket bound, not CGI, send normal status.
             ((request-sock request)
              (servlet-write-status out status msg)
              headers)
             ;; No socket bound, we're in CGI, send status as a header.
             (else
              (let ((str (string-append (number->string status) " " msg)))
                `((Status . ,str)
                  ,@headers))))))
      (mime-write-headers headers out)
      (display "\r\n" out)
      (flush-output-port out)))))

;;> Write the contents of a string to the request.  If no status has
;;> been sent, assume a default of 200.

(define (servlet-write request str . o)
  (if (not (request-status request))
      (apply servlet-respond request 200 "OK" o))
  (display str (request-out request)))

(define (extract-form-data sxml)
  (define (form-data x)
    (and (pair? x) (eq? 'mime (car x))
         (pair? (cdr x)) (pair? (cadr x)) (eq? '@ (car (cadr x)))
         (or (string? (car (cddr x)))
             (bytevector? (car (cddr x))))
         (assq 'content-disposition (cdr (cadr x)))))
  (let lp ((ls sxml) (res '()) (files '()))
    (cond
     ((null? ls)
      (cons (reverse res) (reverse files)))
     ((form-data (car ls))
      => (lambda (x)
           (let ((disp (mime-parse-content-type (cdr x))))
             (cond
              ((and (pair? disp) (assq 'name (cdr disp)))
               => (lambda (y)
                    (let ((name (cdr y))
                          (val (cadr (cdar ls))))
                      (cond
                       ((assq 'filename (cdr disp))
                        => (lambda (z)
                             ;; If it has a filename it's an upload,
                             ;; we take the param value to be the
                             ;; filename, and accumulate the file.
                             (let ((upload (make-upload name (cdr z) (car ls))))
                               (lp (cdr ls)
                                   (cons (cons name (cdr z)) res)
                                   (cons (cons name upload) files)))))
                       (else
                        (lp (cdr ls) (cons (cons name val) res) files))))))
              (else
               (log-warn "ignoring form-data with no name: " x)
               (lp (cdr ls) res files))))))
     (else
      (lp (cdr ls) res files)))))

(define (servlet-parse-body! request)
  (let* ((headers (request-headers request))
         (ctype
          (mime-parse-content-type
           (cond ((assq 'content-type headers) => cdr)
                 (else ""))))
         (in (request-in request)))
    (cond
     ((and (pair? ctype) (eq? 'multipart/form-data (car ctype)))
      (let* ((sxml (mime-message->sxml in headers))
             (vars+files (extract-form-data sxml))
             (vars (append (request-params request) (car vars+files))))
        (request-body-set! request sxml)
        (request-params-set! request vars)
        (request-uploads-set! request (cdr vars+files))))
     ((and (pair? ctype) (eq? 'application/x-www-form-urlencoded (car ctype)))
      (let ((line (read-line in)))
        (request-body-set! request line)
        (if (not (eof-object? line))
            (request-params-set! request
                                 (append (request-params request)
                                         (uri-query->alist line)))))))))

(define (make-status-servlet status msg . o)
  (lambda (cfg request next restart)
    (apply servlet-respond request status msg o)))

(define servlet-bad-request
  (make-status-servlet 400 "Bad request"))

;; Generic interface.

(define servlet-handler (make-parameter #f))

(define (servlet-run servlet)
  (let ((handler (servlet-handler)))
    (cond
     ((procedure? handler)
      ;; A servlet handler has been set, so we're in a persistent server.
      (handler servlet))
     (else
      ;; Otherwise this is basic CGI.
      (let ((cfg (make-conf '() #f #f #f)))
        (let restart ((request (make-cgi-request)))
          (servlet cfg request servlet-bad-request restart)))))))
