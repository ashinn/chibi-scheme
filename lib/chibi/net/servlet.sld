
(define-library (chibi net servlet)
  (export
   ;; uploads
   upload? upload-name upload-filename
   upload-headers upload->string upload-input-port upload-save
   ;; requests
   request? request-method request-host
   request-uri request-version request-headers request-body request-params
   request-in request-out request-sock request-addr request-param
   request-method-set! request-host-set! request-uri-set!
   request-version-set! request-headers-set! request-body-set!
   request-params-set! request-in-set! request-out-set!
   request-sock-set! request-addr-set!
   request-param request-param-list request-upload request-upload-list
   request-uri-string request-with-uri request-path
   copy-request make-request make-cgi-request
   ;; servlets
   servlet-write servlet-respond servlet-parse-body!
   make-status-servlet servlet-handler servlet-run
   servlet-bad-request)
  (import
   (chibi) (srfi 9) (srfi 39) (srfi 69) (srfi 98)
   (chibi ast) (chibi io) (chibi uri) (chibi mime) (chibi log) (chibi config)
   (chibi filesystem) (chibi net) (chibi net server-util))
  (include "servlet.scm"))
