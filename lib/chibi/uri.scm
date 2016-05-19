;; Copyright (c) 2009-2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Library for parsing and constructing URI objects.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URI representation

(define-record-type Uri
  (%make-uri scheme user host port path query fragment)
  uri?
  (scheme uri-scheme)
  (user uri-user)
  (host uri-host)
  (port uri-port)
  (path uri-path)
  (query uri-query)
  (fragment uri-fragment))

;;> Accessors for the URI type.
;;/

;;> \procedure{(make-uri scheme [user host port path query fragment])}

(define (make-uri scheme . o)
  (let* ((user (if (pair? o) (car o) #f))
         (o (if (pair? o) (cdr o) '()))
         (host (if (pair? o) (car o) #f))
         (o (if (pair? o) (cdr o) '()))
         (port (if (pair? o) (car o) #f))
         (o (if (pair? o) (cdr o) '()))
         (path (if (pair? o) (car o) #f))
         (o (if (pair? o) (cdr o) '()))
         (query (if (pair? o) (car o) #f))
         (o (if (pair? o) (cdr o) '()))
         (fragment (and (pair? o) (car o))))
    (%make-uri scheme user host port path query fragment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (uri-with-scheme u scheme)
  (%make-uri scheme (uri-user u) (uri-host u) (uri-port u)
             (uri-path u) (uri-query u) (uri-fragment u)))

(define (uri-with-user u user)
  (%make-uri (uri-scheme u) user (uri-host u) (uri-port u)
             (uri-path u) (uri-query u) (uri-fragment u)))

(define (uri-with-host u host)
  (%make-uri (uri-scheme u) (uri-user u) host (uri-port u)
             (uri-path u) (uri-query u) (uri-fragment u)))

(define (uri-with-port u port)
  (%make-uri (uri-scheme u) (uri-user u) (uri-host u) port
             (uri-path u) (uri-query u) (uri-fragment u)))

(define (uri-with-path u path)
  (%make-uri (uri-scheme u) (uri-user u) (uri-host u) (uri-port u)
             path (uri-query u) (uri-fragment u)))

(define (uri-with-query u query)
  (%make-uri (uri-scheme u) (uri-user u) (uri-host u) (uri-port u)
             (uri-path u) query (uri-fragment u)))

(define (uri-with-fragment u fragment)
  (%make-uri (uri-scheme u) (uri-user u) (uri-host u) (uri-port u)
             (uri-path u) (uri-query u) fragment))

;;> Functional updaters - returns a new uri identical to \var{u}
;;> with only the specified field changed.
;;/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing - without :// we just split into scheme & path

(define (char-uri-scheme-unsafe? ch)
  (not (or (char-alphabetic? ch) (char-numeric? ch) (memv ch '(#\+ #\- #\.)))))

;;> Parses a string with a default scheme and returns a new URI
;;> object.  If the string does not begin with a scheme it is take to
;;> be a simple path URI with the default scheme.  This is a
;;> lightweight permissive parser.

(define (string->path-uri scheme str . o)
  (define decode? (and (pair? o) (car o)))
  (define decode (if decode? uri-decode (lambda (x) x)))
  (define decode-query
    (if (and (pair? o) (pair? (cdr o)) (cadr o))
        (if decode? (lambda (q) (uri-query->alist q #t)) uri-query->alist)
        decode))
  (if (uri? str)
      str
      (let* ((start (string-cursor-start str))
             (end (string-cursor-end str))
             (colon0 (string-find str #\:))
             (colon
              (if (string-cursor>=?
                   (string-find str char-uri-scheme-unsafe? start colon0)
                   colon0)
                  colon0
                  end)))
        (if (string-cursor>=? colon end)
            (and scheme
                 (let* ((quest (string-find str #\?))
                        (pound
                         (string-find
                          str
                          #\#
                          (if (string-cursor<? quest end) quest start))))
                   (make-uri scheme #f #f #f
                             (decode
                              (substring-cursor
                               str start (if (string-cursor<? quest end)
                                             quest
                                             pound)))
                             (and (string-cursor<? quest end)
                                  (decode-query
                                   (substring-cursor str (string-cursor-next str quest) pound)))
                             (and (string-cursor<? pound end)
                                  (decode
                                   (substring-cursor str (string-cursor-next str pound) end))))))
            (let ((sc1 (string-cursor-next str colon))
                  (scheme (string->symbol
                           (string-downcase-ascii
                            (substring-cursor str start colon)))))
              (if (string-cursor>=? sc1 end)
                  (make-uri scheme)
                  (if (or (string-cursor>=? (string-cursor-next str sc1) end)
                          (not
                           (and (eqv? #\/ (string-cursor-ref str sc1))
                                (eqv? #\/ (string-cursor-ref str (string-cursor-next str sc1))))))
                      (make-uri scheme #f #f #f (substring-cursor str sc1 end))
                      (if (string-cursor>=? (string-cursor-forward str sc1 2)
                                            end)
                          (make-uri scheme #f "")
                          (let* ((sc2 (string-cursor-forward str sc1 2))
                                 (slash (string-find str #\/ sc2))
                                 (at (string-find-right str #\@ sc2 slash))
                                 (colon3
                                  (string-find
                                   str #\: (if (string-cursor>? at sc2)
                                               at
                                               sc2)
                                   slash))
                                 (quest (string-find str #\? slash))
                                 (pound
                                  (string-find
                                   str #\# (if (string-cursor<? quest end)
                                               quest
                                               slash))))
                            (%make-uri
                             scheme
                             (and (string-cursor>? at sc2)
                                  (decode (substring-cursor str sc2 at)))
                             (decode
                              (substring-cursor
                               str
                               (if (string-cursor>? at sc2) (string-cursor-next str at) sc2)
                               (if (string-cursor<? colon3 slash)
                                   colon3
                                   slash)))
                             (and (string-cursor<? colon3 slash)
                                  (string->number
                                   (substring-cursor str (string-cursor-next str colon3) slash)))
                             (and (string-cursor<? slash end)
                                  (decode
                                   (substring-cursor
                                    str slash (if (string-cursor<? quest end)
                                                  quest
                                                  pound))))
                             (and (string-cursor<? quest end)
                                  (decode-query
                                   (substring-cursor str (string-cursor-next str quest) pound)))
                             (and (string-cursor<? pound end)
                                  (decode
                                   (substring-cursor str (string-cursor-next str pound) end)))
                             ))))))))))

;;> Parses a string and returns a new URI object.  If the string does
;;> not have a scheme, returns false.

(define (string->uri str . o)
  (apply string->path-uri #f str o))

;;> Convert a URI object to a string.  Returns #f if the uri has no scheme.

(define (uri->string uri . o)
  (define encode? (and (pair? o) (car o)))
  (define encode (if encode? uri-encode (lambda (x) x)))
  (if (string? uri)
      uri
      (let ((fragment (uri-fragment uri))
            (query (uri-query uri))
            (path (uri-path uri))
            (port (uri-port uri))
            (host (uri-host uri))
            (user (uri-user uri)))
        (string-append
         (if (and (not host) (memq (uri-scheme uri) '(http https)))
             ""
             (string-append (symbol->string (uri-scheme uri)) ":"))
         (if (or user host port) "//" "")
         (if user (encode user) "") (if user "@" "")
         (or host "")                   ; host shouldn't need encoding
         (if port ":" "") (if port (number->string port) "")
         (if path (encode path) "")
         (if query "?" "")
         (if (pair? query) (uri-alist->query query) (or query ""))
         (if fragment "#" "") (if fragment (encode fragment) "")))))

;;> Returns true iff the given URI string has a scheme.

(define uri-has-scheme?
  (let ((no-scheme (list 'no-scheme)))
    (lambda (url)
      (not (eq? no-scheme (uri-scheme (string->path-uri no-scheme url)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; query encoding and decoding

(define (uri-safe-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (case ch
        ((#\- #\_ #\. #\! #\~ #\* #\' #\( #\)) #t)
        (else #f))))

(define (collect str from to res)
  (if (string-cursor>=? from to)
      res
      (cons (substring-cursor str from to) res)))

;;> \procedure{(uri-encode str [plus?])}

;;> Return the URI encoded version of the string \var{str},
;;> using hex escapes as needed and replacing spaces with "+"
;;> iff the optional argument \var{plus?} is true.

(define (uri-encode str . o)
  (define (encode-1-space ch)
    (if (eqv? ch #\space)
        "+"
        (encode-1-normal ch)))
  (define (encode-1-normal ch)
    (let* ((i (char->integer ch))
           (hex (number->string i 16)))
      (if (< i 16)
          (string-append "%0" hex)
          (string-append "%" hex))))
  (let ((start (string-cursor-start str))
        (end (string-cursor-end str))
        (encode-1 (if (and (pair? o) (car o))
                      encode-1-space
                      encode-1-normal)))
    (let lp ((from start) (to start) (res '()))
      (if (string-cursor>=? to end)
          (if (string-cursor<=? from start)
              str
              (string-concatenate (reverse (collect str from to res))))
          (let* ((ch (string-cursor-ref str to))
                 (next (string-cursor-next str to)))
            (if (uri-safe-char? ch)
                (lp from next res)
                (lp next next (cons (encode-1 ch)
                                    (collect str from to res)))))))))

;;> \procedure{(uri-decode str [plus?])}

;;> Decodes any URI hex escapes in the given string, and
;;> translates any pluses ("+") to space iff the optional
;;> argument \var{plus?} is true.

(define (uri-decode str . o)
  (let ((space-as-plus? (and (pair? o) (car o)))
        (start (string-cursor-start str))
        (end (string-cursor-end str)))
    (let lp ((from start) (to start) (res '()))
      (if (string-cursor>=? to end)
          (if (string-cursor<=? from start)
              str
              (string-concatenate (reverse (collect str from to res))))
          (let* ((ch (string-cursor-ref str to))
                 (next (string-cursor-next str to)))
            (cond
             ((eqv? ch #\%)
              (if (string-cursor>=? next end)
                  (lp next next (collect str from to res))
                  (let ((next2 (string-cursor-next str next)))
                    (if (string-cursor>=? next2 end)
                        (lp next2 next2 (collect str from to res))
                        (let* ((next3 (string-cursor-next str next2))
                               (hex (substring-cursor str next next3))
                               (i (string->number hex 16)))
                          (lp next3 next3 (cons (string (integer->char i))
                                                (collect str from to res))))))))
             ((and space-as-plus? (eqv? ch #\+))
              (lp next next (cons " " (collect str from to res))))
             (else
              (lp from next res))))))))

;;> \procedure{(uri-query->alist str [plus?])}

;;> Parses the query part of a URI as a delimited list of
;;> URI encoded \rawcode{VAR=VALUE} pairs, decodes them and
;;> returns the result as an alist.

(define (uri-query->alist str . o)
  (define (split-char? c) (if (eqv? c #\&) #t (eqv? c #\;)))
  (let ((end (string-cursor-end str))
        (plus? (and (pair? o) (car o))))
    (let lp ((i (string-cursor-start str)) (res '()))
      (if (string-cursor>=? i end)
          (reverse res)
          (let* ((j (string-find str split-char? i))
                 (k (string-find str #\= i j))
                 (cell
                  (if (string-cursor<? k end)
                      (cons (uri-decode (substring-cursor str i k) plus?)
                            (uri-decode (substring-cursor str (string-cursor-next str k) j) plus?))
                      (cons (uri-decode (substring-cursor str i j) plus?) #f))))
            (lp (string-cursor-next str j) (cons cell res)))))))

;;> \procedure{(uri-alist->query ls [plus?])}

;;> The reverse of the above, formats the alist as a URI
;;> query string.

(define (uri-alist->query ls . o)
  (define plus? (and (pair? o) (car o)))
  (define (encode key val res)
    (let ((res (cons (uri-encode key plus?) res)))
      (if val (cons (uri-encode val plus?) (cons "=" res)) res)))
  (if (null? ls)
      ""
      (let lp ((x (car ls)) (ls (cdr ls)) (res '()))
        (let ((res (encode (car x) (cdr x) res)))
          (if (null? ls)
              (string-concatenate (reverse res))
              (lp (car ls) (cdr ls) (cons "&" res)))))))

;;> Returns a new URI from \var{path}, a string or URI object, as
;;> would be interpreted from as a reference from \var{uri}.  Thus if
;;> any components of \var{path} are missing, or if \var{path} is a
;;> raw path, it is taken relative to \var{uri}.

(define (uri-resolve path orig-uri)
  (or (string->uri path)
      (let ((uri (string->uri orig-uri)))
        (if uri
            (uri-with-path
             (uri-with-fragment (uri-with-query uri #f) #f)
             (path-resolve path
                           (if (string-suffix? "/" (uri-path uri))
                               (uri-path uri)
                               (path-directory (uri-path uri)))))
            (path-resolve path orig-uri)))))
