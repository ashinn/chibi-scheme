;; Copyright (c) 2009-2011 Alex Shinn.  All rights reserved.
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

;;> @subsubsubsection{@scheme{(make-uri scheme [user host port path query fragment])}}

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
         (fragment (if (and (pair? o) (pair? (cdr o))) (car (cdr o)) #f)))
    (%make-uri scheme user host port path query fragment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string utils (don't feel like using SRFI-13 and these are more
;; specialised)

(define (string-scan str ch . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (let lp ((i start))
      (and (< i end)
           (if (eqv? ch (string-ref str i))
               i
               (lp (+ i 1)))))))

(define (string-scan-right str ch . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (let lp ((i (- end 1)))
      (and (>= i start)
           (if (eqv? ch (string-ref str i))
               i
               (lp (- i 1)))))))

(define (string-index-of str pred . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (let lp ((i start))
      (cond ((>= i end) #f)
            ((pred (string-ref str i)) i)
            (else (lp (+ i 1)))))))

(define (string-downcase->symbol str)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (cond
       ((= i len)
        (string->symbol str))
       ((char-upper-case? (string-ref str i))
        (let ((res (make-string len)))
          (do ((j 0 (+ j 1)))
              ((= j i))
            (string-set! res j (string-ref str j)))
          (string-set! res i (char-downcase (string-ref str i)))
          (do ((j (+ i 1) (+ j 1)))
              ((= j len))
            (string-set! res j (char-downcase (string-ref str j))))
          (string->symbol res)))
       (else
        (lp (+ i 1)))))))

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

;;> Functional updaters - returns a new uri identical to @var{u}
;;> with only the specified field changed.
;;/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing - without :// we just split into scheme & path

(define (char-uri-scheme-unsafe? ch)
  (not (or (char-alphabetic? ch) (char-numeric? ch) (memv ch '(#\_ #\-)))))

(define (string->path-uri scheme str . o)
  (define decode? (and (pair? o) (car o)))
  (define decode (if decode? uri-decode (lambda (x) x)))
  (define decode-query
    (if (and (pair? o) (pair? (cdr o)) (cadr o))
        uri-query->alist
        decode))
  (if (pair? str)
      str
      (let* ((len (string-length str))
             (colon0 (string-scan str #\:))
             (colon
              (and (not (string-index-of str char-uri-scheme-unsafe?
                                         0 (or colon0 len)))
                   colon0)))
        (if (or (not colon) (zero? colon))
            (and scheme
                 (let* ((quest (string-scan str #\? 0))
                        (pound (string-scan str #\# (or quest 0))))
                   (make-uri scheme #f #f #f
                             (decode (substring str 0 (or quest pound len)))
                             (and quest
                                  (decode-query
                                   (substring str (+ quest 1) (or pound len))))
                             (and pound
                                  (decode (substring str (+ pound 1) len))))))
            (let ((sc1 (+ colon 1))
                  (scheme (string-downcase->symbol (substring str 0 colon))))
              (if (= sc1 len)
                  (make-uri scheme)
                  (if (or (>= (+ sc1 1) len)
                          (not (and (eqv? #\/ (string-ref str sc1))
                                    (eqv? #\/ (string-ref str (+ sc1 1))))))
                      (make-uri scheme #f #f #f (substring str sc1 len))
                      (if (>= (+ sc1 2) len)
                          (make-uri scheme #f "")
                          (let* ((sc2 (+ sc1 2))
                                 (slash (string-scan str #\/ sc2))
                                 (sc3 (or slash len))
                                 (at (string-scan-right str #\@ sc2 sc3))
                                 (colon3 (string-scan str #\: (or at sc2) sc3))
                                 (quest (string-scan str #\? sc3))
                                 (pound (string-scan str #\# (or quest sc3))))
                            (%make-uri
                             scheme
                             (and at (decode (substring str sc2 at)))
                             (decode
                              (substring str
                                         (if at (+ at 1) sc2)
                                         (or colon3 sc3)))
                             (and colon3
                                  (string->number
                                   (substring str (+ colon3 1) sc3)))
                             (and slash
                                  (decode
                                   (substring str slash (or quest pound len))))
                             (and quest
                                  (decode-query
                                   (substring str (+ quest 1)
                                              (or pound len))))
                             (and pound
                                  (decode (substring str (+ pound 1) len)))
                             ))))))))))

;;> Parses a string and returns a new URI object.

(define (string->uri str . o)
  (apply string->path-uri #f str o))

;;> Convert a URI object to a string.

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
         (symbol->string (uri-scheme uri)) ":"
         (if (or user host port) "//" "")
         (if user (encode user) "") (if user "@" "")
         (or host "")                   ; host shouldn't need encoding
         (if port ":" "") (if port (number->string port) "")
         (if path (encode path) "")
         (if query "?" "")
         (if (pair? query) (uri-alist->query query) (or query ""))
         (if fragment "#" "") (if fragment (encode fragment) "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; query encoding and decoding

(define (uri-safe-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (case ch
        ((#\- #\_ #\. #\! #\~ #\* #\' #\( #\)) #t)
        (else #f))))

(define (collect str from to res)
  (if (>= from to)
      res
      (cons (substring str from to) res)))

;;> @subsubsubsection{@scheme{(uri-encode str [plus?])}}

;;> Return the URI encoded version of the string @var{str},
;;> using hex escapes as needed and replacing spaces with "+"
;;> iff the optional argument @var{plus?} is true.

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
  (let ((start 0)
        (end (string-length str))
        (encode-1 (if (and (pair? o) (car o))
                      encode-1-space
                      encode-1-normal)))
    (let lp ((from start) (to start) (res '()))
      (if (>= to end)
          (if (zero? from)
              str
              (string-concatenate (reverse (collect str from to res))))
          (let* ((ch (string-ref str to))
                 (next (+ to 1)))
            (if (uri-safe-char? ch)
                (lp from next res)
                (lp next next (cons (encode-1 ch)
                                    (collect str from to res)))))))))

;;> @subsubsubsection{@scheme{(uri-decode str [plus?])}}

;;> Decodes any URI hex escapes in the given string, and
;;> translates any pluses ("+") to space iff the optional
;;> argument @var{plus?} is true.

(define (uri-decode str . o)
  (let ((space-as-plus? (and (pair? o) (car o)))
        (start 0)
        (end (string-length str)))
    (let lp ((from start) (to start) (res '()))
      (if (>= to end)
          (if (zero? from)
              str
              (string-concatenate (reverse (collect str from to res))))
          (let* ((ch (string-ref str to))
                 (next (+ to 1)))
            (cond
             ((eqv? ch #\%)
              (if (>= next end)
                  (lp next next (collect str from to res))
                  (let ((next2 (+ next 1)))
                    (if (>= next2 end)
                        (lp next2 next2 (collect str from to res))
                        (let* ((next3 (+ next2 1))
                               (hex (substring str next next3))
                               (i (string->number hex 16)))
                          (lp next3 next3 (cons (string (integer->char i))
                                                (collect str from to res))))))))
             ((and space-as-plus? (eqv? ch #\+))
              (lp next next (cons " " (collect str from to res))))
             (else
              (lp from next res))))))))

;;> @subsubsubsection{@scheme{(uri-query->alist str [plus?])}}

;;> Parses the query part of a URI as a delimited list of
;;> URI encoded @rawcode{VAR=VALUE} pairs, decodes them and
;;> returns the result as an alist.

(define (uri-query->alist str . o)
  (define (split-char? c) (if (eqv? c #\&) #t (eqv? c #\;)))
  (let ((len (string-length str))
        (plus? (and (pair? o) (car o))))
    (let lp ((i 0) (res '()))
      (if (>= i len)
          (reverse res)
          (let* ((j (or (string-index-of str split-char? i) len))
                 (k (string-scan str #\= i j))
                 (cell (if k
                           (cons (uri-decode (substring str i k) plus?)
                                 (uri-decode (substring str (+ k 1) j) plus?))
                           (cons (uri-decode (substring str i j) plus?) #f))))
            (lp (+ j 1) (cons cell res)))))))

;;> @subsubsubsection{@scheme{(uri-alist->query ls [plus?])}}

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
