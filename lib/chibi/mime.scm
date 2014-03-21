;; mime.scm -- RFC2045 MIME library
;; Copyright (c) 2005-2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> A library to parse MIME headers and bodies into SXML.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mime-line-length-limit 4096)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; association lists

;;> \procedure{(assq-ref ls key [default])}
;;> Returns the \scheme{cdr} of the cell in \var{ls} whose
;;> \scheme{car} is \scheme{eq?} to \var{key}, or \var{default}
;;> if not found.  Useful for retrieving values associated with
;;> MIME headers.

(define (assq-ref ls key . o)
  (cond ((assq key ls) => cdr) (else (and (pair? o) (car o)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple matching instead of regexps

(define (match-mbox-from-line line)
  (let ((len (string-length line)))
    (and (> len 5)
         (string=? (substring line 0 5) "From ")
         (let lp ((i 6))
           (cond
             ((= i len) (list (substring line 5 len) ""))
             ((memq (string-ref line i) '(#\space #\tab))
              (list (substring line 5 i) (substring line (+ i 1) len)))
             (else (lp (+ i 1))))))))

(define (string-scan-colon-or-maybe-equal str)
  (let ((len (string-length str)))
    (let lp ((i 0) (best #f))
      (if (= i len)
          best
          (let ((c (string-ref str i)))
            (cond ((or (char-alphabetic? c)
                       (char-numeric? c)
                       (memv c '(#\- #\_)))
                   (lp (+ i 1) best))
                  ((eq? c #\:)
                   (if (= i 0) #f i))
                  ((eqv? c #\=)
                   (lp (+ i 1) (or best i)))
                  (else
                   best)))))))

(define (string-skip-white-space str i)
  (let ((lim (string-length str)))
    (let lp ((i i))
      (cond ((>= i lim) lim)
            ((char-whitespace? (string-ref str i)) (lp (+ i 1)))
            (else i)))))

(define (match-mime-header-line line)
  (let ((i (string-scan-colon-or-maybe-equal line)))
    (and i
         (let ((j (string-skip-white-space line (+ i 1))))
           (list (string->symbol (string-downcase-ascii (substring line 0 i)))
                 (substring line j (string-length line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dummy encoder

;; TODO: add conversion routines
(define (ces-convert str . x)
  str)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{RFC2822 Headers}

;;> \procedure{(mime-header-fold kons knil [source [limit [kons-from]]])}
;;>
;;> Performs a fold operation on the MIME headers of source which can be
;;> either a string or port, and defaults to current-input-port.  \var{kons}
;;> is called on the three values:
;;>    \scheme{(kons header value accumulator)}
;;> where accumulator begins with \var{knil}.  Neither the header nor the
;;> value are modified, except wrapped lines are handled for the value.
;;>
;;> The optional procedure \var{kons-from} is a procedure to be called when
;;> the first line of the headers is an "From <address> <date>" line, to
;;> enable this procedure to be used as-is on mbox files and the like.
;;> It defaults to \var{kons}, and if such a line is found the fold will begin
;;> with \scheme{(kons-from '%from <address> (kons-from '%date <date> knil))}.
;;>
;;> The optional \var{limit} gives a limit on the number of headers to read.

(define (mime-header-fold kons knil . o)
  (let ((src (and (pair? o) (car o)))
        (limit (and (pair? o) (pair? (cdr o)) (car (cdr o))))
        (kons-from (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o))) (car (cddr o)) kons)))
    ((if (string? src) mime-header-fold-string mime-header-fold-port)
     kons knil (or src (current-input-port)) limit kons-from)))

(define (mime-header-fold-string kons knil str limit kons-from)
  (call-with-input-string str
    (lambda (in) (mime-header-fold-port kons knil in limit kons-from))))

(define (mime-header-fold-port kons knil port limit kons-from)
  (define (out line acc count)
    (cond
     ((or (and limit (> count limit)) (eof-object? line) (string=? line ""))
      acc)
     ((match-mime-header-line line)
      => (lambda (m) (in (car m) (list (cadr m)) acc (+ count 1))))
     (else
      ;;(warn "invalid header line: ~S\n" line)
      (out (read-line port mime-line-length-limit) acc (+ count 1)))))
  (define (in header value acc count)
    (let ((line (read-line port mime-line-length-limit)))
      (cond
       ((and limit (> count limit))
        acc)
       ((or (eof-object? line) (string=? line ""))
        (kons header (string-join (reverse value)) acc))
       ((char-whitespace? (string-ref line 0))
        (in header (cons line value) acc (+ count 1)))
       (else
        (out line
             (kons header (string-join (reverse value)) acc)
             (+ count 1))))))
  (let ((first-line (read-line port mime-line-length-limit)))
    (cond
     ((eof-object? first-line)
      knil)
     ((and kons-from (match-mbox-from-line first-line))
      => (lambda (m) ; special case check on first line for mbox files
           (out (read-line port mime-line-length-limit)
                (kons-from '%from (car m)
                           (kons-from '%date (cadr m) knil))
                0)))
     (else
      (out first-line knil 0)))))

;;> \procedure{(mime-headers->list [source])}
;;> Return an alist of the MIME headers from source with headers all
;;> downcased.

(define (mime-headers->list . o)
  (reverse
   (apply
    mime-header-fold
    (lambda (h v acc) (cons (cons h v) acc))
    '()
    o)))

(define (mime-split-name+value s)
  (let ((i (string-find s #\=)))
    (if i
        (cons (string->symbol
               (string-downcase-ascii (string-trim (substring s 0 i))))
              (if (= i (string-length s))
                  ""
                  (if (eqv? #\" (string-ref s (+ i 1)))
                      (substring s (+ i 2) (- (string-length s) 1))
                      (substring s (+ i 1) (string-length s)))))
        (cons (string->symbol (string-downcase-ascii (string-trim s))) ""))))

;;> \procedure{(mime-parse-content-type str)}
;;> Parses \var{str} as a Content-Type style-value returning the list
;;> \scheme{(type (attr . val) ...)}.

;;> \example{
;;> (mime-parse-content-type "text/html; CHARSET=UTF-8; filename=index.html")
;;> }

(define (mime-parse-content-type str)
  (let ((res (map mime-split-name+value
                  (string-split str (lambda (ch)
                                      (or (eqv? ch #\;) (eqv? ch #\,)))))))
    (if (and (pair? res) (pair? (car res)) (equal? "" (cdar res)))
        (cons (caar res) (cdr res))
        res)))

;;> \procedure{(mime-decode-header str)}
;;> Replace all occurrences of RFC1522 =?ENC?...?= escapes in \var{str} with
;;> the appropriate decoded and charset converted value.

(define (mime-decode-header str)
  (let* ((len (string-length str))
         (limit (- len 8))) ; need at least 8 chars: "=?Q?X??="
    (let lp ((i 0) (from 0) (res '()))
      (if (>= i limit)
        (string-join (reverse (cons (substring str from len) res)))
        (if (and (eqv? #\= (string-ref str i))
                 (eqv? #\? (string-ref str (+ i 1))))
          (let* ((j (string-find str #\? (+ i 3)))
                 (k (string-find str #\? (+ j 3))))
            (if (and j k (< (+ k 1) len)
                     (eqv? #\? (string-ref str (+ j 2)))
                     (memq (string-ref str (+ j 1)) '(#\Q #\B #\q #\b))
                     (eqv? #\= (string-ref str (+ k 1))))
              (let ((decode (if (memq (string-ref str (+ j 1)) '(#\Q #\q))
                               quoted-printable-decode-string
                               base64-decode-string))
                    (cset (substring str (+ i 2) j))
                    (content (substring str (+ j 3) k))
                    (k2 (+ k 2)))
                (lp k2 k2 (cons (ces-convert (decode content) cset)
                                (cons (substring str from i) res))))
              (lp (+ i 2) from res)))
          (lp (+ i 1) from res))))))

;;> Write out an alist of headers in mime format.

(define (mime-write-headers headers out)
  (for-each
   (lambda (h)
     (display (car h) out) (display ": " out)
     (display (cdr h) out) (display "\r\n" out))
   headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message parsing

(define (mime-read-to-boundary port boundary next final)
  (let ((final-boundary (and boundary (string-append boundary "--"))))
    (let lp ((res '()))
      (let ((line (read-line port mime-line-length-limit)))
        (cond
         ((or (eof-object? line) (equal? line final-boundary))
          (final (string-join (reverse res)
                              (call-with-output-string newline))))
         ((equal? line boundary)
          (next (string-join (reverse res)
                             (call-with-output-string newline))))
         (else
          (lp (cons line res))))))))

(define (mime-convert-part str text? cte enc)
  (let ((res (cond
              ((and (string? cte) (string-ci=? cte "quoted-printable"))
               (if text?
                   (quoted-printable-decode-string str)
                   (quoted-printable-decode-bytevector (string->utf8 str))))
              ((and (string? cte) (string-ci=? cte "base64"))
               (if text?
                   (base64-decode-string str)
                   (base64-decode-bytevector (string->utf8 str))))
              (text?
               str)
              (else
               (string->utf8 str)))))
    (if (string? res) (ces-convert res enc) res)))

(define (mime-read-part port type cte enc boundary next final)
  (let ((text? (and (symbol? type)
                    (string-prefix? "text/" (symbol->string type)))))
    (mime-read-to-boundary
     port boundary
     (lambda (x) (next (mime-convert-part x text? cte enc)))
     (lambda (x) (final (mime-convert-part x text? cte enc))))))

;;> \section{RFC2045 MIME Encoding}

;;> \procedure{(mime-message-fold src kons knil [down up headers])}
;;> Performs a tree fold operation on the given string or port
;;> \var{src} as a MIME body corresponding to the headers give in
;;> \var{headers}.  If \var{headers} are false or not provided they
;;> are first read from \var{src}.
;;>
;;> \var{kons} is called on the successive values:
;;>
;;> \schemeblock{(kons parent-headers part-headers part-body accumulator)}
;;>
;;> where \var{part-headers} are the headers for the given MIME part (the
;;> original headers for single-part MIME), \var{part-body} is the
;;> appropriately decoded and charset-converted body of the message,
;;> and the \var{accumulator} begins with \var{knil}.
;;>
;;> If a multipart body is found, then a tree fold is performed,
;;> calling \var{down} once to get a new accumulator to pass to
;;> \var{kons}, and \var{up} on the result when returning.  Their
;;> signatures are:
;;>
;;> \schemeblock{(down headers seed)}
;;> \schemeblock{(up headers parent-seed seed)}
;;>
;;> The default \var{down} simply returns null, and the default
;;> \var{up} wraps the seed in the following sxml:
;;>
;;> \schemeblock{
;;>  ((mime (@ headers ...)
;;>     seed ...)
;;>   parent-seed ...)
;;> }

(define (mime-message-fold src kons init-seed . o)
  (let ((port (if (string? src) (open-input-string src) src)))
    (let ((kons-down
           (or (and (pair? o) (car o)) (lambda (headers seed) '())))
          (kons-up
           (or (and (pair? o) (pair? (cdr o)) (car (cdr o)))
               (lambda (headers parent-seed seed)
                 `((mime (@ ,@headers)
                         ,@(if (pair? seed) (reverse seed) seed))
                   ,@parent-seed))))
          (headers
           (or (and (pair? o) (pair? (cdr o)) (pair? (cdr (cdr o)))
                    (car (cdr (cdr o))))
               (mime-headers->list port))))
      (let tfold ((parent-headers '())
                  (headers headers)
                  (seed init-seed)
                  (boundary #f)
                  (next (lambda (x) x))
                  (final (lambda (x) x)))
        (let* ((ctype (mime-parse-content-type
                       (assq-ref headers 'content-type "text/plain")))
               (type (car ctype))
               (enc (string-trim
                     (or (assq-ref ctype 'charset)
                         (assq-ref headers 'charset "ascii"))))
               (cte (string-trim
                     (or (assq-ref headers 'content-transfer-encoding)
                         (assq-ref headers 'encoding "7-bit")))))
          (cond
           ((and (symbol? type)
                 (string-prefix? "multipart/" (symbol->string type))
                 (assq-ref (cdr ctype) 'boundary))
            => (lambda (boundary2)
                 (let ((boundary2 (string-append "--" boundary2)))
                   ;; skip preamble
                   (mime-read-to-boundary port boundary2 (lambda (x) x) (lambda (x) x))
                   (let lp ((part-seed (kons-down headers seed)))
                     (let ((part-headers (mime-headers->list port)))
                       (tfold headers part-headers
                              part-seed boundary2
                              lp
                              (lambda (x)
                                ;; skip epilogue
                                (if boundary
                                    (mime-read-to-boundary
                                     port boundary
                                     (lambda (x) x) (lambda (x) x)))
                                (next (kons-up headers seed x)))))))))
           (else
            (mime-read-part
             port type cte enc boundary
             (lambda (x) (next (kons parent-headers headers x seed)))
             (lambda (x) (final (kons parent-headers headers x seed)))))))))))

;;> \procedure{(mime-message->sxml [src [headers]])}
;;> 
;;> Parse the given source as a MIME message and return
;;> the result as an SXML object of the form:
;;> \scheme{(mime (@ (header . value) ...) parts ...)}.

(define (mime-message->sxml . o)
  (car
   (apply
    mime-message-fold
    (if (pair? o) (car o) (current-input-port))
    (lambda (parent-headers headers body seed)
      ;; Discard empty bodies.
      (if (and (equal? body "") (null? headers))
          seed
          `((mime (@ ,@headers) ,body) ,@seed)))
    '() #f #f (if (pair? o) (cdr o) '()))))
