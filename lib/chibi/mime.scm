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
  (cond ((and (pair? ls) (pair? (car ls)) (assq key ls)) => cdr)
        (else (and (pair? o) (car o)))))

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

(define (ces-convert bv . o)
  (let ((enc (if (pair? o) (car o) "utf8")))
    ;; TODO: add conversion routines for non-utf8 encodings
    (utf8->string bv)))

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
  (let ((i (string-find s #\=))
        (start (string-cursor-start s))
        (end (string-cursor-end s)))
    (if (string-cursor<? i end)
        (cons (string->symbol
               (string-downcase-ascii
                (string-trim (substring-cursor s start i))))
              (if (string-cursor=? (string-cursor-next s i) end)
                  ""
                  (if (eqv? #\" (string-cursor-ref s (string-cursor-next s i)))
                      (substring-cursor s
                                        (string-cursor-forward s i 2)
                                        (string-cursor-prev s end))
                      (substring-cursor s (string-cursor-next s i) end))))
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
  (let* ((end (string-cursor-end str))
         ;; need at least 8 chars: "=?Q?X??="
         (limit (string-cursor-back end 8))
         (start (string-cursor-start str)))
    (let lp ((i start) (from start) (res '()))
      (cond
       ((string-cursor>=? i limit)
        (string-join (reverse (cons (substring-cursor str from end) res))))
       ((and (eqv? #\= (string-cursor-ref str i))
             (eqv? #\? (string-cursor-ref str (string-cursor-next str i))))
        (let* ((j (string-find str #\? (string-cursor-forward str i 3)))
               (k (string-find str #\? (string-cursor-forward str j 3))))
          (if (and j k (string-cursor<? (string-cursor-next str k) end)
                   (eqv? #\?
                         (string-cursor-ref str
                                            (string-cursor-forward str j 2)))
                   (memq (string-cursor-ref str (string-cursor-next str j))
                         '(#\Q #\B #\q #\b))
                   (eqv? #\=
                         (string-cursor-ref str (string-cursor-next str k))))
              (let ((decode
                     (if (memq (string-cursor-ref str
                                                  (string-cursor-next str j))
                               '(#\Q #\q))
                         quoted-printable-decode-string
                         base64-decode-string))
                    (cset
                     (substring-cursor str (string-cursor-forward str i 2) j))
                    (content
                     (substring-cursor str (string-cursor-forward str j 3) k))
                    (k2 (string-cursor-forward k 2)))
                (lp k2 k2 (cons (ces-convert (decode content) cset)
                                (cons (substring-cursor str from i) res))))
              (lp (string-cursor-forward str i 2) from res))))
       (else
        (lp (string-cursor-forward str i 1) from res))))))

;;> Write out an alist of headers in mime format.

(define (mime-write-headers headers out)
  (for-each
   (lambda (h)
     (write-string (car h) out) (write-string ": " out)
     (write-string (cdr h) out) (write-string "\r\n" out))
   headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message parsing

(define (read-line/binary in)
  (let ((out (open-output-bytevector)))
    (let lp ()
      (let ((ch (read-u8 in)))
        (cond ((eof-object? ch)
               (let ((res (get-output-bytevector out)))
                 (if (zero? (bytevector-length res))
                     ch
                     res)))
              ((eqv? ch 10)
               (get-output-bytevector out))
              (else
               (write-u8 ch out)
               (lp)))))))

(define (bv-length-before-cr bv)
  (let ((len (bytevector-length bv)))
    (if (and (> len 0) (= 13 (bytevector-u8-ref bv (- len 1))))
        (- len 1)
        len)))

(define (mime-read-to-boundary/binary port boundary next final)
  (let* ((boundary (if (string? boundary) (string->utf8 boundary) boundary))
         (boundary-cr (and boundary (bytevector-append boundary #u8(13))))
         (final-boundary
          (and boundary (bytevector-append boundary #u8(45 45))))
         (final-boundary-cr
          (and final-boundary (bytevector-append final-boundary #u8(13))))
         (out (open-output-bytevector)))
    (let lp ((prev #f))
      (let ((line (read-line/binary port)))
        (cond
         ((or (eof-object? line)
              (equal? line final-boundary)
              (equal? line final-boundary-cr))
          (if prev
              (write-bytevector prev out 0 (bv-length-before-cr prev)))
          (final (get-output-bytevector out)))
         ((or (equal? line boundary) (equal? line boundary-cr))
          (if prev
              (write-bytevector prev out 0 (bv-length-before-cr prev)))
          (next (get-output-bytevector out)))
         (else
          (cond (prev
                 (write-bytevector prev out)
                 (write-u8 10 out)))
          (lp line)))))))

(define (mime-read-to-boundary/text port boundary next final)
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

(define (mime-read-to-boundary port boundary next final)
  ((if (binary-port? port)
       mime-read-to-boundary/binary
       mime-read-to-boundary/text)
   port boundary next final))

(define (mime-convert-part part text? cte enc)
  (let ((res (cond
              ((and (string? cte) (string-ci=? cte "quoted-printable"))
               (if text?
                   (quoted-printable-decode-string part)
                   (quoted-printable-decode-bytevector
                    (if (string? part) (string->utf8 part) part))))
              ((and (string? cte) (string-ci=? cte "base64"))
               (if text?
                   (base64-decode-string part)
                   (base64-decode-bytevector
                    (if (string? part) (string->utf8 part) part))))
              ((and (not text?) (string? part))
               (string->utf8 part))
              (else
               part))))
    (cond
     ((and text? (bytevector? res)) (ces-convert res enc))
     (else res))))

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
                       (flush-output-port (current-error-port))
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
