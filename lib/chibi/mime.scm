;; mime.scm -- RFC2045 MIME library
;; Copyright (c) 2005-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> A library to parse MIME headers and bodies into SXML.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mime-line-length-limit 4096)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; association lists

(define (assoc* key ls . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (let lp ((ls ls))
      (cond
        ((null? ls) #f)
        ((eq key (caar ls)) (car ls))
        (else (lp (cdr ls)))))))

(define (assoc-ref ls key . o)
  (let ((default (and (pair? o) (car o)))
        (eq (if (and (pair? o) (pair? (cdr o))) (car (cdr o)) equal?)))
    (cond ((assoc* key ls eq) => cdr)
          (else default))))

;;> @subsubsubsection{@scheme{(mime-ref headers str [default])}}
;;> A case-insensitive @scheme{assoc-ref}.

(define (mime-ref ls key . o)
  (assoc-ref ls key (and (pair? o) (car o)) string-ci=?))

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
           (list (substring line 0 i)
                 (substring line j (string-length line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dummy encoder

(define (ces-convert str . x)
  str)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some srfi-13 & string utils

(define (string-copy! to tstart from . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length from))))
    (let lp ((i start) (j tstart))
      (cond
       ((< i end)
        (string-set! to j (string-ref from i))
        (lp (+ i 1) (+ j 1)))))))

(define (string-concatenate-reverse ls)
  (let lp ((ls ls) (rev '()) (len 0))
    (if (null? ls)
        (let ((res (make-string len)))
          (let lp ((ls rev) (i 0))
            (cond
             ((null? ls)
              res)
             (else
              (string-copy! res i (car ls))
              (lp (cdr ls) (+ i (string-length (car ls))))))))
        (lp (cdr ls) (cons (car ls) rev) (+ len (string-length (car ls)))))))

(define (string-downcase s . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length s))))
    (let* ((len (- end start)) (s2 (make-string len)))
      (let lp ((i start) (j 0))
        (cond
         ((>= i end)
          s2)
         (else
          (string-set! s2 j (char-downcase (string-ref s i)))
          (lp (+ i 1) (+ j 1))))))))

(define (string-char-index str c . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (let lp ((i start))
      (cond
       ((= i end) #f)
       ((eq? c (string-ref str i)) i)
       (else (lp (+ i 1)))))))

(define (string-trim-white-space s)
  (let ((len (string-length s)))
    (let lp ((i 0))
      (cond ((= i len) "")
            ((char-whitespace? (string-ref s i)) (lp (+ i 1)))
            (else
             (let lp ((j (- len 1)))
               (cond ((<= j i) "")
                     ((char-whitespace? (string-ref s j)) (lp (- j 1)))
                     (else (substring s i (+ j 1))))))))))

(define (string-split str ch)
  (let ((len (string-length str)))
    (let lp ((i 0) (res '()))
      (let ((j (string-char-index str ch i)))
        (if j
            (lp (+ j 1) (cons (substring str i j) res))
            (reverse (cons (substring str i len) res)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> @subsubsection{RFC2822 Headers}

;;> @subsubsubsection{@scheme{(mime-header-fold kons knil [source [limit [kons-from]]])}}
;;>
;;> Performs a fold operation on the MIME headers of source which can be
;;> either a string or port, and defaults to current-input-port.  @var{kons}
;;> is called on the three values:
;;>    @scheme{(kons header value accumulator)}
;;> where accumulator begins with @var{knil}.  Neither the header nor the
;;> value are modified, except wrapped lines are handled for the value.
;;>
;;> The optional procedure @var{kons-from} is a procedure to be called when
;;> the first line of the headers is an "From <address> <date>" line, to
;;> enable this procedure to be used as-is on mbox files and the like.
;;> It defaults to @var{kons}, and if such a line is found the fold will begin
;;> with @scheme{(kons-from "%from" <address> (kons-from "%date" <date> knil))}.
;;>
;;> The optional @var{limit} gives a limit on the number of headers to read.

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
        (kons header (string-concatenate-reverse value) acc))
       ((char-whitespace? (string-ref line 0))
        (in header (cons line value) acc (+ count 1)))
       (else
        (out line
             (kons header (string-concatenate-reverse value) acc)
             (+ count 1))))))
  (let ((first-line (read-line port mime-line-length-limit)))
    (cond
     ((eof-object? first-line)
      knil)
     ((and kons-from (match-mbox-from-line first-line))
      => (lambda (m) ; special case check on first line for mbox files
           (out (read-line port mime-line-length-limit)
                (kons-from "%from" (car m)
                           (kons-from "%date" (cadr m) knil))
                0)))
     (else
      (out first-line knil 0)))))

;;> @subsubsubsection{@scheme{(mime-headers->list [source])}}
;;> Return an alist of the MIME headers from source with headers all
;;> downcased.

(define (mime-headers->list . o)
  (reverse
   (apply
    mime-header-fold
    (lambda (h v acc) (cons (cons (string-downcase h) v) acc))
    '()
    o)))

(define (mime-split-name+value s)
  (let ((i (string-char-index s #\=)))
    (if i
      (cons (string-downcase (string-trim-white-space (substring s 0 i)))
            (if (= i (string-length s))
              ""
              (if (eqv? #\" (string-ref s (+ i 1)))
                (substring s (+ i 2) (- (string-length s) 1))
                (substring s (+ i 1) (string-length s)))))
      (cons (string-downcase (string-trim-white-space s)) ""))))

;;> @subsubsubsection{@scheme{(mime-parse-content-type str)}}
;;> Parses @var{str} as a Content-Type style-value returning the list
;;> @scheme{(type (attr . val) ...)}.

;;> @example{
;;> (mime-parse-content-type "text/html; CHARSET=UTF-8; filename=index.html")
;;> }

(define (mime-parse-content-type str)
  (map mime-split-name+value (string-split str #\;)))

;;> @subsubsubsection{@scheme{(mime-decode-header str)}}
;;> Replace all occurrences of RFC1522 =?ENC?...?= escapes in @var{str} with
;;> the appropriate decoded and charset converted value.

(define (mime-decode-header str)
  (let* ((len (string-length str))
         (limit (- len 8))) ; need at least 8 chars: "=?Q?X??="
    (let lp ((i 0) (from 0) (res '()))
      (if (>= i limit)
        (string-concatenate (reverse (cons (substring str from len) res)))
        (if (and (eqv? #\= (string-ref str i))
                 (eqv? #\? (string-ref str (+ i 1))))
          (let* ((j (string-char-index str #\? (+ i 3)))
                 (k (string-char-index str #\? (+ j 3))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message parsing

(define (mime-read-to-boundary port boundary next final)
  (let ((final-boundary (and boundary (string-append boundary "--"))))
    (let lp ((res '()))
      (let ((line (read-line port mime-line-length-limit)))
        (cond
         ((or (eof-object? line) (equal? line final-boundary))
          (final (string-concatenate (reverse res)
                                     (call-with-output-string newline))))
         ((equal? line boundary)
          (next (string-concatenate (reverse res)
                                    (call-with-output-string newline))))
         (else
          (lp (cons line res))))))))

(define (mime-convert-part str cte enc)
  (let ((str (cond
              ((and (string? cte) (string-ci=? cte "quoted-printable"))
               (quoted-printable-decode-string str))
              ((and (string? cte) (string-ci=? cte "base64"))
               (base64-decode-string str))
              (else
               str))))
    (if (string? enc) (ces-convert str enc) str)))

(define (mime-read-part port cte enc boundary next final)
  (mime-read-to-boundary
   port boundary
   (lambda (x) (next (mime-convert-part x cte enc)))
   (lambda (x) (final (mime-convert-part x cte enc)))))

;;> @subsubsection{RFC2045 MIME Encoding}

;;> @subsubsubsection{@scheme{(mime-message-fold src kons knil [start end headers])}}
;;> Performs a fold operation on the given string or port @var{src} as a
;;> MIME body corresponding to the headers give in @var{headers}.  @var{kons}
;;> is called on the successive values:
;;>
;;> @schemeblock{(kons parent-headers part-headers part-body accumulator)}
;;>
;;> where @var{part-headers} are the headers for the given MIME part (the
;;> original headers for single-part MIME), @var{part-body} is the
;;> appropriately decoded and charset-converted body of the message,
;;> and the @var{accumulator} begins with @var{knil}.

(define (mime-message-fold src kons init-seed . o)
  (let ((port (if (string? src) (open-input-string src) src)))
    (let ((kons-start
           (if (pair? o) (car o) (lambda (headers seed) '())))
          (kons-end
           (if (and (pair? o) (pair? (cdr o)))
               (car (cdr o))
               (lambda (headers parent-seed seed)
                 `((mime (^ ,@headers)
                         ,@(if (pair? seed) (reverse seed) seed))
                   ,@parent-seed))))
          (headers
           (if (and (pair? o) (pair? (cdr o)) (pair? (cdr (cdr o))))
               (car (cdr (cdr o)))
               (mime-headers->list port))))
      (let tfold ((parent-headers '())
                  (headers headers)
                  (seed init-seed)
                  (boundary #f)
                  (next (lambda (x) x))
                  (final (lambda (x) x)))
        (let* ((ctype (mime-parse-content-type
                       (mime-ref headers "Content-Type" "text/plain")))
               (type (string-trim-white-space (caar ctype)))
               (enc (string-trim-white-space
                     (or (mime-ref ctype "charset")
                         (mime-ref headers "charset" "ASCII"))))
               (cte (string-trim-white-space
                     (or (mime-ref headers "Content-Transfer-Encoding")
                         (mime-ref headers "Encoding" "7-bit")))))
          (cond
           ((and (string-ci=? type "multipart/")
                 (mime-ref ctype "boundary"))
            => (lambda (boundary2)
                 (let ((boundary2 (string-append "--" boundary2)))
                   ;; skip preamble
                   (mime-read-to-boundary port boundary2 (lambda (x) x) (lambda (x) x))
                   (let lp ((part-seed (kons-start headers seed)))
                     (let ((part-headers (mime-headers->list port)))
                       (tfold parent-headers part-headers
                              part-seed boundary2
                              lp
                              (lambda (x)
                                ;; skip epilogue
                                (if boundary
                                    (mime-read-to-boundary port boundary
                                                           (lambda (x) x) (lambda (x) x)))
                                (next (kons-end headers seed x)))
                              ))))))
           (else
            (mime-read-part
             port cte enc boundary
             (lambda (x) (next (kons parent-headers headers x seed)))
             (lambda (x) (final (kons parent-headers headers x seed)))))))))))

;;> @subsubsubsection{@scheme{(mime-message->sxml [src])}}
;;> 
;;> Parse the given source as a MIME message and return
;;> the result as an SXML object of the form:
;;> @scheme{(mime (^ (header . value) ...) parts ...)}.

(define (mime-message->sxml . o)
  (car
   (apply
    mime-message-fold
    (if (pair? o) (car o) (current-input-port))
    (lambda (parent-headers headers body seed)
      `((mime (^ ,@headers) ,body) ,@seed))
    '()
    (lambda (headers seed) '())
    (lambda (headers parent-seed seed)
      `((mime (^ ,@headers)
              ,@(if (pair? seed) (reverse seed) seed))
        ,@parent-seed))
    (if (pair? o) (cdr o) '()))))
