;; io.scm -- various input/output utilities
;; Copyright (c) 2010-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define eof
  (call-with-input-string " "
    (lambda (in) (read-char in) (read-char in))))

(define (string-copy! dst start src from to)
  (do ((i from (+ i 1)) (j start (+ j 1)))
      ((>= i to))
    (string-set! dst j (string-ref src i))))

(define (utf8->string vec . o)
  (if (pair? o)
      (let ((start (car o))
            (end (if (pair? (cdr o)) (cadr o) (bytevector-length vec))))
        (utf8->string (subbytes vec start end)))
      (string-copy (utf8->string! vec))))

(define (string->utf8 str . o)
  (if (pair? o)
      (let ((start (car o))
            (end (if (pair? (cdr o)) (cadr o) (string-length str))))
        (string->utf8 (substring str start end)))
      (%string->utf8 str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reading and writing

;; Display @var{str} to the given output port, defaulting to
;; @scheme{(current-output-port)}, followed by a newline.

(define (write-line str . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (display str out)
    (newline out)))

;;> @subsubsubsection{(write-string str [out [start [end]]])}

;;> Writes the characters from @var{start} to @var{end} of string
;;> @var{str} to output port @var{out}, where @var{start} defaults
;;> to 0 and @var{end} defaults to @scheme{(string-length @var{str})}.

(define (write-string str . o)
  (let ((out (if (pair? o) (car o) (current-output-port)))
        (o (if (pair? o) (cdr o) o)))
    (if (pair? o)
        (let ((start (car o))
              (end (if (pair? (cdr o)) (cadr o) (string-length str))))
          (cond-expand
           (string-streams
            (if (zero? start)
                (%write-string str end out)
                (display (substring str start end) out)))
           (else
            (display (substring str start end) out))))
        (display str out))))

;;> @subsubsubsection{(read-line [in [n]])}

;;> Read a line from the input port @var{in}, defaulting to
;;> @scheme{(current-input-port)}, and return the result as
;;> a string not including the newline.  Reads at most @var{n}
;;> characters, defaulting to 8192.

(cond-expand
 ((not string-streams)
  (define (%read-line n in)
    (let ((out (open-output-string)))
      (let lp ()
        (let ((ch (read-char in)))
          (cond
           ((eof-object? ch)
            (let ((res (get-output-string out)))
              (and (not (equal? res "")) res)))
           (else
            (write-char ch out)
            (cond
             ((eqv? ch #\newline)
              (get-output-string out))
             ((eqv? ch #\return)
              (if (eqv? #\newline (peek-char in))
                  (read-char in))
              (get-output-string out))
             (else
              (lp)))))))))))

(define (read-line . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (n (if (and (pair? o) (pair? (cdr o))) (car (cdr o)) 8192)))
    (let ((res (%read-line n in)))
      (port-line-set! in (+ 1 (port-line in)))
      (if (not res)
          eof
          (let ((len (string-length res)))
            (cond
             ((and (> len 0) (eqv? #\newline (string-ref res (- len 1))))
              (if (and (> len 1) (eqv? #\return (string-ref res (- len 2))))
                  (substring res 0 (- len 2))
                  (substring res 0 (- len 1))))
             ((and (> len 0) (eqv? #\return (string-ref res (- len 1))))
              (substring res 0 (- len 1)))
             (else
              res)))))))

;;> @subsubsubsection{(read-string n [in])}

;;> Reads @var{n} characters from input-port @var{in},
;;> defaulting to @scheme{(current-input-port)}, and
;;> returns the result as a string.  Returns @scheme{""}
;;> if @var{n} is zero.  May return a string with fewer
;;> than @var{n} characters if the end of file is reached,
;;> or the eof-object if no characters are available.

(cond-expand
 ((not string-streams)
  (define (%read-string n in)
    (let ((out (open-output-string)))
      (let lp ((i 0))
        (cond ((or (= i n) (eof-object? (peek-char in)))
               (list i (get-output-string out)))
              (else (write-char (read-char in) out) (lp (+ i 1)))))))))

(define (read-string n . o)
  (if (zero? n)
      ""
      (let ((in (if (pair? o) (car o) (current-input-port))))
        (let ((res (%read-string n in)))
          (cond
           ((if (pair? res) (= 0 (car res)) #t)
            eof)
           (else
            (port-line-set! in (+ (string-count #\newline (cadr res) 0)
                                  (port-line in)))
            (cadr res)))))))

;;> @subsubsubsection{(read-string! str n [in])}

;;> Reads @var{n} characters from port @var{in}, which
;;> defaults to @scheme{(current-input-port)}, and writes
;;> them into the string @var{str} starting at index 0.
;;> Returns the number of characters read.
;;> An error is signalled if the length of @var{str} is smaller
;;> than @var{n}.

(cond-expand
 ((not string-streams)
  (define (%read-string! str n in)
    (let lp ((i 0))
      (cond ((or (= i n) (eof-object? (peek-char in))) i)
            (else (string-set! str i (read-char in)) (lp (+ i 1))))))))

(define (read-string! str n . o)
  (if (> n (string-length str))
      (error "string to small to read chars" str n))
  (let* ((in (if (pair? o) (car o) (current-input-port)))
         (res (%read-string! str n in)))
    (port-line-set! in (+ (string-count #\newline str 0 n) (port-line in)))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; higher order port operations

;;> The fundamental port iterator.

(define (port-fold kons knil . o)
  (let ((read (if (pair? o) (car o) read))
        (in (if (and (pair? o) (pair? (cdr o)))
                (car (cdr o))
                (current-input-port))))
    (let lp ((acc knil))
      (let ((x (read in)))
        (if (eof-object? x) acc (lp (kons x acc)))))))

(define (port-fold-right kons knil . o)
  (let ((read (if (pair? o) (car o) read))
        (in (if (and (pair? o) (pair? (cdr o)))
                (car (cdr o))
                (current-input-port))))
    (let lp ()
      (let ((x (read in)))
        (if (eof-object? x) knil (kons x (lp)))))))

(define (port-map fn . o)
  (reverse (apply port-fold (lambda (x ls) (cons (fn x) ls)) '() o)))

(define (port->list read in)
  (port-map (lambda (x) x) read in))

(define (port->sexp-list in)
  (port->list read in))

(define (port->string-list in)
  (port->list read-line in))

(define (port->string in)
  (string-concatenate (port->list (lambda (in) (read-string 1024 in)) in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom port utilities

(define (make-custom-input-port read . o)
  (let ((seek (and (pair? o) (car o)))
        (close (and (pair? o) (pair? (cdr o)) (car (cdr o)))))
    (%make-custom-input-port read seek close)))

(define (make-custom-output-port write . o)
  (let ((seek (and (pair? o) (car o)))
        (close (and (pair? o) (pair? (cdr o)) (car (cdr o)))))
    (%make-custom-output-port write seek close)))

(define (make-null-output-port)
  (make-custom-output-port (lambda (str start end) 0)))

(define (make-broadcast-port . ports)
  (make-custom-output-port
   (lambda (str start end)
     (let ((str (if (zero? start) str (substring str start)))
           (n (- end start)))
       (for-each (lambda (p) (%write-string str n p)) ports)
       n))))

(define (make-filtered-output-port filter out)
  (make-custom-output-port
   (lambda (str start end)
     (let* ((len (string-length str))
            (s1 (if (and (zero? start) (= end len)) str (substring str start end)))
            (s2 (filter s1)))
       (if (string? s2)
           (%write-string s2 (string-length s2) out))))))

(define (make-concatenated-port . ports)
  (make-custom-input-port
   (lambda (str start end)
     (if (null? ports)
         0
         (let ((str (if (zero? start) str (substring str start)))
               (n (- end start)))
           (let lp ((i (read-string! str n (car ports))))
             (cond
              ((>= i n)
               i)
              (else
               (set! ports (cdr ports))
               (cond
                ((null? ports)
                 i)
                (else
                 (let* ((s (read-string (- n i) (car ports)))
                        (len (if (string? s) (string-length s) 0)))
                   (if (and (string? str) (> len 0))
                       (string-copy! str i s 0 len))
                   (lp (+ i len)))))))))))))

(define (make-null-input-port)
  (make-concatenated-port))

(define (make-generated-input-port generator)
  (let ((buf "")
        (len 0)
        (offset 0))
    (make-custom-input-port
     (lambda (str start end)
       (let ((n (- end start)))
         (cond
          ((>= (- len offset) n)
           (string-copy! str start buf offset (+ offset n))
           (set! offset (+ offset n))
           n)
          (else
           (string-copy! str start buf offset len)
           (let lp ((i (+ start (- len offset))))
             (set! buf (generator))
             (cond
              ((not (string? buf))
               (set! buf "")
               (set! len 0)
               (set! offset 0)
               (- i start))
              (else
               (set! len (string-length buf))
               (set! offset 0)
               (cond
                ((>= (- len offset) (- n i))
                 (string-copy! str i buf offset (+ offset (- n i)))
                 (set! offset (+ offset (- n i)))
                 n)
                (else
                 (string-copy! str i buf offset len)
                 (lp (+ i (- len offset)))))))))))))))

(define (make-filtered-input-port filter in)
  (make-generated-input-port
   (lambda ()
     (let ((res (read-string 1024 in)))
       (if (string? res) (filter res) res)))))
