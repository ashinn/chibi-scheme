;;; Copyright (C) William D Clinger (2016). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;; FIXME: a lot of these procedures should do more error checking
;;; up front, instead of letting some other procedure deal with it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following procedures are not part of R7RS (small).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (last-pair x)
  (if (and (pair? x) (pair? (cdr x)))
      (last-pair (cdr x))
      x))

;;; Returns first n elements of the list x.

(define (list-take x n)
  (let loop ((n n)
             (x x)
             (y '()))
    (if (= n 0)
        (reverse y)
        (loop (- n 1) (cdr x) (cons (car x) y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some macros to make textual arguments and optional arguments
;;; less painful.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax %textual->text
  (syntax-rules ()
    ((_ x)
     (if (string? x)
         (string->text x)
         x))
    ((_ x name arg ...)
     (cond ((string? x)
            (string->text x))
           ((text? x)
            x)
           (else
            (complain name arg ...))))))

;;; Several procedures take a first argument that can be either
;;; a text or a string.  They can be written as though the first
;;; argument is always a text:
;;;
;;; (define-textual (f textual args ...) ...)

(define-syntax define-textual
  (syntax-rules ()
    ((_ (f textual arg . args) expr1 expr2 ...)
     (define (f textual arg . args)
       (let ((textual (%textual->text textual 'f textual arg)))
         expr1 expr2 ...)))))

;;; Several procedures take optional start and end arguments
;;; that follow a textual argument.  They can be written as
;;; though the textual argument is always a text, the start
;;; and end arguments are always provided, and the start and
;;; end arguments are always legal:
;;;
;;; (define-textual-start-end (f args ... textual start end)
;;;   ...)

(define-syntax define-textual-start-end
  (syntax-rules ()
    ((_ (f args ... textual start end) expr1 expr2 ...)
     (define f
       ;; Don't change this to letrec or an internal definition,
       ;; because recursive calls should call the version that checks.
       (let ((f
              (lambda (args ... textual start end) expr1 expr2 ...)))
         (case-lambda
          ((args ... textual)
           (let ((text (%textual->text textual f args ... textual)))
             (f args ... text 0 (%text-length text))))
          ((args ... textual start)
           (let* ((text (%textual->text textual f args ... textual start))
                  (n (%text-length text)))
             (if (and (exact-integer? start)
                      (<= 0 start n))
                 (f args ... text start n)
                 (complain 'f args ... textual start))))
          ((args ... textual start end)
           (let* ((text (%textual->text textual f args ... textual start end))
                  (n (%text-length text)))
             (if (and (exact-integer? start)
                      (exact-integer? end)
                      (<= 0 start end n))
                 (f args ... text start end)
                 (complain 'f args ... textual start end))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Predicates
;;;
;;; text? is defined by the kernel

(define (textual? x)
  (or (text? x)
      (string? x)))

(define (textual-null? txt)
  (= 0 (textual-length txt)))

(define-textual-start-end (textual-every pred textual start end)
  (if (= start end)
      #t
      (let ((end-1 (- end 1)))
        (let loop ((i start))
          (if (= i end-1)
              (pred (%text-ref textual i))
              (and (pred (%text-ref textual i))
                   (loop (+ i 1))))))))

(define-textual-start-end (textual-any pred textual start end)
  (let loop ((i start))
    (if (= i end)
        #f
        (or (pred (%text-ref textual i))
            (loop (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constructors
;;;
;;; text-tabulate is defined by the kernel

(define (make-text n c)
  (text-tabulate (lambda (i) c) n))

(define (text . chars)
  (string->text (list->string chars)))

;;; These next two procedures take care to accumulate texts of
;;; the kernel's preferred size, N.

(define text-unfold
  (case-lambda
   ((stop? mapper succ seed)
    (text-unfold stop? mapper succ seed (text) (lambda (x) (text))))
   ((stop? mapper succ seed base)
    (text-unfold stop? mapper succ seed base (lambda (x) (text))))
   ((stop? mapper succ seed base make-final)
    (let* ((txt (%textual->text (if (char? base) (text base) base)
                                'text-unfold
                                stop? mapper succ seed base make-final))
           (k (%text-length txt)))
      (let loop ((k k)
                 (texts (list txt))
                 (chars '())
                 (seed seed))
        (cond ((>= k N)
               (let* ((k/N   (quotient k N))
                      (k     (- k (* k/N N)))
                      (texts (cons (reverse-list->text (list-tail chars k))
                                   texts))
                      (chars (list-take chars k)))
                 (loop k texts chars seed)))
              ((stop? seed)
               (let* ((texts (if (null? chars)
                                 texts
                                 (cons (reverse-list->text chars) texts)))
                      (final (make-final seed))
                      (final (cond ((char? final) (text final))
                                   ((string? final) (string->text final))
                                   ((text? final) final)
                                   (else
                                    (%bad-final 'text-unfold final)))))
                 (textual-concatenate-reverse texts final)))
              (else
               (let ((x (mapper seed)))
                 (cond ((char? x)
                        (loop (+ k 1)
                              texts
                              (cons x chars)
                              (succ seed)))
                       ((string? x)
                        (loop (+ k (string-length x))
                              texts
                              (append (reverse (string->list x)) chars)
                              (succ seed)))
                       ((text? x)
                        (loop (+ k (%text-length x))
                              texts
                              (append (reverse (textual->list x)) chars)
                              (succ seed)))
                       (else
                        (complain 'text-unfold
                                  stop? mapper succ seed
                                  base make-final)))))))))))

(define text-unfold-right
  (case-lambda
   ((stop? mapper succ seed)
    (text-unfold-right stop? mapper succ seed (text) (lambda (x) (text))))
   ((stop? mapper succ seed base)
    (text-unfold-right stop? mapper succ seed base (lambda (x) (text))))
   ((stop? mapper succ seed base make-final)
    (let* ((txt (%textual->text (if (char? base) (text base) base)
                                'text-unfold-right
                                stop? mapper succ seed base make-final))
           (k (%text-length txt)))
      (let loop ((k k)
                 (texts (list txt))
                 (chars '())
                 (seed seed))
        (cond ((>= k N)
               (let* ((k/N   (quotient k N))
                      (k     (- k (* k/N N)))
                      (texts (cons (list->text (list-tail chars k)) texts))
                      (chars (list-take chars k)))
                 (loop k texts chars seed)))
              ((stop? seed)
               (let* ((texts (if (null? chars)
                                 texts
                                 (cons (list->text chars) texts)))
                      (final (make-final seed))
                      (final (cond ((char? final) (text final))
                                   ((string? final) (string->text final))
                                   ((text? final) final)
                                   (else
                                    (%bad-final 'text-unfold-right
                                                final)))))
                 (textual-concatenate (cons final texts))))
              (else
               (let ((x (mapper seed)))
                 (cond ((char? x)
                        (loop (+ k 1)
                              texts
                              (cons x chars)
                              (succ seed)))
                       ((string? x)
                        (loop (+ k (string-length x))
                              texts
                              (append (string->list x) chars)
                              (succ seed)))
                       ((text? x)
                        (loop (+ k (%text-length x))
                              texts
                              (append (textual->list x) chars)
                              (succ seed)))
                       (else
                        (complain 'text-unfold-right
                                  stop? mapper succ seed
                                  base make-final)))))))))))

(define (%bad-final name final)
  (error (string-append (symbol->string name)
                        " : make-final returned illegal value : ")
         final))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conversion
;;;
;;; FIXME: a lot of these could be made more efficient, especially
;;; when a string is passed instead of a text.

(define (textual->text x . rest)
  (cond ((string? x)
         (string->text x))
        ((text? x)
         x)
        ((null? rest)
         (error "illegal argument passed to textual->text : " x))
        (else (apply error rest))))

(define textual->string
  (case-lambda
   ((txt)
    (if (string? txt)
        txt
        (textual->string txt 0 (textual-length txt))))
   ((txt start)
    (if (string? txt)
        (substring txt start (string-length txt))
        (textual->string txt start (textual-length txt))))
   ((txt start end)
    (let* ((txt (%textual->text txt 'textual->string txt start end))
           (n (- end start))
           (s (make-string n)))
      (do ((i start (+ i 1)))
          ((= i end)
           s)
        (string-set! s (- i start) (%text-ref txt i)))))))

(define-textual-start-end (textual->vector txt start end)
  (list->vector (string->list (textual->string (subtext txt start end)))))

(define-textual-start-end (textual->list txt start end)
  (string->list (textual->string (subtext txt start end))))

(define string->text
  (case-lambda
   ((s)
    (%string->text s))
   ((s start)
    (%string->text (substring s start (string-length s))))
   ((s start end)
    (%string->text (substring s start end)))))

(define (vector->text v . start/end)
  (%string->text (list->string (apply vector->list v start/end))))

(define (list->text chars . start/end)
  (apply string->text (list->string chars) start/end))

(define (reverse-list->text chars)
  (string->text (list->string (reverse chars))))

;;; FIXME: if txt is a string, should just call string->utf8

(define-textual-start-end (textual->utf8 txt start end)
  (string->utf8 (textual->string (subtext txt start end))))

(define-textual-start-end (textual->utf16 txt start end)
  (%textual->utf16 txt start end #f))

(define-textual-start-end (textual->utf16be txt start end)
  (%textual->utf16 txt start end 'big))

(define-textual-start-end (textual->utf16le txt start end)
  (%textual->utf16 txt start end 'little))

;;; FIXME: should this check for illegal code points?

(define (%textual->utf16 txt start end endianness)
  (let* ((n (textual-fold (lambda (c n)
                            (cond ((< (char->integer c) #x10000)
                                   (+ n 2))
                                  (else
                                   (+ n 4))))
                          0
                          txt start end))
         (n (if endianness n (+ n 2)))
         (result (make-bytevector n 0))
         (hibits (case endianness
                  ((big) 0)
                  ((little) 1)
                  (else 0)))
         (lobits (- 1 hibits)))
    (if (not endianness)
        (begin (bytevector-u8-set! result 0 #xfe)
               (bytevector-u8-set! result 1 #xff)))
    (let loop ((i start)
               (j (if endianness 0 2)))
      (if (= i end)
          result
          (let* ((c (text-ref txt i))
                 (cp (char->integer c)))
            (cond ((< cp #x10000)
                   (let* ((high (quotient cp 256))
                          (low  (- cp (* 256 high))))
                     (bytevector-u8-set! result (+ j hibits) high)
                     (bytevector-u8-set! result (+ j lobits) low))
                   (loop (+ i 1) (+ j 2)))
                  (else
                   (let* ((k (- cp #x10000))
                          (high-surrogate (+ #xd800 (quotient k 1024)))
                          (low-surrogate  (+ #xdc00 (remainder k 1024)))
                          (high0 (quotient high-surrogate 256))
                          (low0  (- high-surrogate (* 256 high0)))
                          (high1 (quotient low-surrogate 256))
                          (low1  (- low-surrogate  (* 256 high1))))
                     (bytevector-u8-set! result (+ j hibits) high0)
                     (bytevector-u8-set! result (+ j lobits) low0)
                     (bytevector-u8-set! result (+ j 2 hibits) high1)
                     (bytevector-u8-set! result (+ j 2 lobits) low1))
                   (loop (+ i 1) (+ j 4)))))))))

(define utf8->text
  (case-lambda
   ((bv)
    (if (bytevector? bv)
        (string->text (utf8->string bv))
        (complain 'utf8->text bv)))
   ((bv start)
    (if (and (bytevector? bv)
             (exact-integer? start)
             (<= 0 start (bytevector-length bv)))
        (string->text (utf8->string bv start))
        (complain 'utf8->text bv start)))
   ((bv start end)
    (if (and (bytevector? bv)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (bytevector-length bv)))
        (string->text (utf8->string bv start end))
        (complain 'utf8->text bv start end)))))

(define utf16->text
  (case-lambda
   ((bv)
    (if (bytevector? bv)
        (%utf16->text bv 0 (bytevector-length bv) #f)
        (complain 'utf16->text bv)))
   ((bv start)
    (if (and (bytevector? bv)
             (exact-integer? start)
             (<= 0 start (bytevector-length bv)))
        (%utf16->text bv start (bytevector-length bv) #f)
        (complain 'utf16->text bv start)))
   ((bv start end)
    (if (and (bytevector? bv)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (bytevector-length bv)))
        (%utf16->text bv start end #f)
        (complain 'utf16->text bv start end)))))

(define utf16be->text
  (case-lambda
   ((bv)
    (if (bytevector? bv)
        (%utf16->text bv 0 (bytevector-length bv) 'big)
        (complain 'utf16be->text bv)))
   ((bv start)
    (if (and (bytevector? bv)
             (exact-integer? start)
             (<= 0 start (bytevector-length bv)))
        (%utf16->text bv start (bytevector-length bv) 'big)
        (complain 'utf16be->text bv start)))
   ((bv start end)
    (if (and (bytevector? bv)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (bytevector-length bv)))
        (%utf16->text bv start end 'big)
        (complain 'utf16be->text bv start end)))))

(define utf16le->text
  (case-lambda
   ((bv)
    (if (bytevector? bv)
        (%utf16->text bv 0 (bytevector-length bv) 'little)
        (complain 'utf16le->text bv)))
   ((bv start)
    (if (and (bytevector? bv)
             (exact-integer? start)
             (even? start)
             (<= 0 start (bytevector-length bv)))
        (%utf16->text bv start (bytevector-length bv) 'little)
        (complain 'utf16le->text bv start)))
   ((bv start end)
    (if (and (bytevector? bv)
             (exact-integer? start)
             (exact-integer? end)
             (even? start)
             (even? end)
             (<= 0 start end (bytevector-length bv)))
        (%utf16->text bv start end 'little)
        (complain 'utf16le->text bv start end)))))

(define (%utf16->text bv start end endianness)
  (let* ((bom (and (not endianness)
                   (< start end)
                   (let ((byte0 (bytevector-u8-ref bv start))
                         (byte1 (bytevector-u8-ref bv (+ start 1))))
                     (cond ((and (= byte0 #xfe) (= byte1 #xff))
                            'big)
                           ((and (= byte1 #xfe) (= byte0 #xff))
                            'little)
                           (else #f)))))
         (start (if bom (+ start 2) start))
         (endianness (or endianness bom 'big))
         (hibits (if (eq? endianness 'big) 0 1))
         (lobits (- 1 hibits)))
    (text-unfold
     (lambda (i) (>= i end))
     (lambda (i)
       (let* ((high (bytevector-u8-ref bv (+ i hibits)))
              (low  (bytevector-u8-ref bv (+ i lobits)))
              (cp   (if (= high 0) low (+ (* 256 high) low))))
         (cond ((< cp #xd800)
                (integer->char cp))
               ((and (< cp #xdc00)
                     (< (+ i 2) end))
                (let* ((i (+ i 2))
                       (high (bytevector-u8-ref bv (+ i hibits)))
                       (low  (bytevector-u8-ref bv (+ i lobits)))
                       (cp2  (if (= high 0) low (+ (* 256 high) low))))
                  (cond ((<= #xdc00 cp2 #xdfff)
                         (integer->char
                          (+ #x10000
                             (* 1024 (- cp #xd800))
                             (- cp2 #xdc00))))
                        (else
                         (%illegal-utf16 bv (- i 2) cp cp2)))))
               ((< cp #x10000)
                (integer->char cp))
               (else
                (%illegal-utf16 bv i cp)))))
     (lambda (i)
       (let ((cp (+ (* 256 (bytevector-u8-ref bv (+ i hibits)))
                    (bytevector-u8-ref bv (+ i lobits)))))
         (if (or (< cp #xd800)
                 (<= #xe000 cp #xffff))
             (+ i 2)
             (+ i 4))))
     start)))

(define (%illegal-utf16 bv i cp . rest)
  (if (null? rest)
      (error "illegal UTF-16: " bv i cp)
      (error "illegal UTF-16: " bv i cp (car rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Selection
;;;
;;; text-length, text-ref, and subtext are defined by the kernel

(define (textual-length txt)
  (cond ((string? txt)
         (string-length txt))
        ((text? txt)
         (%text-length txt))
        (else
         (complain 'textual-length txt))))

(define (textual-ref txt i)
  (cond ((string? txt)
         (string-ref txt i))
        ((text? txt)
         (%text-ref txt i))
        (else
         (complain 'textual-ref txt))))

(define-textual (subtextual txt start end)
  (subtext txt start end))

;;; FIXME: could be faster, but this procedure shouldn't be used much

(define-textual-start-end (textual-copy text start end)
  (string->text (textual->string text start end)))

(define-textual (textual-take txt nchars)
  (subtextual txt 0 nchars))

(define-textual (textual-drop txt nchars)
  (subtextual txt nchars (%text-length txt)))

(define-textual (textual-take-right txt nchars)
  (let ((n (%text-length txt)))
    (subtextual txt (- n nchars) n)))

(define-textual (textual-drop-right txt nchars)
  (let ((n (%text-length txt)))
    (subtextual txt 0 (- n nchars))))

(define textual-pad
  (case-lambda
   ((txt len)
    (let ((txt (%textual->text txt 'textual-pad txt len)))
      (%text-pad txt len #\space 0 (%text-length txt))))
   ((txt len c)
    (let ((txt (%textual->text txt 'textual-pad txt len c)))
      (%text-pad txt len c 0 (%text-length txt))))
   ((txt len c start)
    (let ((txt (%textual->text txt 'textual-pad txt len c start)))
      (%text-pad txt len c start (%text-length txt))))
   ((txt len c start end)
    (%text-pad (%textual->text txt 'textual-pad txt len c start end)
               len c start end))))

(define (%text-pad txt len c start end)
  (if (and (exact-integer? len)
           (char? c)
           (exact-integer? start)
           (exact-integer? end)
           (<= 0 len)
           (<= 0 start end))
      (let* ((n (%text-length txt))
             (k (- end start)))
        (cond ((not (<= end n))
               (complain 'textual-pad txt len c start end))
              ((= n k len)
               txt)
              ((= k len)
               (if (= n k)
                   txt
                   (subtext txt start end)))
              ((< k len)
               (textual-append (make-text (- len k) c)
                               (if (= n k)
                                   txt
                                   (subtext txt start end))))
              (else
               (subtext txt (- end len) end))))
      (complain 'textual-pad txt len c start end)))

(define textual-pad-right
  (case-lambda
   ((txt len)
    (let ((txt (%textual->text txt 'textual-pad-right txt len)))
      (%text-pad-right txt len #\space 0 (%text-length txt))))
   ((txt len c)
    (let ((txt (%textual->text txt 'textual-pad-right txt len c)))
      (%text-pad-right txt len c 0 (%text-length txt))))
   ((txt len c start)
    (let ((txt (%textual->text txt 'textual-pad-right txt len c start)))
      (%text-pad-right txt len c start (%text-length txt))))
   ((txt len c start end)
    (%text-pad-right (%textual->text txt
                                     'textual-pad-right txt len c start end)
                     len c start end))))

(define (%text-pad-right txt len c start end)
  (if (and (exact-integer? len)
           (char? c)
           (exact-integer? start)
           (exact-integer? end)
           (<= 0 len)
           (<= 0 start end))
      (let* ((n (%text-length txt))
             (k (- end start)))
        (cond ((not (<= end n))
               (complain 'textual-pad-right txt len c start end))
              ((= n k len)
               txt)
              ((= k len)
               (if (= n k)
                   txt
                   (subtext txt start end)))
              ((< k len)
               (textual-append (if (= n k)
                                   txt
                                   (subtext txt start end))
                               (make-text (- len k) c)))
              (else
               (subtext txt start (+ start len)))))
      (complain 'textual-pad-right txt len c start end)))

(define textual-trim
  (case-lambda
   ((txt)
    (textual-trim txt char-whitespace? 0))
   ((txt pred)
    (textual-trim txt pred 0))
   ((txt pred start)
    (let ((txt (%textual->text txt 'textual-trim txt pred start)))
      (%text-trim txt pred start (%text-length txt))))
   ((txt pred start end)
    (let ((txt (%textual->text txt 'textual-trim txt pred start end)))
      (%text-trim txt pred start end)))))

(define (%text-trim txt pred start end)
  (if (and (procedure? pred)
           (exact-integer? start)
           (exact-integer? end)
           (<= 0 start end (%text-length txt)))
      (let loop ((i start))
        (cond ((= i end)
               (text))
              ((pred (%text-ref txt i))
               (loop (+ i 1)))
              (else
               (subtext txt i end))))
      (complain 'textual-trim txt pred start end)))

(define textual-trim-right
  (case-lambda
   ((txt)
    (textual-trim-right txt char-whitespace? 0))
   ((txt pred)
    (textual-trim-right txt pred 0))
   ((txt pred start)
    (let ((txt (%textual->text txt 'textual-trim-right txt pred start)))
      (%text-trim-right txt pred start (%text-length txt))))
   ((txt pred start end)
    (let ((txt (%textual->text txt 'textual-trim-right txt pred start end)))
      (%text-trim-right txt pred start end)))))

(define (%text-trim-right txt pred start end)
  (if (and (procedure? pred)
           (exact-integer? start)
           (exact-integer? end)
           (<= 0 start end (%text-length txt)))
      (let loop ((i (- end 1)))
        (cond ((< i start)
               (text))
              ((pred (%text-ref txt i))
               (loop (- i 1)))
              (else
               (subtext txt start (+ i 1)))))
      (complain 'textual-trim-right txt pred start end)))

(define textual-trim-both
  (case-lambda
   ((txt)
    (textual-trim-both txt char-whitespace? 0))
   ((txt pred)
    (textual-trim-both txt pred 0))
   ((txt pred start)
    (let ((txt (%textual->text txt 'textual-trim-both txt pred start)))
      (%text-trim-both txt pred start (%text-length txt))))
   ((txt pred start end)
    (let ((txt (%textual->text txt 'textual-trim-both txt pred start end)))
      (%text-trim-both txt pred start end)))))

;;; This is efficient because subtext is fast.

(define (%text-trim-both txt pred start end)
  (if (and (procedure? pred)
           (exact-integer? start)
           (exact-integer? end)
           (<= 0 start end (%text-length txt)))
      (textual-trim (textual-trim-right txt pred start end)
                    pred)
      (complain 'textual-trim-both txt pred start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Replacement

(define textual-replace
  (case-lambda
   ((txt1 txt2 start1 end1 start2 end2)
    (textual-append (subtextual txt1 0 start1)
                    (subtextual txt2 start2 end2)
                    (subtextual txt1 end1 (textual-length txt1))))
   ((txt1 txt2 start1 end1 start2)
    (textual-append (subtextual txt1 0 start1)
                    (subtextual txt2 start2 (textual-length txt2))
                    (subtextual txt1 end1 (textual-length txt1))))
   ((txt1 txt2 start1 end1)
    (textual-append (subtextual txt1 0 start1)
                    txt2
                    (subtextual txt1 end1 (textual-length txt1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Comparison

(define (make-nary-comparison name binop0)
  (let ((binop (lambda (a b)
                 (let ((a (%textual->text a name a b))
                       (b (%textual->text b name a b)))
                   (binop0 a b)))))
    (letrec ((loop (lambda (first rest)
                     (cond ((null? rest)
                            #t)
                           ((binop first (car rest))
                            (loop (car rest) (cdr rest)))
                           (else
                            #f)))))
      (lambda (a b . rest)
        (if (null? rest)
            (binop a b)
            (and (binop a b)
                 (loop b rest)))))))

(define textual=?
  (make-nary-comparison 'textual=?
                        (lambda (a b)
                          (%text-compare a b =))))

(define textual<?
  (make-nary-comparison 'textual<?
                        (lambda (a b)
                          (%text-compare a b <))))

(define textual<=?
  (make-nary-comparison 'textual<=?
                        (lambda (a b)
                          (%text-compare a b <=))))

(define textual>?
  (make-nary-comparison 'textual>?
                        (lambda (a b)
                          (%text-compare a b >))))

(define textual>=?
  (make-nary-comparison 'textual>=?
                        (lambda (a b)
                          (%text-compare a b >=))))

(define textual-ci=?
  (make-nary-comparison 'textual-ci=?
                        (lambda (a b)
                          (%text-compare-ci a b = string-ci=?))))

(define textual-ci<?
  (make-nary-comparison 'textual-ci<?
                        (lambda (a b)
                          (%text-compare-ci a b < string-ci<?))))

(define textual-ci<=?
  (make-nary-comparison 'textual-ci<=?
                        (lambda (a b)
                          (%text-compare-ci a b <= string-ci<=?))))

(define textual-ci>?
  (make-nary-comparison 'textual-ci>?
                        (lambda (a b)
                          (%text-compare-ci a b > string-ci>?))))

(define textual-ci>=?
  (make-nary-comparison 'textual-ci>=?
                        (lambda (a b)
                          (%text-compare-ci a b >= string-ci>=?))))

;;; Compares texts a and b.
;;; Determines whether a is less than b (-1), equal (0), or
;;; greater than b (+1), computes the boolean result by
;;; calling make-boolean on that numerical value and 0.

(define (%text-compare a b make-boolean)
  (let* ((na (%text-length a))
         (nb (%text-length b))
         (n (if (<= na nb) na nb)))
    (define (loop i)
      (if (= i n)
          (cond ((< na nb) (make-boolean -1 0))
                ((> na nb) (make-boolean +1 0))
                (else (make-boolean 0 0)))
          (let ((ca (%text-ref a i))
                (cb (%text-ref b i)))
            (cond ((char<? ca cb) (make-boolean -1 0))
                  ((char>? ca cb) (make-boolean +1 0))
                  (else (loop (+ i 1)))))))
    (loop 0)))

;;; Compares texts a and b, folding case.
;;; If either text contains non-ASCII characters, both are converted
;;; to strings and compared using string-pred.

(define (%text-compare-ci a b make-boolean string-pred)
  (let* ((na (%text-length a))
         (nb (%text-length b))
         (n (if (<= na nb) na nb)))
    (define (loop i)
      (if (= i n)
          (cond ((< na nb) (make-boolean -1 0))
                ((> na nb) (make-boolean +1 0))
                (else (make-boolean 0 0)))
          (let ((ca (%text-ref a i))
                (cb (%text-ref b i)))
            (if (or (char>? ca #\delete)
                    (char>? cb #\delete))
                (string-pred (textual->string a)
                             (textual->string b))
                (let ((ca (char-foldcase ca))
                      (cb (char-foldcase cb)))
                  (cond ((char<? ca cb) (make-boolean -1 0))
                        ((char>? ca cb) (make-boolean +1 0))
                        (else (loop (+ i 1)))))))))
    (loop 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Prefixes & suffixes

;;; FIXME: this is a prototype of how optional arguments should
;;; be handled.

(define (%make-text-prefix/suffix-proc proc name)
  (case-lambda
   (()
    (complain name))
   ((x)
    (complain name x))
   ((t1 t2)
    (let ((txt1 (%textual->text t1 name t1 t2))
          (txt2 (%textual->text t2 name t1 t2)))
      (proc txt1 txt2 0 (%text-length txt1) 0 (%text-length txt2))))
   ((t1 t2 start1)
    (let* ((txt1 (%textual->text t1 name t1 t2))
           (txt2 (%textual->text t2 name t1 t2))
           (n1 (%text-length txt1)))
      (if (and (exact-integer? start1)
               (<= 0 start1 n1))
          (proc txt1 txt2 start1 n1 0 (%text-length txt2))
          (complain name t1 t2 start1))))
   ((t1 t2 start1 end1)
    (let* ((txt1 (%textual->text t1 name t1 t2))
           (txt2 (%textual->text t2 name t1 t2))
           (n1 (%text-length txt1)))
      (if (and (exact-integer? start1)
               (exact-integer? end1)
               (<= 0 start1 end1 n1))
          (proc txt1 txt2 start1 end1 0 (%text-length txt2))
          (complain name t1 t2 start1 end1))))
   ((t1 t2 start1 end1 start2)
    (let* ((txt1 (%textual->text t1 name t1 t2))
           (txt2 (%textual->text t2 name t1 t2))
           (n1 (%text-length txt1))
           (n2 (%text-length txt2)))
      (if (and (exact-integer? start1)
               (exact-integer? end1)
               (exact-integer? start2)
               (<= 0 start1 end1 n1)
               (<= 0 start2 n2))
          (proc txt1 txt2 start1 end1 start2 n2)
          (complain name t1 t2 start1 end1 start2))))
   ((t1 t2 start1 end1 start2 end2)
    (let* ((txt1 (%textual->text t1 name t1 t2))
           (txt2 (%textual->text t2 name t1 t2))
           (n1 (%text-length txt1))
           (n2 (%text-length txt2)))
      (if (and (exact-integer? start1)
               (exact-integer? end1)
               (exact-integer? start2)
               (exact-integer? end2)
               (<= 0 start1 end1 n1)
               (<= 0 start2 end2 n2))
          (proc txt1 txt2 start1 end1 start2 end2)
          (complain name t1 t2 start1 end1 start2 end2))))
   ((t1 t2 start1 end1 start2 end2 oops . rest)
    (apply complain name t1 t2 start1 end1 start2 end2 oops rest))))

(define textual-prefix-length
  (%make-text-prefix/suffix-proc
   (lambda (txt1 txt2 start1 end1 start2 end2)
     (%text-prefix-length txt1 txt2 start1 end1 start2 end2))
   'textual-prefix-length))

(define textual-suffix-length
  (%make-text-prefix/suffix-proc
   (lambda (txt1 txt2 start1 end1 start2 end2)
     (%text-suffix-length txt1 txt2 start1 end1 start2 end2))
   'textual-suffix-length))

(define textual-prefix?
  (%make-text-prefix/suffix-proc
   (lambda (txt1 txt2 start1 end1 start2 end2)
     (%text-prefix? txt1 txt2 start1 end1 start2 end2))
   'textual-prefix?))

(define textual-suffix?
  (%make-text-prefix/suffix-proc
   (lambda (txt1 txt2 start1 end1 start2 end2)
     (%text-suffix? txt1 txt2 start1 end1 start2 end2))
   'textual-suffix?))

;;; All error checking has already been done.

(define (%text-prefix-length txt1 txt2 start1 end1 start2 end2)
  (let* ((k1   (- end1 start1))
         (k2   (- end2 start2))
         (k    (min k1 k2))
         (end1 (+ start1 k)))
    (let loop ((i start1)
               (j start2))
      (cond ((= i end1) k)
            ((char=? (%text-ref txt1 i) (%text-ref txt2 j))
             (loop (+ i 1) (+ j 1)))
            (else (- i start1))))))

(define (%text-suffix-length txt1 txt2 start1 end1 start2 end2)
  (let* ((k1     (- end1 start1))
         (k2     (- end2 start2))
         (k      (min k1 k2))
         (start1 (- end1 k)))
    (let loop ((i (- end1 1))
               (j (- end2 1)))
      (cond ((< i start1) k)
            ((char=? (%text-ref txt1 i) (%text-ref txt2 j))
             (loop (- i 1) (- j 1)))
            (else (- end1 i 1))))))

(define (%text-prefix? txt1 txt2 start1 end1 start2 end2)
  (let ((k1 (- end1 start1))
        (k2 (- end2 start2)))
    (and (<= k1 k2)
         (= k1 (%text-prefix-length txt1 txt2 start1 end1 start2 end2)))))

(define (%text-suffix? txt1 txt2 start1 end1 start2 end2)
  (let ((k1 (- end1 start1))
        (k2 (- end2 start2)))
    (and (<= k1 k2)
         (= k1 (%text-suffix-length txt1 txt2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Searching

(define-textual (textual-index txt pred . rest)
  (let ((start (if (null? rest) 0 (car rest)))
        (end (if (or (null? rest) (null? (cdr rest)))
                 (%text-length txt)
                 (car (cdr rest)))))
    (if (and (procedure? pred)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (%text-length txt)))
        (let loop ((i start))
          (cond ((= i end)
                 #f)
                ((pred (%text-ref txt i))
                 i)
                (else
                 (loop (+ i 1)))))
        (apply complain 'textual-index txt pred rest))))

(define-textual (textual-index-right txt pred . rest)
  (let ((start (if (null? rest) 0 (car rest)))
        (end (if (or (null? rest) (null? (cdr rest)))
                 (%text-length txt)
                 (car (cdr rest)))))
    (if (and (procedure? pred)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (%text-length txt)))
        (let loop ((i (- end 1)))
          (cond ((< i start)
                 #f)
                ((pred (%text-ref txt i))
                 i)
                (else
                 (loop (- i 1)))))
        (apply complain 'textual-index-right txt pred rest))))

(define (textual-skip txt pred . rest)
  (apply textual-index txt (lambda (x) (not (pred x))) rest))

(define (textual-skip-right txt pred . rest)
  (apply textual-index-right txt (lambda (x) (not (pred x))) rest))

(define (textual-contains t1 t2 . rest0)
  (let* ((txt1 (%textual->text t1 'textual-contains t1 t2))
         (txt2 (%textual->text t2 'textual-contains t1 t2))
         (rest rest0)
         (start1 (if (null? rest) 0 (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (end1 (if (null? rest) (%text-length txt1) (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (start2 (if (null? rest) 0 (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (end2 (if (null? rest) (%text-length txt2) (car rest)))
         (rest (if (null? rest) rest (cdr rest))))
    (if (and (null? rest)
             (exact-integer? start1)
             (exact-integer? end1)
             (exact-integer? start2)
             (exact-integer? end2)
             (<= 0 start1 end1 (%text-length txt1))
             (<= 0 start2 end2 (%text-length txt2)))
        (%textual-contains txt1 txt2 start1 end1 start2 end2)
        (apply complain 'textual-contains t1 t2 rest0))))

;;; No checking needed here.
;;;
;;; Naive search works well when
;;;     txt1 is very short
;;;     txt2 is very short
;;;     txt2 is almost as long as txt1
;;; Boyer-Moore-Horspool search works well when
;;;     txt2 is very short
;;;     txt1 is considerably longer than txt2, txt2 is not too short,
;;;         and the rightmost character of txt2 is distinct from
;;;         (in its low 8 bits) from several characters that precede it
;;; Rabin-Karp works reasonably well all the time, so is used when
;;;     neither naive search nor Boyer-Moore-Horspool do well

(define %threshold:short1 10)   ; is txt1 shorter than this?
(define %threshold:short2 3)    ; is txt2 shorter than this?
(define %threshold:longer 1)    ; is txt1 at least this much longer?
(define %threshold:rightmost 2) ; are rightmost characters the same?

(define (%textual-contains txt1 txt2 start1 end1 start2 end2)
  (let ((n1 (- end1 start1))
        (n2 (- end2 start2)))
    (cond ((< n1 %threshold:short1)
           (%textual-contains:naive txt1 txt2 start1 end1 start2 end2))
          ((< (- n1 n2) %threshold:longer)
           (%textual-contains:naive txt1 txt2 start1 end1 start2 end2))
          ((< n2 %threshold:short2)
           (%textual-contains:boyer-moore txt1 txt2 start1 end1 start2 end2))
          ((and (> n2 %threshold:rightmost)
                (let ((j (remainder (char->integer (text-ref txt2 (- end2 1)))
                                    128)))
                  (let loop ((i (- end2 %threshold:rightmost)))
                    (cond ((= i (- end2 1))
                           #t)
                          ((= j
                              (remainder (char->integer (text-ref txt2 i))
                                         128))
                           #f)
                          (else
                           (loop (+ i 1)))))))
           (%textual-contains:boyer-moore txt1 txt2 start1 end1 start2 end2))
          (else
           (%textual-contains:rabin-karp txt1 txt2 start1 end1 start2 end2)))))

(define (%textual-contains:naive txt1 txt2 start1 end1 start2 end2)
  (let* ((n1 (- end1 start1))
         (n2 (- end2 start2))
         (lim1 (- end1 n2)))
    (let loop ((i start1))
      (cond ((> i lim1)
             #f)
            ((textual-prefix? txt2 txt1 start2 end2 i end1)
             i)
            (else
             (loop (+ i 1)))))))

(define (%textual-contains:rabin-karp txt1 txt2 start1 end1 start2 end2)
  (define (hash txt start end)
    (do ((i start (+ i 1))
         (h 0 (+ h (char->integer (text-ref txt i)))))
        ((= i end)
         h)))
  (let* ((n1 (- end1 start1))
         (n2 (- end2 start2))
         (lim1 (- end1 n2))
         (h1 (hash txt1 start1 (min (+ start1 n2) end1)))
         (h2 (hash txt2 start2 end2)))
    (let loop ((i start1)
               (h1 h1))
      (cond ((> i lim1)
             #f)
            ((and (= h1 h2)
                  (textual-prefix? txt2 txt1 start2 end2 i end1))
             i)
            ((= i lim1)
             #f)
            (else
             (loop (+ i 1)
                   (+ (- h1 (char->integer (text-ref txt1 i)))
                      (char->integer (text-ref txt1 (+ i n2))))))))))

;;; This is actually the Boyer-Moore-Horspool algorithm,
;;; but the name is already pretty long.

(define (%textual-contains:boyer-moore txt1 txt2 start1 end1 start2 end2)
  (if (= start2 end2)
      start1
      (let* ((n1 (- end1 start1))
             (n2 (- end2 start2))
             (lim1 (- end1 n2))
             (lastchar (text-ref txt2 (- end2 1)))
             (lastj (remainder (char->integer lastchar) 128))
             (table (make-vector 128 n2)))
        (do ((i 0 (+ i 1)))
            ((>= i (- n2 1)))
          (let* ((c  (text-ref txt2 (+ i start2)))
                 (cp (char->integer c))
                 (j  (remainder cp 128)))
            (vector-set! table j (- n2 i 1))))
        (let loop ((i start1))
          (if (>= i lim1)
              (if (and (= i lim1)
                       (textual-prefix? txt2 txt1 start2 end2 i end1))
                  i
                  #f)
              (let* ((c  (text-ref txt1 (+ i n2 -1)))
                     (cp (char->integer c))
                     (j  (remainder cp 128)))
                (cond ((not (char=? c lastchar))
                       (loop (+ i (vector-ref table j))))
                      ((textual-prefix? txt2 txt1 start2 end2 i end1)
                       i)
                      (else
                       (loop (+ i (vector-ref table lastj)))))))))))

;;; FIXME: no Rabin-Karp algorithm for now

(define (textual-contains-right t1 t2 . rest0)
  (let* ((txt1 (%textual->text t1 'textual-contains-right t1 t2))
         (txt2 (%textual->text t2 'textual-contains-right t1 t2))
         (rest rest0)
         (start1 (if (null? rest) 0 (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (end1 (if (null? rest) (%text-length txt1) (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (start2 (if (null? rest) 0 (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (end2 (if (null? rest) (%text-length txt2) (car rest)))
         (rest (if (null? rest) rest (cdr rest))))
    (if (and (null? rest)
             (exact-integer? start1)
             (exact-integer? end1)
             (exact-integer? start2)
             (exact-integer? end2)
             (<= 0 start1 end1 (%text-length txt1))
             (<= 0 start2 end2 (%text-length txt2)))
        (%textual-contains-right txt1 txt2 start1 end1 start2 end2)
        (apply complain 'textual-contains-right t1 t2 rest0))))

(define (%textual-contains-right txt1 txt2 start1 end1 start2 end2)
  (let ((n1 (- end1 start1))
        (n2 (- end2 start2)))
    (cond ((< n1 %threshold:short1)
           (%textual-contains-right:naive
            txt1 txt2 start1 end1 start2 end2))
          ((< (- n1 n2) %threshold:longer)
           (%textual-contains-right:naive
            txt1 txt2 start1 end1 start2 end2))
          ((< n2 %threshold:short2)
           (%textual-contains-right:boyer-moore
            txt1 txt2 start1 end1 start2 end2))
          (else
           (%textual-contains-right:boyer-moore
            txt1 txt2 start1 end1 start2 end2)))))

(define (%textual-contains-right:naive txt1 txt2 start1 end1 start2 end2)
  (let* ((n1 (- end1 start1))
         (n2 (- end2 start2))
         (lim1 (- end1 n2)))
    (let loop ((i lim1))
      (cond ((< i start1)
             #f)
            ((textual-prefix? txt2 txt1 start2 end2 i end1)
             i)
            (else
             (loop (- i 1)))))))

;;; This is actually the Boyer-Moore-Horspool algorithm,
;;; but the name is already pretty long.

(define (%textual-contains-right:boyer-moore txt1 txt2 start1 end1 start2 end2)
  (if (= start2 end2)
      end1
      (let* ((n1 (- end1 start1))
             (n2 (- end2 start2))
             (firstchar (text-ref txt2 0))
             (firstj (remainder (char->integer firstchar) 128))
             (table (make-vector 128 n2)))
        (do ((i (- n2 1) (- i 1)))
            ((<= i 0))
          (let* ((c  (text-ref txt2 (+ i start2)))
                 (cp (char->integer c))
                 (j  (remainder cp 128)))
            (vector-set! table j i)))
        (let loop ((i (- end1 n2)))
          (if (<= i start1)
              (if (and (= i start1)
                       (textual-prefix? txt2 txt1 start2 end2 i end1))
                  i
                  #f)
              (let* ((c  (text-ref txt1 i))
                     (cp (char->integer c))
                     (j  (remainder cp 128)))
                (cond ((not (char=? c firstchar))
                       (loop (- i (vector-ref table j))))
                      ((textual-prefix? txt2 txt1 start2 end2 i end1)
                       i)
                      (else
                       (loop (- i (vector-ref table firstj)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Case conversion

;;; Two special cases:
;;;     the given text can be returned as is
;;;     the given text is entirely ASCII
;;;
;;; For all other cases, calls the corresponding procedures for strings.

(define (textual-upcase txt)
  (cond ((string? txt)
         (string->text (string-upcase txt)))
        ((text? txt)
         (%text-upcase txt))
        (else
         (complain 'textual-upcase txt))))

(define (textual-downcase txt)
  (cond ((string? txt)
         (string->text (string-downcase txt)))
        ((text? txt)
         (%text-downcase txt string-downcase))
        (else
         (complain 'textual-downcase txt))))

(define (textual-foldcase txt)
  (cond ((string? txt)
         (string->text (string-foldcase txt)))
        ((text? txt)
         (%text-downcase txt string-foldcase))
        (else
         (complain 'textual-foldcase txt))))

(define (textual-titlecase txt)
  (cond ((string? txt)
         (string->text (string-titlecase txt)))
        ((text? txt)
         (string->text
          (string-titlecase (textual->string txt))))
        (else
         (complain 'textual-titlecase txt))))

(define (%text-upcase txt)
  (let* ((n (%text-length txt)))

    ;; So far, no conversion has been necessary.

    (define (fastest i)
      (if (= i n)
          txt
          (let ((c (%text-ref txt i)))
            (cond ((char>? c #\delete)
                   (textual-upcase (textual->string txt)))
                  ((char<=? #\a c #\z)
                   (fast i (list (subtext txt 0 i)) '()))
                  (else
                   (fastest (+ i 1)))))))

    ;; Conversions are necessary but it's been all-ASCII so far.
    ;; The upcased text for characters with index < i is
    ;;     (text-concatenate (reverse (cons (list->text (reverse chars))
    ;;                                      texts)))

    (define (fast i texts chars)
      (cond ((= i n)
             (if (null? chars)
                 (textual-concatenate-reverse texts)
                 (textual-concatenate-reverse texts
                                              (reverse-list->text chars))))
            ((and (= 0 (remainder i N))
                  (not (null? chars)))
             (fast i (cons (reverse-list->text chars) texts) '()))
            (else
             (let ((c (%text-ref txt i)))
               (cond ((char>? c #\delete)
                      (textual-append (textual-concatenate-reverse texts)
                                      (reverse-list->text chars)
                                      (string->text
                                       (string-upcase (subtext txt i n)))))
                     ((char<=? #\a c #\z)
                      (fast (+ i 1) texts (cons (char-upcase c) chars)))
                     (else
                      (fast (+ i 1) texts (cons c chars))))))))

    (fastest 0)))

;;; The string-caser is either string-downcase or string-foldcase.
;;; For ASCII, down-casing and fold-casing are the same.

(define (%text-downcase txt string-caser)
  (let* ((n (%text-length txt)))

    ;; So far, no conversion has been necessary.

    (define (fastest i)
      (if (= i n)
          txt
          (let ((c (%text-ref txt i)))
            (cond ((char>? c #\delete)
                   (textual-downcase (textual->string txt)))
                  ((char<=? #\A c #\Z)
                   (fast i (list (subtext txt 0 i)) '()))
                  (else
                   (fastest (+ i 1)))))))

    ;; Conversions are necessary but it's been all-ASCII so far.
    ;; The downcased text for characters with index < i is
    ;;     (textual-concatenate (reverse (cons (list->text (reverse chars))
    ;;                                         texts)))

    (define (fast i texts chars)
      (cond ((= i n)
             (if (null? chars)
                 (textual-concatenate-reverse texts)
                 (textual-concatenate-reverse texts
                                              (reverse-list->text chars))))
            ((and (= 0 (remainder i N))
                  (not (null? chars)))
             (fast i (cons (reverse-list->text chars) texts) '()))
            (else
             (let ((c (%text-ref txt i)))
               (cond ((char>? c #\delete)
                      (textual-append (textual-concatenate-reverse texts)
                                      (reverse-list->text chars)
                                      (string->text
                                       (string-caser (subtext txt i n)))))
                     ((char<=? #\A c #\Z)
                      (fast (+ i 1) texts (cons (char-downcase c) chars)))
                     (else
                      (fast (+ i 1) texts (cons c chars))))))))

    (fastest 0)))

;;; This is a fake version of string-titlecase, to be used only
;;; if there is no Unicode-conforming version available.

(cond-expand
 ((and (not (library (rnrs unicode)))
       (not (library (srfi 129))))
  (define (%string-titlecase s)
    (let* ((s (string-copy (string-foldcase s)))
           (n (string-length s)))
      (define (first-character-of-word! i)
        (if (< i n)
          (let ((c (string-ref s i)))
            (if (char-whitespace? c)
                (first-character-of-word! (+ i 1))
                (begin (string-set! s i (char-upcase c))
                       (subsequent-character! (+ i 1)))))))
      (define (subsequent-character! i)
        (if (< i n)
          (let ((c (string-ref s i)))
            (if (char-whitespace? c)
                (first-character-of-word! (+ i 1))
                (subsequent-character! (+ i 1))))))
      (first-character-of-word! 0)
      s)))
 (else))      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Concatenation
;;;
;;; textual-concatenate is defined by the kernel

(define (textual-append . texts)
  (textual-concatenate texts))

(define textual-concatenate-reverse
  (case-lambda
   ((texts)
    (textual-concatenate (reverse texts)))
   ((texts final-textual)
    (textual-concatenate-reverse (cons final-textual texts)))
   ((texts final-textual end)
    (textual-concatenate-reverse texts
                                 (subtext
                                  (%textual->text final-textual
                                                  'textual-concatenate-reverse
                                                  texts final-textual end)
                                  0 end)))))

(define textual-join
  (case-lambda
   ((textuals)
    (textual-join textuals " " 'infix))
   ((textuals delimiter)
    (textual-join textuals delimiter 'infix))
   ((textuals delimiter grammar)
    (let* ((texts (map (lambda (t) (%textual->text t 'textual-join textuals))
                       textuals))
           (delimiter (%textual->text delimiter
                                      'textual-join textuals delimiter)))
      (if (memq grammar '(infix strict-infix prefix suffix))
          (if (null? texts)
              (case grammar
                ((strict-infix)
                 (complain 'textual-join textuals delimiter grammar))
                (else (text)))
              (let loop ((rtxts (reverse texts))
                         (texts (if (eq? grammar 'suffix)
                                    (list delimiter)
                                    '())))
                (cond ((null? rtxts)
                       (let ((texts (if (eq? grammar 'prefix)
                                        texts
                                        (cdr texts))))
                         (textual-concatenate texts)))
                      (else
                       (loop (cdr rtxts)
                             (cons delimiter (cons (car rtxts) texts)))))))
          (complain 'textual-join textuals delimiter grammar))))))                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fold & map & friends

(define-textual-start-end (textual-fold kons knil txt start end)
  (if (procedure? kons)
      (let loop ((knil knil)
                 (i start))
        (if (< i end)
            (loop (kons (%text-ref txt i) knil)
                  (+ i 1))
            knil))
      (complain 'textual-fold kons knil txt start end)))

(define-textual-start-end (textual-fold-right kons knil txt start end)
  (if (procedure? kons)
      (let loop ((knil knil)
                 (i (- end 1)))
        (if (>= i start)
            (loop (kons (%text-ref txt i) knil)
                  (- i 1))
            knil))
      (complain 'textual-fold-right kons knil txt start end)))

(define textual-map
  (case-lambda
   ((proc txt)
    (%textual-map1 proc txt))
   ((proc txt1 txt2 . rest)
    (%textual-mapn proc (cons txt1 (cons txt2 rest))))))

(define (%textual-map1 proc txt)
  (let ((txt (%textual->text txt 'textual-map proc txt)))
    (if (procedure? proc)
        (let ((n (%text-length txt)))
          (let loop ((i 0)
                     (pieces '())
                     (chars '())
                     (k 0))
            (cond ((= i n)
                   (textual-concatenate
                    (reverse (%text-map-pieces pieces chars))))
                  ((>= k N)
                   (loop i
                         (%text-map-pieces pieces chars)
                         '()
                         (remainder k N)))
                  (else
                   (let ((x (proc (%text-ref txt i))))
                     (loop (+ i 1)
                           pieces
                           (cons x chars)
                           (+ k (cond ((char? x) 1)
                                      ((string? x) (string-length x))
                                      ((text? x) (%text-length x))
                                      (else
                                       (%textual-map-bad-result proc x))))))))))
        (complain 'textual-map proc txt))))

(define (%textual-mapn proc textuals)
  (if (procedure? proc)
      (let* ((texts (map (lambda (txt)
                           (%textual->text txt 'textual-map textuals))
                         textuals))
             (n (apply min (map %text-length texts))))
        (let loop ((i 0)
                   (pieces '())
                   (chars '())
                   (k 0))
          (cond ((= i n)
                 (textual-concatenate
                  (reverse (%text-map-pieces pieces chars))))
                ((>= k N)
                 (loop i
                       (%text-map-pieces pieces chars)
                       '()
                       (remainder k N)))
                (else
                 (let ((x (apply proc (%fetch-all texts i))))
                   (loop (+ i 1)
                         pieces
                         (cons x chars)
                         (+ k (cond ((char? x) 1)
                                    ((string? x) (string-length x))
                                    ((text? x) (%text-length x))
                                    (else
                                     (%textual-map-bad-result proc x))))))))))
      (complain 'textual-map proc textuals)))

(define (%textual-map-bad-result proc x)
  (error "textual-map: proc returned non-character" x))

;;; Given a list of texts and a list of mixed characters/strings/texts,
;;; in reverse order, converts the second argument into a text and
;;; returns that text consed onto the first argument.

(define (%text-map-pieces texts stuff)
  (let loop ((revstuff stuff)
             (stuff '())
             (n 0))
    (if (null? revstuff)
        (let ((s (make-string n)))    ; probably short
          (let inner-loop ((stuff stuff)
                           (i 0))
            (if (null? stuff)
                (cons (string->text s) texts)
                (let ((x (car stuff)))
                  (cond ((char? x)
                         (string-set! s i x)
                         (inner-loop (cdr stuff) (+ i 1)))
                        ((string? x)
                         (string-copy! s i x)
                         (inner-loop (cdr stuff) (+ i (string-length x))))
                        (else
                         (string-copy! s i (textual->string x))
                         (inner-loop (cdr stuff) (+ i (text-length x)))))))))
        (let* ((x (car revstuff))
               (revstuff (cdr revstuff))
               (stuff (cons x stuff)))
          (loop revstuff
                stuff
                (+ n (cond ((char? x) 1)
                           ((string? x) (string-length x))
                           (else (text-length x)))))))))

(define textual-for-each
  (case-lambda
   ((proc txt)
    (%textual-for-each1 proc txt))
   ((proc txt1 txt2 . rest)
    (%textual-for-eachn proc (cons txt1 (cons txt2 rest))))))

(define (%textual-for-each1 proc txt)
  (let ((txt (%textual->text txt 'textual-for-each proc txt)))
    (if (procedure? proc)
        (let ((n (%text-length txt)))
          (let loop ((i 0))
            (if (< i n)
                (begin (proc (%text-ref txt i))
                       (loop (+ i 1))))))
        (complain 'textual-for-each proc txt))))

(define (%textual-for-eachn proc textuals)
  (if (procedure? proc)
      (let* ((texts (map (lambda (txt)
                           (%textual->text txt 'textual-map textuals))
                         textuals))
             (n (apply min (map %text-length texts))))
        (let loop ((i 0))
          (if (< i n)
              (begin (apply proc (%fetch-all texts i))
                     (loop (+ i 1))))))
      (complain 'textual-for-each proc textuals)))

(define (%fetch-all texts i)
  (if (null? texts)
      '()
      (cons (%text-ref (car texts) i)
            (%fetch-all (cdr texts) i))))

;;; FIXME: there's no reason to convert a string to a text here

(define-textual-start-end (textual-map-index proc txt start end)
  (if (procedure? proc)
      (let ((n end))
        (let loop ((i start)
                   (pieces '())
                   (chars '())
                   (k 0))
          (cond ((= i n)
                 (textual-concatenate
                  (reverse (%text-map-pieces pieces chars))))
                ((>= k N)
                 (loop i
                       (%text-map-pieces pieces chars)
                       '()
                       (remainder k N)))
                (else
                 (let ((x (proc i)))
                   (loop (+ i 1)
                         pieces
                         (cons x chars)
                         (+ k (cond ((char? x) 1)
                                    ((string? x) (string-length x))
                                    ((text? x) (%text-length x))
                                    (else
                                     (%textual-map-bad-result proc x))))))))))
      (complain 'textual-map-index proc txt)))

;;; FIXME: there's no reason to convert a string to a text here

(define-textual-start-end (textual-for-each-index proc txt start end)
  (if (procedure? proc)
      (let ((n end))
        (let loop ((i start))
          (if (< i n)
              (begin (proc i)
                     (loop (+ i 1))))))
      (complain 'textual-for-each-index proc txt)))

(define-textual (textual-count txt pred . rest)
  (let ((start (if (null? rest) 0 (car rest)))
        (end (if (or (null? rest) (null? (cdr rest)))
                 (%text-length txt)
                 (car (cdr rest)))))
    (if (and (procedure? pred)
             (or (null? rest) (null? (cdr rest)) (null? (cdr (cdr rest))))
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (%text-length txt)))
        (textual-fold (lambda (c n)
                        (if (pred c)
                            (+ n 1)
                            n))
                      0 txt start end)
        (complain 'textual-count txt pred start end))))

(define-textual-start-end (textual-filter pred txt start end)
  (if (procedure? pred)
      (textual-map (lambda (c) (if (pred c) c ""))
                   (subtext txt start end))
      (complain 'textual-filter pred txt start end)))

;;; FIXME: checks arguments twice

(define-textual-start-end (textual-remove pred txt start end)
  (textual-filter (lambda (c) (not (pred c))) txt start end))

;;; FIXME: not linear-time unless string-set! is O(1)
;;; (but this is a pretty useless procedure anyway)

(define-textual-start-end (textual-reverse txt start end)
  (let* ((n (- end start))
         (s (make-string n)))
    (do ((i start (+ i 1)))
        ((= i end)
         (string->text s))
      (string-set! s (- n (- i start) 1) (%text-ref txt i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Replication & splitting

(define textual-replicate
  (case-lambda
   ((s from to start end)
    (let ((s (%textual->text s 'textual-replicate s from to start end)))
      (textual-replicate (subtext s start end) from to)))
   ((s from to start)
    (let ((s (%textual->text s 'textual-replicate s from to start)))
      (textual-replicate (subtext s start (textual-length s)) from to)))
   ((s0 from to)
    (let* ((s (%textual->text s0 'textual-replicate s0 from to))
           (n (- to from))
           (len (%text-length s)))
      (cond ((= n 0)
             "")
            ((or (< n 0)
                 (= len 0))
             (complain 'textual-replicate s from to))
            (else
             (let* ((from (mod from len)) ; make from non-negative
                    (to (+ from n)))
               (do ((replicates '() (cons s replicates))
                    (replicates-length 0 (+ replicates-length len)))
                   ((>= replicates-length to)
                    (subtext (apply textual-append replicates)
                             from to))))))))))

(define textual-split
  (case-lambda
   ((s delimiter grammar limit start end)
    (textual-split (subtextual s start end) delimiter grammar limit))
   ((s delimiter grammar limit start)
    (textual-split (subtextual s start (textual-length s))
                   delimiter grammar limit))
   ((s delimiter)
    (textual-split s delimiter 'infix #f))
   ((s delimiter grammar)
    (textual-split s delimiter grammar #f))
   ((s0 delimiter grammar limit)
    (define (bad-arguments)
      (complain 'textual-split s0 delimiter grammar limit))
    (let* ((s (%textual->text s0 'textual-split s0 delimiter grammar limit))
           (delimiter
            (%textual->text delimiter
                            'textual-split s0 delimiter grammar limit))
           (limit (or limit (%text-length s)))
           (splits
            (cond ((= 0 (%text-length delimiter))
                   (%text-split-into-characters s limit))
                  (else
                   (%text-split-using-word s delimiter limit)))))
      (case grammar
        ((infix strict-infix)
         (if (= 0 (%text-length s))
             (if (eq? grammar 'infix)
                 '()
                 (bad-arguments))
             splits))
        ((prefix)
         (if (and (pair? splits)
                  (= 0 (%text-length (car splits))))
             (cdr splits)
             splits))
        ((suffix)
         (if (and (pair? splits)
                  (= 0 (%text-length (car (last-pair splits)))))
             (reverse (cdr (reverse splits)))
             splits))
        (else
         (bad-arguments)))))))

(define (%text-split-into-characters s limit)
  (let ((n (%text-length s)))
    (cond ((> n (+ limit 1))
           (append (%text-split-into-characters (subtext s 0 limit) limit)
                   (list (subtext s limit n))))
          (else
           (map text (textual->list s))))))

;;; FIXME: inefficient

(define (%text-split-using-word txt sep limit)
  (let loop ((i 0)
             (limit limit)
             (texts '()))
    (if (= 0 limit)
        (reverse (cons (subtext txt i (%text-length txt)) texts))
        (let ((i2 (textual-contains txt sep i)))
          (if i2
              (loop (+ i2 (%text-length sep))
                    (- limit 1)
                    (cons (subtext txt i i2) texts))
              (loop i 0 texts))))))

;;; eof
