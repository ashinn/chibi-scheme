
;; PCRE parsing, adapted from IrRegex.

(define ~none 0)
(define ~save? 1)
(define ~case-insensitive? 2)
(define ~multi-line? 4)
(define ~single-line? 8)
(define ~ignore-space? 16)

(define (flag-set? flags i)
  (= i (bitwise-and flags i)))
(define (flag-join a b)
  (if b (bitwise-ior a b) a))
(define (flag-clear a b)
  (bitwise-and a (bitwise-not b)))

(define (symbol-list->flags ls)
  (let lp ((ls ls) (res ~none))
    (cond
     ((null? ls)
      res)
     ((not (pair? ls))
      (lp (list ls) res))
     (else
      (lp (cdr ls)
          (flag-join
           res
           (case (car ls)
             ((i ci case-insensitive) ~case-insensitive?)
             ((m multi-line) ~multi-line?)
             ((s single-line) ~single-line?)
             ((x ignore-space) ~ignore-space?)
             (else #f))))))))

(define posix-escape-sequences
  `((#\n . #\newline)
    (#\r . #\return)
    (#\t . #\tab)
    (#\a . #\alarm)
    (#\e . #\escape)))

(define (char-altcase c)
  (if (char-upper-case? c) (char-downcase c) (char-upcase c)))

(define (char-mirror c)
  (case c ((#\<) #\>) ((#\{) #\}) ((#\() #\)) ((#\[) #\]) (else c)))

(define (string-scan-char-escape str c . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((eqv? c (string-ref str i)) i)
            ((eqv? c #\\) (scan (+ i 2)))
            (else (scan (+ i 1)))))))

(define (string-parse-hex-escape str i end)
  (cond
   ((>= i end)
    (error "incomplete hex escape" str i))
   ((eqv? #\{ (string-ref str i))
    (let ((j (string-scan-char-escape str #\} (+ i 1))))
      (if (not j)
          (error "incomplete hex brace escape" str i)
          (let* ((s (substring str (+ i 1) j))
                 (n (string->number s 16)))
            (if n
                (list (integer->char n) j)
                (error "bad hex brace escape" s))))))
   ((>= (+ i 1) end)
    (error "incomplete hex escape" str i))
   (else
    (let* ((s (substring str i (+ i 2)))
           (n (string->number s 16)))
      (if n
          (list (integer->char n) (+ i 2))
          (error "bad hex escape" s))))))

(define (string-parse-cset str start flags)
  (let* ((end (string-length str))
         (invert? (and (< start end) (eqv? #\^ (string-ref str start)))))
    (define (cset-union a b)
      (cond ((not a) b)
            ((not b) a)
            ((and (pair? a) (eq? 'or (car a))) `(,@a ,b))
            (else `(or ,a ,b))))
    (define (go i prev-char ones pairs classes)
      (if (>= i end)
          (error "incomplete char set" str i end))
      (case (string-ref str i)
        ((#\])
         (if (and (null? ones) (null? pairs))
             (go (+ i 1) #\] (cons #\] ones) pairs classes)
             (list
              (let ((res
                     (cset-union
                      (cset-union
                       (and (pair? classes)
                            `(or ,@classes))
                       (and (pair? ones)
                            `(,(list->string (reverse ones)))))
                      (and (pair? pairs)
                           `(/ ,(list->string (reverse pairs)))))))
                (if invert? `(~ ,res) res))
              i)))
        ((#\-)
         (cond
          ((or (= i start)
               (and (= i (+ start 1)) invert?)
               (eqv? #\] (string-ref str (+ i 1))))
           (go (+ i 1) #\- (cons #\- ones) pairs classes))
          ;; alternately permissively allow this as a -
          ((not prev-char)
           (error "bad pcre char-set, unexpected -" str))
          (else
           (let ((ch (string-ref str (+ i 1))))
             (apply
              (lambda (c j)
                (if (char<? c prev-char)
                    (error "inverted range in pcre char-set" prev-char c)
                    (go j #f (cdr ones) (cons c (cons prev-char pairs))
                        classes)))
              (cond
               ((and (eqv? #\\ ch)
                     (assv (string-ref str (+ i 2)) posix-escape-sequences))
                => (lambda (x) (list (cdr x) (+ i 3))))
               ((and (eqv? #\\ ch)
                     (eqv? (string-ref str (+ i 2)) #\x))
                (string-parse-hex-escape str (+ i 3) end))
               (else
                (list ch (+ i 2)))))))))
        ((#\[)
         (let* ((inv? (eqv? #\^ (string-ref str (+ i 1))))
                (i2 (if inv? (+ i 2) (+ i 1))))
           (case (string-ref str i2)
             ((#\:)
              (let ((j (string-find str #\: (+ i2 1) end)))
                (if (or (>= (+ j 1) end)
                        (not (eqv? #\] (string-ref str (+ j 1)))))
                    (error "incomplete character class" str)
                    (let* ((class (string->symbol (substring str (+ i2 1) j)))
                           (class (if inv? `(~ ,class) class)))
                      (go (+ j 2) #f ones pairs (cons class classes))))))
             ((#\= #\.)
              (error "collating sequences not supported" str))
             (else
              (go (+ i 1) #\[ (cons #\[ ones) pairs classes)))))
        ((#\\)
         (let ((c (string-ref str (+ i 1))))
           (case c
             ((#\d #\D #\s #\S #\w #\W)
              (go (+ i 2) #f ones pairs
                  (cons (pcre->sre (string #\\ c)) classes)))
             ((#\x)
              (apply
               (lambda (c j) (go j c (cons c ones) pairs classes))
               (string-parse-hex-escape str (+ i 2) end)))
             (else
              (let ((c (cond ((assv c posix-escape-sequences) => cdr)
                             (else c))))
                (go (+ i 2) c (cons c ones) pairs classes))))))
        (else
         => (lambda (c) (go (+ i 1) c (cons c ones) pairs classes)))))
    (if invert?
        (let ((ones (if (flag-set? flags ~multi-line?) '(#\newline) '())))
          (go (+ start 1) #f ones '() '()))
        (go start #f '() '() '()))))

;; build a (seq ls ...) sre from a list
(define (sre-sequence ls)
  (cond
   ((null? ls) 'epsilon)
   ((null? (cdr ls)) (car ls))
   (else (cons 'seq ls))))

;; build a (or ls ...) sre from a list
(define (sre-alternate ls)
  (cond
   ((null? ls) '(or))
   ((null? (cdr ls)) (car ls))
   (else (cons 'or ls))))

;; returns #t if the sre can ever be empty
(define (sre-empty? sre)
  (if (pair? sre)
      (case (car sre)
        ((* ? look-ahead look-behind neg-look-ahead neg-look-behind) #t)
        ((**) (or (not (number? (cadr sre))) (zero? (cadr sre))))
        ((or) (any sre-empty? (cdr sre)))
        ((: seq $ submatch => submatch-named + atomic)
         (every sre-empty? (cdr sre)))
        (else #f))
      (memq sre '(epsilon bos eos bol eol bow eow commit))))

;; returns #t if the sre is a */+ repetition
(define (sre-repeater? sre)
  (and (pair? sre)
       (or (memq (car sre) '(* +))
           (and (memq (car sre) '($ submatch => submatch-named seq :))
                (pair? (cdr sre))
                (null? (cddr sre))
                (sre-repeater? (cadr sre))))))

(define (pcre->sre str . o)
  (if (not (string? str))
      (error "pcre->sre: expected a string" str))
  (let ((end (string-length str))
        (orig-flags (if (pair? o) (symbol-list->flags (car o)) ~none)))
    (let lp ((i 0) (from 0) (flags orig-flags) (res '()) (st '()))
      ;; accumulate the substring from..i as literal text
      (define (collect)
        (if (= i from) res (cons (substring str from i) res)))
      ;; like collect but breaks off the last single character when
      ;; collecting literal data, as the argument to ?/*/+ etc.
      (define (collect/single)
        (let ((j (- i 1)))
          (cond
           ((< j from)
            res)
           (else
            (let ((c (string-ref str j)))
              (cond
               ((= j from)
                (cons c res))
               (else
                (cons c (cons (substring str from j) res)))))))))
      ;; collects for use as a result, reversing and grouping OR
      ;; terms, and some ugly tweaking of `function-like' groups and
      ;; conditionals
      (define (collect/terms)
        (let* ((ls (collect))
               (func
                (and (pair? ls)
                     (memq (last ls)
                           '(atomic if look-ahead neg-look-ahead
                                    look-behind neg-look-behind
                                    => submatch-named))))
               (prefix (if (and func (memq (car func) '(=> submatch-named)))
                           (list 'submatch-named (cadr (reverse ls)))
                           (and func (list (car func)))))
               (ls (if func
                       (if (memq (car func) '(=> submatch-named))
                           (reverse (cddr (reverse ls)))
                           (reverse (cdr (reverse ls))))
                       ls)))
          (let lp ((ls ls) (term '()) (res '()))
            (define (shift)
              (cons (sre-sequence term) res))
            (cond
             ((null? ls)
              (let* ((res (sre-alternate (shift)))
                     (res (if (flag-set? flags ~save?)
                              (list 'submatch res)
                              res)))
                (if prefix
                    (if (eq? 'if (car prefix))
                        (cond
                         ((not (pair? res))
                          'epsilon)
                         ((memq (car res)
                                '(look-ahead neg-look-ahead
                                             look-behind neg-look-behind))
                          res)
                         ((eq? 'seq (car res))
                          `(if ,(cadr res)
                               ,(sre-sequence (cddr res))))
                         (else
                          `(if ,(cadadr res)
                               ,(sre-sequence (cddadr res))
                               ,(sre-alternate (cddr res)))))
                        `(,@prefix ,res))
                    res)))
             ((eq? 'or (car ls)) (lp (cdr ls) '() (shift)))
             (else (lp (cdr ls) (cons (car ls) term) res))))))
      (define (save)
        (cons (cons flags (collect)) st))
      ;; main parsing
      (cond
       ((>= i end)
        (if (pair? st)
            (error "unterminated parenthesis in regexp" str)
            (collect/terms)))
       (else
        (case (string-ref str i)
          ((#\.)
           (lp (+ i 1) (+ i 1) flags
               (cons (if (flag-set? flags ~single-line?) 'any 'nonl)
                     (collect))
               st))
          ((#\?)
           (let ((res (collect/single)))
             (if (null? res)
                 (error "? can't follow empty pattern" str res)
                 (let ((x (car res)))
                   (lp (+ i 1)
                       (+ i 1)
                       flags
                       (cons
                        (if (pair? x)
                            (case (car x)
                              ((*)  `(*? ,@(cdr x)))
                              ((+)  `(**? 1 #f ,@(cdr x)))
                              ((?)  `(?? ,@(cdr x)))
                              ((**) `(**? ,@(cdr x)))
                              ((=)  `(**? ,(cadr x) ,@(cdr x)))
                              ((>=)  `(**? ,(cadr x) #f ,@(cddr x)))
                              (else `(? ,x)))
                            `(? ,x))
                        (cdr res))
                       st)))))
          ((#\+ #\*)
           (let* ((res (collect/single))
                  (x (if (pair? res) (car res) 'epsilon))
                  (op (string->symbol (string (string-ref str i)))))
             (cond
              ((sre-repeater? x)
               (error "duplicate repetition (e.g. **) in pattern" str res))
              ((sre-empty? x)
               (error "can't repeat empty pattern (e.g. ()*)" str res))
              (else
               (lp (+ i 1) (+ i 1) flags
                   (cons (list op x) (cdr res))
                   st)))))
          ((#\()
           (cond
            ((>= (+ i 1) end)
             (error "unterminated parenthesis in regexp" str))
            ((not (memv (string-ref str (+ i 1)) '(#\? #\*))) ; normal case
             (lp (+ i 1) (+ i 1) (flag-join flags ~save?) '() (save)))
            ((>= (+ i 2) end)
             (error "unterminated parenthesis in regexp" str))
            ((eqv? (string-ref str (+ i 1)) #\*)
             (error "bad regexp syntax: (*FOO) not supported" str))
            (else ;; (?...) case
             (case (string-ref str (+ i 2))
               ((#\#)
                (let ((j (string-find str #\) (+ i 3))))
                  (lp (+ j i) (min (+ j 1) end) flags (collect) st)))
               ((#\:)
                (lp (+ i 3) (+ i 3) (flag-clear flags ~save?) '() (save)))
               ((#\=)
                (lp (+ i 3) (+ i 3) (flag-clear flags ~save?)
                    '(look-ahead) (save)))
               ((#\!)
                (lp (+ i 3) (+ i 3) (flag-clear flags ~save?)
                    '(neg-look-ahead) (save)))
               ((#\<)
                (cond
                 ((>= (+ i 3) end)
                  (error "unterminated parenthesis in regexp" str))
                 (else
                  (case (string-ref str (+ i 3))
                    ((#\=)
                     (lp (+ i 4) (+ i 4) (flag-clear flags ~save?)
                         '(look-behind) (save)))
                    ((#\!)
                     (lp (+ i 4) (+ i 4) (flag-clear flags ~save?)
                         '(neg-look-behind) (save)))
                    (else
                     (let ((j (and (char-alphabetic?
                                    (string-ref str (+ i 3)))
                                   (string-find str #\> (+ i 4)))))
                       (if (< j end)
                           (lp (+ j 1) (+ j 1) (flag-clear flags ~save?)
                               `(,(string->symbol (substring str (+ i 3) j))
                                 submatch-named)
                               (save))
                           (error "invalid (?< sequence" str))))))))
               ((#\>)
                (lp (+ i 3) (+ i 3) (flag-clear flags ~save?)
                    '(atomic) (save)))
               ;;((#\' #\P) ; named subpatterns
               ;; )
               ;;((#\R) ; recursion
               ;; )
               ((#\()
                (cond
                 ((>= (+ i 3) end)
                  (error "unterminated parenthesis in regexp" str))
                 ((char-numeric? (string-ref str (+ i 3)))
                  (let* ((j (string-find str #\) (+ i 3)))
                         (n (string->number (substring str (+ i 3) j))))
                    (if (or (= j end) (not n))
                        (error "invalid conditional reference" str)
                        (lp (+ j 1) (+ j 1) (flag-clear flags ~save?)
                            `(,n if) (save)))))
                 ((char-alphabetic? (string-ref str (+ i 3)))
                  (let ((j (string-find str #\) (+ i 3))))
                    (if (= j end)
                        (error "invalid named conditional reference" str)
                        (lp (+ j 1) (+ j 1) (flag-clear flags ~save?)
                            `(,(string->symbol (substring str (+ i 3) j)) if)
                            (save)))))
                 (else
                  (lp (+ i 2) (+ i 2) (flag-clear flags ~save?)
                      '(if) (save)))))
               ((#\{)
                (error "unsupported Perl-style cluster" str))
               (else
                (let ((old-flags flags))
                  (let lp2 ((j (+ i 2)) (flags flags) (invert? #f))
                    (define (join x)
                      ((if invert? flag-clear flag-join) flags x))
                    (cond
                     ((>= j end)
                      (error "incomplete cluster" str i))
                     (else
                      (case (string-ref str j)
                        ((#\i)
                         (lp2 (+ j 1) (join ~case-insensitive?) invert?))
                        ((#\m)
                         (lp2 (+ j 1) (join ~multi-line?) invert?))
                        ((#\x)
                         (lp2 (+ j 1) (join ~ignore-space?) invert?))
                        ((#\-)
                         (lp2 (+ j 1) flags (not invert?)))
                        ((#\))
                         (lp (+ j 1) (+ j 1) flags (collect)
                             st))
                        ((#\:)
                         (lp (+ j 1) (+ j 1) flags '()
                             (cons (cons old-flags (collect)) st)))
                        (else
                         (error "unknown regex cluster modifier" str)
                         )))))))))))
          ((#\))
           (if (null? st)
               (error "too many )'s in regexp" str)
               (lp (+ i 1)
                   (+ i 1)
                   (caar st)
                   (cons (collect/terms) (cdar st))
                   (cdr st))))
          ((#\[)
           (apply
            (lambda (sre j)
              (lp (+ j 1) (+ j 1) flags (cons sre (collect)) st))
            (string-parse-cset str (+ i 1) flags)))
          ((#\{)
           (cond
            ((or (>= (+ i 1) end)
                 (not (or (char-numeric? (string-ref str (+ i 1)))
                          (eqv? #\, (string-ref str (+ i 1))))))
             (lp (+ i 1) from flags res st))
            (else
             (let ((res (collect/single)))
               (cond
                ((null? res)
                 (error "{ can't follow empty pattern"))
                (else
                 (let* ((x (car res))
                        (tail (cdr res))
                        (j (string-find str #\} (+ i 1)))
                        (s2 (string-split (substring str (+ i 1) j) #\,))
                        (n (string->number (car s2)))
                        (m (and (pair? (cdr s2))
                                (string->number (cadr s2)))))
                   (cond
                    ((or (= j end)
                         (not n)
                         (and (pair? (cdr s2))
                              (not (equal? "" (cadr s2)))
                              (not m)))
                     (error "invalid {n} repetition syntax" s2))
                    ((null? (cdr s2))
                     (lp (+ j 1) (+ j 1) flags `((= ,n ,x) ,@tail) st))
                    (m
                     (lp (+ j 1) (+ j 1) flags `((** ,n ,m ,x) ,@tail) st))
                    (else
                     (lp (+ j 1) (+ j 1) flags `((>= ,n ,x) ,@tail) st)
                     )))))))))
          ((#\\)
           (cond
            ((>= (+ i 1) end)
             (error "incomplete escape sequence" str))
            (else
             (let ((c (string-ref str (+ i 1))))
               (case c
                 ((#\d)
                  (lp (+ i 2) (+ i 2) flags `(numeric ,@(collect)) st))
                 ((#\D)
                  (lp (+ i 2) (+ i 2) flags `((~ numeric) ,@(collect)) st))
                 ((#\s)
                  (lp (+ i 2) (+ i 2) flags `(space ,@(collect)) st))
                 ((#\S)
                  (lp (+ i 2) (+ i 2) flags `((~ space) ,@(collect)) st))
                 ((#\w)
                  (lp (+ i 2) (+ i 2) flags
                      `((or alphanumeric ("_")) ,@(collect)) st))
                 ((#\W)
                  (lp (+ i 2) (+ i 2) flags
                      `((~ (or alphanumeric ("_"))) ,@(collect)) st))
                 ((#\b)
                  (lp (+ i 2) (+ i 2) flags
                      `((or bow eow) ,@(collect)) st))
                 ((#\B)
                  (lp (+ i 2) (+ i 2) flags `(nwb ,@(collect)) st))
                 ((#\A)
                  (lp (+ i 2) (+ i 2) flags `(bos ,@(collect)) st))
                 ((#\Z)
                  (lp (+ i 2) (+ i 2) flags
                      `((? #\newline) eos ,@(collect)) st))
                 ((#\z)
                  (lp (+ i 2) (+ i 2) flags `(eos ,@(collect)) st))
                 ((#\R)
                  (lp (+ i 2) (+ i 2) flags `(newline ,@(collect)) st))
                 ((#\K)
                  (lp (+ i 2) (+ i 2) flags `(reset ,@(collect)) st))
                 ;; these two are from Emacs and TRE, but not in PCRE
                 ((#\<)
                  (lp (+ i 2) (+ i 2) flags `(bow ,@(collect)) st))
                 ((#\>)
                  (lp (+ i 2) (+ i 2) flags `(eow ,@(collect)) st))
                 ((#\x)
                  (apply
                   (lambda (ch j)
                     (lp (+ j 1) (+ j 1) flags `(,ch ,@(collect)) st))
                   (string-parse-hex-escape str (+ i 2) end)))
                 ((#\k)
                  (let ((c (string-ref str (+ i 2))))
                    (if (not (memv c '(#\< #\{ #\')))
                        (error "bad \\k usage, expected \\k<...>" str)
                        (let* ((terminal (char-mirror c))
                               (j (string-find str terminal (+ i 2)))
                               (s (substring str (+ i 3) j))
                               (backref
                                (if (flag-set? flags ~case-insensitive?)
                                    'backref-ci
                                    'backref)))
                          (if (= j end)
                              (error "unterminated named backref" str)
                              (lp (+ j 1) (+ j 1) flags
                                  `((,backref ,(string->symbol s))
                                    ,@(collect))
                                  st))))))
                 ((#\Q) ;; \Q..\E escapes
                  (let ((res (collect)))
                    (let lp2 ((j (+ i 2)))
                      (cond
                       ((>= j end)
                        (lp j (+ i 2) flags res st))
                       ((eqv? #\\ (string-ref str j))
                        (cond
                         ((>= (+ j 1) end)
                          (lp (+ j 1) (+ i 2) flags res st))
                         ((eqv? #\E (string-ref str (+ j 1)))
                          (lp (+ j 2) (+ j 2) flags
                              (cons (substring str (+ i 2) j) res) st))
                         (else
                          (lp2 (+ j 2)))))
                       (else
                        (lp2 (+ j 1)))))))
                 ;;((#\p)  ; XXXX unicode properties
                 ;; )
                 ;;((#\P)
                 ;; )
                 (else
                  (cond
                   ((char-numeric? c)
                    (let* ((j (string-skip str char-numeric? (+ i 2)))
                           (backref
                            (if (flag-set? flags ~case-insensitive?)
                                'backref-ci
                                'backref))
                           (res `((,backref ,(string->number
                                              (substring str (+ i 1) j)))
                                  ,@(collect))))
                      (lp j j flags res st)))
                   ((char-alphabetic? c)
                    (let ((cell (assv c posix-escape-sequences)))
                      (if cell
                          (lp (+ i 2) (+ i 2) flags
                              (cons (cdr cell) (collect)) st)
                          (error "unknown escape sequence" str c))))
                   (else
                    (lp (+ i 2) (+ i 1) flags (collect) st)))))))))
          ((#\|)
           (lp (+ i 1) (+ i 1) flags (cons 'or (collect)) st))
          ((#\^)
           (let ((sym (if (flag-set? flags ~multi-line?) 'bol 'bos)))
             (lp (+ i 1) (+ i 1) flags (cons sym (collect)) st)))
          ((#\$)
           (let ((sym (if (flag-set? flags ~multi-line?) 'eol 'eos)))
             (lp (+ i 1) (+ i 1) flags (cons sym (collect)) st)))
          ((#\space)
           (if (flag-set? flags ~ignore-space?)
               (lp (+ i 1) (+ i 1) flags (collect) st)
               (lp (+ i 1) from flags res st)))
          ((#\#)
           (if (flag-set? flags ~ignore-space?)
               (let ((j (string-find str #\newline (+ i 1))))
                 (lp (+ j 1) (min (+ j 1) end) flags (collect) st))
               (lp (+ i 1) from flags res st)))
          (else
           (lp (+ i 1) from flags res st))))))))

(define (pcre->regexp pcre . o)
  (regexp (apply pcre->sre pcre o)))
