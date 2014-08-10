;; regexp.scm -- simple non-bactracking NFA implementation
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;; An rx represents a start state and meta-info such as the number
;;; and names of submatches.
(define-record-type Rx
  (make-rx start-state num-matches num-save-indexes match-rules match-names sre)
  regexp?
  (start-state rx-start-state rx-start-state-set!)
  (num-matches rx-num-matches rx-num-matches-set!)
  (num-save-indexes rx-num-save-indexes rx-num-save-indexes-set!)
  (match-rules rx-rules rx-rules-set!)
  (match-names rx-names rx-names-set!)
  (sre regexp->sre))

;; Syntactic sugar.
(define-syntax rx
  (syntax-rules ()
    ((rx sre ...)
     (regexp `(: sre ...)))))

;;; A state is a single nfa state with transition rules.
(define-record-type State
  (%make-state accept? chars match match-rule next1 next2)
  state?
  ;; A boolean indicating if this is an accepting state.
  (accept? state-accept? state-accept?-set!)
  ;; A char or char-set indicating when we can transition.
  ;; Alternately, #f indicates an epsilon transition, while a
  ;; procedure of the form (lambda (ch i matches) ...) is a predicate
  ;; which should return #t if the char matches.
  (chars state-chars state-chars-set!)
  ;; A single integer indicating the match position to record.
  (match state-match state-match-set!)
  ;; The rule for merging ambiguous matches.  Can be any of: left,
  ;; right, (list i j).  Posix semantics are equivalent to using left
  ;; for the beginning of a submatch and right for the end.  List is
  ;; used to capture a list of submatch data in the current match.
  (match-rule state-match-rule state-match-rule-set!)
  ;; The destination if the char match succeeds.
  (next1 state-next1 state-next1-set!)
  ;; An optional additional transition used for forking to two states.
  (next2 state-next2 state-next2-set!))

(define (make-state accept? chars match match-rule next1 next2)
  (if (and next1 (not (state? next1)))
      (error "expected a state" next1))
  (if (and next2 (not (state? next2)))
      (error "expected a state" next2))
  (%make-state accept? chars match match-rule next1 next2))

(define ~none 0)
(define ~ci? 1)
(define ~ascii? 2)
(define ~nocapture? 4)

(define (flag-set? flags i) (= i (bitwise-and flags i)))
(define (flag-join a b) (if b (bitwise-ior a b) a))
(define (flag-clear a b) (bitwise-and a (bitwise-not b)))

(define (char-set-ci cset)
  (let ((res (char-set)))
    (char-set-for-each
     (lambda (ch)
       (char-set-adjoin! res (char-upcase ch))
       (char-set-adjoin! res (char-downcase ch)))
     cset)
    res))

(define (make-char-state ch flags next)
  (if (flag-set? flags ~ci?)
      (let ((cset (cond ((char? ch) (char-set-ci (char-set ch)))
                        ((char-set? ch) (char-set-ci ch))
                        (else ch))))
        (make-state #f cset #f #f next #f))
      (make-state #f ch #f #f next #f)))
(define (make-fork-state next1 next2)
  (make-state #f #f #f #f next1 next2))
(define (make-epsilon-state next)
  (make-fork-state next #f))
(define (make-accept-state)
  (make-state #t #f #f #f #f #f))

;; A record holding the current match data - essentially a wrapper
;; around a vector, plus a reference to the RX for meta-info.
(define-record-type Regexp-Match
  (%make-regexp-match matches rx string)
  regexp-match?
  (matches regexp-match-matches regexp-match-matches-set!)
  (rx regexp-match-rx)
  (string regexp-match-string))

(define (regexp-match-rules md)
  (rx-rules (regexp-match-rx md)))
(define (regexp-match-names md)
  (rx-names (regexp-match-rx md)))
(define (make-regexp-match len rx str)
  (%make-regexp-match (make-vector len #f) rx str))
(define (make-regexp-match-for-rx rx str)
  (make-regexp-match (rx-num-save-indexes rx) rx str))
(define (regexp-match-count md)
  (- (quotient (vector-length (regexp-match-matches md)) 2) 1))

(define (regexp-match-name-offset md name)
  (let lp ((ls (regexp-match-names md)) (first #f))
    (cond
     ((null? ls) (or first (error "unknown match name" md name)))
     ((eq? name (caar ls))
      (if (regexp-match-submatch-start+end md (cdar ls))
          (cdar ls)
          (lp (cdr ls) (or first (cdar ls)))))
     (else (lp (cdr ls) first)))))

(define (regexp-match-ref md n)
  (vector-ref (regexp-match-matches md)
              (if (integer? n)
                  n
                  (regexp-match-name-offset md n))))

(define (regexp-match-set! md n val)
  (vector-set! (regexp-match-matches md) n val))

(define (copy-regexp-match md)
  (let* ((src (regexp-match-matches md))
         (len (vector-length src))
         (dst (make-vector len #f)))
    (do ((i 0 (+ i 1)))
        ((= i len)
         (%make-regexp-match dst (regexp-match-rx md) (regexp-match-string md)))
      (vector-set! dst i (vector-ref src i)))))

;;> Returns the matching result for the given named or indexed
;;> submatch \var{n}, possibly as a list for a submatch-list, or
;;> \scheme{#f} if not matched.

(define (regexp-match-submatch/list md n)
  (let ((n (if (integer? n) n (regexp-match-name-offset md n))))
    (cond
     ((>= n (vector-length (regexp-match-rules md)))
      #f)
     (else
      (let ((rule (vector-ref (regexp-match-rules md) n)))
        (cond
         ((pair? rule)
          (let ((start (regexp-match-ref md (car rule)))
                (end (regexp-match-ref md (cdr rule)))
                (str (regexp-match-string md)))
            (and start end (substring-cursor str start end))))
         (else
          (let ((res (regexp-match-ref md rule)))
            (if (pair? res)
                (reverse res)
                res)))))))))

;;> Returns the matching substring for the given named or indexed
;;> submatch \var{n}, or \scheme{#f} if not matched.

(define (regexp-match-submatch md n)
  (let ((res (regexp-match-submatch/list md n)))
    (if (pair? res) (car res) res)))

(define (regexp-match-submatch-start+end md n)
  (let ((n (if (string-cursor? n) n (regexp-match-name-offset md n))))
    (and (< n (vector-length (regexp-match-rules md)))
         (let ((rule (vector-ref (regexp-match-rules md) n)))
           (if (pair? rule)
               (let ((start (regexp-match-ref md (car rule)))
                     (end (regexp-match-ref md (cdr rule)))
                     (str (regexp-match-string md)))
                 (and start end
                      (cons (string-offset->index str start)
                            (string-offset->index str end))))
               #f)))))

;;> Returns the start index for the given named or indexed submatch
;;> \var{n}, or \scheme{#f} if not matched.

(define (regexp-match-submatch-start md n)
  (cond ((regexp-match-submatch-start+end md n) => car) (else #f)))

;;> Returns the end index for the given named or indexed submatch
;;> \var{n}, or \scheme{#f} if not matched.

(define (regexp-match-submatch-end md n)
  (cond ((regexp-match-submatch-start+end md n) => cdr) (else #f)))

(define (regexp-match-convert recurse? md str)
  (cond
   ((vector? md)
    (let lp ((i 0) (res '()))
      (cond
       ((>= i (vector-length md))
        (reverse res))
       ((string-cursor? (vector-ref md i))
        (lp (+ i 2)
            (cons (substring-cursor str
                                    (vector-ref md i)
                                    (vector-ref md (+ i 1)))
                  res)))
       (else
        (lp (+ i 1)
            (cons (regexp-match-convert recurse? (vector-ref md i) str)
                  res))))))
   ((list? md)
    (if recurse?
        (map (lambda (x) (regexp-match-convert recurse? x str)) (reverse md))
        (regexp-match-convert recurse? (car md) str)))
   ((and (pair? md) (string-cursor? (car md)) (string-cursor? (cdr md)))
    (substring-cursor str (car md) (cdr md)))
   ((regexp-match? md)
    (regexp-match-convert
     recurse? (regexp-match-matches md) (regexp-match-string md)))
   (else
    md)))

;;> Convert an regexp-match result to a list of submatches, beginning
;;> with the full match, using \scheme{#f} for unmatched submatches.

(define (regexp-match->list md)
  (regexp-match-convert #f md #f))

;;> Convert an regexp-match result to a forest of submatches, beginning
;;> with the full match, using \scheme{#f} for unmatched submatches.

(define (regexp-match->sexp md)
  (regexp-match-convert #t md #f))

;; Collect results from a list match.
(define (match-collect md spec)
  (define (match-extract md n)
    (let* ((vec (regexp-match-matches md))
           (rules (regexp-match-rules md))
           (n-rule (vector-ref rules n))
           (rule (vector-ref rules n-rule)))
      (if (pair? rule)
          (let ((start (regexp-match-ref md (car rule)))
                (end (regexp-match-ref md (cdr rule))))
            (and start end (cons start end)))
          (regexp-match-ref md rule))))
  (let ((end (cadr spec))
        (vec (regexp-match-matches md)))
    (let lp ((i (+ 1 (car spec)))
             (ls '()))
      (if (>= i end)
          (reverse ls)
          (lp (+ i 1) (cons (match-extract md i) ls))))))

;; A searcher represents a single rx state and match information.
(define-record-type Searcher
  (make-searcher state matches)
  searcher?
  (state searcher-state searcher-state-set!)
  (matches searcher-matches searcher-matches-set!))

;; Merge two regexp-matches, preferring the leftmost-longest of their
;; matches.
(define (regexp-match>=? m1 m2)
  (let ((end (- (vector-length (regexp-match-matches m1)) 1)))
    (let lp ((i 0))
      (cond
       ((>= i end)
        #t)
       ((and (eqv? (regexp-match-ref m1 i)
                   (regexp-match-ref m2 i))
             (eqv? (regexp-match-ref m1 (+ i 1))
                   (regexp-match-ref m2 (+ i 1))))
        (lp (+ i 2)))
       ((and (string-cursor? (regexp-match-ref m2 i))
             (or (not (string-cursor? (regexp-match-ref m1 i)))
                 (string-cursor<? (regexp-match-ref m2 i)
                                  (regexp-match-ref m1 i))
                 (and
                  (string-cursor=? (regexp-match-ref m2 i)
                                   (regexp-match-ref m1 i))
                  (or (not (string-cursor? (regexp-match-ref m2 (+ i 1))))
                      (and (string-cursor? (regexp-match-ref m1 (+ i 1)))
                           (string-cursor>? (regexp-match-ref m2 (+ i 1))
                                            (regexp-match-ref m1 (+ i 1))))))))
        #f)
       (else
        #t)))))

(define (regexp-match-max m1 m2)
  (if (regexp-match>=? m1 m2) m1 m2))

;; Merge match data from sr2 into sr1, preferring the leftmost-longest
;; match in the event of a conflict.
(define (searcher-merge! sr1 sr2)
  (let ((m (regexp-match-max (searcher-matches sr1) (searcher-matches sr2))))
    (searcher-matches-set! sr1 m)))

(define (searcher-max sr1 sr2)
  (if (or (not (searcher? sr2))
          (regexp-match>=? (searcher-matches sr1) (searcher-matches sr2)))
      sr1
      sr2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A posse is a group of searchers.

(define (make-posse . o)
  (make-hash-table eq?))

(define posse? hash-table?)
(define (posse-empty? posse) (zero? (hash-table-size posse)))

(define (posse-ref posse sr)
  (hash-table-ref/default posse (searcher-state sr) #f))
(define (posse-add! posse sr)
  (hash-table-set! posse (searcher-state sr) sr))
(define (posse-clear! posse)
  (hash-table-walk posse (lambda (key val) (hash-table-delete! posse key))))
(define (posse-for-each proc posse)
  (hash-table-walk posse (lambda (key val) (proc val))))

(define (posse->list posse)
  (hash-table-values posse))
(define (list->posse ls)
  (let ((searchers (make-posse)))
    (for-each (lambda (sr) (posse-add! searchers sr)) ls)
    searchers))
(define (posse . args)
  (list->posse args))

(define (make-start-searcher rx str)
  (make-searcher (rx-start-state rx) (make-regexp-match-for-rx rx str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution

;; A transition which doesn't advance the index.

(define (epsilon-state? st)
  (or (not (state-chars st))
      (procedure? (state-chars st))))

;; Match the state against a char and index.

(define (state-matches? st str i ch start end matches)
  (let ((matcher (state-chars st)))
    (cond
     ((char? matcher)
      (eqv? matcher ch))
     ((char-set? matcher)
      (char-set-contains? matcher ch))
     ((pair? matcher)
      (and (char<=? (car matcher) ch) (char<=? ch (cdr matcher))))
     ((procedure? matcher)
      (matcher str i ch start end matches))
     ((not matcher))
     (else
      (error "unknown state matcher" (state-chars st))))))

;; Advance epsilons together - if the State is newly added to the
;; group and is an epsilon state, recursively add the transition.

(define (posse-advance! new seen accept sr str i start end)
  (let advance! ((sr sr))
    (let ((st (searcher-state sr)))
      ;; Update match data.
      (cond
       ((state-match st)
        (let ((index (state-match st))
              (matches (searcher-matches sr)))
          (cond
           ((pair? index)
            ;; Submatch list, accumulate and push.
            (let* ((prev (regexp-match-ref matches (car index)))
                   (new (cons (match-collect matches (cdr index))
                              (if (pair? prev) prev '()))))
              (regexp-match-set! matches (car index) new)))
           (else
            (regexp-match-set! matches index i))))))
      ;; Follow transitions.
      (cond
       ((state-accept? st)
        (set-cdr! accept (searcher-max sr (cdr accept))))
       ((posse-ref seen sr)
        => (lambda (sr-prev) (searcher-merge! sr-prev sr)))
       ((epsilon-state? st)
        (let ((ch (and (string-cursor<? i end) (string-cursor-ref str i))))
          ;; Epsilon transition.  If there is a procedure matcher,
          ;; it's a guarded epsilon and needs to be checked.
          (cond
           ((state-matches? st str i ch start end (searcher-matches sr))
            (posse-add! seen sr)
            (let ((next1 (state-next1 st))
                  (next2 (state-next2 st)))
              (cond
               (next1
                (searcher-state-set! sr next1)
                (advance! sr))) 
              (cond
               (next2
                (let ((sr2 (make-searcher
                            next2
                            (copy-regexp-match (searcher-matches sr)))))
                  (advance! sr2)))))))))
       ;; Non-special, non-epsilon searcher, add to posse.
       ((posse-ref new sr)
        ;; Merge regexp-match for existing searcher.
        => (lambda (sr-prev) (searcher-merge! sr-prev sr)))
       (else
        ;; Add new searcher.
        (posse-add! new sr))))))

;; Run so long as there is more to match.

(define (regexp-run-offsets search? rx str start end)
  (let ((rx (regexp rx))
        (epsilons (posse))
        (accept (list #f)))
    (let lp ((i start)
             (searchers1 (posse))
             (searchers2 (posse)))
      ;; Advance initial epsilons once from the first index, or every
      ;; time when searching.
      (cond
       ((or search? (string-cursor=? i start))
        (posse-advance! searchers1 epsilons accept (make-start-searcher rx str)
                        str i start end)
        (posse-clear! epsilons))) 
      (cond
       ((or (string-cursor>=? i end)
            (and (or (not search?) (searcher? (cdr accept)))
                 (posse-empty? searchers1)))
        ;; Terminate when the string is done or there are no more
        ;; searchers.  If we terminate prematurely and are not
        ;; searching, return false.
        (and (searcher? (cdr accept))
             (let ((matches (searcher-matches (cdr accept))))
               (and (or search? (>= (regexp-match-ref matches 1) end))
                    (searcher-matches (cdr accept))))))
       (else
        ;; Otherwise advance normally.
        (let ((ch (string-cursor-ref str i))
              (i2 (string-cursor-next str i)))
          (posse-for-each  ;; NOTE: non-deterministic from hash order
           (lambda (sr)
             (cond
              ((state-matches? (searcher-state sr) str i ch
                               start end (searcher-matches sr))
               (searcher-state-set! sr (state-next1 (searcher-state sr)))
               ;; Epsilons are considered at the next position.
               (posse-advance! searchers2 epsilons accept sr str i2 start end)
               (posse-clear! epsilons))))
           searchers1)
          (posse-clear! searchers1)
          (lp i2 searchers2 searchers1)))))))

;; Wrapper to determine start and end offsets.

(define (regexp-run search? rx str . o)
  (let ((start (string-start-arg str o))
        (end (string-end-arg str (if (pair? o) (cdr o) o))))
    (regexp-run-offsets search? rx str start end)))

;;> Match the given regexp or SRE against the entire string and return
;;> the match data on success.  Returns \scheme{#f} on failure.

(define (regexp-matches rx str . o)
  (apply regexp-run #f rx str o))

;;> Match the given regexp or SRE against the entire string and return
;;> the \scheme{#t} on success.  Returns \scheme{#f} on failure.

(define (regexp-matches? rx str . o)
  (and (apply regexp-matches rx str o) #t))

;;> Search for the given regexp or SRE within string and return
;;> the match data on success.  Returns \scheme{#f} on failure.

(define (regexp-search rx str . o)
  (apply regexp-run #t rx str o))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiling

(define (parse-flags ls)
  (define (symbol->flag s)
    (case s ((i ci case-insensitive) ~ci?) (else ~none)))
  (let lp ((ls ls) (res ~none))
    (if (not (pair? ls))
        res
        (lp (cdr ls) (flag-join res (symbol->flag (car ls)))))))

(define char-set:nonl
  (char-set-difference char-set:full (char-set #\newline)))
(define char-set:control (ucs-range->char-set 0 32))
(define char-set:word-constituent
  (char-set-union char-set:letter char-set:digit (char-set #\_)))
(define %char-set:word-constituent
  (char-set-union %char-set:letter %char-set:digit (char-set #\_)))
(define (char-word-constituent? ch)
  (char-set-contains? char-set:word-constituent ch))
(define get-char-set:cased
  (let ((char-set:cased #f))
    (lambda ()
      (if (not char-set:cased)
          (set! char-set:cased
                (char-set-union char-set:upper-case
                                char-set:lower-case
                                char-set:title-case)))
      char-set:cased)))

(define (match/bos str i ch start end matches)
  (string-cursor=? i start))
(define (match/eos str i ch start end matches)
  (string-cursor>=? i end))
(define (match/bol str i ch start end matches)
  (or (string-cursor=? i start)
      (eqv? #\newline (string-cursor-ref str (string-cursor-prev str i)))))
(define (match/eol str i ch start end matches)
  (or (string-cursor>=? i end)
      (eqv? #\newline (string-cursor-ref str i))))
(define (match/bow str i ch start end matches)
  (and (string-cursor<? i end)
       (or (string-cursor=? i start)
           (not (char-word-constituent?
                 (string-cursor-ref str (string-cursor-prev str i)))))
       (char-word-constituent? ch)))
(define (match/eow str i ch start end matches)
  (and (or (string-cursor>=? i end)
           (not (char-word-constituent? ch)))
       (string-cursor>? i start)
       (char-word-constituent?
        (string-cursor-ref str (string-cursor-prev str i)))))
(define (match/nwb str i ch start end matches)
  (and (not (match/bow str i ch start end matches))
       (not (match/eow str i ch start end matches))))
(define (match/bog str i ch start end matches)
  (and (string-cursor<? i end)
       (or (string-cursor=? i start)
           (match/eog str (string-cursor-prev str i) ch start end matches))))
(define (match/eog str i ch start end matches)
  (and (string-cursor>? i start)
       (or (string-cursor>=? i end)
           (let ((m (regexp-search re:grapheme str
                                   (string-offset->index str i)
                                   (string-offset->index str end))))
             (and m (string-cursor<=? (regexp-match-submatch-end m 0) i))))))

(define (lookup-char-set name flags)
  (cond
   ((flag-set? flags ~ascii?)
    (case name
      ((any) char-set:full)
      ((nonl) char-set:nonl)
      ((lower-case lower)
       (if (flag-set? flags ~ci?) %char-set:letter %char-set:lower-case))
      ((upper-case upper)
       (if (flag-set? flags ~ci?) %char-set:letter %char-set:upper-case))
      ((title-case title)
       (if (flag-set? flags ~ci?) %char-set:letter (char-set)))
      ((alphabetic alpha) %char-set:letter)
      ((numeric num digit) %char-set:digit)
      ((alphanumeric alphanum alnum) %char-set:letter+digit)
      ((punctuation punct) %char-set:punctuation)
      ((graphic graph) %char-set:graphic)
      ((word-constituent) %char-set:word-constituent)
      ((whitespace white space) %char-set:whitespace)
      ((printing print) %char-set:printing)
      ((control cntrl) %char-set:iso-control)
      ((hex-digit xdigit hex) char-set:hex-digit)
      ((ascii) char-set:ascii)
      (else #f)))
   (else
    (case name
      ((any) char-set:full)
      ((nonl) char-set:nonl)
      ((lower-case lower)
       (if (flag-set? flags ~ci?) (get-char-set:cased) char-set:lower-case))
      ((upper-case upper)
       (if (flag-set? flags ~ci?) (get-char-set:cased) char-set:upper-case))
      ((title-case title)
       (if (flag-set? flags ~ci?) (get-char-set:cased) char-set:title-case))
      ((alphabetic alpha) char-set:letter)
      ((numeric num digit) char-set:digit)
      ((alphanumeric alphanum alnum) char-set:letter+digit)
      ((punctuation punct) char-set:punctuation)
      ((graphic graph) char-set:graphic)
      ((word-constituent) char-set:word-constituent)
      ((whitespace white space) char-set:whitespace)
      ((printing print) char-set:printing)
      ((control cntrl) char-set:control)
      ((hex-digit xdigit hex) char-set:hex-digit)
      ((ascii) char-set:ascii)
      (else #f)))))

(define (sre-flatten-ranges orig-ls)
  (let lp ((ls orig-ls) (res '()))
    (cond
     ((null? ls)
      (reverse res))
     ((string? (car ls))
      (lp (append (string->list (car ls)) (cdr ls)) res))
     ((null? (cdr ls))
      (error "unbalanced cset / range" orig-ls))
     ((string? (cadr ls))
      (lp (cons (car ls) (append (string->list (cadr ls)) (cddr ls))) res))
     (else
      (lp (cddr ls) (cons (cons (car ls) (cadr ls)) res))))))

(define (every pred ls)
  (or (null? ls) (and (pred (car ls)) (every pred (cdr ls)))))

(define (char-set-sre? sre)
  (or (char? sre)
      (and (string? sre) (= 1 (string-length sre)))
      (lookup-char-set sre ~none)
      (and (pair? sre)
           (or (string? (car sre))
               (memq (car sre)
                     '(char-set / char-range & and ~ complement - difference))
               (and (memq (car sre) '(|\|| or))
                    (every char-set-sre? (cdr sre)))))))

(define (valid-sre? x)
  (guard (exn (else #f)) (regexp x) #t))

(define (sre->char-set sre . o)
  (let ((flags (if (pair? o) (car o) ~none)))
    (define (->cs sre) (sre->char-set sre flags))
    (cond
     ((lookup-char-set sre flags))
     ((char-set? sre) (char-set-ci sre))
     ((char? sre) (char-set-ci (char-set sre)))
     ((string? sre)
      (if (= 1 (string-length sre))
          (string->char-set sre)
          (error "only single char strings can be char-sets")))
     ((pair? sre)
      (if (string? (car sre))
          (string->char-set (car sre))
          (case (car sre)
            ((char-set) (string->char-set (cadr sre)))
            ((/ char-range)
             (->cs
              `(or ,@(map (lambda (x)
                            (char-set-ci
                             (ucs-range->char-set
                              (char->integer (car x))
                              (+ 1 (char->integer (cdr x))))))
                          (sre-flatten-ranges (cdr sre))))))
            ((& and) (apply char-set-intersection (map ->cs (cdr sre))))
            ((|\|| or) (apply char-set-union (map ->cs (cdr sre))))
            ((~ complement) (char-set-complement (->cs `(or ,@(cdr sre)))))
            ((- difference) (char-set-difference (->cs (cadr sre))
                                                 (->cs `(or ,@(cddr sre)))))
            (else (error "invalid sre char-set" sre)))))
     (else (error "invalid sre char-set" sre)))))

(define (char-set->sre cset)
  (list (char-set->string cset)))

(define (strip-submatches sre)
  (if (pair? sre)
      (case (car sre)
        (($ submatch) (strip-submatches (cons ': (cdr sre))))
        ((-> => submatch-named) (strip-submatches (cons ': (cddr sre))))
        (else (cons (strip-submatches (car sre))
                    (strip-submatches (cdr sre)))))
      sre))

(define (sre-expand-reps from to sre)
  (let ((sre0 (strip-submatches sre)))
    (let lp ((i 0) (res '(:)))
      (if (= i from)
          (cond
           ((not to)
            (reverse (cons `(* ,sre) res)))
           ((= from to)
            (reverse (cons sre (cdr res))))
           (else
            (let lp ((i (+ i 1)) (res res))
              (if (>= i to)
                  (reverse (cons `(? ,sre) res))
                  (lp (+ i 1) (cons `(? ,sre0) res))))))
          (lp (+ i 1) (cons sre0 res))))))

;;> Compile an \var{sre} into a regexp.

(define (regexp sre . o)
  (define current-index 2)
  (define current-match 0)
  (define match-names '())
  (define match-rules (list (cons 0 1)))
  (define (make-submatch-state sre flags next index)
    (let* ((n3 (make-epsilon-state next))
           (n2 (->rx sre flags n3))
           (n1 (make-epsilon-state n2)))
      (state-match-set! n1 index)
      (state-match-rule-set! n1 'left)
      (state-match-set! n3 (+ index 1))
      (state-match-rule-set! n3 'right)
      n1))
  (define (->rx sre flags next)
    (cond
     ;; The base cases chars and strings match literally.
     ((char? sre)
      (make-char-state sre flags next))
     ((char-set? sre)
      (make-char-state sre flags next))
     ((string? sre)
      (->rx (cons 'seq (string->list sre)) flags next))
     ((and (symbol? sre) (lookup-char-set sre flags))
      => (lambda (cset) (make-char-state cset ~none next)))
     ((symbol? sre)
      (case sre
        ((epsilon) next)
        ((bos) (make-char-state match/bos flags next))
        ((eos) (make-char-state match/eos flags next))
        ((bol) (make-char-state match/bol flags next))
        ((eol) (make-char-state match/eol flags next))
        ((bow) (make-char-state match/bow flags next))
        ((eow) (make-char-state match/eow flags next))
        ((nwb) (make-char-state match/nwb flags next))
        ((bog) (make-char-state match/bog flags next))
        ((eog) (make-char-state match/eog flags next))
        ((grapheme)
         (->rx
          `(or (: (* ,char-set:hangul-l) (+ ,char-set:hangul-v)
                  (* ,char-set:hangul-t))
               (: (* ,char-set:hangul-l) ,char-set:hangul-v
                  (* ,char-set:hangul-v) (* ,char-set:hangul-t))
               (: (* ,char-set:hangul-l) ,char-set:hangul-lvt
                  (* ,char-set:hangul-t))
               (+ ,char-set:hangul-l)
               (+ ,char-set:hangul-t)
               (+ ,char-set:regional-indicator)
               (: "\r\n")
               (: (~ control ("\r\n"))
                  (+ ,char-set:extend-or-spacing-mark))
               control)
          flags
          next))
        ((word) (->rx '(word+ any) flags next))
        (else (error "unknown sre" sre))))
     ((pair? sre)
      (case (car sre)
        ((seq :)
         ;; Sequencing.  An empty sequence jumps directly to next,
         ;; otherwise we join the first element to the sequence formed
         ;; of the remaining elements followed by next.
         (if (null? (cdr sre))
             next
             ;; Make a dummy intermediate to join the states so that
             ;; we can generate n1 first, preserving the submatch order.
             (let* ((n2 (make-epsilon-state #f))
                    (n1 (->rx (cadr sre) flags n2))
                    (n3 (->rx (cons 'seq (cddr sre)) flags next)))
               (state-next1-set! n2 n3)
               n1)))
        ((or |\||)
         ;; Alternation.  An empty alternation always fails.
         ;; Otherwise we fork between any of the alternations, each
         ;; continuing to next.
         (cond
          ((null? (cdr sre))
           #f)
          ((char-set-sre? sre)
           (make-char-state (sre->char-set sre) flags next))
          ((null? (cddr sre))
           (->rx (cadr sre) flags next))
          (else
           (let* ((n1 (->rx (cadr sre) flags next))
                  (n2 (->rx (cons 'or (cddr sre)) flags next)))
             (make-fork-state n1 n2)))))
        ((? optional)
         ;; Optionality.  Either match the body or fork to the next
         ;; state directly.
         (make-fork-state (->rx (cons 'seq (cdr sre)) flags next) next))
        ((* zero-or-more)
         ;; Repetition.  Introduce two fork states which can jump from
         ;; the end of the loop to the beginning and from the
         ;; beginning to the end (to skip the first iteration).
         (let* ((n2 (make-fork-state next #f))
                (n1 (make-fork-state (->rx (cons 'seq (cdr sre)) flags n2) n2)))
           (state-next2-set! n2 n1)
           n1))
        ((+ one-or-more)
         ;; One-or-more repetition.  Same as above but the first
         ;; transition is required so the rx is simpler - we only
         ;; need one fork from the end of the loop to the beginning.
         (let* ((n2 (make-fork-state next #f))
                (n1 (->rx (cons 'seq (cdr sre)) flags n2)))
           (state-next2-set! n2 n1)
           n1))
        ((= exactly)
         ;; Exact repetition.
         (->rx (sre-expand-reps (cadr sre) (cadr sre) (cons 'seq (cddr sre)))
               flags next))
        ((>= at-least)
         ;; n-or-more repetition.
         (->rx (sre-expand-reps (cadr sre) #f (cons 'seq (cddr sre)))
               flags next))
        ((** repeated)
         ;; n-to-m repetition.
         (->rx (sre-expand-reps (cadr sre) (car (cddr sre))
                                (cons 'seq (cdr (cddr sre))))
               flags next))
        ((-> => submatch-named)
         ;; Named submatches just record the name for the current
         ;; match and rewrite as a non-named submatch.
         (cond
          ((flag-set? flags ~nocapture?)
           (->rx (cons 'seq (cddr sre)) flags next))
          (else
           (set! match-names
                 (cons (cons (cadr sre) (+ 1 current-match)) match-names))
           (->rx (cons 'submatch (cddr sre)) flags next))))
        ((*-> *=> submatch-named-list)
         (cond
          ((flag-set? flags ~nocapture?)
           (->rx (cons 'seq (cddr sre)) flags next))
          (else
           (set! match-names (cons (cons (cadr sre) current-match) match-names))
           (->rx (cons 'submatch-list (cddr sre)) flags next))))
        (($ submatch)
         ;; A submatch wraps next with an epsilon transition before
         ;; next, setting the start and end index on the result and
         ;; wrapped next respectively.
         (cond
          ((flag-set? flags ~nocapture?)
           (->rx (cons 'seq (cdr sre)) flags next))
          (else
           (let ((num current-match)
                 (index current-index))
             (set! current-match (+ current-match 1))
             (set! current-index (+ current-index 2))
             (set! match-rules `((,index . ,(+ index 1)) ,@match-rules))
             (make-submatch-state (cons 'seq (cdr sre)) flags next index)))))
        ((*$ submatch-list)
         ;; A submatch-list wraps a range of submatch results into a
         ;; single match value.
         (cond
          ((flag-set? flags ~nocapture?)
           (->rx (cons 'seq (cdr sre)) flags next))
          (else
           (let* ((num current-match)
                  (index current-index))
             (set! current-match (+ current-match 1))
             (set! current-index (+ current-index 1))
             (set! match-rules `(,index ,@match-rules))
             (let* ((n2 (make-epsilon-state next))
                    (n1 (->rx (cons 'submatch (cdr sre)) flags n2)))
               (state-match-set! n2 (list index num current-match))
               (state-match-rule-set! n2 'list)
               n1)))))
        ((~ - & / complement difference and char-range char-set)
         (make-char-state (sre->char-set sre flags) ~none next))
        ((word)
         (->rx `(: bow ,@(cdr sre) eow) flags next))
        ((word+)
         (->rx `(word (+ ,(if (equal? '(any) (cdr sre))
                              'word-constituent
                              (char-set-intersection
                               char-set:word-constituent
                               (sre->char-set `(or ,@(cdr sre)) flags)))))
               flags
               next))
        ((w/case)
         (->rx `(: ,@(cdr sre)) (flag-clear flags ~ci?) next))
        ((w/nocase)
         (->rx `(: ,@(cdr sre)) (flag-join flags ~ci?) next))
        ((w/unicode)
         (->rx `(: ,@(cdr sre)) (flag-clear flags ~ascii?) next))
        ((w/ascii)
         (->rx `(: ,@(cdr sre)) (flag-join flags ~ascii?) next))
        ((w/nocapture)
         (->rx `(: ,@(cdr sre)) (flag-join flags ~nocapture?) next))
        (else
         (if (string? (car sre))
             (make-char-state (sre->char-set sre flags) ~none next)
             (error "unknown sre" sre)))))))
  (let ((flags (parse-flags (and (pair? o) (car o)))))
    (if (regexp? sre)
        sre
        (let ((start (make-submatch-state sre flags (make-accept-state) 0)))
          (make-rx start current-match current-index
                   (list->vector (reverse match-rules)) match-names sre)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define (regexp-fold rx kons knil str . o)
  (let* ((rx (regexp rx))
         (finish (if (pair? o) (car o) (lambda (from md str acc) acc)))
         (o (if (pair? o) (cdr o) o))
         (start (string-start-arg str o))
         (end (string-end-arg str (if (pair? o) (cdr o) o))))
    (let lp ((i start)
             (from start)
             (acc knil))
      (cond
       ((and (string-cursor<? i end) (regexp-run-offsets #t rx str i end))
        => (lambda (md)
             (let ((j (regexp-match-ref md 1)))
               (lp (if (and (string-cursor=? i j) (string-cursor<? j end))
                       (string-cursor-next str j)
                       j)
                   j
                   (kons (string-offset->index str from) md str acc)))))
       (else
        (finish (string-offset->index str from) #f str acc))))))

(define (regexp-extract rx str . o)
  (apply regexp-fold
         rx
         (lambda (from md str a)
           (let ((s (regexp-match-submatch md 0)))
             (if (equal? s "") a (cons s a))))
         '()
         str
         (lambda (from md str a) (reverse a))
         o))

(define (regexp-split rx str . o)
  ;; start and end in indices passed to regexp-fold
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (regexp-fold
     rx
     (lambda (from md str a)
       (let ((i (regexp-match-submatch-start md 0)))
         (if (< from i) (cons (substring str from i) a) a)))
     '()
     str
     (lambda (from md str a)
       (reverse (if (< from end) (cons (substring str from end) a) a)))
     start
     end)))

(define (regexp-replace rx str subst . o)
  (let* ((start (if (and (pair? o) (car o)) (car o) 0))
         (o (if (pair? o) (cdr o) '()))
         (end (if (and (pair? o) (car o)) (car o) (string-length str)))
         (o (if (pair? o) (cdr o) '()))
         (count (if (pair? o) (car o) 0)))
    (let lp ((i start) (count count))
      (let ((m (regexp-search rx str i end)))
        (cond
         ((not m) str)
         ((positive? count)
          (lp (regexp-match-submatch-end m 0) (- count 1)))
         (else
          (string-concatenate
           (cons
            (substring-cursor str
                              (string-index->offset str start)
                              (regexp-match-submatch-start m 0))
            (append
             (reverse (regexp-apply-match m str subst))
             (list (substring-cursor str
                                     (regexp-match-submatch-end m 0)
                                     (string-index->offset str end))))))))))))

(define (regexp-replace-all rx str subst . o)
  (regexp-fold
   rx
   (lambda (i m str acc)
     (let ((m-start (regexp-match-submatch-start m 0)))
       (append (regexp-apply-match m str subst)
               (if (>= i m-start)
                   acc
                   (cons (substring str i m-start) acc)))))
   '()
   str
   (lambda (i m str acc)
     (let ((end (string-length str)))
       (string-concatenate-reverse
        (if (>= i end)
            acc
            (cons (substring str i end) acc)))))))

(define (regexp-apply-match m str ls)
  (let lp ((ls ls) (res '()))
    (cond
     ((null? ls)
      res)
     ((not (pair? ls))
      (lp (list ls) res))
     ((integer? (car ls))
      (lp (cdr ls) (cons (or (regexp-match-submatch m (car ls)) "") res)))
     ((procedure? (car ls))
      (lp (cdr ls) (cons ((car ls) m) res)))
     ((symbol? (car ls))
      (case (car ls)
        ((pre)
         (lp (cdr ls)
             (cons (substring-cursor str 0 (regexp-match-submatch-start m 0))
                   res)))
        ((post)
         (lp (cdr ls)
             (cons (substring str
                              (regexp-match-submatch-end m 0)
                              (string-length str))
                   res)))
        (else
         (cond
          ((assq (car ls) (regexp-match-names m))
           => (lambda (x) (lp (cons (cdr x) (cdr ls)) res)))
          (else
           (error "unknown match replacement" (car ls)))))))
     (else
      (lp (cdr ls) (cons (car ls) res))))))

(define re:grapheme (regexp 'grapheme))
