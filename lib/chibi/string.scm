;; strings.scm -- cursor-oriented string library
;; Copyright (c) 2012-2015 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> \section{High-level API}

;;> The procedures below are similar to those in SRFI 13 or other
;;> string libraries, except instead of receiving and returning
;;> character indexes they use opaque string cursors.

;;> \procedure{(string-null? str)}
;;> Returns true iff \var{str} is equal to the empty string \scheme{""}.

(define (string-null? str)
  (equal? str ""))

(define (->cursor str x)
  (if (string-cursor? x)
      x
      (string-index->cursor str x)))

(define (make-char-predicate x)
  (cond ((procedure? x) x)
        ((char? x) (lambda (ch) (eq? ch x)))
        ((char-set? x) (lambda (ch) (char-set-contains? x ch)))
        (else (error "invalid character predicate" x))))

(define (complement pred) (lambda (x) (not (pred x))))

;;> Returns true iff \var{check} is true for any character in
;;> \var{str}.  \var{check} can be a procedure, char (to test for
;;> \scheme{char=?} equivalence) or char-set (to test for
;;> \var{char-set-contains?}).  Always returns false if \var{str} is
;;> empty.

(define (string-any check str . o)
  (let ((pred (make-char-predicate check))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (->cursor str (cadr o))
                 (string-cursor-end str))))
    (and (string-cursor>? end (if (pair? o)
                                  (->cursor str (car o))
                                  (string-cursor-start str)))
         (let lp ((i (string-cursor-start str)))
           (let ((i2 (string-cursor-next str i))
                 (ch (string-cursor-ref str i)))
             (if (string-cursor>=? i2 end)
                 (pred ch)  ;; tail call
                 (or (pred ch) (lp i2))))))))

;;> Returns true iff \var{check} is true for every character in
;;> \var{str}.  \var{check} can be a procedure, char or char-set as in
;;> \scheme{string-any}.  Always returns true if \var{str} is empty.

(define (string-every check str . o)
  (not (apply string-any (complement (make-char-predicate check)) str o)))

;;> Returns a cursor pointing to the first position from the left in
;;> string for which \var{check} is true.  \var{check} can be a
;;> procedure, char or char-set as in \scheme{string-any}.  The
;;> optional cursors \var{start} and \var{end} can specify a substring
;;> to search, and default to the whole string.  Returns a cursor just
;;> past the end of \var{str} if no character matches.

(define (string-find str check . o)
  (let ((pred (make-char-predicate check))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (->cursor str (cadr o))
                 (string-cursor-end str))))
    (let lp ((i (if (pair? o) (car o) (string-cursor-start str))))
      (cond ((string-cursor>=? i end) end)
            ((pred (string-cursor-ref str i)) i)
            (else (lp (string-cursor-next str i)))))))

;;> As above, ignoring the position and returning true iff any
;;> character matches.

(define (string-find? str check . o)
  (let ((start (if (pair? o) (->cursor str (car o)) (string-cursor-start str)))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (->cursor str (cadr o))
                 (string-cursor-end str))))
    (string-cursor<? (string-find str check start end) end)))

;;> As \scheme{string-find}, but returns the position of the first
;;> character from the right of \var{str}.  If no character matches,
;;> returns a string cursor pointing just before \var{start}.

(define (string-find-right str check . o)
  (let ((pred (make-char-predicate check))
        (start (if (pair? o) (->cursor str (car o)) (string-cursor-start str))))
    (let lp ((i (if (and (pair? o) (pair? (cdr o)))
                    (->cursor str (cadr o))
                    (string-cursor-end str))))
      (let ((i2 (string-cursor-prev str i)))
        (cond ((string-cursor<? i2 start) start)
              ((pred (string-cursor-ref str i2)) i)
              (else (lp i2)))))))

;;> As \scheme{string-find}, but inverts the check, returning the
;;> position of the first character which doesn't match.

(define (string-skip str check . o)
  (apply string-find str (complement (make-char-predicate check)) o))

;;> As \scheme{string-find-right}, but inverts the check, returning
;;> the position of the first character which doesn't match.

(define (string-skip-right str check . o)
  (apply string-find-right str (complement (make-char-predicate check)) o))

;;> \procedure{(string-join list-of-strings [separator])}
;;>
;;> Concatenates the \var{list-of-strings} and return the result as a
;;> single string.  If \var{separator} is provided it is inserted
;;> between each pair of strings.

(define string-join string-concatenate)

;;> Split \var{str} into a list of substrings separated by \var{pred},
;;> which defaults to \scheme{#\\space}.  Multiple adjacent characters
;;> which satisy \var{pred} will result in empty strings in the list.
;;> If the optional \var{limit} is provided, splits into at most that
;;> many substrings starting from the left.

(define (string-split str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) #\space)))
        (limit (if (and (pair? o) (pair? (cdr o)))
                   (cadr o)
                   (+ 1 (string-size str))))
        (start (string-cursor-start str))
        (end (string-cursor-end str)))
    (if (string-cursor>=? start end)
        '()
        (let lp ((i start) (n 1) (res '()))
          (cond
           ((>= n limit)
            (reverse (cons (substring-cursor str i) res)))
           (else
            (let* ((j (string-find str pred i))
                   (res (cons (substring-cursor str i j) res)))
              (if (string-cursor>=? j end)
                  (reverse res)
                  (lp (string-cursor-next str j) (+ n 1) res)))))))))

;;> Returns a copy of the string \var{str} with all characters
;;> matching \var{pred} (default \scheme{#\\space}) removed from the
;;> left.

(define (string-trim-left str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) #\space))))
    (substring-cursor str (string-skip str pred))))

;;> Returns a copy of the string \var{str} with all characters
;;> matching \var{pred} (default \scheme{#\\space}) removed from the
;;> right.

(define (string-trim-right str . o)
  (let ((pred (make-char-predicate (if (pair? o) (car o) #\space))))
    (substring-cursor str
                      (string-cursor-start str)
                      (string-skip-right str pred))))

;;> Returns a copy of the string \var{str} with all characters
;;> matching \var{pred} (default \scheme{#\\space}) removed from both
;;> sides.

(define (string-trim str . o)
  (let* ((pred (if (pair? o) (car o) #\space))
         (left (string-skip str pred))
         (right (string-skip-right str pred)))
    (if (string-cursor>=? left right)
        ""
        (substring-cursor str left right))))

;;> Returns two values: the first cursors from the left in
;;> \var{prefix} and in \var{str} where the two strings don't match.

(define (string-mismatch prefix str)
  (let ((end1 (string-cursor-end prefix))
        (end2 (string-cursor-end str)))
    (let lp ((i (string-cursor-start prefix))
             (j (string-cursor-start str)))
      (if (or (string-cursor>=? i end1)
              (string-cursor>=? j end2)
              (not (eq? (string-cursor-ref prefix i) (string-cursor-ref str j))))
          (values i j)
          (lp (string-cursor-next prefix i) (string-cursor-next str j))))))

;;> Returns two values: the first cursors from the right in
;;> \var{prefix} and in \var{str} where the two strings don't match.

(define (string-mismatch-right suffix str)
  (let ((end1 (string-cursor-start suffix))
        (end2 (string-cursor-start str)))
    (let lp ((i (string-cursor-prev suffix (string-cursor-end suffix)))
             (j (string-cursor-prev str (string-cursor-end str))))
      (if (or (string-cursor<? i end1)
              (string-cursor<? j end2)
              (not (eq? (string-cursor-ref suffix i) (string-cursor-ref str j))))
          (values i j)
          (lp (string-cursor-prev suffix i) (string-cursor-prev str j))))))

;;> Returns true iff \var{prefix} is a prefix of \var{str}.

(define (string-prefix? prefix str)
  (call-with-values (lambda () (string-mismatch prefix str))
    (lambda (i j) (string-cursor=? (string-cursor-end prefix) i))))

;;> Returns true iff \var{suffix} is a suffix of \var{str}.

(define (string-suffix? suffix str)
  (let ((diff (- (string-size str) (string-size suffix))))
    (and (>= diff 0)
         (string-cursor=? (string-cursor-prev suffix
                                              (string-cursor-start suffix))
                          (string-cursor-back
                           str
                           (call-with-values
                               (lambda () (string-mismatch-right suffix str))
                             (lambda (i j) j))
                           diff)))))

;;> The fundamental string iterator.  Calls \var{kons} on each
;;> character of \var{str} and an accumulator, starting with
;;> \var{knil}.  If multiple strings are provided, calls \var{kons} on
;;> the corresponding characters of all strings, with the accumulator
;;> as the final argument, and terminates when the shortest string
;;> runs out.

(define (string-fold kons knil str . los)
  (if (null? los)
      (let ((end (string-cursor-end str)))
        (let lp ((i (string-cursor-start str)) (acc knil))
          (if (string-cursor>=? i end)
              acc
              (lp (string-cursor-next str i)
                  (kons (string-cursor-ref str i) acc)))))
      (let ((los (cons str los)))
        (let lp ((is (map string-cursor-start los))
                 (acc knil))
          (if (any (lambda (str i)
                     (string-cursor>=? i (string-cursor-end str)))
                   los is)
              acc
              (lp (map string-cursor-next los is)
                  (apply kons (append (map string-cursor-ref los is)
                                      (list acc)))))))))

;;> Equivalent to \scheme{string-fold}, but iterates over \var{str}
;;> from right to left.

(define (string-fold-right kons knil str)
  (let ((end (string-cursor-end str)))
    (let lp ((i (string-cursor-start str)))
      (if (string-cursor>=? i end)
          knil
          (kons (string-cursor-ref str i) (lp (string-cursor-next str i)))))))

;;> \procedure{(string-map proc str)}
;;>
;;> Returns a new string composed of applying the procedure \var{proc}
;;> to every character in \var{string}.

;;> \procedure{(string-for-each proc str)}
;;>
;;> Apply \var{proc} to every character in \var{str} in order and
;;> discard the result.

;;> \procedure{(string-count str check)}
;;>
;;> Count the number of characters in \var{str} for which \var{check}
;;> is true.

(define (string-count str check)
  (let ((pred (make-char-predicate check)))
    (string-fold (lambda (ch count) (if (pred ch) (+ count 1) count)) 0 str)))

;;> \procedure{(string-contains s1 s2 [start])}
;;>
;;> Returns a cursor pointing to the first position in the string
;;> \var{s1} where \var{s2} occurs, or \scheme{#f} if there is no such
;;> match.

;;> \procedure{(make-string-searcher needle)}
;;>
;;> Partial application of \scheme{string-contains}.  Return a
;;> procedure of one argument, a string, which runs
;;> \scheme{(string-contains str \var{needle})}.

(define (make-string-searcher needle)
  (lambda (haystack) (string-contains haystack needle)))

;;> Return a copy of string \var{s} with all 26 upper-case ASCII
;;> characters mapped to their corresponding 26 lower-case ASCII
;;> characters.

(define (string-downcase-ascii s)
  (call-with-output-string
    (lambda (out)
      (string-for-each (lambda (ch) (write-char (char-downcase ch) out)) s))))

;;> Return a copy of string \var{s} with all 26 lower-case ASCII
;;> characters mapped to their corresponding 26 upper-case ASCII
;;> characters.

(define (string-upcase-ascii s)
  (call-with-output-string
    (lambda (out)
      (string-for-each (lambda (ch) (write-char (char-upcase ch) out)) s))))

;;> \section{Cursor API}

;;> \procedure{(substring-cursor str i [j])}
;;>
;;> Returns the substring of \var{str} between \var{i} (inclusive) and
;;> optional \var{j} (exclusive), which defaults to the end of the
;;> string.

;;> \procedure{(string-cursor-ref str i)}
;;>
;;> Returns the character of \var{str} at position \var{i}.

;;> \procedure{(string-cursor-start str)}
;;>
;;> Returns a string cursor pointing to the start of \var{str}.

;;> \procedure{(string-cursor-end str)}
;;>
;;> Returns a string cursor pointing just past the end of \var{str}.

;;> \procedure{(string-cursor-next str i)}
;;>
;;> Returns a string cursor to the character in \var{str} just after
;;> the cursor \var{i}.

;;> \procedure{(string-cursor-prev str i)}
;;>
;;> Returns a string cursor to the character in \var{str} just before
;;> the cursor \var{i}.

(define (string-cursor-forward str cursor n)
  (if (positive? n)
      (string-cursor-forward str (string-cursor-next str cursor) (- n 1))
      cursor))

(define (string-cursor-back str cursor n)
  (if (positive? n)
      (string-cursor-back str (string-cursor-prev str cursor) (- n 1))
      cursor))

;;> \procedure{(string-cursor<? i j)}
;;> \procedure{(string-cursor>? i j)}
;;> \procedure{(string-cursor=? i j)}
;;> \procedure{(string-cursor<=? i j)}
;;> \procedure{(string-cursor>=? i j)}
;;>
;;> String cursor comparators.
;;/
