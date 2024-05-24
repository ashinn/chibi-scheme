
(define (mark-anchor! mark)
  (text-marks-set! (mark-text mark) (cons mark (text-marks (mark-text mark))))
  mark)

(define (mark-release! mark)
  (text-marks-set! (mark-text mark) (remove! mark (text-marks (mark-text mark))))
  mark)

(define (mark-copy mark)
  (make-mark (mark-text mark) (mark-offset mark) (mark-data mark)))

(define (mark-copy! mark)
  (let ((res (mark-copy mark)))
    (mark-anchor! res)
    res))

;;> Returns a new mark into \var{text} pointing at the current
;;> codepoint offset indicated by index (default the end of the
;;> text).  Subsequent mutations to \var{text} may change the
;;> offset of the mark, but not it's relation to the surrounding
;;> text.
(define (text-mark! text index . o)
  (mark-anchor! (apply text-mark text index o)))

;;> Similar to \scheme{text-mark!}, but doesn't anchor the new
;;> mark, such that mutations to \var{text} may break it.
(define (text-mark text index . o)
  (receive (text mark) (apply text&mark-at text index o)
    mark))

;;> (text&mark-at text mark-or-index [data])
(define (text&mark-at text index . o)
  (if (mark? index)
      (values (mark-text index) index)
      (let ((at-offset (if (pair? o)
                           (if (mark? (car o)) (mark-offset (car o)) (car o))
                           (text-start text)))
            (data (and (pair? o) (pair? (cdr o)) (cadr o))))
        (let lp ((n index)
                 (text text)
                 (bv (text-bytes text))
                 (sc (text-start text)))
          (cond
           ((positive? n)
            (if (>= sc (text-end text))
                (let ((text2 (text-next text)))
                  (lp n text2 (text-bytes text2) (text-start text2)))
                (lp (- n 1) text bv (utf8-next bv sc (text-end text)))))
           ((zero? n)
            (values text (make-mark text sc data)))
           (else
            (if (<= sc (text-start text))
                (let ((text2 (text-prev text)))
                  (lp n text2 (text-bytes text2) (text-end text2)))
                (lp (+ n 1) text bv (utf8-prev bv sc (text-start text))))))))))
