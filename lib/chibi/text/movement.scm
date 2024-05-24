
;;> Advances \var{mark} \var{count} codepoints forward (default 1),
;;> and returns \var{mark}.  If \var{count} is negative, moves
;;> backwards.  If this would advance beyond the end (or before the
;;> beginning) of the text, \var{mark} is bound to the end (start).
(define (text-forward-char! mark)
  (cond
   ((< (mark-offset mark) (text-end (mark-text mark)))
    ;; there was space in the current piece
    (let ((ch (utf8-ref (text-bytes (mark-text mark)) (mark-offset mark)))
          (offset2 (utf8-next (text-bytes (mark-text mark))
                              (mark-offset mark)
                              (text-end (mark-text mark)))))
      (mark-offset-set! mark offset2)
      ch))
   (else
    (let lp ((text (text-next (mark-text mark))))
      (and text
           (if (< (text-start text) (text-end text))
               ;; advanced to a new piece, need to also move the mark
               (let ((ch (utf8-ref (text-bytes text) (text-start text)))
                     (offset2 (utf8-next (text-bytes text)
                                         (text-start text)
                                         (text-end text))))
                 (mark-offset-set! mark offset2)
                 (text-marks-set! (mark-text mark)
                                  (delete (text-marks (mark-text mark)) mark))
                 (text-marks-set! text (cons mark (text-marks text)))
                 ch)
               (lp (text-next text))))))))

;;> Moves \var{mark} \var{count} codepoints backward (default 1),
;;> and returns the new char pointed to, or \scheme{#f} at the start
;;> of text.  If \var{count} is negative, moves forward.  If this
;;> would advance before the beginning (or beyond the end) of the
;;> text, \var{mark} is bound to the start (end).
(define (text-backward-char! mark)
  (cond
   ((> (mark-offset mark) (text-start (mark-text mark)))
    (cond
     ((utf8-prev (text-bytes (mark-text mark))
                 (mark-offset mark)
                 (text-start (mark-text mark)))
      => (lambda (offset) (mark-offset-set! mark offset) (text-ref mark)))
     (else #f)))
   (else
    (let lp ((text (text-prev (mark-text mark))))
      (and text
           (if (< (text-start text) (text-end text))
               ;; advanced to a new piece, need to also move the mark
               (cond
                ((utf8-prev (text-bytes text) (text-end text) (text-start text))
                 => (lambda (offset)
                      (mark-offset-set! mark offset)
                      (text-marks-set! (mark-text mark)
                                       (delete (text-marks (mark-text mark)) mark))
                      (text-marks-set! text (cons mark (text-marks text)))
                      (text-ref mark)))
                (else #f))
               (lp (text-prev text))))))))

;;> Similar to \scheme{text-forward-char!} but advances to the end of the next
;;> word (consecutive sequence of alphabetic characters).
(define (text-forward-word! mark)
  (let lp ((in-word? #f))
    (let ((ch (text-ref mark)))
      (cond
       ((not ch) (and in-word? mark))
       ((char-alphabetic? ch) (text-forward-char! mark) (lp #t))
       (in-word? mark)
       (else (text-forward-char! mark) (lp #f))))))

;;> Similar to \scheme{text-backward-char!} but advances to the beginning
;;> of the prev word (consecutive sequence of alphabetic characters).
(define (text-backward-word! mark)
  (let lp ((in-word? #f))
    (let ((ch (text-backward-char! mark)))
      (cond
       ((not ch) (and in-word? mark))
       ((char-alphabetic? ch) (lp #t))
       (in-word? (text-forward-char! mark) mark)
       (else (lp #f))))))

;;> Returns true iff \var{mark} is currently at the beginning of a line.
(define (text-beginning-of-line? mark)
  (let ((ch (text-backward-char! (mark-copy mark))))
    (or (not ch) (eqv? ch #\newline))))

;;> Returns true iff \var{mark} is currently at the end of a line.
(define (text-end-of-line? mark)
  (let ((ch (text-ref mark)))
    (or (not ch) (eqv? ch #\newline))))

;;> Advances \var{mark} to the beginning of the current line.
(define (text-beginning-of-line! mark)
  (let lp ((ch (text-ref mark)) (count 0))
    ;; TODO: crlf
    (cond
     ((not ch))
     ((eqv? ch #\newline)
      (if (zero? count)
          (lp (text-backward-char! mark) (+ count 1))
          (text-forward-char! mark)))
     (else (lp (text-backward-char! mark) (+ count 1)))))
  mark)

;;> Advances \var{mark} to the end of the current line.
(define (text-end-of-line! mark)
  (let lp ()
    (let ((ch (text-ref mark)))
      (cond
       ((not ch))
       ((eqv? ch #\newline))
       (else (text-forward-char! mark) (lp)))))
  mark)

(define (text-count-chars-since mark sentinel)
  (let ((mark (mark-copy mark)))
    (let lp ((count 1))
      (let ((ch (text-backward-char! mark)))
        (if (or (not ch) (eqv? ch sentinel))
            count
            (lp (+ count 1)))))))

;; Note in the full editor we should track horizontal position given dynamic
;; width fonts, composing codepoints, ligatures, half/full-width forms in fixed
;; width fonts, etc.

(define (text-current-column mark)
  (text-count-chars-since mark #\newline))

;; Note in the full editor, when scrolling up multiple lines we should record
;; the original start column, even if it some lines are shorter.

;;> Advances \var{mark} to the next line. If the next line has at least as many
;;> characters as the current, advances to the same column, otherwise to the end
;;> of the line.
(define (text-forward-line! mark)
  (let ((col (text-current-column mark)))
    (text-end-of-line! mark)
    (let lp ((i 1))
      (text-forward-char! mark)
      (or (>= i col)
          (text-end-of-line? mark)
          (lp (+ i 1))))
    mark))

;;> Advances \var{mark} to the previous line. If the previous line has at least
;;> as many characters as the current, advances to the same column, otherwise to
;;> the end of the line.
(define (text-backward-line! mark)
  (let ((col (text-current-column mark)))
    (text-beginning-of-line! mark)
    (text-backward-char! mark)
    (text-beginning-of-line! mark)
    (let lp ((i 1))
      (or (>= i col)
          (text-end-of-line? mark)
          (begin
            (text-forward-char! mark)
            (lp (+ i 1)))))
    mark))
