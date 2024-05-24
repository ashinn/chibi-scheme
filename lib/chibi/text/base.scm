
;;> Returns the length in codepoints of the text object.
(define (text-char-length text)
  (let lp ((text (text-first text)) (sum 0))
    (if text
        (lp (text-next text)
            (+ sum
               (string-length (utf8->string! (text-bytes text)
                                             (text-start text)
                                             (text-end text)))))
        sum)))

;;> Returns the length the text object would require encoded as UTF-8.
(define (text-utf8-length text)
  (let lp ((text (text-first text)) (sum 0))
    (if text
        (lp (text-next text) (+ sum (- (text-end text) (text-start text))))
        sum)))

(define (text-piece-length text)
  (let lp ((text (text-first text)) (count 0))
    (if text
        (lp (text-next text) (+ count 1))
        count)))

(define (text-empty? text)
  (zero? (text-char-length text)))

;;> Returns a new text object representing the same codepoints as the string \var{str}.
(define (string->text str . o)
  (let* ((bv (string->utf8 str))
         (start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (bytevector-length bv))))
    (make-text bv start end #f #f '() #f)))

(define (text-string text)
  (utf8->string (text-bytes text) (text-start text) (text-end text)))

;;> Returns the utf8 representation of the codepoints in \var{text}.
(define (text->utf8 text)
  (if (and (not (text-prev text)) (not (text-next text)))
      (bytevector-copy (text-bytes text) (text-start text) (text-end text))
      (let ((out (open-output-bytevector)))
        (let lp ((piece (text-first text)))
          (cond
           ((not piece)
            (get-output-bytevector out))
           (else
            (write-bytevector (text-bytes piece) out (text-start piece) (text-end piece))
            (lp (text-next piece))))))))

;;> Returns a string representing the same codepoints as \var{text}.
(define (text->string text)
  (utf8->string (text->utf8 text)))

;;> Returns the character \var{mark} points to, or \scheme{#f}
;;> if mark points to the end of the text.
(define (text-ref mark)
  (cond
   ((< (mark-offset mark) (text-start (mark-text mark)))
    (let lp ((text (text-prev (mark-text mark))))
      (and text
           (if (< (text-start text) (text-end text))
               (utf8-ref (text-bytes text) (- (text-end text) 1))
               (lp (text-prev text))))))
   ((>= (mark-offset mark) (text-end (mark-text mark)))
    (let lp ((text (text-next (mark-text mark))))
      (and text
           (if (< (text-start text) (text-end text))
               (utf8-ref (text-bytes text) (text-start text))
               (lp (text-next text))))))
   (else
    (utf8-ref (text-bytes (mark-text mark)) (mark-offset mark)))))

(define (text-piece-copy text)
  (let ((res (make-text (bytevector-copy (text-bytes text))
                        (text-start text)
                        (text-end text)
                        (text-prev text)
                        (text-next text)
                        '()
                        (text-source text))))
    (text-marks-set! res
                     (map (lambda (mk)
                            (make-mark res (mark-offset mk) (mark-data mk)))
                          (text-marks res)))
    res))

(define (text-copy text)
  (let ((text0 (text-piece-copy (text-first text))))
    (let lp ((text (text-next text0))
             (prev text0))
      (if text
          (let ((text1 (text-piece-copy text)))
            (text-next-set! text0 text1)
            (text-prev-set! text1 text0)
            (lp text1 (text-next text1)))
          text0))))

(define (->text obj)
  (cond
   ((text? obj) obj)
   ((string? obj) (string->text obj))
   ((char? obj) (string->text (string obj)))
   (else (error "not a textlike object" obj))))

(define (text-append! . ls)
  (text-concatenate! ls))

(define (text-append . ls)
  (text-concatenate ls))

(define (text-concatenate ls)
  (text-concatenate! (map (lambda (x) (if (text? x) (text-copy x) x)) ls)))

(define (text-concatenate! ls)
  (if (null? ls)
      (string->text "")
      (let ((res (->text (car ls))))
        (let lp ((tx (text-last res))
                 (ls (cdr ls)))
          (if (null? ls)
              res
              (let* ((tx2 (->text (car ls)))
                     (tx2-first (text-first tx2)))
                (text-next-set! tx tx2-first)
                (text-prev-set! tx2-first tx)
                (lp (text-last tx2) (cdr ls))))))))

;; inserts a new right piece and returns it
(define (text-new-right! text . o)
  (let* ((size (if (pair? o) (car o) 256))
         (right (make-text (make-bytevector size) 0 0 text (text-next text) '() #f)))
    (cond ((text-next text) => (lambda (orig-right) (text-prev-set! orig-right right))))
    (text-next-set! text right)
    right))

;; splits the text at the given point into two pieces, returning the original
;; text which becomes the left piece
(define (text-split! text at . o)
  (receive (text at) (text&mark-at text at)
    (let* ((at-mark (if (mark? at) at (text-mark text at)))
           (at-offset (mark-offset at-mark))
           (size (max (- at-offset (text-start text))
                      (if (pair? o) (car o) 64)))
           (right (text-new-right! text size)))
      (bytevector-copy! (text-bytes right)
                        0
                        (text-bytes text)
                        at-offset
                        (text-end text))
      (text-end-set! right (- (text-end text) at-offset))
      (text-end-set! text at-offset)
      (receive (left-marks right-marks)
          (partition (lambda (mk) (<= (mark-offset mk) at-offset)) (text-marks text))
        (text-marks-set! text left-marks)
        (text-marks-set! right (map (lambda (mk)
                                      (mark-text-set! mk right)
                                      (mark-offset-set! mk (- (mark-offset mk) at-offset))
                                      mk)
                                    right-marks))
        text))))

;;> Inserts \var{textlike} into the text immediately before
;;> the point indicated by \var{mark1}, leaving \var{mark1}
;;> (and all same position marks) after the inserted text.
;;> Returns \var{mark1}.
(define (text-insert! text str . o)
  (receive (text at-mark) (text&mark-at text (if (pair? o) (car o) 0))
   (let* ((at-offset (mark-offset at-mark))
          (src (string->utf8 str))
          (size (bytevector-length src))
          (dst (text-bytes text))
          (dst-size (bytevector-length dst)))
     (cond
      ((= at-offset (text-end text))
       (let* ((avail-size (- dst-size at-offset))
              (end (min (+ at-offset size) dst-size))
              (copy-size (- end at-offset)))
         ;; TODO: Don't insert a partial utf8 char.
         (bytevector-copy! dst at-offset src 0 copy-size)
         (text-end-set! text end)
         ;; Advance marks that were pointing to the end of this piece.
         (for-each (lambda (mk)
                     (if (>= (mark-offset mk) at-offset)
                         (mark-offset-set! mk (+ (mark-offset mk) copy-size))))
                   (text-marks text))
         (if (< copy-size size)
             ;; TODO: better sizing?
             (let ((right (text-new-right! text (* 2 size)))
                   (right-size (- size copy-size)))
               (bytevector-copy! (text-bytes right) 0 src copy-size size)
               (text-end-set! right right-size)))
         text))
      ;; ((= at-offset (text-start text))
      ;; TODO: insert before start
      ;;  )
      ;; TODO: optimization: use prev buffer if at start of text?
      ;; TODO: optimization: shift bytes in place if near end?
      ((negative? at-offset)
       (error "bad offset" at-offset))
      (else
       (text-split! text at-mark)
       (text-insert! text str at-mark))))))

;; Set the start of text and adjust marks before that to the new start
;; accordingly.
(define (text-truncate-left! text new-start)
  (text-start-set! text new-start)
  (text-marks-set! text
                   (map (lambda (mk)
                          (mark-offset-set! mk (max (mark-offset mk) new-start))
                          mk)
                        (text-marks text))))

;; Set the end of text and adjust marks after that to the new end accordingly.
(define (text-truncate-right! text new-end)
  (text-end-set! text new-end)
  (text-marks-set! text
                   (map (lambda (mk)
                          (mark-offset-set! mk (min (mark-offset mk) new-end))
                          mk)
                        (text-marks text))))

;;> Deletes the codepoints between \var{from} (inclusive)
;;> and \var{to} (exclusive, defaulting to the end of the
;;> text), leaving the two marks pointing to the same location.
;;> Returns \var{from}.
(define (text-delete! text from to)
  (let-values (((from-text from-mark) (text&mark-at text from))
               ((to-text to-mark) (text&mark-at text to)))
    (let ((from-at (mark-offset from-mark))
          (to-at (mark-offset to-mark)))
      (cond
       ((eq? from-text to-text)
        (let* ((from-text2 (text-split! from-text from-mark))
               (to-text2 (text-next from-text2))
               (to-at2 (mark-offset to-mark)))
          (text-truncate-left! to-text2 to-at2)))
       (else
        (text-truncate-right! from-text from-at)
        (text-truncate-left! to-text to-at)
        ;; Erase any pieces in between and point them to the start of to-text.
        (let lp ((text (text-next from-text)) (marks '()))
          (cond
           ((and text (not (eq? text to-text)))
            (lp (text-next text) (cons (text-splice! text) marks)))
           (else
            (text-marks-set! to-text
                             (append
                              (map (lambda (mk)
                                     (mark-text-set! mk to-text)
                                     (mark-offset-set! mk to-at)
                                     mk)
                                   (concatenate (reverse marks)))
                              (text-marks to-text))))))))
      )))

(define (text-piece-empty? text)
  (= (text-start text) (text-end text)))

;; returns a reference to a new start of text with empty pieces removed
(define (text-clean! text)
  (define (text-first-non-empty! text)
    (let lp ((tx (text-first text)))
      (cond
       ((and (text-piece-empty? tx) (text-next tx))
        (let ((next (text-next tx)))
          (text-next-set! tx #f)
          (text-prev-set! next #f)
          (lp next)))
       (else
        tx))))
  (let ((tx-first (text-first-non-empty! text)))
    (let lp ((prev tx-first) (tx (text-next tx-first)))
      (cond
       ((not tx)
        tx-first)
       ((text-piece-empty? tx)
        (text-next-set! prev (text-next tx))
        (text-prev-set! (text-next tx) prev)
        (lp prev (text-next tx)))
       (else
        (lp tx (text-next tx)))))))
