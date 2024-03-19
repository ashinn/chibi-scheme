
;; A lightweight mutable string-like object supporting:
;; - text insertion at arbitrary points
;; - marks which preserve their position after insertions
;; - lazy loading of text data
;;
;; Basically implemented as a piece table with mark management.

(define-record-type Text
  (make-text source start end prev next marks data)
  text?
  (source text-source text-source-set!)
  (start text-start text-start-set!)
  (end text-end text-end-set!)
  (prev text-prev text-prev-set!)
  (next text-next text-next-set!)
  (marks text-marks text-marks-set!)
  (data text-data text-data-set!))

(define (text-first text)
  (cond ((text-prev text) => text-first)
        (else text)))

(define (text-last text)
  (cond ((text-next text) => text-last)
        (else text)))

(define-record-type Mark
  (make-mark text offset data)
  mark?
  (text mark-text mark-text-set!)
  (offset mark-offset mark-offset-set!)
  (data mark-data mark-data-set!))
