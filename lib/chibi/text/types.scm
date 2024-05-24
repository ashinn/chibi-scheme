
;; A lightweight mutable string-like object supporting:
;; - text insertion at arbitrary points
;; - marks which preserve their position after insertions
;; - lazy loading of text data
;;
;; Basically implemented as a piece table with mark management.

(define-record-type Text
  (make-text bytes start end prev next marks source)
  text?
  (bytes text-bytes text-bytes-set!)
  (start text-start text-start-set!)
  (end text-end text-end-set!)
  (prev text-prev text-prev-set!)
  (next text-next text-next-set!)
  (marks text-marks text-marks-set!)
  (source text-source text-source-set!))

(define (text-first text)
  (cond ((text-prev text) => text-first)
        (else text)))

(define (text-last text)
  (cond ((text-next text) => text-last)
        (else text)))

(define (text-splice! text)
  ;; TODO: splice out the nodes themselves
  (let ((marks (text-marks text)))
    (text-start-set! text (text-end text))
    (text-marks-set! text '())
    marks))

(define-record-type Mark
  (make-mark text offset data)
  mark?
  (text mark-text mark-text-set!)
  (offset mark-offset mark-offset-set!)
  (data mark-data mark-data-set!))

(define-record-type Text-Source
  (make-text-source loader path data)
  text-source?
  (loader text-source-loader text-source-loader-set!)
  (path text-source-path text-source-path-set!)
  (data text-source-data text-source-data-set!))

(define-record-type Text-Loader
  (make-text-loader load reload write modified?)
  text-loader?
  ;; load is used on construction
  (load text-loader-load text-loader-load-set!)
  ;; reload updates the text to match the file (discards changes)
  (reload text-loader-reload text-loader-reload-set!)
  ;; write updates the file to match the text (overwrites external edits)
  (write text-loader-write text-loader-write-set!)
  ;; modified tells us if the file has been modified since we last synced
  ;; (either write or reload)
  (modified? text-loader-modified? text-loader-modified?-set!)
  )
