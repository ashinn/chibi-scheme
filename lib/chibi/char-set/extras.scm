
(define (char-set . args)
  (list->char-set args))

;; This is a mistake in the SRFI-14 design - end should be inclusive.
(define (ucs-range->char-set start end)
  (make-iset start (- end 1)))

(define char-set-copy iset-copy)

(define char-set-size iset-size)

(define (list->char-set ls)
  (list->iset (map char->integer ls)))
(define (char-set->list cset)
  (map integer->char (iset->list cset)))

(define (string->char-set str)
  (list->char-set (string->list str)))
(define (char-set->string cset)
  (list->string (char-set->list cset)))

(define (char-set-adjoin! cset ch)
  (iset-adjoin! cset (char->integer ch)))
(define (char-set-adjoin cset ch)
  (iset-adjoin cset (char->integer ch)))

(define char-set-union iset-union)
(define char-set-union! iset-union!)
(define char-set-intersection iset-intersection)
(define char-set-intersection! iset-intersection!)
(define char-set-difference iset-difference)
(define char-set-difference! iset-difference!)

(define char-set:empty (immutable-char-set (%make-iset 0 0 0 #f #f)))
(define char-set:ascii (immutable-char-set (%make-iset 0 #x7F #f #f #f)))

(cond-expand
 (full-unicode
  (define char-set:full
    (immutable-char-set
     (%make-iset 0 #xD7FF #f #f (%make-iset #xE000 #x10FFFD #f #f #f)))))
 (else
  (define char-set:full (immutable-char-set (%make-iset 0 #xFF #f #f #f)))))

(define (char-set-complement cset)
  (char-set-difference char-set:full cset))
