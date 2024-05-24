
;;> Searches for the leftmost longest match for \var{rx} starting from the mark
;;> \var{mk}.  If found, advances the mark.  Returns the mark.
(define (text-search! mk rx)
  (let ((rx (regexp rx))
        (state (make-regexp-state)))
    (let lp ((tx (mark-text mk)))
      (and tx
           ;; Note string size is mis-named, it's actually the end offset.
           (let* ((bv (text-bytes tx))
                  (start (if (eqv? tx (mark-text mk)) (mark-offset mk) (text-start tx)))
                  (end (text-end tx))
                  (str (utf8->string! bv start end))
                  (sc1 (string-cursor-start str))
                  (sc2 (string-cursor-end str)))
             (regexp-advance! #t (eq? tx (mark-text mk)) rx str sc1 sc2 state)
             (cond
              ((regexp-state-matches state)
               => (lambda (match)
                    (let ((offset (string-cursor-offset (regexp-match-ref match 1))))
                      ;; TODO: the match could have been a previous text
                      (mark-text-set! mk tx)
                      (mark-offset-set! mk (+ start offset))
                      mk)))
              (else
               (lp (text-next tx)))))))))
