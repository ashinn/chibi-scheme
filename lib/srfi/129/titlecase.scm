
;;> Returns #t if argument is a titlecase character, #f if not
(define (char-title-case? ch)
  (char-set-contains? char-set:title-case ch))

;;> Returns the single-character titlecase mapping of argument
(define (char-titlecase ch)
  (cond ((assv ch title-single-map) => cadr)
        (else (char-upcase ch))))

;; Returns #t if a character is caseless, otherwise #f
(define (char-caseless? ch)
  (not (or (char-lower-case? ch) (char-upper-case? ch) (char-title-case? ch))))

;;> Returns the string titlecase mapping of argument
(define (string-titlecase str)
  (let ((end (string-cursor-end str)))
    (let lp ((n (string-cursor-start str))
             (prev-caseless? #t)
             (result '()))
      (if (string-cursor>=? str n end)
          (list->string (reverse result))
          (let ((ch (string-cursor-ref str n))
                (n2 (string-cursor-next str n)))
            (if prev-caseless?
                ;; ch must be titlecased
                (let ((multi-title (assv ch title-multiple-map)))
                  (if multi-title
                      ;; ch has multiple- or single-character titlecase mapping
                      (lp n2 #f (append-reverse (cdr multi-title) result))
                      ;; ch has single-character uppercase mapping
                      (lp n2 (char-caseless? ch) (cons (char-upcase ch) result))))
                ;; ch must be lowercased
                (let ((multi-downcase (assv ch lower-multiple-map)))
                  (if multi-downcase
                      ;; ch has multiple-character lowercase mapping
                      (lp n2 #f (append-reverse (cdr multi-downcase) result))
                      ;; ch has single-character lowercase mapping
                      (lp n2 (char-caseless? ch) (cons (char-downcase ch) result))))))))))
