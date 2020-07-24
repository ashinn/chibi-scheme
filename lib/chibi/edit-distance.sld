
(define-library (chibi edit-distance)
  (export edit-distance find-nearest-edits)
  (import (scheme base) (srfi 130))
  (begin
    ;;> Returns the levenshtein distance between s1 and s2 - a cost of
    ;;> 1 per character insertion, deletion or update.  Runs in
    ;;> quadratic time and linear memory.
    ;;>
    ;;> \example{(edit-distance "same" "same")}
    ;;> \example{(edit-distance "same" "sand")}
    ;;> \example{(edit-distance "Saturday" "Sunday")}
    (define (edit-distance s1 s2)
      (let* ((len1 (string-length s1))
             (len2 (string-length s2))
             (vec (make-vector (+ len1 1) 0)))
        (do ((i 0 (+ i 1)))
            ((> i len1))
          (vector-set! vec i i))
        (do ((i 1 (+ i 1))
             (sc2 (string-cursor-start s2) (string-cursor-next s2 sc2)))
            ((> i len2)
             (vector-ref vec len1))
          (vector-set! vec 0 i)
          (let ((ch2 (string-ref/cursor s2 sc2)))
            (let lp ((j 1)
                     (sc1 (string-cursor-start s1))
                     (last-diag (- i 1)))
              (when (<= j len1)
                (let ((old-diag (vector-ref vec j))
                      (ch1 (string-ref/cursor s1 sc1)))
                  (vector-set! vec j (min (+ (vector-ref vec j) 1)
                                          (+ (vector-ref vec (- j 1)) 1)
                                          (+ last-diag
                                             (if (eqv? ch1 ch2) 0 1))))
                  (lp (+ j 1)
                      (string-cursor-next s1 sc1)
                      old-diag))))))))
    ;;> Returns a list of strings in \var{str-ls} with the smallest
    ;;> edit distance to \var{str}, preserving order.  If
    ;;> \var{max-distance} is provided and positive, only return if
    ;;> the edits are less or equal to that distance.
    (define (find-nearest-edits str str-ls . o)
      (let ((max-distance (if (pair? o) (car o) 1e100)))
        (let lp ((ls str-ls) (dist (+ max-distance 1)) (res '()))
          (if (null? ls)
              (reverse res)
              (let ((ed (edit-distance str (car ls))))
                (cond
                 ((= ed dist) (lp (cdr ls) dist (cons (car ls) res)))
                 ((< ed dist) (lp (cdr ls) ed (list (car ls))))
                 (else (lp (cdr ls) dist res))))))))))
