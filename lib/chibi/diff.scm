
;; utility for lcs-with-positions
(define (max-seq . o)
  (if (null? o)
      (list 0 '())
      (let loop ((a (car o)) (ls (cdr o)))
        (if (null? ls)
            a
            (let ((b (car ls)))
              (if (>= (car a) (car b))
                  (loop a (cdr ls))
                  (loop b (cdr ls))))))))

(define (lcs-with-positions a-ls b-ls . o)
  (let* ((eq (if (pair? o) (car o) equal?))
         (a-len (+ 1 (length a-ls)))
         (b-len (+ 1 (length b-ls)))
         (results (make-vector (* a-len b-len) #f)))
    (let loop ((a a-ls) (a-pos 0) (b b-ls) (b-pos 0))
      ;; cache this step if not already done
      (let ((i (+ (* a-pos b-len) b-pos)))
        (or (vector-ref results i)
            (let ((res
                   (if (or (null? a) (null? b))
                       (list 0 '()) ;; base case
                       (let ((a1 (car a))
                             (b1 (car b))
                             (a-tail (loop (cdr a) (+ a-pos 1) b b-pos))
                             (b-tail (loop a a-pos (cdr b) (+ b-pos 1))))
                         (cond
                          ((eq a1 b1)
                           ;; match found, we either use it or we don't
                           (let* ((a-b-tail (loop (cdr a) (+ a-pos 1)
                                                  (cdr b) (+ b-pos 1)))
                                  (a-b-res (list (+ 1 (car a-b-tail))
                                                 (cons (list a1 a-pos b-pos)
                                                       (cadr a-b-tail)))))
                             (max-seq a-b-res a-tail b-tail)))
                          (else
                           ;; not a match
                           (max-seq a-tail b-tail)))))))
              (vector-set! results i res)
              res))))
    (cadr (vector-ref results 0))))

(define (lcs a b . o)
  (map car (cadr (lcs-with-positions a b (if (pair? o) (car o) equal?)))))

(define (source->list x reader)
  (port->list
   reader
   (cond ((port? x) x)
         ((string? x) (open-input-string x))
         (else (error "don't know how to diff from:" x)))))

;; lcs on text, generates lists from ports (or strings)
(define (diff a b . o)
  (let-optionals o ((reader read-line)
                    (equal equal?))
    (let ((a-ls (source->list a reader))
          (b-ls (source->list b reader)))
      (list a-ls b-ls (lcs-with-positions a-ls b-ls)))))

(define (write-line-diffs lines type out)
  (for-each
   (lambda (line)
     (case type
       ((add)
        (write-char #\+ out))
       ((remove)
        (write-char #\- out))
       ((same)
        (write-char #\space out))
       (else (error "unknown diff type:" type)))
     (write-string line out)
     (newline out))
   lines))

(define (write-line-diffs/color lines type out)
  (for-each
   (lambda (line)
     (case type
       ((add)
        (write-string (green "+") out)
        (write-string (green line) out))
       ((remove)
        (write-string (red "-") out)
        (write-string (red line out)))
       ((same)
        (write-char #\space out)
        (write-string line out))
       (else (error "unknown diff type:" type)))
     (newline out))
   lines))

(define (write-char-diffs chars type out)
  (case type
    ((add)
     (write-string " «" out)
     (write-string (list->string chars) out)
     (write-string "» " out))
    ((remove)
     (write-string " »" out)
     (write-string (list->string chars) out)
     (write-string "« " out))
    ((same)
     (write-string (list->string chars) out))
    (else (error "unknown diff type:" type))))

(define (write-char-diffs/color chars type out)
  (case type
    ((add)
     (write-string (green (list->string chars)) out))
    ((remove)
     (write-string (red (list->string chars)) out))
    ((same)
     (write-string (list->string chars) out))
    (else (error "unknown diff type:" type))))

(define (write-diff diff . o)
  (let-optionals o ((writer write-line-diffs)
                    (out (current-output-port)))
    (let* ((a-ls (car diff))
           (b-ls (cadr diff))
           (d-ls (car (cddr diff))))
      ;; context diff
      (let lp ((d d-ls) (a a-ls) (a-pos 0) (b b-ls) (b-pos 0))
        (unless (null? d)
          (let* ((d1 (car d))
                 (a-off (cadr d1))
                 (a-skip (- a-off a-pos))
                 (b-off (car (cddr d1)))
                 (b-skip (- b-off b-pos)))
            (let-values (((a-head a-tail) (split-at a a-skip))
                         ((b-head b-tail) (split-at b b-skip)))
              ;; elements only in a have been removed
              (if (pair? a-head)
                  (writer (cdr a-head) 'remove out))
              ;; elements only in b have been added
              (if (pair? b-head)
                  (writer (cdr b-head) 'add out))
              ;; reprint this common element
              (writer (list (car d1)) 'same out)
              ;; recurse
              (lp (cdr d) a-tail a-off b-tail b-off))))))))

(define (diff->string diff . o)
  (let ((out (open-output-string)))
    (write-diff diff (if (pair? o) (car o) write-line-diffs) out)
    (get-output-string out)))

(define (write-edits ls lcs . o)
  (let-optionals o ((index 1)
                    (writer write-line-diffs)
                    (out (current-output-port)))
    (let ((type (if (eq? index 1) 'remove 'add)))
      (let lp ((ls ls) (lcs lcs) (buf '(#f)) (i 0))
        (define (output ch type)
          (cond
           ((eq? type (car buf))
            (cons type (cons ch (cdr buf))))
           (else
            (if (car buf)
                (writer (reverse (cdr buf)) (car buf) out))
            (list type ch))))
        (cond
         ((null? ls) (output #f 'done))
         ((null? lcs)
          (lp (cdr ls) lcs (output (car ls) type) (+ i 1)))
         ((= i (list-ref (car lcs) index))
          (lp (cdr ls) (cdr lcs) (output (car ls) 'same) (+ i 1)))
         (else
          (lp (cdr ls) lcs (output (car ls) type) (+ i 1))))))))

(define (edits->string ls lcs . o)
  (let-optionals o ((type 'add)
                    (writer (if (and (pair? ls) (char? (car ls)))
                                write-char-diffs
                                write-line-diffs)))
    (let ((out (open-output-string)))
      (write-edits ls lcs type writer out)
      (get-output-string out))))

(define (edits->string/color ls lcs . o)
  (let-optionals o ((type 'add)
                    (writer (if (and (pair? ls) (char? (car ls)))
                                write-char-diffs/color
                                write-line-diffs/color)))
    (let ((out (open-output-string)))
      (write-edits ls lcs type writer out)
      (get-output-string out))))
