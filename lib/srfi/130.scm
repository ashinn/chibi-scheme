
(define (string-cursor-diff str start end)
  (if (string-cursor? start)
      (- (string-cursor->index str end) (string-cursor->index str start))
      (- end start)))

(define (string-unfold/aux k stop? mapper successor seed . o)
  (let ((base (if (pair? o) (car o) ""))
        (make-final (if (and (pair? o) (pair? (cdr o))) (cadr o) (lambda (x) ""))))
    (do ((acc seed (successor acc))
         (ls '() (cons (mapper acc) ls)))
        ((stop? acc) (k base ls (make-final acc))))))

(define (string-unfold . o)
  (apply string-unfold/aux
         (lambda (base ls final)
           (string-append base (reverse-list->string ls) final))
         o))

(define (string-unfold-right . o)
  (apply string-unfold/aux
         (lambda (base ls final)
           (string-append final (list->string ls) base))
         o))

(define (string-tabulate proc len)
  (string-unfold (lambda (i) (= i len)) proc (lambda (i) (+ i 1)) 0))

(define (string->list/cursors str . o)
  (let ((start (if (pair? o) (car o) (string-cursor-start str)))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (string-cursor-end str))))
    (let lp ((i end) (res '()))
      (if (string-cursor<=? i start)
          res
          (let ((i (string-cursor-prev str i)))
            (lp i (cons (string-cursor-ref str i) res)))))))

(define (string->vector/cursors str . o)
  (list->vector (apply string->list/cursors str o)))

(define (reverse-list->string ls)
  (list->string (reverse ls)))

(define (string-join str-ls . o)
  (let ((sep (if (pair? o) (car o) ""))
        (grammar (if (and (pair? o) (pair? (cdr o))) (cadr o) 'infix)))
    (case grammar
      ((infix) (%string-join str-ls sep))
      ((strict-infix)
       (if (null? str-ls)
           (error "string-join 'strict-infix called on an empty list")
           (%string-join str-ls sep)))
      ((prefix) (%string-join (cons "" str-ls) sep))
      ((suffix) (string-append (%string-join str-ls sep) sep))
      (else (error "unknown string-join grammar" grammar)))))

(define (string-ref/cursor str x)
  (if (string-cursor? x)
      (string-cursor-ref str x)
      (string-ref str x)))

(define (substring/cursors str start end)
  (if (string-cursor? start)
      (substring-cursor str start end)
      (substring str start end)))

(define (string-copy/cursors str . o)
  (cond ((null? o) (substring-cursor str (string-cursor-start str)))
        ((string-cursor? (car o)) (apply substring-cursor str o))
        (else (apply substring str o))))

(define (string-arg str o)
  (if (pair? o) (apply string-copy/cursors str o) str))

(define (cursor-arg str x)
  (if (string-cursor? x) x (string-index->cursor str x)))

(define (cursor-args str o)
  (if (pair? o)
      (cons (cursor-arg str (car o)) (cursor-args str (cdr o)))
      '()))

(define (string-take str n)
  (substring str 0 n))
(define (string-take-right str n)
  (let ((start (string-cursor-backward str (string-cursor-end str) n)))
    (substring-cursor str start)))
(define (string-drop str n)
  (substring str n))
(define (string-drop-right str n)
  (let ((end (string-cursor-backward str (string-cursor-end str) n)))
    (substring-cursor str (string-cursor-start str) end)))

(define (string-pad str len . o)
  (let* ((pad-char (if (pair? o) (car o) #\space))
         (str (if (and (pair? o) (pair? (cdr o))) (string-arg str (cdr o)) str))
         (str-len (string-length str)))
    (cond
     ((> str-len len) (string-take-right str len))
     ((< str-len len)
      (string-append (make-string (- len str-len) pad-char) str))
     (else str))))

(define (string-pad-right str len . o)
  (let* ((pad-char (if (pair? o) (car o) #\space))
         (str (if (and (pair? o) (pair? (cdr o))) (string-arg str (cdr o)) str))
         (str-len (string-length str)))
    (cond
     ((> str-len len) (string-take str len))
     ((< str-len len)
      (string-append str (make-string (- len str-len) pad-char)))
     (else str)))) 

(define (string-trim str . o)
  (let ((pred (if (pair? o) (car o) char-whitespace?))
        (str (if (and (pair? o) (pair? (cdr o))) (string-arg str (cdr o)) str)))
    (substring-cursor str (string-skip str pred))))
(define (string-trim-right str . o)
  (let ((pred (if (pair? o) (car o) char-whitespace?))
        (str (if (and (pair? o) (pair? (cdr o))) (string-arg str (cdr o)) str)))
    (substring-cursor str (string-cursor-start str) (string-skip-right str pred))))
(define (string-trim-both str . o)
  (let ((pred (if (pair? o) (car o) char-whitespace?))
        (str (if (and (pair? o) (pair? (cdr o))) (string-arg str (cdr o)) str)))
    (string-trim-right (string-trim str pred) pred)))

(define (string-prefix-length s1 s2 . o)
  (let ((s1 (string-arg s1 o))
        (s2 (if (and (pair? o) (pair? (cdr o))) (string-arg s2 (cddr o)) s2)))
    (string-cursor->index s1 (string-mismatch s1 s2))))
(define (string-suffix-length s1 s2 . o)
  (let* ((s1 (string-arg s1 o))
         (s2 (if (and (pair? o) (pair? (cdr o))) (string-arg s2 (cddr o)) s2))
         (mismatch (string-mismatch-right s2 s1)))
    (string-cursor-diff s1
                        (string-cursor-next s1 mismatch)
                        (string-cursor-end s1))))

(define (string-prefix? s1 s2 . o)
  (equal? (string-length s1) (apply string-prefix-length s1 s2 o)))
(define (string-suffix? s1 s2 . o)
  (equal? (string-length s1) (apply string-suffix-length s1 s2 o)))

(define (string-index str pred . o)
  (apply string-find str pred (cursor-args str o)))
(define (string-index-right str pred . o)
  (apply string-find-right str pred (cursor-args str o)))

(define (string-contains s1 s2 . o)
  (let ((start1 (if (pair? o) (car o) (string-cursor-start s1)))
        (end1 (if (and (pair? o) (pair? (cdr o)))
                  (cadr o)
                  (string-cursor-end s1)))
        (s2 (if (and (pair? o) (pair? (cdr o))) (string-arg s2 (cddr o)) s2)))
    (let ((res (%string-contains s1 s2 start1)))
      (and res (string-cursor<=? res end1) res))))

(define (string-contains-right s1 s2 . o)
  (let* ((start1
          (cursor-arg s1 (if (pair? o) (car o) (string-cursor-start s1))))
         (end1 (cursor-arg s1 (if (and (pair? o) (pair? (cdr o)))
                                  (cadr o)
                                  (string-cursor-end s1))))
         (s2 (if (and (pair? o) (pair? (cdr o))) (string-arg s2 (cddr o)) s2))
         (start2 (string-cursor-start s2))
         (end2 (string-cursor-end s2)))
    (let lp ((sc1-base end1)
             (sc1 end1)
             (sc2-base end2)
             (sc2 end2))
      (cond
       ((string-cursor=? sc2 start2)
        sc1)
       ((string-cursor=? sc1 start1)
        #f)
       (else
        (let ((sc1 (string-cursor-prev s1 sc1))
              (sc2 (string-cursor-prev s2 sc2)))
          (if (eqv? (string-cursor-ref s1 sc1) (string-cursor-ref s2 sc2))
              (lp sc1-base sc1 sc2-base sc2)
              (let ((sc1-base (string-cursor-prev s1 sc1-base)))
                (lp sc1-base sc1-base sc2-base sc2-base)))))))))

(define (string-reverse str . o)
  (list->string (reverse (string->list/cursors (string-arg str o)))))

(define string-concatenate %string-join)

(define (string-concatenate-reverse str-ls . o)
  (let ((str-ls
         (if (pair? o)
             (cons (apply string-copy/cursors (car o) 0 (cdr o)) str-ls)
             str-ls)))
    (string-concatenate (reverse str-ls))))

(define (string-fold kons knil str . o)
  (%string-fold kons knil (string-arg str o)))

(define (string-fold-right kons knil str . o)
  (%string-fold-right kons knil (string-arg str o)))

(define (string-for-each-cursor proc str . o)
  (let ((end (cursor-arg str
                         (if (and (pair? o) (pair? (cdr o)))
                             (cadr o)
                             (string-cursor-end str)))))
    (let lp ((i (cursor-arg str
                            (if (pair? o) (car o) (string-cursor-start str)))))
      (when (string-cursor<? i end)
        (proc i)
        (lp (string-cursor-next str i))))))

(define (string-replicate str from to . o)
  (let* ((str (string-arg str o))
         (start (string-cursor-start str))
         (end (string-cursor-end str))
         (out (open-output-string)))
    (let lp ((i from)
             (sc (string-cursor-forward str
                                        start
                                        (modulo from (string-length str)))))
      (cond
       ((= i to)
        (get-output-string out))
       (else
        (write-char (string-cursor-ref str sc) out)
        (let ((sc (string-cursor-next str sc)))
          (lp (+ i 1) (if (string-cursor=? sc end) start sc))))))))

(define (string-count str pred . o)
  (apply string-fold (lambda (ch n) (if (pred ch) (+ n 1) n)) 0 str o))

(define (string-replace s1 s2 start1 end1 . o)
  (string-append (substring/cursors s1 0 start1)
                 (string-arg s2 o)
                 (substring/cursors s1 end1 (string-cursor-end s1))))

(define (string-split str delim . o)
  (let* ((delim-len (string-length delim))
         (grammar (if (pair? o) (car o) 'infix))
         (o (if (pair? o) (cdr o) '()))
         (limit (and (pair? o) (cadr o)))
         (o (if (pair? o) (cdr o) '()))
         (start (if (pair? o) (car o) (string-cursor-start str)))
         (end (if (and (pair? o) (pair? (cdr o)))
                  (cadr o)
                  (string-cursor-end str))))
    (define (trim-for-grammar res for-grammer)
      (if (and (eq? grammar for-grammer) (pair? res) (equal? "" (car res)))
          (cdr res)
          res))
    (if (and (eq? grammar 'strict-infix) (string-cursor>=? start end))
        (error "string-split 'strict-infix called on an empty string"))
    (let lp ((sc start) (res '()))
      (cond
       ((string-cursor>=? sc end)
        (trim-for-grammar (reverse (trim-for-grammar res 'suffix)) 'prefix))
       ((string-contains str delim sc end)
        => (lambda (sc2)
             (lp (string-cursor-forward str sc2 delim-len)
                 (cons (substring-cursor str sc sc2) res))))
       (else
        (lp end (cons (substring-cursor str sc end) res)))))))

(define (string-split-right str delim . o)
  #f)

(define (string-filter pred str . o)
  (let ((out (open-output-string)))
    (apply string-fold (lambda (ch acc) (if (pred ch) (write-char ch out))) #f str o)
    (get-output-string out)))

(define (string-remove pred str . o)
  (apply string-filter (lambda (ch) (not (pred ch))) str o))
