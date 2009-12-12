
(define (string-scan c str . o)
  (let ((limit (string-length str)))
    (let lp ((i (if (pair? o) (car o) 0)))
      (cond ((>= i limit) #f)
            ((eqv? c (string-ref str i)) i)
            (else (lp (+ i 1)))))))

(define (string-scan-right c str . o)
  (let lp ((i (if (pair? o) (car o) (- (string-length str) 1))))
    (cond ((negative? i) #f)
          ((eqv? c (string-ref str i)) i)
          (else (lp (- i 1))))))

(define (string-skip c str . o)
  (let ((limit (string-length str)))
    (let lp ((i (if (pair? o) (car o) 0)))
      (cond ((>= i limit) #f)
            ((not (eqv? c (string-ref str i))) i)
            (else (lp (+ i 1)))))))

(define (string-skip-right c str . o)
  (let lp ((i (if (pair? o) (car o) (- (string-length str) 1))))
    (cond ((negative? i) #f)
          ((not (eqv? c (string-ref str i))) i)
          (else (lp (- i 1))))))

;; POSIX basename
;; (define (path-strip-directory path)
;;   (if (string=? path "")
;;       path
;;       (let ((end (string-skip-right #\/ path)))
;;         (if (not end)
;;             "/"
;;             (let ((start (string-scan-right #\/ path (- end 1))))
;;               (substring path (if start (+ start 1) 0) (+ end 1)))))))

;; GNU basename
(define (path-strip-directory path)
  (if (string=? path "")
      path
      (let ((len (string-length path)))
        (if (eqv? #\/ (string-ref path (- len 1)))
            ""
            (let ((slash (string-scan-right #\/ path)))
              (if (not slash)
                  path
                  (substring path (+ slash 1) len)))))))

(define (path-directory path)
  (if (string=? path "")
      "."
      (let ((end (string-skip-right #\/ path)))
        (if (not end)
            "/"
            (let ((start (string-scan-right #\/ path (- end 1))))
              (if (not start)
                  "."
                  (let ((start (string-skip-right #\/ path start)))
                    (if (not start) "/" (substring path 0 (+ start 1))))))))))

(define (path-extension-pos path) (string-scan-right #\. path))

(define (path-extension path)
  (let ((i (path-extension-pos path)))
    (and i
         (let ((start (+ i 1)) (end (string-length path)))
           (and (< start end) (substring path start end))))))

(define (path-strip-extension path)
  (let ((i (path-extension-pos path)))
    (if (and i (< (+ i 1) (string-length path)))
        (substring path 0 i)
        path)))

(define (path-replace-extension path ext)
  (string-append (path-strip-extension path) "." ext))

(define (path-absolute? path)
  (and (not (string=? "" path)) (eqv? #\/ (string-ref path 0))))

(define (path-relative? path) (not (path-absolute? path)))

;; This looks big and hairy, but it's mutation-free and guarantees:
;;   (string=? s (path-normalize s))  <=>  (eq? s (path-normalize s))
;; i.e. fast and simple for already normalized paths.

(define (path-normalize path)
  (let* ((len (string-length path)) (len-1 (- len 1)))
    (define (collect i j res)
      (if (>= i j) res (cons (substring path i j) res)))
    (define (finish i res)
      (if (zero? i)
          path
          (apply string-append (reverse (collect i len res)))))
    ;; loop invariants:
    ;;   - res is a list such that (string-concatenate-reverse res)
    ;;     is always the normalized string up to j
    ;;   - the tail of the string from j onward can be concatenated to
    ;;     the above value to get a partially normalized path referring
    ;;     to the same location as the original path
    (define (inside i j res)
      (if (>= j len)
          (finish i res)
          (if (eqv? #\/ (string-ref path j))
              (boundary i (+ j 1) res)
              (inside i (+ j 1) res))))
    (define (boundary i j res)
      (if (>= j len-1)
          (finish i res)
          (case (string-ref path j)
            ((#\.)
             (case (string-ref path (+ j 1))
               ((#\.)
                (if (or (>= j (- len 2)) (eqv? #\/ (string-ref path (+ j 2))))
                    (if (>= i (- j 1))
                        (if (null? res)
                            (backup j "" '())
                            (backup j (car res) (cdr res)))
                        (backup j (substring path i j) res))
                    (inside i (+ j 2) res)))
               ((#\/)
                (if (= i j)
                    (boundary (+ j 2) (+ j 2) res)
                    (let ((s (substring path i j)))
                      (boundary (+ j 2) (+ j 2) (cons s res)))))
               (else (inside i (+ j 1) res))))
            ((#\/) (boundary (+ j 1) (+ j 1) (collect i j res)))
            (else (inside i (+ j 1) res)))))
    (define (backup j s res)
      (let ((pos (+ j 3)))
        (cond
         ;; case 1: we're reduced to accumulating parents of the cwd
         ((or (string=? s "/..") (string=? s ".."))
          (boundary pos pos (cons "/.." (cons s res))))
         ;; case 2: the string isn't a component itself, skip it
         ((or (string=? s "") (string=? s ".") (string=? s "/"))
          (if (pair? res)
              (backup j (car res) (cdr res))
              (boundary pos pos (if (string=? s "/") '("/") '("..")))))
         ;; case3: just take the directory of the string
         (else
          (let ((d (path-directory s)))
            (cond
             ((string=? d "/")
              (boundary pos pos (if (null? res) '("/") res)))
             ((string=? d ".")
              (boundary pos pos res))
             (else (boundary pos pos (cons "/" (cons d res))))))))))
    ;; start with boundary if abs path, otherwise inside
    (if (zero? len)
        path
        ((if (eqv? #\/ (string-ref path 0)) boundary inside) 0 1 '()))))

(define (make-path . args)
  (define (x->string x)
    (cond ((string? x) x)
          ((symbol? x) (symbol->string x))
          ((number? x) (number->string x))
          (else (error "not a valid path component" x))))
  (define (trim-trailing-slash s)
    (let ((i (string-skip-right #\/ s)))
      (if i (substring s 0 (+ i 1)) "")))
  (if (null? args)
      ""
      (let ((start (trim-trailing-slash (x->string (car args)))))
        (let lp ((ls (cdr args))
                 (res (if (string=? "" start) '() (list start))))
          (cond
           ((null? ls)
            (apply string-append (reverse res)))
           ((pair? (car ls))
            (lp (append (car ls) (cdr ls)) res))
           (else
            (let ((x (trim-trailing-slash (x->string (car ls)))))
              (lp (cdr ls)
                  (if (string=? x "") res (cons x (cons "/" res)))))))))))
