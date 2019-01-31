;; Copyright (c) 2009-2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; POSIX basename
;; (define (path-strip-directory path)
;;   (if (string=? path "")
;;       path
;;       (let ((end (string-skip-right path #\/)))
;;         (if (zero? end)
;;             "/"
;;             (let ((start (string-find-right path #\/ 0 end)))
;;               (substring-cursor path start end))))))

;;> Returns just the basename of \var{path}, with any directory
;;> removed.  If \var{path} does not contain a directory separator,
;;> return the whole \var{path}.  If \var{path} ends in a directory
;;> separator (i.e. path is a directory), or is empty, return the
;;> empty string.

;; GNU basename
(define (path-strip-directory path)
  (substring-cursor path (string-find-right path #\/)))

;;> Returns just the directory of \var{path}.
;;> If \var{path} is relative (or empty), return \scheme{"."}.

(define (path-directory path)
  (if (string=? path "")
      "."
      (let ((start (string-cursor-start path))
            (end (string-skip-right path #\/)))
        (if (string-cursor=? start end)
            "/"
            (let ((slash (string-find-right path #\/ start end)))
              (if (string-cursor=? start slash)
                  "."
                  (let ((start2 (string-skip-right path #\/ start slash)))
                    (if (string-cursor=? start start2)
                        "/"
                        (substring-cursor path start start2)))))))))

(define (path-extension-pos path)
  (let ((start (string-cursor-start path))
        (end (string-cursor-end path)))
    (let lp ((i end) (dot #f))
      (if (string-cursor<=? i start)
          #f
          (let* ((i2 (string-cursor-prev path i))
                 (ch (string-cursor-ref path i2)))
            (cond ((eqv? #\. ch)
                   (and (string-cursor<? i end) (lp i2 (or dot i))))
                  ((eqv? #\/ ch) #f)
                  (dot)
                  (else (lp i2 #f))))))))

;;> Returns the rightmost extension of \var{path}, not including the
;;> \scheme{"."}.  If there is no extension, returns \scheme{#f}.  The
;;> extension will always be non-empty and contain no \scheme{"."}s.

(define (path-extension path)
  (let ((i (path-extension-pos path)))
    (and i
         (substring-cursor path i))))

;;> Returns \var{path} with the extension, if any, removed,
;;> along with the \scheme{"."}.

(define (path-strip-extension path)
  (let ((i (path-extension-pos path)))
    (if i
        (substring-cursor path
                          (string-cursor-start path)
                          (string-cursor-prev path i))
        path)))

;;> Returns \var{path} with the extension, if any, replaced
;;> with \var{ext}.

(define (path-replace-extension path ext)
  (string-append (path-strip-extension path) "." ext))

;;> Returns \var{path} with any leading ../ removed.

(define (path-strip-leading-parents path)
  (if (string-prefix? "../" path)
      (path-strip-leading-parents
       (substring-cursor
        path
        (string-cursor-forward path (string-cursor-start path) 3)))
      (if (equal? path "..") "" path)))

;;> Returns \scheme{#t} iff \var{path} is an absolute path,
;;> i.e. begins with "/".

(define (path-absolute? path)
  (and (not (string=? "" path)) (eqv? #\/ (string-ref path 0))))

;;> Returns \scheme{#t} iff \var{path} is a relative path.

(define (path-relative? path) (not (path-absolute? path)))

;;> Returns the suffix of \var{path} relative to the directory
;;> \var{dir}, or \scheme{#f} if \var{path} is not contained in
;;> \var{dir}.  If the two are the same (modulo a trailing
;;> \scheme{"/"}), then \scheme{"."} is returned.

(define (path-relative-to path dir)
  (let* ((path (path-normalize path))
         (path-end (string-cursor-end path))
         (dir (path-normalize dir))
         (dir-end (string-cursor-end dir)))
    (call-with-values
        (lambda () (string-mismatch dir path))
      (lambda (i j)
        (cond
         (
          ;;(not (string-cursor<=?
          ;;      (string-cursor-next path (string-cursor-start path))
          ;;      dir-end i path-end))
          (not (and (string-cursor<=?
                     (string-cursor-next dir (string-cursor-start dir))
                     dir-end i)
                    (string-cursor<=? j path-end)))
          (and (string-cursor=? j path-end)
               (string-cursor=? (string-cursor-next dir i) dir-end)
               (eqv? #\/ (string-cursor-ref dir i))
               "."))
         ((string-cursor=? j path-end)
          ".")
         ((eqv? #\/ (string-cursor-ref path j))
          (let ((j2 (string-cursor-next path j)))
            (if (string-cursor=? j2 path-end) "." (substring-cursor path j2))))
         ((eqv? #\/ (string-cursor-ref path (string-cursor-prev path j)))
          (substring-cursor path j))
         (else
          #f))))))

;;> Resolve \var{path} relative to the given directory.  Returns
;;> \var{path} unchanged if already absolute.

(define (path-resolve path dir)
  (if (path-absolute? path) path (make-path dir path)))

;; This looks big and hairy, but it's mutation-free and guarantees:
;;   (string=? s (path-normalize s))  <=>  (eq? s (path-normalize s))
;; i.e. fast and simple for already normalized paths.

;;> Returns a normalized version of path, with duplicate directory
;;> separators removed and "/./" and "x/../" references removed.
;;> Does not take symbolic links into account - this is meant to
;;> be abstract and applicable to paths on remote systems and in
;;> URIs.  Returns \var{path} itself if \var{path} is already
;;> normalized.

(define (path-normalize path)
  (let* ((start (string-cursor-start path))
         (end (string-cursor-end path))
         (end-1 (string-cursor-prev path end)))
    (define (collect i j res)
      (if (string-cursor>=? i j) res (cons (substring-cursor path i j) res)))
    (define (finish i res)
      (if (string-cursor=? start i)
          path
          (string-join (reverse (collect i end res)))))
    ;; loop invariants:
    ;;   - res is a list such that (string-concatenate-reverse res)
    ;;     is always the normalized string up to j
    ;;   - the tail of the string from j onward can be concatenated to
    ;;     the above value to get a partially normalized path referring
    ;;     to the same location as the original path
    (define (inside i j res)
      (if (string-cursor>=? j end)
          (finish i res)
          (if (eqv? #\/ (string-cursor-ref path j))
              (boundary i (string-cursor-next path j) res)
              (inside i (string-cursor-next path j) res))))
    (define (boundary i j res)
      (if (string-cursor>=? j end)
          (finish i res)
          (case (string-cursor-ref path j)
            ((#\.)
             (cond
              ((or (string-cursor=? j end-1)
                   (eqv? #\/ (string-cursor-ref path (string-cursor-next path j))))
               (if (string-cursor=? i j)
                   (boundary (string-cursor-forward path j 2)
                             (string-cursor-forward path j 2)
                             res)
                   (let ((s (substring-cursor path i j)))
                     (boundary (string-cursor-forward path j 2)
                               (string-cursor-forward path j 2)
                               (cons s res)))))
              ((eqv? #\. (string-cursor-ref path (string-cursor-next path j)))
               (if (or (string-cursor>=? j (string-cursor-back path end 2))
                       (eqv? #\/ (string-cursor-ref
                                  path
                                  (string-cursor-forward path j 2))))
                   (if (string-cursor>=? i (string-cursor-prev path j))
                       (if (null? res)
                           (backup j "" '())
                           (backup j (car res) (cdr res)))
                       (backup j (substring-cursor path i j) res))
                   (inside i (string-cursor-forward path j 2) res)))
              (else
               (inside i (string-cursor-next path j) res))))
            ((#\/)
             (boundary (string-cursor-next path j)
                       (string-cursor-next path j)
                       (collect i j res)))
            (else (inside i (string-cursor-next path j) res)))))
    (define (backup j s res)
      (let ((pos (string-cursor-forward path j 3)))
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
    (if (string-cursor=? start end)
        path
        ((if (eqv? #\/ (string-ref path 0)) boundary inside)
         start (string-cursor-next path start) '()))))

;;> Return a new string representing the path where each of \var{args}
;;> is a path component, separated with the directory separator.
;;> \var{args} may include symbols and integers, in addition to
;;> strings.

(define (make-path . args)
  (define (x->string x)
    (cond ((string? x) x)
          ((symbol? x) (symbol->string x))
          ((number? x) (number->string x))
          (else (error "not a valid path component" x))))
  (define (trim-trailing-slash s)
    (substring-cursor s (string-cursor-start s) (string-skip-right s #\/)))
  (if (null? args)
      ""
      (let* ((args0 (x->string (car args)))
             (start (trim-trailing-slash args0)))
        (let lp ((ls (cdr args))
                 (res (if (string=? "" start) '() (list start))))
          (cond
           ((null? ls)
            (if (and (null? res) (not (string=? "" args0)))
                "/"
                (string-join (reverse res))))
           ((pair? (car ls))
            (lp (append (car ls) (cdr ls)) res))
           (else
            (let ((x (trim-trailing-slash (x->string (car ls)))))
              (cond
               ((string=? x "")
                (lp (cdr ls) res))
               ((eqv? #\/ (string-ref x 0))
                (lp (cdr ls) (cons x res)))
               (else
                (lp (cdr ls) (cons x (cons "/" res))))))))))))
