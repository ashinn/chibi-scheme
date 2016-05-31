(define-binary-record-type tar
  (make (make-tar))
  (write write-tar-raw)
  (block
   (path (padded-string 100) (getter tar-path-raw) (setter tar-path-raw-set!))
   (mode (octal 8))
   (uid (octal 8))
   (gid (octal 8))
   (size (octal 12))
   (time (octal 12))
   (checksum (octal 8))
   (type (fixed-string 1))
   (link-name (padded-string 100))
   (ustar (padded-string 6))
   (ustar-version (padded-string 2))
   (owner (padded-string 32))
   (group (padded-string 32))
   (device-major (octal 8))
   (device-minor (octal 8))
   (path-prefix (padded-string 155))
   #u8(0 0 0 0 0 0 0 0 0 0 0 0)))

(define (tar-compute-checksum tar)
  (let ((tmp (open-output-bytevector)))
    (write-tar-raw tar tmp)
    (let ((bv (get-output-bytevector tmp)))
      (do ((i 0 (+ i 1))) ((= i 8))
        (bytevector-u8-set! bv (+ i 148) 32))
      (do ((i 0 (+ i 1))
           (sum 0 (+ sum (bytevector-u8-ref bv i))))
          ((= i 512) sum)))))

;; wrap the writer to automatically compute the checksum
(define (write-tar tar out)
  (tar-checksum-set! tar (tar-compute-checksum tar))
  (write-tar-raw tar out))

;; wrap the path to use the prefix
(define (tar-path tar)
  (string-append (tar-path-prefix tar) (tar-path-raw tar)))

(define (tar-normalize-path tar path)
  (cond ((string-suffix? "/." path) (string-trim-right path #\.))
        ((and (not (string-suffix? "/" path)) (equal? "5" (tar-type tar)))
         (string-append path "/"))
        (else path)))

(define (tar-path-set! tar path)
  (let* ((path (tar-normalize-path tar path))
         (len (string-length path)))
    (cond ((< len 100)
           (tar-path-raw-set! tar path))
          ((< len 255)
           (tar-path-raw-set! tar (substring path (- len 100)))
           (tar-path-prefix-set! tar (substring path 0 (- len 100))))
          (else (error "path name too long")))))

;; utilities

(define (read-modulo-bytevector in len mod)
  (let ((res (read-bytevector len in))
        (rem (modulo len mod)))
    (if (positive? rem)
        (read-bytevector (- mod rem) in))
    res))

(define (write-modulo-file out file mod)
  (let ((in (open-binary-input-file file)))
    (let lp ()
      (let ((bv (read-bytevector mod in)))
        (cond
         ((eof-object? bv))
         (else
          (write-bytevector bv out)
          (let ((len (bytevector-length bv)))
            (if (< len mod)
                (write-bytevector (make-bytevector (- mod len) 0) out)
                (lp)))))))))

;; fundamental iterator
(define (tar-fold src kons knil)
  (let ((in (cond ((string? src) (open-binary-input-file src))
                  ((bytevector? src) (open-input-bytevector src))
                  (else src))))
    (let lp ((acc knil) (empty 0))
      (cond
       ((or (eof-object? (peek-u8 in)) (>= empty 2))
        (close-input-port in)
        acc)
       (else
        (let ((tar (read-tar in)))
          (if (and (equal? "" (tar-path tar)) (zero? (tar-size tar)))
              (lp acc (+ empty 1))
              (let ((bv (read-modulo-bytevector in (tar-size tar) 512)))
                (lp (kons tar bv acc) 0)))))))))

;; not a tar-bomb and no absolute paths
(define (tar-safe? tarball)
  (define (path-top path)
    (substring-cursor path (string-cursor-start path) (string-find path #\/)))
  (let ((files (map path-normalize (tar-files tarball))))
    (and (every path-relative? files)
         (or (< (length files) 2)
             (let ((dir (path-top (car files))))
               (every (lambda (f) (equal? dir (path-top f))) (cdr files)))))))

(define (tar-for-each tarball proc)
  (tar-fold tarball (lambda (tar bv acc) (proc tar bv)) #f))

;; list the files in the archive
(define (tar-files tarball)
  (reverse (tar-fold tarball (lambda (tar bv acc) (cons (tar-path tar) acc)) '())))

;; extract to the current filesystem
(define (tar-extract tarball . o)
  (define (safe-path path)
    (string-trim-left
     (path-strip-leading-parents (path-normalize path))
     #\/))
  (let ((rename (if (pair? o) (car o) safe-path)))
    (tar-for-each
     tarball
     (lambda (tar bv)
       (let ((path (rename (tar-path tar))))
         (case (string-ref (tar-type tar) 0)
           ((#\0 #\null)
            (let ((out (open-output-file-descriptor
                        (open path
                              (bitwise-ior open/write
                                           open/create
                                           open/non-block)
                              (tar-mode tar)))))
              (write-bytevector bv out)
              (close-output-port out)))
           ((#\1) (link-file (rename (tar-link-name tar)) path))
           ((#\2) (symbolic-link-file (rename (tar-link-name tar)) path))
           ((#\5) (create-directory* path (tar-mode tar)))
           ((#\g #\x))                   ;; meta data
           ((#\3 #\4 #\6) (error "devices not supported" (tar-type tar)))
           (else (error "invalid tar type" (tar-type tar)))))))))

(define (tar-extract-file tarball file)
  (call-with-current-continuation
   (lambda (return)
     (tar-for-each
      tarball
      (lambda (tar bv) (if (equal? (tar-path tar) file) (return bv))))
     #f)))

(define (file-owner-or-nobody st)
  (or (user-name (user-information (file-owner st))) "nobody"))
(define (file-group-or-nobody st)
  (or (group-name (group-information (file-group st))) "nobody"))

(define (file->tar file)
  (let ((tar (make-tar))
        (st (file-link-status file)))
    (tar-path-set! tar file)
    (tar-ustar-set! tar "ustar")
    (tar-ustar-version-set! tar "00")
    (cond
     (st
      (tar-mode-set! tar (file-mode st))
      (tar-uid-set! tar (file-owner st))
      (tar-gid-set! tar (file-group st))
      (tar-owner-set! tar (file-owner-or-nobody st))
      (tar-group-set! tar (file-group-or-nobody st))
      (tar-time-set! tar (file-modification-time st))
      (tar-type-set! tar (cond ((file-link? st) "2")
                               ((file-character? st) "3")
                               ((file-block? st) "4")
                               ((file-directory? st) "5")
                               (else "0")))
      (if (equal? "0" (tar-type tar))
          (tar-size-set! tar (file-size st)))
      (if (file-link? st)
          (tar-link-name-set! tar (read-link file)))))
    tar))

(define (inline->tar file content . o)
  (let ((tar (make-tar)))
    (tar-path-set! tar file)
    (tar-ustar-set! tar "ustar")
    (tar-ustar-version-set! tar "00")
    (tar-mode-set! tar (if (pair? o) (car o) #o644))
    (tar-uid-set! tar (current-user-id))
    (tar-gid-set! tar (current-group-id))
    (tar-owner-set! tar (user-name (user-information (current-user-id))))
    (tar-group-set! tar (group-name (group-information (current-group-id))))
    (tar-time-set! tar (exact (round (current-second))))
    (tar-type-set! tar "0")
    (tar-size-set! tar (bytevector-length content))
    tar))

(define (tar-add-directories tar out acc)
  (let lp ((dir (path-directory (tar-path tar))) (acc acc))
    (let ((dir/ (if (string-suffix? "/" dir) dir (string-append dir "/"))))
      (cond
       ((member dir '("" "." "/")) acc)
       ((assoc dir/ acc) (lp (path-directory dir) acc))
       (else
        (let ((acc (lp (path-directory dir) (cons (cons dir/ #f) acc))))
          (let ((tar2 (make-tar)))
            (tar-path-set! tar2 dir/)
            (tar-ustar-set! tar2 "ustar")
            (tar-ustar-version-set! tar2 "00")
            (tar-mode-set! tar2 (bitwise-ior #o111 (tar-mode tar) ))
            (tar-uid-set! tar2 (tar-uid tar))
            (tar-gid-set! tar2 (tar-gid tar))
            (tar-owner-set! tar2 (tar-owner tar))
            (tar-group-set! tar2 (tar-group tar))
            (tar-time-set! tar2 (tar-time tar))
            (tar-type-set! tar2 "5")
            (tar-size-set! tar2 0)
            (write-tar tar2 out)
            acc)))))))

;; create an archive for a given file list
(define (tar-create tarball files . o)
  (let* ((rename (if (pair? o) (car o) (lambda (f) f)))
         (no-recurse? (and (pair? o) (pair? (cdr o)) (cadr o)))
         (get-src
          (lambda (x) (if (pair? x) (and (eq? 'rename (car x)) (cadr x)) x)))
         (get-dest
          (lambda (x)
            (rename (if (pair? x)
                        (if (eq? 'rename (car x))
                            (car (cddr x))
                            (cadr x))
                        x))))
         (get-content
          (lambda (x) (and (pair? x) (eq? 'inline (car x))
                       (let ((c (car (cddr x))))
                         (if (string? c) (string->utf8 c) c))))))
    (let ((out (cond ((eq? #t tarball) (current-output-port))
                     ((eq? #f tarball) (open-output-bytevector))
                     (else (open-binary-output-file tarball)))))
      (fold
       (lambda (file acc)
         (let ((src0 (get-src file))
               (dest0 (get-dest file))
               (content0 (get-content file)))
           (define (kons x acc)
             (let* ((src (get-src x))
                    (dest (if (equal? x src0) dest0 (get-dest x)))
                    (content (if (equal? x src0) content0 (get-content x)))
                    (tar (if content
                             (inline->tar dest content)
                             (file->tar src))))
               (tar-path-set! tar dest)
               (cond
                ((assoc (tar-path tar) acc)
                 => (lambda (prev)
                      (if (not (and (file-directory? src)
                                    (file-directory? (cdr prev))))
                          (write-string
                           (string-append "tar-create: duplicate file: "
                                          dest "\n")
                           (current-error-port)))
                      acc))
                (else
                 (let ((acc (tar-add-directories tar out acc)))
                   (write-tar tar out)
                   (cond
                    ((and (string? src) (equal? "0" (tar-type tar)))
                     (write-modulo-file out src 512))
                    (content
                     (write-bytevector content out)
                     (let ((rem (modulo (bytevector-length content) 512)))
                       (if (positive? rem)
                           (write-bytevector
                            (make-bytevector (- 512 rem) 0) out)))))
                   (cons (cons (tar-path tar) src) acc))))))
           (if (and src0 (not no-recurse?))
               (directory-fold-tree src0 #f #f kons acc)
               (kons src0 acc))))
       '() files)
      (write-bytevector (make-bytevector 1024 0) out)
      (let ((res (if (eq? #f tarball) (get-output-bytevector out))))
        (close-output-port out)
        res))))

(define (main args)
  (let ((args (cdr args)))
    (cond
     ((equal? "t" (car args))
      (for-each (lambda (f) (write-string f) (newline)) (tar-files (cadr args))))
     ((equal? "x" (car args))
      (if (tar-safe? (cadr args))
          (tar-extract (cadr args))
          (error "tar file not a single relative directory" (cadr args))))
     ((equal? "c" (car args))
      (tar-create (cadr args) (cddr args)))
     ((equal? "f" (car args))
      (write-string
       (utf8->string (tar-extract-file (cadr args) (car (cddr args))))))
     (else
      (error "unknown tar command" (car args))))))
