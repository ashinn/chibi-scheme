
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

(define (tar-path-set! tar path)
  (let ((len (string-length path)))
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
  (let ((in (if (string? src) (open-binary-input-file src) src)))
    (let lp ((acc knil))
      (cond
       ((eof-object? (peek-char in))
        (close-input-port in)
        acc)
       (else
        (let* ((tar (read-tar in))
               (bv (read-modulo-bytevector in (tar-size tar) 512)))
          (lp (kons tar bv acc))))))))

;; not a tar-bomb and no absolute paths
(define (tar-safe? tarball)
  (define (path-top path)
    (substring path 0 (string-find path #\/)))
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
           ((#\5) (create-directory path (tar-mode tar)))
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

(define (file->tar file)
  (let ((tar (make-tar))
        (st (file-link-status file)))
    (tar-path-set! tar file)
    (tar-ustar-set! tar "ustar")
    (tar-ustar-version-set! tar "00")
    (tar-mode-set! tar (file-mode st))
    (tar-uid-set! tar (file-owner st))
    (tar-gid-set! tar (file-group st))
    (tar-owner-set! tar (user-name (user-information (file-owner st))))
    (tar-group-set! tar (group-name (group-information (file-group st))))
    (tar-time-set! tar (+ 1262271600 (file-modification-time st)))
    (tar-type-set! tar (cond ((file-link? st) "2")
                             ((file-character? st) "3")
                             ((file-block? st) "4")
                             ((file-directory? st) "5")
                             (else "0")))
    (if (equal? "0" (tar-type tar))
        (tar-size-set! tar (file-size st)))
    (if (file-link? st)
        (tar-link-name-set! tar (read-link file)))
    tar))

;; create an archive for a given file list
(define (tar-create tarball files . o)
  (let ((rename (if (pair? o) (car o) (lambda (f) #t))))
    (let ((out (open-binary-output-file tarball)))
      (for-each
       (lambda (file)
         (directory-fold-tree
          file
          (lambda (dir acc) (write-tar (file->tar dir) out))
          #f
          (lambda (path acc)
            (let ((f (rename path)))
              (if f
                  (let ((tar (file->tar path)))
                    (if (string? f)
                        (tar-path-set! tar f))
                    (write-tar tar out)
                    (if (equal? "0" (tar-type tar))
                        (write-modulo-file out path 512))))))))
       files)
      (close-output-port out))))

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
      (display (utf8->string (tar-extract-file (cadr args) (car (cddr args))))))
     (else
      (error "unknown tar command" (car args))))))
