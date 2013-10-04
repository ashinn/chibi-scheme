;; filesystem.scm -- additional filesystem utilities
;; Copyright (c) 2009-2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Creates the directory \var{dir}, including any parent directories
;;> as needed.  Returns \scheme{#t} on success and \scheme{#f} on
;;> failure.

(define (create-directory* dir . o)
  (let ((mode (if (pair? o) (car o) #o755)))
    (or (create-directory dir mode)
        (let ((slash
               (string-find-right dir #\/ 0 (string-skip-right dir #\/))))
          (and (> slash 0)
               (let ((parent (substring-cursor dir 0 slash)))
                 (and (not (equal? parent dir))
                      (not (file-exists? parent))
                      (create-directory* parent mode)
                      (create-directory dir mode))))))))

;;> The fundamental directory iterator.  Applies \var{kons} to
;;> each filename in directory \var{dir} and the result of the
;;> previous application, beginning with \var{knil}.  With
;;> \var{kons} as \scheme{cons} and \var{knil} as \scheme{'()},
;;> equivalent to \scheme{directory-files}.

(define (directory-fold dir kons knil)
  (let ((dir (opendir dir)))
    (if (not dir)
        knil
        (let lp ((res knil))
          (let ((file (readdir dir)))
            (if file
                (lp (kons (dirent-name file) res))
                (begin (closedir dir) res)))))))

;;> Returns a list of the files in \var{dir} in an unspecified
;;> order.

(define (directory-files dir)
  (directory-fold dir cons '()))

;;> The fundamental directory traverser.

(define (directory-fold-tree file down up here . o)
  ;; TODO: Use link count to reduce stats.
  ;; TODO: Provide higher-level wrapper for filtering and avoids links.
  (let ((knil (and (pair? o) (car o)))
        (down (or down (lambda (f acc) acc)))
        (up (or up (lambda (f acc) acc)))
        (here (or here (lambda (f acc) acc))))
    (let fold ((file file) (acc knil))
      (cond
       ((file-directory? file)
        (let ((d (opendir file)))
          (if (not d)
              acc
              (let lp ((acc acc))
                (let ((e (readdir d)))
                  (cond
                   (e
                    (let ((f (dirent-name e)))
                      (if (member f '("." ".."))
                          (lp acc)
                          (let ((path (string-append file "/" f)))
                            (lp (fold path (down path acc)))))))
                   (else
                    (closedir d)
                    (up file acc))))))))
       (else
        (here file acc))))))

;;> Unlinks the file named \var{string} from the filesystem.
;;> Returns \scheme{#t} on success and \scheme{#f} on failure.

(define (delete-file file)
  (if (not (%delete-file file))
      (raise-continuable
       (make-exception 'file "couldn't delete file" file delete-file #f))))

;;> Recursively delete all files and directories under \var{dir}.
;;> Unless optional arg \var{ignore-errors?} is true, raises an error
;;> if any file can't be deleted.

(define (delete-file-hierarchy dir . o)
  (let ((ignore-errors? (and (pair? o) (car o))))
    (if (member dir '("" "/"))
        (error "won't delete unsafe directory" dir))
    (directory-fold-tree
     dir
     #f
     (lambda (d acc)
       (if (and (not (delete-directory d)) (not ignore-errors?))
           (error "couldn't delete directory" d)))
     (lambda (f acc)
       (if (and (not (delete-file f)) (not ignore-errors?))
           (error "couldn't delete file" f))))))

;;> Runs \var{thunk} with the current directory of the process temporarily
;;> set to \var{dir}.

(define (with-directory dir thunk)
  (let ((pwd (current-directory)))
    (dynamic-wind
      (lambda () (change-directory dir))
      thunk
      (lambda () (change-directory pwd)))))

;;> Returns the \scheme{status} object for the given \var{file},
;;> which should be a string indicating the path or a file
;;> descriptor.

(define (file-status file)
  (if (string? file) (stat file) (fstat file)))

(define (file-device x) (stat-dev (if (stat? x) x (file-status x))))
(define (file-inode x) (stat-ino (if (stat? x) x (file-status x))))
(define (file-mode x) (stat-mode (if (stat? x) x (file-status x))))
(define (file-num-links x) (stat-nlinks (if (stat? x) x (file-status x))))
(define (file-owner x) (stat-uid (if (stat? x) x (file-status x))))
(define (file-group x) (stat-gid (if (stat? x) x (file-status x))))
(define (file-represented-device x) (stat-rdev (if (stat? x) x (file-status x))))
(define (file-size x) (stat-size (if (stat? x) x (file-status x))))
(define (file-block-size x) (stat-blksize (if (stat? x) x (file-status x))))
(define (file-num-blocks x) (stat-blocks (if (stat? x) x (file-status x))))
(define (file-access-time x) (stat-atime (if (stat? x) x (file-status x))))
(define (file-modification-time x) (stat-mtime (if (stat? x) x (file-status x))))
(define (file-change-time x) (stat-ctime (if (stat? x) x (file-status x))))

;;> File status accessors.  \var{x} should be a string indicating
;;> the file to lookup the status for, or an existing status object.
;;> Raises an error in the string case for non-existing files.
;;/

(define-syntax file-test-mode
  (syntax-rules ()
    ((file-test-mode op x)
     (let* ((tmp x)
            (st (if (stat? tmp) tmp (file-status tmp))))
       (and st (op (stat-mode st)))))))

(define (file-regular? x) (file-test-mode S_ISREG x))
(define (file-directory? x) (file-test-mode S_ISDIR x))
(define (file-character? x) (file-test-mode S_ISCHR x))
(define (file-block? x) (file-test-mode S_ISBLK x))
(define (file-fifo? x) (file-test-mode S_ISFIFO x))
(define (file-link? x) (file-test-mode S_ISLNK x))
(define (file-socket? x) (file-test-mode S_ISSOCK x))
(define (file-exists? x) (and (if (stat? x) #t (file-status x)) #t))

;;> File type tests.  \var{x} should be a string indicating the
;;> file to lookup the status for, or an existing status object.
;;> Returns \scheme{#t} if the file exists and the given type
;;> is satisfied, and \scheme{#f} otherwise.
;;/

;;> Equivalent to duplicating the file descriptor \var{old} to
;;> \var{new} and closing \var{old}.

(define (renumber-file-descriptor old new)
  (and (duplicate-file-descriptor-to old new)
       (close-file-descriptor old)))
