
;;> Interface to the filesystem and file descriptor objects.
;;> Note that file descriptors are currently represented as
;;> integers, but may be replaced with opaque (and gc-managed)
;;> objects in a future release.

(define-library (chibi filesystem)
  (export duplicate-file-descriptor duplicate-file-descriptor-to
          close-file-descriptor renumber-file-descriptor
          open-input-file-descriptor open-output-file-descriptor
          delete-file link-file symbolic-link-file rename-file
          directory-files directory-fold directory-fold-tree
          delete-file-hierarchy delete-directory
          create-directory create-directory*
          current-directory change-directory with-directory
          open open-pipe make-fifo
          read-link
          file-status               file-link-status
          file-device               file-inode
          file-mode                 file-num-links
          file-owner                file-group
          file-represented-device   file-size
          file-block-size           file-num-blocks
          file-access-time file-change-time
          file-modification-time  file-modification-time/safe
          file-regular?    file-directory?        file-character?
          file-block?      file-fifo?             file-link?
          file-socket?     file-exists?
          get-file-descriptor-flags   set-file-descriptor-flags!
          get-file-descriptor-status  set-file-descriptor-status!
          open/read        open/write             open/read-write
          open/create      open/exclusive         open/truncate
          open/append      open/non-block
          file-lock        file-truncate
          file-is-readable? file-is-writable? file-is-executable?
          chmod is-a-tty?)
  (cond-expand
   (chibi
    (export lock/shared lock/exclusive lock/non-blocking lock/unlock)
    (import (chibi) (chibi string))
    (include-shared "filesystem")
    (include "filesystem.scm"))
   (chicken
    (import (scheme base) (srfi 1)
            (only (chicken) delete-file rename-file file-exists?)
            (rename (posix) (file-truncate %file-trunc))
            (chibi string))
    (begin
      (define file-status file-stat)
      (define (file-link-status x) (file-stat x #t))
      (define (stat-dev x) (vector-ref x 9))
      (define (stat-ino x) (vector-ref x 0))
      (define (stat-mode x) (vector-ref x 1))
      (define (stat-nlinks x) (vector-ref x 2))
      (define (stat-uid x) (vector-ref x 3))
      (define (stat-gid x) (vector-ref x 4))
      (define (stat-rdev x) (vector-ref x 10))
      (define (stat-size x) (vector-ref x 5))
      (define (stat-blksize x) (vector-ref x 11))
      (define (stat-blocks x) (vector-ref x 12))
      (define (stat-atime x) (vector-ref x 6))
      (define (stat-mtime x) (vector-ref x 7))
      (define (stat-ctime x) (vector-ref x 8))
      (define (file-mode x) (stat-mode (if (vector? x) x (file-stat x))))
      (define (file-num-links x) (stat-nlinks (if (vector? x) x (file-stat x))))
      (define (file-group x) (stat-gid (if (vector? x) x (file-stat x))))
      (define (file-inode x) (stat-ino (if (vector? x) x (file-stat x))))
      (define (file-device x) (stat-dev (if (vector? x) x (file-stat x))))
      (define (file-represented-device x) (if (vector? x) x (file-stat x)))
      (define (file-block-size x) (stat-blksize (if (vector? x) x (file-stat x))))
      (define (file-num-blocks x) (stat-blocks (if (vector? x) x (file-stat x))))
      (define duplicate-file-descriptor duplicate-fileno)
      (define duplicate-file-descriptor-to duplicate-fileno)
      (define close-file-descriptor file-close)
      (define open-input-file-descriptor open-input-file*)
      (define open-output-file-descriptor open-output-file*)
      (define link-file file-link)
      (define symbolic-link-file create-symbolic-link)
      (define read-link read-symbolic-link)
      (define open file-open)
      (define open-pipe create-pipe)
      (define make-fifo create-fifo)
      (define file-regular? regular-file?)
      (define file-directory? directory?)
      (define file-character? character-device?)
      (define file-block? block-device?)
      (define file-fifo? fifo?)
      (define file-link? symbolic-link?)
      (define file-socket? socket?)
      (define file-is-readable? file-read-access?)
      (define file-is-writable? file-write-access?)
      (define file-is-executable? file-execute-access?)
      (define (get-file-descriptor-flags fileno)
        (file-control fileno fcntl/getfd))
      (define (set-file-descriptor-flags! fileno x)
        (file-control fileno fcntl/setfd x))
      (define (get-file-descriptor-status fileno)
        (file-control fileno fcntl/getfl))
      (define (set-file-descriptor-status! fileno x)
        (file-control fileno fcntl/setfl x))
      (define open/read-write open/rdwr)
      (define open/create open/creat)
      (define open/exclusive open/excl)
      (define open/truncate open/trunc)
      (define open/non-block open/nonblock)
      (define chmod change-file-mode)
      (define is-a-tty? terminal-port?)
      (define (file-truncate port len)
        (%file-trunc (if (integer? port) port (port->fileno port)) len))
      (define (create-directory* dir)
        (create-directory dir #t))
      (define (directory-files dir)
        (cons "." (cons ".." (directory dir #t))))
      (define (directory-fold dir kons knil)
        (fold kons knil (directory-files dir)))
      (define (directory-fold-tree file down up here . o)
        (let ((knil (and (pair? o) (car o)))
              (down (or down (lambda (f acc) acc)))
              (up (or up (lambda (f acc) acc)))
              (here (or here (lambda (f acc) acc))))
          (let fold ((file file) (acc knil))
            (cond
             ((file-directory? file)
              (let lp ((ls (directory-files file)) (acc (down file acc)))
                (cond
                 ((null? ls) (up file acc))
                 ((member (car ls) '("." "..")) (lp (cdr ls) acc))
                 (else
                  (lp (cdr ls) (fold (string-append file "/" (car ls)) acc))))))
             (else
              (here file acc))))))
      (define (delete-file-hierarchy dir . o)
        (delete-directory dir #t))
      (define (renumber-file-descriptor old new)
        (and (duplicate-file-descriptor-to old new)
             (close-file-descriptor old)))
      (define (with-directory dir thunk)
        (let ((pwd (current-directory)))
          (dynamic-wind
            (lambda () (change-directory dir))
            thunk
            (lambda () (change-directory pwd)))))
      (define (file-modification-time/safe file)
        (guard (exn (else #f))
          (file-modification-time file)))
      ))
   (sagittarius
    (import (scheme base) (sagittarius))
    (begin
      (define (file-status x) x)
      (define file-link-status file-status)
      (define-syntax define-unimplemented
        (syntax-rules ()
          ((define-unimplemented def ...)
           (define (def . x) (error "unimplemented" 'def)) ...)))
      (define-unimplemented
        stat-dev stat-ino stat-mode stat-nlinks stat-uid stat-gid
        stat-rdev stat-blksize stat-blocks)
      (define (stat-size x) (file-size-in-bytes x))
      (define (stat-atime x) (file-stat-atime x))
      (define (stat-mtime x) (file-stat-mtime x))
      (define (stat-ctime x) (file-stat-ctime x))
      (define file-is-readable? file-readable?)
      (define file-is-writable? file-writable?)
      (define file-is-executable? file-executable?)
      (define file-link? file-symbolic-link?)

      (define-unimplemented
        duplicate-file-descriptor duplicate-file-descriptor-to
        close-file-descriptor open-input-file-descriptor
        open-output-file-descriptor)
      ))))
