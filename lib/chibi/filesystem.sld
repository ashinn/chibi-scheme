
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
          create-directory
          current-directory change-directory with-directory
          open open-pipe make-fifo
          file-status
          file-device               file-inode
          file-mode                 file-num-links
          file-owner                file-group
          file-represented-device   file-size
          file-block-size           file-num-blocks
          file-access-time file-modification-time file-change-time
          file-regular?    file-directory?        file-character?
          file-block?      file-fifo?             file-link?
          file-socket?     file-exists?
          get-file-descriptor-flags   set-file-descriptor-flags!
          get-file-descriptor-status  set-file-descriptor-status!
          open/read        open/write             open/read-write
          open/create      open/exclusive         open/truncate
          open/append      open/non-block
          is-a-tty?)
  (import (chibi))
  (include-shared "filesystem")
  (include "filesystem.scm"))

