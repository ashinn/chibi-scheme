
(define (directory-files path)
  (let ((dir (opendir path)))
    (let lp ((res '()))
      (let ((file (readdir dir)))
        (if file (lp (cons (dirent-name file) res)) res)))))

