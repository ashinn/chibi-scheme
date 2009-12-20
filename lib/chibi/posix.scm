
(define (directory-fold dir kons knil)
  (let ((dir (opendir dir)))
    (let lp ((res knil))
      (let ((file (readdir dir)))
        (if file (lp (kons (dirent-name file) res)) res)))))

(define (directory-files dir)
  (directory-fold dir cons '()))

