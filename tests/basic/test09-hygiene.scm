
(write (or 1))
(newline)
(write (or #f 2))
(newline)
(write (or 3 #t))
(newline)

(let ((tmp 4))
  (write (or #f tmp))
  (newline))

