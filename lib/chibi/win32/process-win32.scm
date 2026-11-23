(define (emergency-exit . code?)
  (%exit (if (pair? code?) 
           (let ((c (car code?)))
            (cond ((integer? c) c)
                  ((eq? #t c) 0)
                  (else 1)))
           0)))

(define (exit . o)
  ;; Run the after thunks of any active dynamic-winds, then exit.  This
  ;; walks the wind chain directly rather than jumping to a continuation
  ;; captured at load time, which may not be resumable (e.g. a native
  ;; stack continuation saved in an image).
  (let* ((here (%dk))
         (root (let lp ((point here))
                 (if (zero? (%point-depth point))
                     point
                     (lp (%point-parent point))))))
    (travel-to-point! here root)
    (%dk root))
  (apply emergency-exit o))
