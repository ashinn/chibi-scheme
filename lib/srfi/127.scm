
(define eof-object (read-char (open-input-string "")))

(define (generator->lseq gen)
  (let ((val (gen)))
    (if (eof-object? val)
        '()
        (cons val gen))))

(define lseq-car car)
(define lseq-first car)

(define (lseq-cdr lseq)
  (if (procedure? (cdr lseq))
      (let ((val ((cdr lseq))))
        (cond
         ((eof-object? val)
          (set-cdr! lseq '())
          '())
         (else
          (let ((cell (cons val (cdr lseq))))
            (set-cdr! lseq cell)
            cell))))
      (cdr lseq)))

(define lseq-rest lseq-cdr)

(define (lseq? x)
  (if (pair? x)
      (or (procedure? (cdr x))
          (lseq? (cdr x)))
      (null? x)))

(define (lseq=? eq lseq1 lseq2)
  (let lp ((ls1 lseq1) (ls2 lseq2))
    (cond
     ((null? ls1) (null? ls2))
     ((null? ls2) #f)
     ((eq (lseq-car ls1) (lseq-car ls2))
      (lp (lseq-cdr ls1) (lseq-cdr ls2)))
     (else #f))))

(define (lseq-drop lseq k)
  (if (positive? k)
      (lseq-drop (lseq-cdr lseq) (- k 1))
      lseq))

(define (lseq-drop-while pred lseq)
  (if (and (pair? lseq) (pred (lseq-car lseq)))
      (lseq-drop-while pred (lseq-cdr lseq))
      lseq))

(define (lseq-take lseq k)
  (generator->lseq
   (lambda ()
     (if (positive? k)
         (let ((val (lseq-car lseq)))
           (set! lseq (lseq-cdr lseq))
           (set! k (- k 1))
           val)
         eof-object))))

(define (lseq-take-while pred lseq)
  (generator->lseq
   (lambda ()
     (if (and (pair? lseq) (pred (lseq-car lseq)))
         (let ((val (lseq-car lseq)))
           (set! lseq (lseq-cdr lseq))
           val)
         eof-object))))

(define (lseq-ref lseq k)
  (lseq-first (lseq-drop lseq k)))

(define (lseq-realize lseq)
  (let lp ((lseq lseq) (ls '()))
    (if (null? lseq)
        (reverse ls)
        (lp (lseq-cdr lseq) (cons (lseq-car lseq) ls)))))

(define (lseq->generator lseq)
  (lambda ()
    (if (null? lseq)
        eof-object
        (let ((val (lseq-car lseq)))
          (set! lseq (lseq-cdr lseq))
          val))))

(define (lseq-length lseq)
  (let lp ((lseq lseq) (len 0))
    (if (null? lseq) len (lp (lseq-cdr lseq) (+ len 1)))))

(define (lseq-append . lseqs)
  (if (every null? lseqs)
      '()
      (let ((lseq1 (car lseqs))
            (ls (cdr lseqs)))
        (define (gen)
          (cond
           ((pair? lseq1)
            (let ((val (lseq-car lseq1)))
              (set! lseq1 (cdr lseq1))
              val))
           ((pair? ls)
            (set! lseq1 (car ls))
            (set! ls (cdr ls))
            (gen))
           (else
            eof-object)))
        (generator->lseq gen))))

(define (lseq-zip . lseqs)
  (generator->lseq
   (lambda ()
     (if (any null? lseqs)
         eof-object
         (let ((val (map lseq-car lseqs)))
           (set! lseqs (map lseq-cdr lseqs))
           val)))))

(define (lseq-map proc lseq . o)
  (if (or (null? lseq) (any null? o))
      '()
      (let ((gen (lseq->generator lseq)))
        (generator->lseq
         (if (null? o)
             (lambda ()
               (let ((val (gen)))
                 (if (eof-object? val)
                     val
                     (proc val))))
             (let ((gens (map lseq->generator o)))
               (lambda ()
                 (let ((val (gen))
                       (vals (map (lambda (f) (f)) gens)))
                   (if (or (eof-object? val)
                           (any eof-object? vals))
                       eof-object
                       (apply proc val vals))))))))))

(define (lseq-for-each proc lseq . o)
  (let lp ((lseq (apply lseq-map proc lseq o)))
    (when (pair? lseq)
      (lseq-car lseq)
      (lp (lseq-cdr lseq)))))

(define (lseq-filter pred lseq)
  (let ((gen (lseq->generator lseq)))
    (define (filt)
      (let ((val (gen)))
        (if (or (eof-object? val) (pred val))
            val
            (filt))))
    (generator->lseq filt)))

(define (lseq-remove pred lseq)
  (lseq-filter (lambda (x) (not (pred x))) lseq))

(define (lseq-find-tail pred lseq)
  (and (pair? lseq)
       (if (pred (lseq-car lseq))
           lseq
           (lseq-find-tail pred (lseq-cdr lseq)))))

(define (lseq-find pred lseq)
  (cond ((lseq-find-tail pred lseq) => lseq-car) (else #f)))

(define (lseq-any pred lseq . o)
  (if (null? o)
      (let any ((lseq lseq))
        (and (pair? lseq)
             (or (pred (lseq-car lseq))
                 (any (lseq-cdr lseq)))))
      (let any ((lseqs (cons lseq o)))
        (and (every pair? lseqs)
             (or (apply pred (map lseq-car lseqs))
                 (any (map lseq-cdr lseqs)))))))

(define (lseq-every pred lseq . o)
  (if (null? o)
      (let every ((lseq lseq) (last #t))
        (if (null? lseq)
            last
            (let ((val (pred (lseq-car lseq))))
              (and val (every (lseq-cdr lseq) val)))))
      (let every ((lseqs (cons lseq o)) (last #t))
        (if (any null? lseqs)
            last
            (let ((val (apply pred (map lseq-car lseqs))))
              (and val (every (map lseq-cdr lseqs) val)))))))

(define (lseq-index pred lseq . o)
  (let ((i -1))
    (and (apply lseq-any (lambda args (set! i (+ i 1)) (apply pred args)) lseq o)
         i)))

(define (lseq-member elt lseq . o)
  (let* ((eq (if (pair? o) (car o) equal?))
         (res (lseq-drop-while (lambda (x) (not (eq x elt))) lseq)))
    (and (pair? res) res)))

(define (lseq-memq elt lseq) (lseq-member elt lseq eq?))
(define (lseq-memv elt lseq) (lseq-member elt lseq eqv?))
