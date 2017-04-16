
(define eof (read-char (open-input-string "")))

(define (list->generator ls)
  (lambda ()
    (if (null? ls)
        eof
        (let ((res (car ls)))
          (set! ls (cdr ls))
          res))))

(define (generator . elts)
  (list->generator elts))

(define (make-iota-generator count . o)
  (let ((val (if (pair? o) (car o) 0))
        (step (if (and (pair? o) (pair? (cdr o))) (cadr o) 1))
        (i 0))
    (lambda ()
      (if (>= i count)
          eof
          (let ((res val))
            (set! val (+ val step))
            (set! i (+ i 1))
            res)))))

(define (make-range-generator start . o)
  (let ((end (if (pair? o) (car o) +inf.0))
        (step (if (and (pair? o) (pair? (cdr o))) (cadr o) 1))
        (val start))
    (lambda ()
      (if (>= val end)
          eof
          (let ((res val))
            (set! val (+ val step))
            res)))))

(define (make-coroutine-generator proc)
  (let ((return #f)
        (resume #f))
    (lambda ()
      (call-with-current-continuation
       (lambda (outer)
         (set! return outer)
         (cond
          (resume
           (resume #f))
          (else
           ;; first time
           (proc (lambda (result)
                   (call-with-current-continuation
                    (lambda (inner)
                      (set! resume inner)
                      (return result)))))
           ;; done
           (set! resume (lambda (v) (return eof)))
           (return eof))))))))

(define (vector->generator vec . o)
  (let ((i (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec))))
    (lambda ()
      (if (>= i end)
          eof
          (let ((res (vector-ref vec i)))
            (set! i (+ i 1))
            res)))))

(define (reverse-vector->generator vec . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (vector-length vec)))
         (i (- end 1)))
    (lambda ()
      (if (< i start)
          eof
          (let ((res (vector-ref vec i)))
            (set! i (- i 1))
            res)))))

(define (string->generator str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (let ((sc (string-index->cursor str start))
          (end-sc (string-index->cursor str end)))
      (lambda ()
        (if (string-cursor>=? sc end-sc)
            eof
            (let ((res (string-ref/cursor str sc)))
              (set! sc (string-cursor-next str sc))
              res))))))

(define (bytevector->generator bv . o)
  (let ((i (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (bytevector-length bv))))
    (lambda ()
      (if (>= i end)
          eof
          (let ((res (bytevector-u8-ref bv i)))
            (set! i (+ i 1))
            res)))))

(define (make-for-each-generator for-each obj)
  (make-coroutine-generator
   (lambda (yield) (for-each yield obj))))

(define (make-unfold-generator stop? mapper successor seed)
  (lambda ()
    (if (stop? seed)
        eof
        (let ((res (mapper seed)))
          (set! seed (successor seed))
          res))))

(define (gcons* . elts)
  (if (null? elts) (error "gcons* requires at least one arg"))
  (lambda ()
    (if (null? (cdr elts))
        ((car elts))
        (let ((res (car elts)))
          (set! elts (cdr elts))
          res))))

(define (gappend . gens)
  (define (g)
    (if (null? gens)
        eof
        (let ((res ((car gens))))
          (cond
           ((eof-object? res)
            (set! gens (cdr gens))
            (g))
           (else
            res)))))
  g)

(define (gcombine proc seed gen . o)
  (if (null? o)
      (lambda ()
        (call-with-values
            (lambda ()
              (let ((elt (gen)))
                (if (eof-object? elt) (values eof seed) (proc elt seed))))
          (lambda (res new-seed)
            (set! seed new-seed)
            res)))
      (lambda ()
        (call-with-values
            (lambda ()
              (let ((elts (cons (gen) (map (lambda (g) (g)) o))))
                (if (memq eof elts)
                    (values eof seed)
                    (apply proc (append elts (list seed))))))
          (lambda (res new-seed)
            (set! seed new-seed)
            res)))))

(define (gfilter pred gen)
  (define (g)
    (let ((res (gen)))
      (cond
       ((eof-object? res) res)
       ((pred res) res)
       (else (g)))))
  g)

(define (gremove pred gen)
  (gfilter (lambda (x) (not (pred x))) gen))

(define (gtake gen k . o)
  (let ((pad? (pair? o))
        (pad (and (pair? o) (car o)))
        (i 0))
    (lambda ()
      (if (>= i k)
          eof
          (let ((res (gen)))
            (set! i (+ i 1))
            (if (and pad? (eof-object? res))
                pad
                res))))))

(define (gdrop gen k)
  (define (g)
    (cond
     ((<= k 0) (gen))
     (else (gen) (set! k (- k 1)) (g))))
  g)

(define (gtake-while pred gen)
  (let ((done? #f))
    (lambda ()
      (if done?
          eof
          (let ((res (gen)))
            (cond
             ((and (not (eof-object? res)) (pred res)) res)
             (else (set! done? #t) eof)))))))

(define (gdrop-while pred gen)
  (define (g)
    (let ((res (gen)))
      (cond
       ((eof-object? res) res)
       ((pred res) (g))
       (else (set! pred (lambda (x) #f)) res))))
  g)

(define (gdelete item gen . o)
  (let ((eq (if (pair? o) (car o) equal?)))
    (define (g)
      (let ((res (gen)))
        (cond
         ((eof-object? res) res)
         ((eq res item) (g))
         (else res))))
    g))

(define (gdelete-neighbor-dups gen . o)
  (let ((eq (if (pair? o) (car o) equal?))
        (prev eof))
    (define (g)
      (let ((res (gen)))
        (cond
         ((eof-object? res)
          res)
         ((and (not (eof-object? prev)) (eq res prev))
          (g))
         (else
          (set! prev res)
          res))))
    g))

(define (gindex value-gen index-gen)
  (let ((index 0)
        (next-index -1))
    (define (g)
      (cond
       ((> index next-index)
        (let ((n (index-gen)))
          (cond
           ((eof-object? n) n)
           (else
            (if (<= n next-index)
                (error "indexes must be monotonically increasing"))
            (set! next-index n)
            (g)))))
       (else
        (let ((value (value-gen))
              (keep? (= index next-index)))
          (set! index (+ index 1))
          (cond
           ((eof-object? value) value)
           (keep? value)
           (else (g)))))))
    g))

(define (gselect value-gen truth-gen)
  (define (g)
    (let ((value (value-gen))
          (keep? (truth-gen)))
      (cond
       ((eof-object? value) value)
       ((eof-object? keep?) keep?)
       (keep? value)
       (else (g)))))
  g)

(define (generator->reverse-list gen . o)
  (let ((gen (if (pair? o) (gtake gen (car o)) gen)))
    (let lp ((res '()))
      (let ((elt (gen)))
        (if (eof-object? elt)
            res
            (lp (cons elt res)))))))

(define (generator->list gen . o)
  (reverse (apply generator->reverse-list gen o)))

(define (generator->vector gen . o)
  (list->vector (generator->list (if (pair? o) (gtake gen (car o)) gen))))

(define (generator->vector! vec at gen)
  (let ((len (vector-length vec)))
    (let lp ((i at))
      (let ((elt (if (>= i len) eof (gen))))
        (cond
         ((eof-object? elt)
          (- len at))
         (else
          (vector-set! vec i elt)
          (lp (+ i 1))))))))

(define (generator->string gen . o)
  (list->string (generator->list (if (pair? o) (gtake gen (car o)) gen))))

(define (generator-fold proc seed gen . o)
  (if (null? o)
      (let lp ((acc seed))
        (let ((elt (gen)))
          (if (eof-object? elt)
              acc
              (lp (proc elt acc)))))
      (let lp ((acc seed))
        (let ((elt (gen))
              (elts (map (lambda (g) (g)) o)))
          (if (or (eof-object? elt) (memq eof elts))
              acc
              (lp (apply proc elt (append elts (list acc)))))))))

(define (generator-for-each proc gen . o)
  (if (null? o)
      (generator-fold (lambda (elt acc) (proc elt)) #f gen)
      (let lp ()
        (let ((elt (gen))
              (elts (map (lambda (g) (g)) o)))
          (unless (or (eof-object? elt) (memq eof elts))
            (apply proc elt elts)
            (lp)))))
  (if #f #f))

(define (generator-find pred gen)
  (let lp ()
    (let ((elt (gen)))
      (cond ((eof-object? elt) #f)
            ((pred elt) elt)
            (else (lp))))))

(define (generator-count pred gen)
  (let lp ((count 0))
    (let ((elt (gen)))
      (cond ((eof-object? elt) count)
            ((pred elt) (lp (+ count 1)))
            (else (lp count))))))

(define (generator-any pred gen)
  (let lp ()
    (let ((elt (gen)))
      (cond ((eof-object? elt) #f)
            ((pred elt))
            (else (lp))))))

(define (generator-every pred gen)
  (let lp ()
    (let ((elt (gen)))
      (cond ((eof-object? elt) #t)
            ((pred elt) (lp))
            (else #f)))))

(define (generator-unfold gen unfold . args)
  (apply unfold eof-object? values (lambda (x) (gen)) (gen) args))
