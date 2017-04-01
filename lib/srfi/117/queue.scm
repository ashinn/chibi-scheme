
(define-record-type List-Queue
  (make-queue list last)
  list-queue?
  (list list-queue-list list-queue-list-set!)
  (last list-queue-last list-queue-last-set!))

(define (make-list-queue ls . o)
  (make-queue ls (if (pair? o) (car o) (if (pair? ls) (last-pair ls) '()))))

(define (list-queue . ls)
  (make-list-queue ls))

(define (list-queue-copy list-queue)
  (make-list-queue (list-copy (list-queue-list list-queue))))

(define (list-queue-unfold stop? mapper successor seed . o)
  (let ((ls (unfold stop? mapper successor seed)))
    (if (pair? o)
        (let ((queue (car o)))
          (list-queue-set-list! queue (append ls (list-queue-list queue)))
          queue)
        (make-list-queue ls))))

(define (list-queue-unfold-right stop? mapper successor seed . o)
  (let ((ls (unfold-right stop? mapper successor seed)))
    (if (pair? o)
        (let ((queue (car o)))
          (list-queue-set-list! queue (append (list-queue-list queue) ls))
          queue)
        (make-list-queue ls))))

(define (list-queue-empty? list-queue)
  (null? (list-queue-list list-queue)))

(define (list-queue-front list-queue)
  (car (list-queue-list list-queue)))

(define (list-queue-back list-queue)
  (car (list-queue-last list-queue)))

(define (list-queue-first-last list-queue)
  (values (list-queue-list list-queue) (list-queue-last list-queue)))

(define (list-queue-add-front! list-queue element)
  (list-queue-list-set! list-queue (cons element (list-queue-list list-queue)))
  (if (null? (list-queue-last list-queue))
      (list-queue-last-set! list-queue (list-queue-list list-queue))))

(define (list-queue-add-back! list-queue element)
  (let ((last (list-queue-last list-queue)))
    (cond
     ((pair? last)
      (set-cdr! last (list element))
      (list-queue-last-set! list-queue (cdr last)))
     (else
      (list-queue-list-set! list-queue (list element))
      (list-queue-last-set! list-queue (list-queue-list list-queue))))))

(define (list-queue-remove-front! list-queue)
  (let ((ls (list-queue-list list-queue)))
    (list-queue-list-set! list-queue (cdr ls))
    (if (null? (cdr ls))
        (list-queue-last-set! list-queue '()))
    (car ls)))

(define (list-queue-remove-back! list-queue)
  (let ((ls (list-queue-list list-queue)))
    (if (null? (cdr ls))
        (car (list-queue-remove-all! list-queue))
        (let lp ((head ls) (tail (cdr ls)))
          (cond
           ((null? (cdr tail))
            (set-cdr! head '())
            (car tail))
           (else
            (lp tail (cdr tail))))))))

(define (list-queue-remove-all! list-queue)
  (let ((res (list-queue-list list-queue)))
    (list-queue-list-set! list-queue '())
    (list-queue-last-set! list-queue '())
    res))

(define (list-queue-set-list! list-queue list . o)
  (list-queue-list-set! list-queue list)
  (list-queue-last-set! list-queue (if (pair? o) (car o) (last-pair list))))

(define (list-queue-concatenate list-of-queues)
  (make-list-queue (list-copy (append-map list-queue-list list-of-queues))))

(define (list-queue-append . list-of-queues)
  (list-queue-concatenate list-of-queues))

(define (list-queue-append! . list-of-queues)
  (make-list-queue (append-map list-queue-list list-of-queues)))

(define (list-queue-map proc list-queue)
  (make-list-queue (map proc (list-queue-list list-queue))))

(define (list-queue-map! proc list-queue)
  (list-queue-set-list! list-queue (map! proc (list-queue-list list-queue)))
  list-queue)

(define (list-queue-for-each proc list-queue)
  (for-each proc (list-queue-list list-queue)))
