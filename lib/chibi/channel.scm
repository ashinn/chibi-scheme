;; channel.scm -- thread-safe channel (FIFO) library
;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define-record-type Channel
  (%make-channel mutex condvar front rear)
  channel?
  (mutex channel-mutex channel-mutex-set!)
  (condvar channel-condvar channel-condvar-set!)
  (front channel-front channel-front-set!)
  (rear channel-rear channel-rear-set!))

(define (make-channel)
  (%make-channel (make-mutex) (make-condition-variable) '() '()))

(define (channel-empty? chan)
  (null? (channel-front chan)))

(define (channel-send! chan obj)
  (mutex-lock! (channel-mutex chan))
  (let ((new (list obj))
        (rear (channel-rear chan)))
    (channel-rear-set! chan new)
    (cond
     ((pair? rear)
      (set-cdr! rear new))
     (else  ; sending to empty channel
      (channel-front-set! chan new)
      (condition-variable-signal! (channel-condvar chan)))))
  (mutex-unlock! (channel-mutex chan)))

(define (channel-receive! chan)
  (mutex-lock! (channel-mutex chan))
  (let ((front (channel-front chan)))
    (cond
     ((null? front)  ; receiving from empty channel
      (mutex-unlock! (channel-mutex chan) (channel-condvar chan))
      (channel-receive! chan))
     (else
      (channel-front-set! chan (cdr front))
      (if (null? (cdr front))
          (channel-rear-set! chan '()))
      (mutex-unlock! (channel-mutex chan))
      (car front)))))
