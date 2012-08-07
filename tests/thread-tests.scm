
(cond-expand
 (modules (import (srfi 18) (only (chibi test) test-begin test test-end)))
 (else #f))

(test-begin "threads")

(test "no threads" 'ok (begin 'ok))

(test "unstarted thread" 'ok
  (let ((t (make-thread (lambda () (error "oops"))))) 'ok))

(test "ignored thread terminates" 'ok
  (let ((t (make-thread (lambda () 'oops)))) (thread-start! t) 'ok))

(test "ignored thread hangs" 'ok
  (let ((t (make-thread (lambda () (let lp () (lp))))))
    (thread-start! t)
    'ok))

(test "joined thread terminates" 'ok
  (let ((t (make-thread (lambda () 'oops))))
    (thread-start! t)
    (thread-join! t)
    'ok))

(test "joined thread hangs, timeout" 'timeout
  (let ((t (make-thread (lambda () (let lp () (lp))))))
    (thread-start! t)
    (thread-join! t 0.1 'timeout)))

(test "basic mutex" 'ok
  (let ((m (make-mutex)))
    (and (mutex? m) 'ok)))

(test "mutex unlock" 'ok
  (let ((m (make-mutex)))
    (and (mutex-unlock! m) 'ok)))

(test "mutex lock/unlock" 'ok
  (let ((m (make-mutex)))
    (and (mutex-lock! m)
         (mutex-unlock! m)
         'ok)))

(test "mutex lock/lock" 'timeout
  (let ((m (make-mutex)))
    (and (mutex-lock! m)
         (if (mutex-lock! m 0.1) 'fail 'timeout))))

(test "mutex lock timeout" 'timeout
  (let* ((m (make-mutex))
         (t (make-thread (lambda () (mutex-lock! m)))))
    (thread-start! t)
    (thread-yield!)
    (if (mutex-lock! m 0.1) 'fail 'timeout)))

(test "mutex lock/unlock/lock/lock" 'timeout
  (let* ((m (make-mutex))
         (t (make-thread (lambda () (mutex-unlock! m)))))
    (mutex-lock! m)
    (thread-start! t)
    (if (mutex-lock! m 0.1)
        (if (mutex-lock! m 0.1) 'fail-second 'timeout)
        'bad-timeout)))

;(test "basic condition-variable" () 'ok)
;(test "condition-variable signal" () 'ok)
;(test "condition-variable broadcast" () 'ok)

;(test "mailbox")

(test-end)

