
(cond-expand
 (modules (import (srfi 18) (only (chibi test) test-begin test test-end)))
 (else #f))

(test-begin "threads")

(test "no threads" (begin 'ok) 'ok)
(test "unstarted thread" (let ((t (make-thread (lambda () (error "oops"))))) 'ok) 'ok)
(test "ignored thread terminates" (let ((t (make-thread (lambda () 'oops)))) (thread-start! t) 'ok) 'ok)
(test "ignored thread hangs" (let ((t (make-thread (lambda () (let lp () (lp)))))) (thread-start! t) 'ok) 'ok)
(test "joined thread terminates" (let ((t (make-thread (lambda () 'oops)))) (thread-start! t) (thread-join! t) 'ok) 'ok)
(test "joined thread hangs, timeout" (let ((t (make-thread (lambda () (let lp () (lp)))))) (thread-start! t) (thread-join! t 0.1 'timeout)) 'timeout)

(test "basic mutex" (let ((m (make-mutex))) (and (mutex? m) 'ok)) 'ok)
(test "mutex unlock" (let ((m (make-mutex))) (and (mutex-unlock! m) 'ok)) 'ok)
(test "mutex lock/unlock" (let ((m (make-mutex))) (and (mutex-lock! m) (mutex-unlock! m) 'ok)) 'ok)
(test "mutex lock timeout" (let* ((m (make-mutex)) (t (make-thread (lambda () (mutex-lock! m))))) (thread-start! t) (thread-yield!) (if (mutex-lock! m 0.1) 'fail 'timeout)) 'timeout)

;(test "basic condition-variable" () 'ok)
;(test "condition-variable signal" () 'ok)
;(test "condition-variable broadcast" () 'ok)

;(test "mailbox")

(test-end)

