
(import (srfi 18))

(define *tests-run* 0)
(define *tests-passed* 0)

(define-syntax test
  (syntax-rules ()
    ((test name expr expect)
     (begin
       (set! *tests-run* (+ *tests-run* 1))
       (let ((str (call-with-output-string (lambda (out) (display name out))))
             (res expr))
         (display str)
         (write-char #\space)
         (display (make-string (max 0 (- 72 (string-length str))) #\.))
         (flush-output)
         (cond
          ((equal? res expect)
           (set! *tests-passed* (+ *tests-passed* 1))
           (display " [PASS]\n"))
          (else
           (display " [FAIL]\n")
           (display "    expected ") (write expect)
           (display " but got ") (write res) (newline))))))))

(define (test-report)
  (write *tests-passed*)
  (display " out of ")
  (write *tests-run*)
  (display " passed (")
  (write (* (/ *tests-passed* *tests-run*) 100))
  (display "%)")
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run tests

(test "no threads" (begin 'ok) 'ok)
(test "unstarted thread" (let ((t (make-thread (lambda () (error "oops"))))) 'ok) 'ok)
(test "ignored thread terminates" (let ((t (make-thread (lambda () 'oops)))) (thread-start! t) 'ok) 'ok)
(test "ignored thread hangs" (let ((t (make-thread (lambda () (let lp () (lp)))))) (thread-start! t) 'ok) 'ok)
(test "joined thread terminates" (let ((t (make-thread (lambda () 'oops)))) (thread-start! t) (thread-join! t) 'ok) 'ok)
(test "joined thread hangs, timeout" (let ((t (make-thread (lambda () (let lp () (lp)))))) (thread-start! t) (thread-join! t 1 'timeout)) 'timeout)

(test "basic mutex" (let ((m (make-mutex))) (and (mutex? m) 'ok)) 'ok)
(test "mutex unlock" (let ((m (make-mutex))) (and (mutex-unlock! m) 'ok)) 'ok)
(test "mutex lock/unlock" (let ((m (make-mutex))) (and (mutex-lock! m) (mutex-unlock! m) 'ok)) 'ok)
(test "mutex lock timeout" (let* ((m (make-mutex)) (t (make-thread (lambda () (mutex-lock! m))))) (thread-start! t) (thread-yield!) (if (mutex-lock! m 1) 'fail 'timeout)) 'timeout)

;(test "basic condition-variable" () 'ok)
;(test "condition-variable signal" () 'ok)
;(test "condition-variable broadcast" () 'ok)

;(test "mailbox")

(test-report)

