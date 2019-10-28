(define-library (srfi 173 test)
  (import (scheme base) (srfi 173) (chibi test))

  (export run-tests)

  (begin
    (define (run-tests)
      (test-begin "srfi-173")
      ;; test hook->list and make-hook
      (test 0 (length (hook->list (make-hook 0))))

      ;; test hook-add!
      (test 1 (let ((hook (make-hook 0))
                    (counter 0))
                (hook-add! hook (lambda () (set! counter (+ counter 1))))
                (length (hook->list hook))))

      ;; test hook-delete!
      (test 0 (let ((hook (make-hook 0))
                    (counter 0))
                (let ((increment (lambda () (set! counter (+ counter 1)))))
                  (hook-add! hook increment)
                  (hook-delete! hook increment)
                  (length (hook->list hook)))))

      ;; test hook-reset!
      (test 0 (let ((hook (make-hook 0))
                    (counter 0))
                (let ((increment (lambda () (set! counter (+ counter 1))))
                      (decrement (lambda () (set! counter (- counter 1)))))
                  (hook-add! hook increment)
                  (hook-reset! hook)
                  (length (hook->list hook)))))

      ;; test hook-run
      (test 0 (let ((hook (make-hook 0))
                    (counter 0))
                (let ((increment (lambda () (set! counter (+ counter 1))))
                      (decrement (lambda () (set! counter (- counter 1)))))
                  (hook-add! hook increment)
                  (hook-add! hook decrement)
                  (hook-run hook)
                  counter)))

      ;; test list->hook
      (test 0 (let* ((counter 0)
                     (increment (lambda () (set! counter (+ counter 1))))
                     (decrement (lambda () (set! counter (- counter 1)))))
                (let ((hook (list->hook 0 (list increment decrement))))
                  (hook-add! hook increment)
                  (hook-add! hook decrement)
                  (hook-run hook)
                  counter)))

      ;; test list->hook!
      (test 0 (let* ((counter 0)
                     (increment (lambda () (set! counter (+ counter 1))))
                     (decrement (lambda () (set! counter (- counter 1))))
                     (hook (make-hook 0)))
                (list->hook! hook (list increment decrement))
                (hook-add! hook increment)
                (hook-add! hook decrement)
                (hook-run hook)
                counter))

      (test-end))))
