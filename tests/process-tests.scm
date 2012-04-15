
(cond-expand
 (modules (import (chibi process) (only (chibi test) test-begin test test-end)))
 (else #f))

(test-begin "processes")

(test #t (process-running? (current-process-id)))
(test #t (process-running? (parent-process-id)))
(test #f (signal-set-contains? (current-signal-mask) signal/alarm))

(test #t (signal-set? (make-signal-set)))
(test #t (signal-set? (current-signal-mask)))
(test #f (signal-set? #f))
(test #f (signal-set? '(#f)))
(test #f (signal-set-contains? (make-signal-set) signal/interrupt))
(test #t (let ((sset (make-signal-set)))
           (signal-set-fill! sset)
           (signal-set-contains? sset signal/interrupt)))
(test #t (let ((sset (make-signal-set)))
           (signal-set-add! sset signal/interrupt)
           (signal-set-contains? sset signal/interrupt)))
(test #f (let ((sset (make-signal-set)))
           (signal-set-fill! sset)
           (signal-set-delete! sset signal/interrupt)
           (signal-set-contains? sset signal/interrupt)))

(test-end)
