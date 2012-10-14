
(define-library (chibi process)
  (export exit sleep alarm fork kill execute waitpid system
          process-command-line  process-running?
          set-signal-action! make-signal-set
          signal-set? signal-set-contains?
          signal-set-fill! signal-set-add! signal-set-delete!
          current-signal-mask current-process-id parent-process-id
          signal-mask-block! signal-mask-unblock! signal-mask-set!
          signal/hang-up    signal/interrupt   signal/quit
          signal/illegal    signal/abort       signal/fpe
          signal/kill       signal/segv        signal/pipe
          signal/alarm      signal/term        signal/user1
          signal/user2      signal/child       signal/continue
          signal/stop       signal/tty-stop    signal/tty-input
          signal/tty-output wait/no-hang
          call-with-process-io
          process->string process->string-list process->output+error)
  (import (chibi) (chibi io) (chibi string) (chibi filesystem))
  (cond-expand (threads (import (srfi 18))) (else #f))
  (include-shared "process")
  (include "process.scm"))
