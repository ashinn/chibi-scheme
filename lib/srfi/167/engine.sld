(define-library (srfi 167 engine)

  (export
   make-engine
   engine?
   engine-open
   engine-close
   engine-in-transaction
   engine-ref
   engine-set!
   engine-delete!
   engine-range-remove!
   engine-range
   engine-prefix-range
   engine-hook-on-transaction-begin
   engine-hook-on-transaction-commit
   engine-pack
   engine-unpack)

  (import (scheme base))

  (begin

    (define-record-type <engine>
      (make-engine open
                   close
                   in-transaction
                   ref
                   set
                   delete
                   range-remove
                   range
                   prefix-range
                   hook-on-transaction-begin
                   hook-on-transaction-commit
                   pack
                   unpack)
      engine?
      (open %engine-open)
      (close %engine-close)
      (in-transaction %engine-in-transaction)
      (ref %engine-ref)
      (set %engine-set)
      (delete %engine-delete)
      (range-remove %engine-range-remove)
      (range %engine-range)
      (prefix-range %engine-prefix-range)
      (hook-on-transaction-begin %engine-hook-on-transaction-begin)
      (hook-on-transaction-commit %engine-hook-on-transaction-commit)
      (pack %engine-pack)
      (unpack %engine-unpack))

    (define (make-invoker accessor)
      (lambda (engine . args)
        (apply (accessor engine) args)))

    (define engine-open (make-invoker %engine-open))
    (define engine-close (make-invoker %engine-close))
    (define engine-in-transaction (make-invoker %engine-in-transaction))
    (define engine-ref (make-invoker %engine-ref))
    (define engine-set! (make-invoker %engine-set))
    (define engine-delete! (make-invoker %engine-delete))
    (define engine-range-remove! (make-invoker %engine-range-remove))
    (define engine-range (make-invoker %engine-range))
    (define engine-prefix-range (make-invoker %engine-prefix-range))
    (define engine-hook-on-transaction-begin (make-invoker %engine-hook-on-transaction-begin))
    (define engine-hook-on-transaction-commit (make-invoker %engine-hook-on-transaction-commit))
    (define engine-pack (make-invoker %engine-pack))
    (define engine-unpack (make-invoker %engine-unpack))))
