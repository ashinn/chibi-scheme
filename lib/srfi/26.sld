
(define-library (srfi 26)
  (export cut cute)
  (import (chibi))
  (begin
   (define-syntax %cut
     (syntax-rules (<> <...>)
       ((%cut e? params args)
        (lambda params args))
       ((%cut e? (params ...) (args ...) <> . rest)
        (%cut e? (params ... tmp) (args ... tmp) . rest))
       ((%cut e? (params ...) (args ...) <...>)
        (%cut e? (params ... . tmp) (apply args ... tmp)))
       ((%cut e? (params ...) (args ...) <...> . rest)
        (error "cut: non-terminal <...>"))
       ((%cut #t (params ...) (args ...) x . rest)
        (let ((tmp x)) (%cut #t (params ...) (args ... tmp) . rest)))
       ((%cut #f (params ...) (args ...) x . rest)
        (%cut #t (params ...) (args ... x) . rest))))
   (define-syntax cut
     (syntax-rules () ((cut args ...) (%cut #f () () args ...))))
   (define-syntax cute
     (syntax-rules () ((cute args ...) (%cut #t () () args ...))))))
