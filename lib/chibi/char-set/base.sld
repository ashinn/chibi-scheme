
(define-library (chibi char-set base)
  (cond-expand
   (chibi
    (import (chibi))
    (begin
      (define-syntax immutable-char-set
        (sc-macro-transformer
         (lambda (expr use-env)
           (eval (cadr expr) use-env))))))
   (else
    (import (scheme base))
    (begin
      (define-syntax immutable-char-set
       (syntax-rules () ((immutable-char-set cs) cs))))))
  (import (chibi iset base))
  (export (rename Integer-Set Char-Set)
          (rename iset? char-set?)
          immutable-char-set
          char-set-contains?)
  (begin
    (define (char-set-contains? cset ch)
      (iset-contains? cset (char->integer ch)))))
