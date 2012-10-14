
(define-library (chibi char-set base)
  (import (chibi) (chibi iset base))
  (export (rename Integer-Set Char-Set)
          (rename iset? char-set?)
          immutable-char-set
          char-set-contains?)
  (begin
    (define-syntax immutable-char-set
      (sc-macro-transformer
       (lambda (expr use-env)
         (eval (cadr expr) use-env))))
    (define (char-set-contains? cset ch)
      (iset-contains? cset (char->integer ch)))))
