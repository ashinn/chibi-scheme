
(define-library (chibi parse)
  (export grammar grammar/unmemoized define-grammar define-grammar/unmemoized
          call-with-parse parse parse-fully parse-fold
          parse->list parse-fully->list
          file->parse-stream string->parse-stream parse-stream-substring
          parse-stream-start? parse-stream-end? parse-stream-ref
          parse-anything parse-nothing parse-epsilon
          parse-seq parse-and parse-or parse-not
          parse-repeat parse-repeat+ parse-optional
          parse-map parse-map-substring parse-ignore parse-assert
          parse-atomic parse-commit parse-memoize
          parse-char parse-not-char parse-char-pred
          parse-string parse-token parse-sre
          parse-beginning parse-end
          parse-beginning-of-line parse-end-of-line
          parse-beginning-of-line parse-end-of-line
          parse-beginning-of-word parse-end-of-word
          parse-word parse-word+
          parse-with-failure-reason)
  (import (chibi) (chibi char-set) (srfi 9))
  (include "parse/parse.scm")
  (cond-expand
   (chibi
    (begin
      (define-syntax grammar-bind
        (er-macro-transformer
         (lambda (expr rename compare)
           (let ((name (cadr expr))
                 (k (car (cddr expr)))
                 (f (cadr (cddr expr)))
                 (bindings (car (cddr (cddr expr)))))
             (if (and (identifier? name)
                      (not (assq name bindings)))
                 (let ((new-tmp (rename 'new-tmp))
                       (save-tmp (rename 'save-tmp))
                       (lambda_ (rename 'lambda))
                       (set!_ (rename 'set!))
                       (s (rename 's))
                       (i (rename 'i))
                       (sk (rename 'sk))
                       (fk (rename 'fk))
                       (r (rename 'r)))
                   (append
                    k
                    (list
                     `(,lambda_
                       (,s ,i ,sk ,fk)
                       ((,lambda_ (,save-tmp)
                                  (,f ,s ,i
                                      (,lambda_ (,r ,s ,i ,fk)
                                                (,set!_ ,new-tmp ,r)
                                                (,sk ,r ,s ,i ,fk))
                                      (,lambda_ (,s ,i ,r)
                                                (,set!_ ,new-tmp ,save-tmp)
                                                (,fk ,s ,i ,r))))
                        ,new-tmp))
                     (cons (list name new-tmp) bindings))))
                 (append k (list f bindings)))))))))
   (else
    (begin
      (define-syntax grammar-bind
        (syntax-rules ()
          ((grammar-bind name (k ...) f ((var tmp) ...))
           (let-syntax ((new-symbol?
                         (syntax-rules (var ...)
                           ((new-symbol? name sk fk) sk)
                           ((new-symbol? _ sk fk) fk))))
             ;; Bind the name only to the first instance in the pattern.
             (new-symbol?
              random-symbol-to-match
              (k ...
                 (lambda (s i sk fk)
                   (let ((save-tmp new-tmp))
                     (f s i
                        (lambda (r s i fk) (set! new-tmp r) (sk r s i fk))
                        (lambda (s i r) (set! new-tmp save-tmp) (fk s i r)))))
                 ((var tmp) ... (name new-tmp)))
              (k ... f ((var tmp) ...)))))))))))
