
;;> A nice assert macro.
;;>
;;> Assert macros are common in Scheme, in particular being helpful
;;> for domain checks at the beginning of a procedure to catch errors
;;> as early as possible.  Compared to statically typed languages this
;;> has the advantages that the assertions are optional, and that they
;;> are not limited by the type system.  SRFI 145 provides the related
;;> notion of assumptions, but the motivation there is to provide
;;> hints to optimizing compilers, and these are not required to
;;> actually signal an error.
;;>
;;> \macro{(assert expr [msg ...])}
;;>
;;> Equivalent to SRFI 145 \code{assume} except that an error is
;;> guaranteed to be raised if \var{expr} is false.  Conceptually
;;> shorthand for
;;>
;;> \code{(or \var{expr}
;;>     (error "assertion failed" \var{msg} ...))}
;;>
;;> that is, evaluates \var{expr} and returns it if true, but raises
;;> an exception otherwise.  The error is augmented to include the
;;> text of the failed \var{expr}.  If no additional \var{msg}
;;> arguments are provided then \var{expr} is scanned for free
;;> variables in non-operator positions to report values from, e.g. in
;;>
;;> \code{(let ((x 3))
;;>  (assert (= x (+ x 1))))}
;;>
;;> the error would also report the bound value of \code{x}.  This
;;> uses the technique from Oleg Kiselyov's \hyperlink[http://okmij.org/ftp/Scheme/assert-syntax-rule.txt]{good assert macro},
;;> which is convenient but fallible.  It is thus best to keep the
;;> body of the assertion simple, moving any predicates you need to
;;> external utilities, or provide an explicit \var{msg}.

(define-library (chibi assert)
  (export assert)
  (cond-expand
   (chibi
    (import (chibi))
    (begin
      (define-syntax syntax-identifier?
        (er-macro-transformer
         (lambda (expr rename compare)
           (if (identifier? (cadr expr))
               (car (cddr expr))
               (cadr (cddr expr))))))
      (define-syntax syntax-id-memq?
        (er-macro-transformer
         (lambda (expr rename compare)
           (let ((expr (cdr expr)))
             (if (any (lambda (x) (compare x (car expr))) (cadr expr))
                 (car (cddr expr))
                 (cadr (cddr expr)))))))))
   (else
    (import (scheme base))
    (begin
      ;; from match.scm
      (define-syntax syntax-identifier?
        (syntax-rules ()
          ((_ (x . y) success-k failure-k) failure-k)
          ((_ #(x ...) success-k failure-k) failure-k)
          ((_ x success-k failure-k)
           (let-syntax
               ((sym?
                 (syntax-rules ()
                   ((sym? x sk fk) sk)
                   ((sym? y sk fk) fk))))
             (sym? abracadabra success-k failure-k)))))
      (define-syntax syntax-id-memq?
        (syntax-rules ()
          ((syntax-memq? id (ids ...) sk fk)
           (let-syntax
               ((memq?
                 (syntax-rules (ids ...)
                   ((memq? id sk2 fk2) fk2)
                   ((memq? any-other sk2 fk2) sk2))))
             (memq? random-symbol-to-match sk fk))))))))
  (begin
    (define-syntax extract-vars
      (syntax-rules ()
        ((report-vars (op arg0 arg1 ...) (next ...) res)
         (syntax-id-memq? op (quote quasiquote lambda let let* letrec letrec*
                              let-syntax letrec-syntax let-values let*-values
                              receive match case define define-syntax do)
                          (next ... res)
                          (extract-vars arg0
                                        (extract-vars (op arg1 ...) (next ...))
                                        res)))
        ((report-vars (op . x) (next ...) res)
         (next ... res))
        ((report-vars x (next ...) (res ...))
         (syntax-identifier? x
                             (syntax-id-memq? x (res ...)
                                              (next ... (res ...))
                                              (next ... (res ... x)))
                             (next ... (res ...))))))
    (define-syntax qq-vars
      (syntax-rules ()
        ((qq-vars (next ...) (var ...))
         (next ... `(var ,var) ...))))
    (define-syntax report-final
      (syntax-rules ()
        ((report-final expr msg ...)
         (error "assertion failed" 'expr msg ...))))
    (define-syntax assert
      (syntax-rules ()
        ((assert test)
         (or test
             (extract-vars test (qq-vars (report-final test)) ())))
        ((assert test msg ...)
         (or test
             (report-final test msg ...)))
        ((assert) #t)))))
