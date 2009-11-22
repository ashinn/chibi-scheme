;;;; match.scm -- portable hygienic pattern matcher
;;
;; This code is written by Alex Shinn and placed in the
;; Public Domain.  All warranties are disclaimed.

;; This is a full superset of the popular MATCH package by Andrew
;; Wright, written in fully portable SYNTAX-RULES (R5RS only, breaks
;; in R6RS SYNTAX-RULES), and thus preserving hygiene.

;; This is a simple generative pattern matcher - each pattern is
;; expanded into the required tests, calling a failure continuation if
;; the tests fail.  This makes the logic easy to follow and extend,
;; but produces sub-optimal code in cases where you have many similar
;; clauses due to repeating the same tests.  Nonetheless a smart
;; compiler should be able to remove the redundant tests.  For
;; MATCH-LET and DESTRUCTURING-BIND type uses there is no performance
;; hit.

;; The original version was written on 2006/11/29 and described in the
;; following Usenet post:
;;   http://groups.google.com/group/comp.lang.scheme/msg/0941234de7112ffd
;; and is still available at
;;   http://synthcode.com/scheme/match-simple.scm
;; A variant of this file which uses COND-EXPAND in a few places can
;; be found at
;;   http://synthcode.com/scheme/match-cond-expand.scm
;;
;; 2008/03/20 - fixing bug where (a ...) matched non-lists
;; 2008/03/15 - removing redundant check in vector patterns
;; 2008/03/06 - you can use `...' portably now (thanks to Taylor Campbell)
;; 2007/09/04 - fixing quasiquote patterns
;; 2007/07/21 - allowing ellipse patterns in non-final list positions
;; 2007/04/10 - fixing potential hygiene issue in match-check-ellipse
;;              (thanks to Taylor Campbell)
;; 2007/04/08 - clean up, commenting
;; 2006/12/24 - bugfixes
;; 2006/12/01 - non-linear patterns, shared variables in OR, get!/set!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; force compile-time syntax errors with useful messages

(define-syntax match-syntax-error
  (syntax-rules ()
    ((_)
     (match-syntax-error "invalid match-syntax-error usage"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The basic interface.  MATCH just performs some basic syntax
;; validation, binds the match expression to a temporary variable `v',
;; and passes it on to MATCH-NEXT.  It's a constant throughout the
;; code below that the binding `v' is a direct variable reference, not
;; an expression.

(define-syntax match
  (syntax-rules ()
    ((match)
     (match-syntax-error "missing match expression"))
    ((match atom)
     (match-syntax-error "missing match clause"))
    ((match (app ...) (pat . body) ...)
     (let ((v (app ...)))
       (match-next v (app ...) (set! (app ...)) (pat . body) ...)))
    ((match #(vec ...) (pat . body) ...)
     (let ((v #(vec ...)))
       (match-next v v (set! v) (pat . body) ...)))
    ((match atom (pat . body) ...)
     (match-next atom atom (set! atom) (pat . body) ...))
    ))

;; MATCH-NEXT passes each clause to MATCH-ONE in turn with its failure
;; thunk, which is expanded by recursing MATCH-NEXT on the remaining
;; clauses.  `g' and `s' are the get! and set! expressions
;; respectively.

(define-syntax match-next
  (syntax-rules (=>)
    ;; no more clauses, the match failed
    ((match-next v g s)
     (error 'match "no matching pattern"))
    ;; named failure continuation
    ((match-next v g s (pat (=> failure) . body) . rest)
     (let ((failure (lambda () (match-next v g s . rest))))
       ;; match-one analyzes the pattern for us
       (match-one v pat g s (match-drop-ids (begin . body)) (failure) ())))
    ;; anonymous failure continuation, give it a dummy name
    ((match-next v g s (pat . body) . rest)
     (match-next v g s (pat (=> failure) . body) . rest))))

;; MATCH-ONE first checks for ellipse patterns, otherwise passes on to
;; MATCH-TWO.

(define-syntax match-one
  (syntax-rules ()
    ;; If it's a list of two values, check to see if the second one is
    ;; an ellipse and handle accordingly, otherwise go to MATCH-TWO.
    ((match-one v (p q . r) g s sk fk i)
     (match-check-ellipse
      q
      (match-extract-vars p (match-gen-ellipses v p r g s sk fk i) i ())
      (match-two v (p q . r) g s sk fk i)))
    ;; Otherwise, go directly to MATCH-TWO.
    ((match-one . x)
     (match-two . x))))

;; This is the guts of the pattern matcher.  We are passed a lot of
;; information in the form:
;;
;;   (match-two var pattern getter setter success-k fail-k (ids ...))
;;
;; usually abbreviated
;;
;;   (match-two v p g s sk fk i)
;;
;; where VAR is the symbol name of the current variable we are
;; matching, PATTERN is the current pattern, getter and setter are the
;; corresponding accessors (e.g. CAR and SET-CAR! of the pair holding
;; VAR), SUCCESS-K is the success continuation, FAIL-K is the failure
;; continuation (which is just a thunk call and is thus safe to expand
;; multiple times) and IDS are the list of identifiers bound in the
;; pattern so far.

(define-syntax match-two
  (syntax-rules (_ ___ quote quasiquote ? $ = and or not set! get!)
    ((match-two v () g s (sk ...) fk i)
     (if (null? v) (sk ... i) fk))
    ((match-two v (quote p) g s (sk ...) fk i)
     (if (equal? v 'p) (sk ... i) fk))
    ((match-two v (quasiquote p) g s sk fk i)
     (match-quasiquote v p g s sk fk i))
    ((match-two v (and) g s (sk ...) fk i) (sk ... i))
    ((match-two v (and p q ...) g s sk fk i)
     (match-one v p g s (match-one v (and q ...) g s sk fk) fk i))
    ((match-two v (or) g s sk fk i) fk)
    ((match-two v (or p) g s sk fk i)
     (match-one v p g s sk fk i))
    ((match-two v (or p ...) g s sk fk i)
     (match-extract-vars (or p ...)
                         (match-gen-or v (p ...) g s sk fk i)
                         i
                         ()))
    ((match-two v (not p) g s (sk ...) fk i)
     (match-one v p g s (match-drop-ids fk) (sk ... i) i))
    ((match-two v (get! getter) g s (sk ...) fk i)
     (let ((getter (lambda () g))) (sk ... i)))
    ((match-two v (set! setter) g (s ...) (sk ...) fk i)
     (let ((setter (lambda (x) (s ... x)))) (sk ... i)))
    ((match-two v (? pred p ...) g s sk fk i)
     (if (pred v) (match-one v (and p ...) g s sk fk i) fk))
    ((match-two v (= proc p) g s sk fk i)
     (let ((w (proc v)))
       (match-one w p g s sk fk i)))
    ((match-two v (p ___ . r) g s sk fk i)
     (match-extract-vars p (match-gen-ellipses v p r g s sk fk i) i ()))
    ((match-two v (p) g s sk fk i)
     (if (and (pair? v) (null? (cdr v)))
       (let ((w (car v)))
         (match-one w p (car v) (set-car! v) sk fk i))
       fk))
    ((match-two v (p . q) g s sk fk i)
     (if (pair? v)
       (let ((w (car v)) (x (cdr v)))
         (match-one w p (car v) (set-car! v)
                    (match-one x q (cdr v) (set-cdr! v) sk fk)
                    fk
                    i))
       fk))
    ((match-two v #(p ...) g s sk fk i)
     (match-vector v 0 () (p ...) sk fk i))
    ((match-two v _ g s (sk ...) fk i) (sk ... i))
    ;; Not a pair or vector or special literal, test to see if it's a
    ;; new symbol, in which case we just bind it, or if it's an
    ;; already bound symbol or some other literal, in which case we
    ;; compare it with EQUAL?.
    ((match-two v x g s (sk ...) fk (id ...))
     (let-syntax
         ((new-sym?
           (syntax-rules (id ...)
             ((new-sym? x sk2 fk2) sk2)
             ((new-sym? y sk2 fk2) fk2))))
       (new-sym? random-sym-to-match
                 (let ((x v)) (sk ... (id ... x)))
                 (if (equal? v x) (sk ... (id ...)) fk))))
    ))

;; QUASIQUOTE patterns

(define-syntax match-quasiquote
  (syntax-rules (unquote unquote-splicing quasiquote)
    ((_ v (unquote p) g s sk fk i)
     (match-one v p g s sk fk i))
    ((_ v ((unquote-splicing p) . rest) g s sk fk i)
     (if (pair? v)
       (match-one v
                  (p . tmp)
                  (match-quasiquote tmp rest g s sk fk)
                  fk
                  i)
       fk))
    ((_ v (quasiquote p) g s sk fk i . depth)
     (match-quasiquote v p g s sk fk i #f . depth))
    ((_ v (unquote p) g s sk fk i x . depth)
     (match-quasiquote v p g s sk fk i . depth))
    ((_ v (unquote-splicing p) g s sk fk i x . depth)
     (match-quasiquote v p g s sk fk i . depth))
    ((_ v (p . q) g s sk fk i . depth)
     (if (pair? v)
       (let ((w (car v)) (x (cdr v)))
         (match-quasiquote
          w p g s
          (match-quasiquote-step x q g s sk fk depth)
          fk i . depth))
       fk))
    ((_ v #(elt ...) g s sk fk i . depth)
     (if (vector? v)
       (let ((ls (vector->list v)))
         (match-quasiquote ls (elt ...) g s sk fk i . depth))
       fk))
    ((_ v x g s sk fk i . depth)
     (match-one v 'x g s sk fk i))))

(define-syntax match-quasiquote-step
  (syntax-rules ()
    ((match-quasiquote-step x q g s sk fk depth i)
     (match-quasiquote x q g s sk fk i . depth))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;; A CPS utility that takes two values and just expands into the
;; first.
(define-syntax match-drop-ids
  (syntax-rules ()
    ((_ expr ids ...) expr)))

;; Generating OR clauses just involves binding the success
;; continuation into a thunk which takes the identifiers common to
;; each OR clause, and trying each clause, calling the thunk as soon
;; as we succeed.

(define-syntax match-gen-or
  (syntax-rules ()
    ((_ v p g s (sk ...) fk (i ...) ((id id-ls) ...))
     (let ((sk2 (lambda (id ...) (sk ... (i ... id ...)))))
       (match-gen-or-step
        v p g s (match-drop-ids (sk2 id ...)) fk (i ...))))))

(define-syntax match-gen-or-step
  (syntax-rules ()
    ((_ v () g s sk fk i)
     ;; no OR clauses, call the failure continuation
     fk)
    ((_ v (p) g s sk fk i)
     ;; last (or only) OR clause, just expand normally
     (match-one v p g s sk fk i))
    ((_ v (p . q) g s sk fk i)
     ;; match one and try the remaining on failure
     (match-one v p g s sk (match-gen-or-step v q g s sk fk i) i))
    ))

;; We match a pattern (p ...) by matching the pattern p in a loop on
;; each element of the variable, accumulating the bound ids into lists.

;; Look at the body - it's just a named let loop, matching each
;; element in turn to the same pattern.  This illustrates the
;; simplicity of this generative-style pattern matching.  It would be
;; just as easy to implement a tree searching pattern.

(define-syntax match-gen-ellipses
  (syntax-rules ()
    ((_ v p () g s (sk ...) fk i ((id id-ls) ...))
     (match-check-identifier p
       ;; simplest case equivalent to ( . p), just bind the list
       (let ((p v))
         (if (list? p)
             (sk ... i)
             fk))
       ;; simple case, match all elements of the list
       (let loop ((ls v) (id-ls '()) ...)
         (cond
           ((null? ls)
            (let ((id (reverse id-ls)) ...) (sk ... i)))
           ((pair? ls)
            (let ((w (car ls)))
              (match-one w p (car ls) (set-car! ls)
                         (match-drop-ids (loop (cdr ls) (cons id id-ls) ...))
                         fk i)))
           (else
            fk)))))
    ((_ v p (r ...) g s (sk ...) fk i ((id id-ls) ...))
     ;; general case, trailing patterns to match
     (match-verify-no-ellipses
      (r ...)
      (let* ((tail-len (length '(r ...)))
             (ls v)
             (len (length ls)))
        (if (< len tail-len)
            fk
            (let loop ((ls ls) (n len) (id-ls '()) ...)
              (cond
                ((= n tail-len)
                 (let ((id (reverse id-ls)) ...)
                   (match-one ls (r ...) #f #f (sk ... i) fk i)))
                ((pair? ls)
                 (let ((w (car ls)))
                   (match-one w p (car ls) (set-car! ls)
                              (match-drop-ids
                               (loop (cdr ls) (- n 1) (cons id id-ls) ...))
                              fk
                              i)))
                (else
                 fk)))))))
    ))

(define-syntax match-verify-no-ellipses
  (syntax-rules ()
    ((_ (x . y) sk)
     (match-check-ellipse
      x
      (match-syntax-error
       "multiple ellipse patterns not allowed at same level")
      (match-verify-no-ellipses y sk)))
    ((_ x sk) sk)
    ))

;; Vector patterns are just more of the same, with the slight
;; exception that we pass around the current vector index being
;; matched.

(define-syntax match-vector
  (syntax-rules (___)
    ((_ v n pats (p q) sk fk i)
     (match-check-ellipse q
                          (match-vector-ellipses v n pats p sk fk i)
                          (match-vector-two v n pats (p q) sk fk i)))
    ((_ v n pats (p ___) sk fk i)
     (match-vector-ellipses v n pats p sk fk i))
    ((_ . x)
     (match-vector-two . x))))

;; Check the exact vector length, then check each element in turn.

(define-syntax match-vector-two
  (syntax-rules ()
    ((_ v n ((pat index) ...) () sk fk i)
     (if (vector? v)
       (let ((len (vector-length v)))
         (if (= len n)
           (match-vector-step v ((pat index) ...) sk fk i)
           fk))
       fk))
    ((_ v n (pats ...) (p . q) sk fk i)
     (match-vector v (+ n 1) (pats ... (p n)) q sk fk i))
    ))

(define-syntax match-vector-step
  (syntax-rules ()
    ((_ v () (sk ...) fk i) (sk ... i))
    ((_ v ((pat index) . rest) sk fk i)
     (let ((w (vector-ref v index)))
       (match-one w pat (vector-ref v index) (vector-set! v index)
                  (match-vector-step v rest sk fk)
                  fk i)))))

;; With a vector ellipse pattern we first check to see if the vector
;; length is at least the required length.

(define-syntax match-vector-ellipses
  (syntax-rules ()
    ((_ v n ((pat index) ...) p sk fk i)
     (if (vector? v)
       (let ((len (vector-length v)))
         (if (>= len n)
           (match-vector-step v ((pat index) ...)
                              (match-vector-tail v p n len sk fk)
                              fk i)
           fk))
       fk))))

(define-syntax match-vector-tail
  (syntax-rules ()
    ((_ v p n len sk fk i)
     (match-extract-vars p (match-vector-tail-two v p n len sk fk i) i ()))))

(define-syntax match-vector-tail-two
  (syntax-rules ()
    ((_ v p n len (sk ...) fk i ((id id-ls) ...))
     (let loop ((j n) (id-ls '()) ...)
       (if (>= j len)
         (let ((id (reverse id-ls)) ...) (sk ... i))
         (let ((w (vector-ref v j)))
           (match-one w p (vector-ref v j) (vetor-set! v j)
                      (match-drop-ids (loop (+ j 1) (cons id id-ls) ...))
                      fk i)))))))

;; Extract all identifiers in a pattern.  A little more complicated
;; than just looking for symbols, we need to ignore special keywords
;; and not pattern forms (such as the predicate expression in ?
;; patterns).
;;
;; (match-extract-vars pattern continuation (ids ...) (new-vars ...))

(define-syntax match-extract-vars
  (syntax-rules (_ ___ ? $ = quote quasiquote and or not get! set!)
    ((match-extract-vars (? pred . p) k i v)
     (match-extract-vars p k i v))
    ((match-extract-vars ($ rec . p) k i v)
     (match-extract-vars p k i v))
    ((match-extract-vars (= proc p) k i v)
     (match-extract-vars p k i v))
    ((match-extract-vars (quote x) (k ...) i v)
     (k ... v))
    ((match-extract-vars (quasiquote x) k i v)
     (match-extract-quasiquote-vars x k i v (#t)))
    ((match-extract-vars (and . p) k i v)
     (match-extract-vars p k i v))
    ((match-extract-vars (or . p) k i v)
     (match-extract-vars p k i v))
    ((match-extract-vars (not . p) k i v)
     (match-extract-vars p k i v))
    ;; A non-keyword pair, expand the CAR with a continuation to
    ;; expand the CDR.
    ((match-extract-vars (p q . r) k i v)
     (match-check-ellipse
      q
      (match-extract-vars (p . r) k i v)
      (match-extract-vars p (match-extract-vars-step (q . r) k i v) i ())))
    ((match-extract-vars (p . q) k i v)
     (match-extract-vars p (match-extract-vars-step q k i v) i ()))
    ((match-extract-vars #(p ...) k i v)
     (match-extract-vars (p ...) k i v))
    ((match-extract-vars _ (k ...) i v)    (k ... v))
    ((match-extract-vars ___ (k ...) i v)  (k ... v))
    ;; This is the main part, the only place where we might add a new
    ;; var if it's an unbound symbol.
    ((match-extract-vars p (k ...) (i ...) v)
     (let-syntax
         ((new-sym?
           (syntax-rules (i ...)
             ((new-sym? p sk fk) sk)
             ((new-sym? x sk fk) fk))))
       (new-sym? random-sym-to-match
                 (k ... ((p p-ls) . v))
                 (k ... v))))
    ))

;; Stepper used in the above so it can expand the CAR and CDR
;; separately.

(define-syntax match-extract-vars-step
  (syntax-rules ()
    ((_ p k i v ((v2 v2-ls) ...))
     (match-extract-vars p k (v2 ... . i) ((v2 v2-ls) ... . v)))
    ))

(define-syntax match-extract-quasiquote-vars
  (syntax-rules (quasiquote unquote unquote-splicing)
    ((match-extract-quasiquote-vars (quasiquote x) k i v d)
     (match-extract-quasiquote-vars x k i v (#t . d)))
    ((match-extract-quasiquote-vars (unquote-splicing x) k i v d)
     (match-extract-quasiquote-vars (unquote x) k i v d))
    ((match-extract-quasiquote-vars (unquote x) k i v (#t))
     (match-extract-vars x k i v))
    ((match-extract-quasiquote-vars (unquote x) k i v (#t . d))
     (match-extract-quasiquote-vars x k i v d))
    ((match-extract-quasiquote-vars (x . y) k i v (#t . d))
     (match-extract-quasiquote-vars
      x
      (match-extract-quasiquote-vars-step y k i v d) i ()))
    ((match-extract-quasiquote-vars #(x ...) k i v (#t . d))
     (match-extract-quasiquote-vars (x ...) k i v d))
    ((match-extract-quasiquote-vars x (k ...) i v (#t . d))
     (k ... v))
    ))

(define-syntax match-extract-quasiquote-vars-step
  (syntax-rules ()
    ((_ x k i v d ((v2 v2-ls) ...))
     (match-extract-quasiquote-vars x k (v2 ... . i) ((v2 v2-ls) ... . v) d))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gimme some sugar baby.

(define-syntax match-lambda
  (syntax-rules ()
    ((_ clause ...) (lambda (expr) (match expr clause ...)))))

(define-syntax match-lambda*
  (syntax-rules ()
    ((_ clause ...) (lambda expr (match expr clause ...)))))

(define-syntax match-let
  (syntax-rules ()
    ((_ (vars ...) . body)
     (match-let/helper let () () (vars ...) . body))
    ((_ loop . rest)
     (match-named-let loop () . rest))))

(define-syntax match-letrec
  (syntax-rules ()
    ((_ vars . body) (match-let/helper letrec () () vars . body))))

(define-syntax match-let/helper
  (syntax-rules ()
    ((_ let ((var expr) ...) () () . body)
     (let ((var expr) ...) . body))
    ((_ let ((var expr) ...) ((pat tmp) ...) () . body)
     (let ((var expr) ...)
       (match-let* ((pat tmp) ...)
         . body)))
    ((_ let (v ...) (p ...) (((a . b) expr) . rest) . body)
     (match-let/helper
      let (v ... (tmp expr)) (p ... ((a . b) tmp)) rest . body))
    ((_ let (v ...) (p ...) ((#(a ...) expr) . rest) . body)
     (match-let/helper
      let (v ... (tmp expr)) (p ... (#(a ...) tmp)) rest . body))
    ((_ let (v ...) (p ...) ((a expr) . rest) . body)
     (match-let/helper let (v ... (a expr)) (p ...) rest . body))
    ))

(define-syntax match-named-let
  (syntax-rules ()
    ((_ loop ((pat expr var) ...) () . body)
     (let loop ((var expr) ...)
       (match-let ((pat var) ...)
         . body)))
    ((_ loop (v ...) ((pat expr) . rest) . body)
     (match-named-let loop (v ... (pat expr tmp)) rest . body))))

(define-syntax match-let*
  (syntax-rules ()
    ((_ () . body)
     (begin . body))
    ((_ ((pat expr) . rest) . body)
     (match expr (pat (match-let* rest . body))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Otherwise COND-EXPANDed bits.

;; This *should* work, but doesn't :(
;;   (define-syntax match-check-ellipse
;;     (syntax-rules (...)
;;       ((_ ... sk fk) sk)
;;       ((_ x sk fk) fk)))

;; This is a little more complicated, and introduces a new let-syntax,
;; but should work portably in any R[56]RS Scheme.  Taylor Campbell
;; originally came up with the idea.
(define-syntax match-check-ellipse
  (syntax-rules ()
    ;; these two aren't necessary but provide fast-case failures
    ((match-check-ellipse (a . b) success-k failure-k) failure-k)
    ((match-check-ellipse #(a ...) success-k failure-k) failure-k)
    ;; matching an atom
    ((match-check-ellipse id success-k failure-k)
     (let-syntax ((ellipse? (syntax-rules ()
                              ;; iff `id' is `...' here then this will
                              ;; match a list of any length
                              ((ellipse? (foo id) sk fk) sk)
                              ((ellipse? other sk fk) fk))))
       ;; this list of three elements will only many the (foo id) list
       ;; above if `id' is `...'
       (ellipse? (a b c) success-k failure-k)))))


;; This is portable but can be more efficient with non-portable
;; extensions.  This trick was originally discovered by Oleg Kiselyov.

(define-syntax match-check-identifier
  (syntax-rules ()
    ;; fast-case failures, lists and vectors are not identifiers
    ((_ (x . y) success-k failure-k) failure-k)
    ((_ #(x ...) success-k failure-k) failure-k)
    ;; x is an atom
    ((_ x success-k failure-k)
     (let-syntax
         ((sym?
           (syntax-rules ()
             ;; if the symbol `abracadabra' matches x, then x is a
             ;; symbol
             ((sym? x sk fk) sk)
             ;; otherwise x is a non-symbol datum
             ((sym? y sk fk) fk))))
       (sym? abracadabra success-k failure-k)))
    ))
