;;;; loop.scm - the chibi loop (aka foof-loop)
;;
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; The loop API is compatible with Taylor Campbell's foof-loop, but
;; the iterator API is different and subject to change.  All loop
;; variables may be implicitly destructured with MATCH semantics.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax let-keyword-form
  (syntax-rules ()
    ((let-keyword-form
      ((labeled-arg-macro-name
        (positional-form-name (arg-name . arg-default) ...)))
      . body)
     (letrec-syntax
         ((labeled-arg-macro-name
           (syntax-rules ()
             ((labeled-arg-macro-name . keyword-val-pairs)
              (letrec-syntax
                  ((find
                    (syntax-rules (=> arg-name ...)
                      ((find kvp k-args (arg-name . default) (=> arg-name val)
                             . others) ; found arg-name among keyword-val-pairs
                       (next kvp val . k-args)) ...
                      ((find kvp k-args key (=> arg-no-match-name val) . others)
                       (find kvp k-args key . others))
                      ;; default must be here
                      ((find kvp k-args (arg-name default))
                       (next kvp default . k-args)) ...
                      ))
                   (next               ; pack the continuation to find
                    (syntax-rules ()
                      ((next kvp val vals key . keys)
                       (find kvp ((val . vals) . keys) key . kvp))
                      ((next kvp val vals) ; processed all arg-descriptors
                       (rev-apply (val) vals))))
                   (match-positionals
                    (syntax-rules (=>)
                      ((match-positionals () res . rest)
                       (rev-apply () res))
                      ((match-positionals args (val . vals) (=> name value)
                                          . rest)
                       (next ((=> name value) . rest) val vals . args))
                      ((match-positionals args (val . vals))
                       (next () val vals . args))
                      ((match-positionals (arg1 . args) res pos-arg . rest)
                       (match-positionals args (pos-arg . res) . rest))))
                   (rev-apply
                    (syntax-rules ()
                      ((rev-apply form (x . xs))
                       (rev-apply (x . form) xs))
                      ((rev-apply form ()) form))))
                (match-positionals ((arg-name . arg-default) ...)
                                   (positional-form-name)
                                   . keyword-val-pairs)
                )))))
       . body))))

;; (define-syntax let-keyword-form
;;   (syntax-rules ()
;;     ((let-keyword-form
;;       ((labeled-arg-macro-name (positional-name (arg default) ...)))
;;       . body)
;;      (letrec-syntax
;;          ((labeled-arg-macro-name
;;            (er-macro-transformer
;;             (lambda (expr rename compare)
;;               (receive (named posns)
;;                   (partition (lambda (x) (and (list? x) (compare (car x) (rename '=>))))
;;                              (cdr expr))
;;                 (let lp ((ls '((arg default) ...)) (posns posns) (args '()))
;;                   (cond
;;                    ((null? ls)
;;                     (if (pair? posns)
;;                         (error "let-keyword-form: too many args" expr)
;;                         (cons 'positional-name (reverse args))))
;;                    ((find (lambda (x) (compare (caar ls) (cadr x))) named)
;;                     => (lambda (x)
;;                          (lp (cdr ls) posns (cons (caddr x) args))))
;;                    ((pair? posns)
;;                     (lp (cdr ls) (cdr posns) (cons (car posns) args)))
;;                    (else
;;                     (lp (cdr ls) posns (cons (cadar ls) args))))))))))
;;        . body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax loop
  (syntax-rules ()
    ;; unnamed, implicit recursion
    ((loop (vars ...) body ...)
     (%loop tmp-loop () () () () () (vars ...) body ... (tmp-loop)))
    ;; named, explicit recursion
    ((loop name (vars ...) body ...)
     (%loop name () () () () () (vars ...) body ...))))

;; Main LOOP macro. Separate the variables from the iterator and
;; parameters, then walk through each parameter expanding the
;; bindings, and build the final form.

(define-syntax %loop
  (syntax-rules (=> for with let while until)
    ;; automatic iteration
    ((_ name l v c r f ((for var1 (iterator source ...)) rest ...) . body)
     (iterator ((var1) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ((_ name l v c r f ((for var1 var2 (iterator source ...)) rest ...) . body)
     (iterator ((var1 var2) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ((_ name l v c r f ((for var1 var2 var3 (iterator source ...)) rest ...) . body)
     (iterator ((var1 var2 var3) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ((_ name l v c r f ((for var1 var2 var3 var4 (iterator source ...)) rest ...) . body)
     (iterator ((var1 var2 var3 var4) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ;; do equivalents, with optional guards
    ((_ name l (vars ...) (checks ...) r f ((with var init step guard) rest ...) . body)
     (%loop name l (vars ... (var init step)) (checks ... (guard var)) r f (rest ...) . body))
    ((_ name l (vars ...) c r f ((with var init step) rest ...) . body)
     (%loop name l (vars ... (var init step)) c r f (rest ...) . body))
    ((_ name l (vars ...) c r f ((with var init) rest ...) . body)
     (%loop name l (vars ... (var init var)) c r f (rest ...) . body))
    ;; user-specified terminators
    ((_ name l vars (checks ...) r f ((until expr) rest ...) . body)
     (%loop name l vars (checks ... expr) r f (rest ...) . body))
    ((_ name l vars (checks ...) r f ((while expr) rest ...) . body)
     (%loop name l vars (checks ... (not expr)) r f (rest ...) . body))
    ;; specify a default done?
    ((_ name l v c r f ())
     (%loop name l v c r f () (#f #f)))
    ((_ name l v c r f () () . body)
     (%loop name l v c r f () (#f #f) . body))
    ;; final expansion
    ((_ name (lets ...) ((var init step) ...) (checks ...) (refs ...) (finals ...) ()
        => result
        . body)
     (let* (lets ...)
       (letrec ((tmp (lambda (var ...)
                       (if (or checks ...)
                           (let-keyword-form ((name (tmp (var step) ...)))
                             (match-let (finals ...) result))
                           (match-let (refs ...)
                             (let-keyword-form ((name (tmp (var step) ...)))
                               (if #f #f)
                               . body))))))
         (tmp init ...))))
    ;; unspecified return value case
    ((_ name (lets ...) ((var init step) ...) (checks ...) (refs ...) (finals ...) ()
        . body)
     (%loop name (lets ...) ((var init step) ...) (checks ...) (refs ...) (finals ...) ()
            => (if #f #f) . body))
    ))

(define-syntax %loop-next
  (syntax-rules ()
    ((_ (new-lets ...) (new-vars ...) (new-checks ...) (new-refs ...) (new-finals ...)
        name (lets ...) (vars ...) (checks ...) (refs ...) (finals ...)
        . rest)
     (%loop name (lets ... new-lets ...) (vars ... new-vars ...)
                 (checks ... new-checks ...) (refs ... new-refs ...)
                 (finals ... new-finals ...)
        . rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterators

;; Each gets passed two lists, those items left of the <- and those to
;; the right, followed by a NEXT and REST continuation.

;; Should finish with
;;
;;  (next (outer-vars ...) (cursor-vars ...) (done?-tests ...)
;;        (loop-vars ...) (final-vars ...) . rest)
;;
;;  OUTER-VARS: bound once outside the loop in a LET*
;;  CURSOR-VARS: DO-style bindings of the form (name init update)
;;  DONE?-TESTS: possibly empty list of forms that terminate the loop on #t
;;  LOOP-VARS: inner variables, updated in parallel after the cursors
;;  FINAL-VARS: final variables, bound only in the => result

(define-syntax in-list                  ; called just "IN" in ITER
  (syntax-rules ()
    ((in-list ((var) source) next . rest)
     (in-list ((var cursor) source) next . rest))
    ((in-list ((var cursor) source) next . rest)
     (in-list ((var cursor succ) source) next . rest))
    ((in-list ((var cursor succ) (source)) next . rest)
     (next ()                              ; outer let bindings
           ((cursor source succ))          ; iterator, init, step
           ((not (pair? cursor)))          ; finish tests for iterator vars
           ;; step variables and values
           ((var (car cursor))
            (succ (cdr cursor)))
           ()                              ; final result bindings
           . rest))
    ((in-list ((var cursor succ) (source step)) next . rest)
     (next ()
           ((cursor source succ))
           ((not (pair? cursor)))
           ((var (car cursor))
            (succ (step cursor)))
           ()
           . rest))))

;; Iterator from Taylor R. Campbell.  If you know the number of lists
;; ahead of time it's much more efficient to iterate over each one
;; separately.
(define-syntax in-lists
  (syntax-rules ()
    ((in-lists ((elts) lol) next . rest)
     (in-lists ((elts pairs) lol) next . rest))
    ((in-lists ((elts pairs) lol) next . rest)
     (in-lists ((elts pairs succ) lol) next . rest))
    ((in-lists ((elts pairs succ) (lol)) next . rest)
     (in-lists ((elts pairs succ) (lol cdr)) next . rest))
    ((in-lists ((elts pairs succ) (lol)) next . rest)
     (in-lists ((elts pairs succ) (lol cdr)) next . rest))
    ((in-lists ((elts pairs succ) (lol step)) next . rest)
     (in-lists ((elts pairs succ) (lol step null?)) next . rest))
    ((in-lists ((elts pairs succ) (lol step done?)) next . rest)
     (next ()
           ((pairs lol succ))
           ((let lp ((ls pairs)) ; an in-lined ANY
              (and (pair? ls) (if (done? (car ls)) #t (lp (cdr ls))))))
           ((elts (map car pairs))
            (succ (map step pairs)))
           ()
           . rest))
    ))

(define-syntax define-in-indexed
  (syntax-rules ()
    ((define-in-indexed in-type in-type-reverse length ref)
     (begin
       (define-syntax in-type
         (syntax-rules ()
           ((in-type ls next . rest)
            (%in-idx >= + 0 (length tmp) ref tmp ls next . rest))))
       (define-syntax in-type-reverse
         (syntax-rules ()
           ((in-type-reverse ls next . rest)
            (%in-idx < - (- (length tmp) 1) 0 ref tmp ls next . rest))))
       ))))

(define-in-indexed in-string in-string-reverse string-length string-ref)
(define-in-indexed in-vector in-vector-reverse vector-length vector-ref)

;; helper for the above string and vector iterators
(define-syntax %in-idx
  (syntax-rules ()
    ;;   cmp inc start end ref
    ((%in-idx ge + s e r tmp-vec ((var) (vec ...)) next . rest)
     (%in-idx ge + s e r tmp-vec ((var vec-index) (vec ...)) next . rest))
    ((%in-idx ge + s e r tmp-vec ((var index) (vec)) next . rest)
     (%in-idx ge + s e r tmp-vec ((var index) (vec s e 1)) next . rest))
    ((%in-idx ge + s e r tmp-vec ((var index) (vec from)) next . rest)
     (%in-idx ge + s e r tmp-vec ((var index) (vec from e 1)) next . rest))
    ((%in-idx ge + s e r tmp-vec ((var index) (vec from to)) next . rest)
     (%in-idx ge + s e r tmp-vec ((var index) (vec from to 1)) next . rest))
    ((%in-idx ge + s e r tmp-vec ((var index) (vec from to step)) next . rest)
     (next ((tmp-vec vec) (end to))
           ((index from (+ index step)))
           ((ge index end))
           ((var (r tmp-vec index)))
           ()
       . rest))
    ))

(define-syntax in-port
  (syntax-rules ()
    ((in-port ((var) source) next . rest)
     (in-port ((var p) source) next . rest))
    ((in-port ((var p) ()) next . rest)
     (in-port ((var p) ((current-input-port))) next . rest))
    ((in-port ((var p) (port)) next . rest)
     (in-port ((var p) (port read-char)) next . rest))
    ((in-port ((var p) (port read-char)) next . rest)
     (in-port ((var p) (port read-char eof-object?)) next . rest))
    ((in-port ((var p) (port reader eof?)) next . rest)
     (next ((p port) (r reader) (e? eof?))
           ((var (r p) (r p)))
           ((e? var))
           ()
           ()
       . rest))))

(define-syntax in-file
  (syntax-rules ()
    ((in-file ((var) source) next . rest)
     (in-file ((var p) source) next . rest))
    ((in-file ((var p) (file)) next . rest)
     (in-file ((var p) (file read-char)) next . rest))
    ((in-file ((var p) (file reader)) next . rest)
     (in-file ((var p) (file reader eof-object?)) next . rest))
    ((in-file ((var p) (file reader eof?)) next . rest)
     (next ((p (open-input-file file)) (r reader) (e? eof?))
           ((var (r p) (r p)))
           ((e? var))
           ()
           ((dummy (close-input-port p)))
       . rest))))

(define-syntax up-from
  (syntax-rules (to by)
    ((up-from (() . args) next . rest)
     (up-from ((var) . args) next . rest))
    ((up-from ((var) (start (to limit) (by step))) next . rest)
     (next ((s start) (l limit) (e step))
           ((var s (+ var e)))
           ((>= var l))
           ()
           ()
           . rest))
    ((up-from ((var) (start (to limit))) next . rest)
     (next ((s start) (l limit))
           ((var s (+ var 1)))
           ((>= var l))
           ()
           ()
           . rest))
    ((up-from ((var) (start (by step))) next . rest)
     (next ((s start) (e step)) ((var s (+ var e))) () () () . rest))
    ((up-from ((var) (start)) next . rest)
     (next ((s start)) ((var s (+ var 1))) () () () . rest))
    ))

(define-syntax down-from
  (syntax-rules (to by)
    ((down-from (() . args) next . rest)
     (down-from ((var) . args) next . rest))
    ((down-from ((var) (start (to limit) (by step))) next . rest)
     (next ((s start) (l limit) (e step))
           ((var (- s e) (- var e)))
           ((< var l))
           ()
           ()
           . rest))
    ((down-from ((var) (start (to limit))) next . rest)
     (next ((s start) (l limit))
           ((var (- s 1) (- var 1)))
           ((< var l))
           ()
           ()
           . rest))
    ((down-from ((var) (start (by step))) next . rest)
     (next ((s start) (e step)) ((var (- s e) (- var e))) () () ()
           . rest))
    ((down-from ((var) (start)) next . rest)
     (next ((s start)) ((var (- s 1) (- var 1))) () () ()
           . rest))
    ))

(define-syntax accumulating
  (syntax-rules (initial if)
    ((accumulating (kons final init) ((var) . x) next . rest)
     (accumulating (kons final init) ((var cursor) . x) next . rest))
    ((accumulating (kons final init) ((var cursor) ((initial i) . x)) n . rest)
     (accumulating (kons final i) ((var cursor) x) n . rest))
    ((accumulating (kons final init) ((var cursor) (expr (if check))) n . rest)
     (n ((tmp-kons kons))
        ((cursor '() (if check (tmp-kons expr cursor) cursor)))
        ()
        ()
        ((var (final cursor)))
        . rest))
    ((accumulating (kons final init) ((var cursor) (expr)) n . rest)
     (n ((tmp-kons kons))
        ((cursor '() (tmp-kons expr cursor)))
        ()
        ()
        ((var (final cursor)))
        . rest))))

(define-syntax listing
  (syntax-rules ()
    ((listing args next . rest)
     (accumulating (cons reverse '()) args next . rest))))

(define-syntax listing-reverse
  (syntax-rules ()
    ((listing-reverse args next . rest)
     (accumulating (cons (lambda (x) x) '()) args next . rest))))

(define (append-reverse ls1 ls2)
  (append (reverse ls1) ls2))

(define-syntax appending
  (syntax-rules ()
    ((appending args next . rest)
     (accumulating (append-reverse reverse '()) args next . rest))))

(define-syntax appending-reverse
  (syntax-rules ()
    ((appending-reverse args next . rest)
     (accumulating (append-reverse (lambda (x) x) '()) args next . rest))))

(define-syntax summing
  (syntax-rules ()
    ((summing args next . rest)
     (accumulating (+ (lambda (x) x) 0) args next . rest))))

(define-syntax multiplying
  (syntax-rules ()
    ((multiplying args next . rest)
     (accumulating (* (lambda (x) x) 1) args next . rest))))
