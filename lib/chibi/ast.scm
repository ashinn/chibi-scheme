;; ast.scm -- ast utilities
;; Copyright (c) 2010-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Abstract Syntax Tree.  Interface to the types used by
;;> the compiler, and other core types less commonly
;;> needed in user code, plus related utilities.

;;> @subsubsection{Analysis and Expansion}

;;> @subsubsubsection{@scheme{(analyze x [env])}}

;;> Expands and analyzes the expression @var{x} and returns the
;;> resulting AST.

;;> @subsubsubsection{@scheme{(optimize ast)}}

;;> Runs an optimization pass on @var{ast} and returns the
;;> resulting simplified expression.

(define (ast-renames ast)
  (define i 0)
  (define renames '())
  (define (rename-symbol id)
    (set! i (+ i 1))
    (string->symbol
     (string-append (symbol->string (identifier->symbol id))
                    "." (number->string i))))
  (define (rename-lambda lam)
    (or (assq lam renames)
        (let ((res (list lam)))
          (set! renames (cons res renames))
          res)))
  (define (rename! id lam)
    (let ((cell (rename-lambda lam)))
      (set-cdr! cell (cons (cons id (rename-symbol id)) (cdr cell)))))
  (define (check-ref id lam env)
    (let ((sym (identifier->symbol id)))
      (let lp1 ((ls env))
        (cond
         ((pair? ls)
          (let lp2 ((ls2 (car ls)) (found? #f))
            (cond
             ((null? ls2)
              (if (not found?) (lp1 (cdr ls))))
             ((and (eq? id (caar ls2)) (eq? lam (cdar ls2)))
               (lp2 (cdr ls2) #t))
             ((eq? sym (identifier->symbol (caar ls2)))
              (rename! (caar ls2) (cdar ls2))
              (lp2 (cdr ls2) found?))
             (else
              (lp2 (cdr ls2) found?)))))))))
  (define (extend-env lam env)
    (cons (map (lambda (x) (cons x lam)) (flatten-dot (lambda-params lam))) env))
  (let lp ((x ast) (env '()))
    (cond
     ((lambda? x) (lp (lambda-body x) (extend-env x env)))
     ((ref? x) (check-ref (ref-name x) (cdr (ref-cell x)) env))
     ((cnd? x) (lp (cnd-test x) env) (lp (cnd-pass x) env) (lp (cnd-fail x) env))
     ((set? x) (lp (set-var x) env) (lp (set-value x) env))
     ((seq? x) (for-each (lambda (x) (lp x env)) (seq-ls x)))
     ((pair? x) (for-each (lambda (x) (lp x env)) x))))
  renames)

(define (flatten-dot x)
  (cond ((pair? x) (cons (car x) (flatten-dot (cdr x))))
        ((null? x) x)
        (else (list x))))

(define (get-rename id lam renames)
  (let ((ls (assq lam renames)))
    (if (not ls)
        (identifier->symbol id)
        (cond ((assq id (cdr ls)) => cdr) (else (identifier->symbol id))))))

(define (map* f ls)
  (cond ((pair? ls) (cons (f (car ls)) (map* f (cdr ls))))
        ((null? ls) '())
        (else (f ls))))

;;> Performs a full syntax expansion of the form @var{x} and
;;> returns the resulting s-expression.

(define (macroexpand x)
  (ast->sexp (analyze x)))

;;> Convert @var{ast} to a s-expression, renaming variables if
;;> necessary.

(define (ast->sexp ast)
  (let ((renames (ast-renames ast)))
    (let a2s ((x ast))
      (cond
       ((lambda? x)
        `(lambda ,(map* (lambda (id) (get-rename id x renames)) (lambda-params x))
           ,@(map (lambda (d) `(define ,(identifier->symbol (caar d)) #f))
                  (lambda-defs x))
           ,@(if (seq? (lambda-body x))
                 (map a2s (seq-ls (lambda-body x)))
                 (list (a2s (lambda-body x))))))
       ((cnd? x) `(if ,(a2s (cnd-test x)) ,(a2s (cnd-pass x)) ,(a2s (cnd-fail x))))
       ((set? x) `(set! ,(a2s (set-var x)) ,(a2s (set-value x))))
       ((ref? x) (get-rename (ref-name x) (cdr (ref-cell x)) renames))
       ((seq? x) `(begin ,@(map a2s (seq-ls x))))
       ((lit? x)
        (let ((v (lit-value x)))
          (if (or (pair? v) (null? v) (symbol? v)) `',v v)))
       ((pair? x) (cons (a2s (car x)) (a2s (cdr x))))
       ((opcode? x) (cond ((opcode-name x) => string->symbol) (else x)))
       (else x)))))

;;> @subsubsection{Types}

;;> All objects have an associated type, and types may have parent
;;> types.  When using
;;> @hyperlink["http://srfi.schemers.org/srfi-9/srfi-9/html"]{SRFI-9}
;;> @scheme{define-record-type}, the name is bound to a first class
;;> type object.

;;> The following core types are also available by name, and may be
;;> used in the @scheme{match} @scheme{($ ...)} syntax.

;;> @itemlist[
;;> @item{@scheme{<object>} - the parent of all types}
;;> @item{@scheme{<number>} - abstract numeric type}
;;> @item{@scheme{<bignum>} - arbitrary precision exact integers}
;;> @item{@scheme{<flonum>} - inexact real numbers}
;;> @item{@scheme{<integer>} - abstract integer type}
;;> @item{@scheme{<symbol>} - symbols}
;;> @item{@scheme{<char>} - character}
;;> @item{@scheme{<boolean>} - @scheme{#t} or @scheme{#f}}
;;> @item{@scheme{<string>} - strings of characters}
;;> @item{@scheme{<byte-vector>} - uniform vector of octets}
;;> @item{@scheme{<pair>} - a @var{car} and @var{cdr}, the basis for lists}
;;> @item{@scheme{<vector>} - vectors}
;;> @item{@scheme{<opcode>} - a primitive opcode or C function}
;;> @item{@scheme{<procedure>} - a closure}
;;> @item{@scheme{<bytecode>} - the compiled code for a closure}
;;> @item{@scheme{<env>} - an environment structure}
;;> @item{@scheme{<macro>} - a macro object, usually not first-class}
;;> @item{@scheme{<lam>} - a lambda AST type}
;;> @item{@scheme{<cnd>} - an conditional AST type (i.e. @scheme{if})}
;;> @item{@scheme{<ref>} - a reference AST type}
;;> @item{@scheme{<set>} - a mutation AST type (i.e. @scheme{set!})}
;;> @item{@scheme{<seq>} - a sequence AST type}
;;> @item{@scheme{<lit>} - a literal AST type}
;;> @item{@scheme{<sc>} - a syntactic closure}
;;> @item{@scheme{<context>} - a context object (including threads)}
;;> @item{@scheme{<exception>} - an exception object}
;;> ]

;;> The following extended type predicates may also be used to test
;;> individual objects for their type:

;;> @itemlist[
;;> @item{@scheme{environment?}}
;;> @item{@scheme{bytecode?}}
;;> @item{@scheme{macro?}}
;;> @item{@scheme{syntactic-closure?}}
;;> @item{@scheme{lambda?}}
;;> @item{@scheme{cnd?}}
;;> @item{@scheme{ref?}}
;;> @item{@scheme{set?}}
;;> @item{@scheme{seq?}}
;;> @item{@scheme{lit?}}
;;> @item{@scheme{opcode?}}
;;> @item{@scheme{type?}}
;;> @item{@scheme{context?}}
;;> @item{@scheme{exception?}}
;;> ]

;;> @subsubsubsection{@scheme{(type-of x)}}

;;> Returns the type of any object @var{x}.

;;> @subsubsubsection{@scheme{(type-name type)}}

;;> Returns the name of type @var{type}.

;;> @subsubsubsection{@scheme{(type-parent type)}}

;;> Returns the immediate parent of type @var{type},
;;> or @scheme{#f} for a type with no parent.

(define (type-parent type)
  (let ((v (type-cpl type)))
    (and (vector? v)
         (> (vector-length v) 1)
         (vector-ref v (- (vector-length v) 2)))))

;;> @subsubsubsection{@scheme{(type-cpl type)}}

;;> Returns the class precedence list of type @var{type} as a
;;> vector, or @scheme{#f} for a type with no parent.

;;> @subsubsubsection{@scheme{(type-slots type)}}

;;> Returns the slot list of type @var{type}.

;;> @subsubsection{Accessors}

;;> This section describes additional accessors on AST and other core
;;> types.

;;> @subsubsubsection{Procedures}

;;> @itemlist[
;;> @item{@scheme{(procedure-code f)} - the compiled bytecode object}
;;> @item{@scheme{(procedure-vars f)} - the variables closed over by @var{f}}
;;> @item{@scheme{(procedure-name f)} - the name of @var{f} if known, else @scheme{#f}}
;;> ]

(define (procedure-name x)
  (bytecode-name (procedure-code x)))

(define (procedure-name-set! x name)
  (bytecode-name-set! (procedure-code x) name))

;;> @subsubsubsection{Macros}

;;> @itemlist[
;;> @item{@scheme{(macro-procedure f)} - the macro procedure}
;;> @item{@scheme{(macro-env f)} - the environment the macro was defined in}
;;> @item{@scheme{(macro-source f)} - the source location the macro was defined in}
;;> ]

;;> @subsubsubsection{Bytecode Objects}

;;> @itemlist[
;;> @item{@scheme{(bytecode-name bc)} - the macro procedure}
;;> @item{@scheme{(bytecode-literals bc)} - literals the bytecode references}
;;> @item{@scheme{(bytecode-source bc)} - the source location the procedure was defined in}
;;> ]

;;> @subsubsubsection{Syntactic Closures}

;;> @itemlist[
;;> @item{@scheme{(syntactic-closure-env sc)}}
;;> @item{@scheme{(syntactic-closure-vars sc)}}
;;> @item{@scheme{(syntactic-closure-expr sc)}}
;;> ]

;;> Return the environment, free variables, and expression
;;> associated with @var{sc} respectively.

;;> @subsubsubsection{Exceptions}

;;> @itemlist[
;;> @item{@scheme{(exception-kind exn)}}
;;> @item{@scheme{(exception-message exn)}}
;;> @item{@scheme{(exception-irritants exn)}}
;;> ]

;;> Return the kind, message, and irritants
;;> associated with @var{exn} respectively.

;;> @subsubsubsection{Lambdas}

;;> @itemlist[
;;> @item{@scheme{(lambda-name lam)} - the name of the lambda, if known}
;;> @item{@scheme{(lambda-name-set! lam x)}}
;;> @item{@scheme{(lambda-params lam)} - the lambda parameter list}
;;> @item{@scheme{(lambda-params-set! lam x)}}
;;> @item{@scheme{(lambda-body lam)} - the body of the lambda}
;;> @item{@scheme{(lambda-body-set! lam x)}}
;;> @item{@scheme{(lambda-defs lam)} - internal definitions of the lambda}
;;> @item{@scheme{(lambda-defs-set! lam x)}}
;;> @item{@scheme{(lambda-locals lam)} - local variables as a list of identifiers}
;;> @item{@scheme{(lambda-locals-set! lam x)}}
;;> @item{@scheme{(lambda-flags lam)} - various flags describing the lambda}
;;> @item{@scheme{(lambda-flags-set! lam x)}}
;;> @item{@scheme{(lambda-free-vars lam)} - free variables the lambda will need to close over}
;;> @item{@scheme{(lambda-free-vars-set! lam x)}}
;;> @item{@scheme{(lambda-set-vars lam)} - variables the lambda mutates}
;;> @item{@scheme{(lambda-set-vars-set! lam x)}}
;;> @item{@scheme{(lambda-return-type lam)} - the return type of the lambda}
;;> @item{@scheme{(lambda-return-type-set! lam x)}}
;;> @item{@scheme{(lambda-param-types lam)} - the types of the input parameters}
;;> @item{@scheme{(lambda-param-types-set! lam x)}}
;;> @item{@scheme{(lambda-source lam)} - the source code of the lambda}
;;> @item{@scheme{(lambda-source-set! lam x)}}
;;> ]

;;> @subsubsubsection{Conditionals}

;;> @itemlist[
;;> @item{@scheme{(cnd-test cnd)} - the test for the conditional}
;;> @item{@scheme{(cnd-test-set! cnd x)}}
;;> @item{@scheme{(cnd-pass cnd)} - the success branch}
;;> @item{@scheme{(cnd-pass-set! cnd x)}}
;;> @item{@scheme{(cnd-fail cnd)} - the failure branch}
;;> @item{@scheme{(cnd-fail-set! cnd x)}}
;;> ]

;;> @subsubsubsection{Sequences}

;;> @itemlist[
;;> @item{@scheme{(seq-ls seq)} - the list of sequence expressions}
;;> @item{@scheme{(seq-ls-set! seq x)}}
;;> ]

;;> @subsubsubsection{References}

;;> @itemlist[
;;> @item{@scheme{(ref-name ref)} - the name of the referenced variable}
;;> @item{@scheme{(ref-name-set! ref x)}}
;;> @item{@scheme{(ref-cell ref)} - the environment cell the reference resolves to}
;;> @item{@scheme{(ref-cell-set! ref x)}}
;;> ]

;;> @subsubsubsection{Mutations}

;;> @itemlist[
;;> @item{@scheme{(set-var set)} - a reference to the mutated variable}
;;> @item{@scheme{(set-var-set! set x)}}
;;> @item{@scheme{(set-value set)} - the value to set the variable to}
;;> @item{@scheme{(set-value-set! set x)}}
;;> ]

;;> @subsubsubsection{Literals}

;;> @itemlist[
;;> @item{@scheme{(lit-value lit)} - the literal value}
;;> @item{@scheme{(lit-value-set! lit x)}}
;;> ]

;;> @subsubsubsection{Pairs}

;;> @itemlist[
;;> @item{@scheme{(pair-source x)}}
;;> @item{@scheme{(pair-source-set! x source)}}
;;> ]

;;> Set or return the source code info associated with a pair x.
;;> Source info is represented as another pair whose @var{car} is
;;> the source file name and whose @var{cdr} is the line number.

;;> @subsubsection{Miscellaneous Utilities}

;;> @subsubsubsection{@scheme{(gc)}}

;;> Force a garbage collection.

;;> @subsubsubsection{@scheme{(object-size x)}}

;;> Returns the heap space directly used by @var{x}, not
;;> counting any elements of @var{x}.

;;> @subsubsubsection{@scheme{(integer->immediate n)}}

;;> Returns the interpretation of the integer @var{n} as
;;> an immediate object, useful for debugging.

;;> @subsubsubsection{@scheme{(string-contains str pat)}}

;;> Returns the first string cursor of @var{pat} in @var{str},
;;> of @scheme{#f} if it's not found.

;;> @subsubsubsection{@scheme{(atomically expr)}}

;;> Run @var{expr} atomically, disabling yields.  Ideally should only be
;;> used for brief, deterministic expressions.  If used incorrectly (e.g.
;;> running an infinite loop) can render the system unusable.
;;> Never expose to a sandbox.

(cond-expand
 (threads
  (define-syntax atomically
    (syntax-rules ()
      ((atomically . body)
       (let* ((atomic? (%set-atomic! #t))
              (res (begin . body)))
         (%set-atomic! atomic?)
         res)))))
 (else
  (define-syntax atomically
    (syntax-rules () ((atomically . body) (begin . body))))))
