;; base.scm - base formatting monad
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> A library of procedures for formatting Scheme objects to text in
;;> various ways, and for easily concatenating, composing and
;;> extending these formatters efficiently without resorting to
;;> capturing and manipulating intermediate strings.

;;> \section{Background}
;;>
;;> There are several approaches to text formatting.  Building strings to
;;> \q{display} is not acceptable, since it doesn't scale to very large
;;> output.  The simplest realistic idea, and what people resort to in
;;> typical portable Scheme, is to interleave \q{display} and \q{write}
;;> and manual loops, but this is both extremely verbose and doesn't
;;> compose well.  A simple concept such as padding space can't be
;;> achieved directly without somehow capturing intermediate output.
;;>
;;> The traditional approach is to use templates - typically strings,
;;> though in theory any object could be used and indeed Emacs' mode-line
;;> format templates allow arbitrary sexps.  Templates can use either
;;> escape sequences (as in C's \q{printf} and \urlh{#BIBITEM_2}{CL's}
;;> \q{format}) or pattern matching (as in Visual Basic's \q{Format},
;;> \urlh{#BIBITEM_6}{Perl6's} \q{form}, and SQL date formats).  The
;;> primary disadvantage of templates is the relative difficulty (usually
;;> impossibility) of extending them, their opaqueness, and the
;;> unreadability that arises with complex formats.  Templates are not
;;> without their advantages, but they are already addressed by other
;;> libraries such as \urlh{#BIBITEM_3}{SRFI-28} and
;;> \urlh{#BIBITEM_4}{SRFI-48}.
;;>
;;> This library takes a combinator approach.  Formats are nested chains
;;> of closures, which are called to produce their output as needed.
;;> The primary goal of this library is to have, first and foremost, a
;;> maximally expressive and extensible formatting library.  The next
;;> most important goal is scalability - to be able to handle
;;> arbitrarily large output and not build intermediate results except
;;> where necessary.  The third goal is brevity and ease of use.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The environment monad with some pre-defined fields for combinator
;; formatting.

(define-environment-monad Show-Env
  (sequence: sequence)
  (bind: fn)
  (bind-fork: fn-fork)
  (local: %with)
  (local!: update!)
  (return: return)
  (run: run)
  (fields:
   (port env-port env-port-set!)
   (row env-row env-row-set!)
   (col env-col env-col-set!)
   (width env-width env-width-set!)
   (radix env-radix env-radix-set!)
   (precision env-precision env-precision-set!)
   (pad-char env-pad-char env-pad-char-set!)
   (decimal-sep env-decimal-sep env-decimal-sep-set!)
   (decimal-align env-decimal-align env-decimal-align-set!)
   (string-width env-string-width env-string-width-set!)
   (ellipsis env-ellipsis env-ellipsis-set!)
   (writer env-writer env-writer-set!)
   (output env-output env-output-set!)))

(define-syntax with
  (syntax-rules ()
    ((with params x) (%with params (displayed x)))
    ((with params . x) (%with params (each . x)))))

;; The base formatting handles outputting raw strings and a simple,
;; configurable handler for formatting objects.

;; Utility - default value of string-width.
(define (substring-length str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (- end start)))

;; Raw output - displays str to the formatter output port and updates
;; row and col.
(define (output-default str)
  (fn (port row col string-width)
    (display str port)
    (let ((nl-index (string-find-right str #\newline)))
      (if (> nl-index 0)
          (update! (row (+ row (string-count str #\newline)))
                   (col (string-width str nl-index)))
          (update! (col (+ col (string-width str))))))))

;; Raw output.  All primitive output should go through this operation.
;; Overridable, defaulting to output-default.
(define (output str)
  (fn (output) ((or output output-default) str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> The primary interface.  Analogous to CL's \scheme{format}, the first
;;> argument is either an output port or a boolean, with \scheme{#t}
;;> indicating \scheme{current-output-port} and \scheme{#f} indicating a
;;> string port.  The remaining arguments are formatters, combined as with
;;> \scheme{each}, run with output to the given destination.  If \var{out}
;;> is \scheme{#f} then the accumulated output is returned, otherwise
;;> the result is unspecified.
(define (show out . args)
  (let ((proc (each-in-list args)))
    (cond
     ((output-port? out)
      (show-run out proc))
     ((eq? #t out)
      (show-run (current-output-port) proc))
     ((eq? #f out)
      (let ((out (open-output-string)))
        (show-run out proc) 
        (get-output-string out)))
     (else
      (error "unknown output to show" out)))))

;; Run with an output port with initial default values.
(define (show-run out proc)
  (run (sequence (update! (port out)
                          (col 0)
                          (row 0)
                          (width 78)
                          (radix 10)
                          (pad-char #\space)
                          (output output-default)
                          (string-width substring-length))
                 proc)))

;;> Captures the output of \var{producer} and formats the result with
;;> \var{consumer}.
(define (call-with-output producer consumer)
  (let ((out (open-output-string)))
    (fn-fork (with ((port out)) producer)
             (fn () (consumer (get-output-string out))))))

;;> The noop formatter.  Generates no output and leaves the state
;;> unmodified.
(define nothing (fn () (update!)))

;;> Formats a displayed version of x - if a string or char, outputs the
;;> raw characters (as with `display'), if x is already a formatter
;;> defers to that, otherwise outputs a written version of x.
(define (displayed x)
  (cond
   ((procedure? x) x)
   ((string? x) (output x))
   ((char? x) (output (string x)))
   (else (written x))))

;;> Formats a written version of x, as with `write'.  The formatting
;;> can be updated with the \scheme{'writer} field.
(define (written x)
  (fn (writer) ((or writer written-default) x)))

;;> Takes a single list of formatters, combined in sequence with
;;> \scheme{each}.
(define (each-in-list args)
  (if (pair? args)
      (sequence (displayed (car args)) (each-in-list (cdr args)))
      nothing))

;;> Combines each of the formatters in a sequence using
;;> \scheme{displayed}, so that strings and chars will be output
;;> directly and other objects will be \scheme{written}.
(define (each . args)
  (each-in-list args))
