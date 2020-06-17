;; base.scm - base formatting monad
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> The minimal base formatting combinators and show interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> The environment monad with some pre-defined fields for combinator
;;> formatting.

(define-environment-monad Show-Env
  (sequence: sequence)
  (bind: %fn)
  (bind-fork: forked)
  (local: %with)
  (local!: with!)
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

(define-syntax fn
  (syntax-rules ()
    ((fn vars expr ... fmt)
     (%fn vars expr ... (displayed fmt)))))

;; The base formatting handles outputting raw strings and a simple,
;; configurable handler for formatting objects.

;; Utility - default value of string-width.
(define (substring-length str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (- end start)))

;; Raw output.  All primitive output should go through this operation.
;; Overridable, defaulting to output-default.
(define (output str)
  (fn (output) ((or output output-default) str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \procedure{(show out [args ...])}
;;>
;;> Run the combinators \var{args}, accumulating the output to
;;> \var{out}, which is either an output port or a boolean, with
;;> \scheme{#t} indicating \scheme{current-output-port} and
;;> \scheme{#f} to collect the output as a string.
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
  (run (sequence (with! (port out)
                        (col 0)
                        (row 0)
                        (width 78)
                        (radix 10)
                        (pad-char #\space)
                        (output output-default)
                        (string-width substring-length))
                 proc)))

;;> Temporarily bind the parameters in the body \var{x}.

(define-syntax with
  (syntax-rules ()
    ((with params x ... y)
     (%with params (each x ... y)))))

;;> The noop formatter.  Generates no output and leaves the state
;;> unmodified.
(define nothing (fn () (with!)))

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

;;> Raw output - displays str to the formatter output port and updates
;;> row and col.
(define (output-default str)
  (fn (port row col string-width)
    (display str port)
    (let ((nl-index (string-find-right str #\newline)))
      (if (string-cursor>? nl-index (string-cursor-start str))
          (with! (row (+ row (string-count str #\newline)))
                 (col (string-width str (string-cursor->index str nl-index))))
          (with! (col (+ col (string-width str))))))))

;;> Captures the output of \var{producer} and formats the result with
;;> \var{consumer}.
(define (call-with-output producer consumer)
  (let ((out (open-output-string)))
    (forked (with ((port out) (output output-default)) producer)
            (fn () (consumer (get-output-string out))))))
