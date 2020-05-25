
;;> The minimal base formatting combinators and show interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax fn
  (syntax-rules ()
    ((fn . x)
     (computation-fn . x))))

;; The base formatting handles outputting raw strings and a simple,
;; configurable handler for formatting objects.

;; Utility - default value of string-width.
(define (substring-length str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (- end start)))

;;> Raw output - displays str to the formatter output port and updates
;;> row and col.
(define (output-default str)
  (fn (port (r row) (c col) string-width)
    (let ((nl-index (string-find-right str #\newline)))
      (write-string str port)
      (if (string-cursor>? nl-index (string-cursor-start str))
          (with! (row (+ r (string-count str #\newline)))
                 (col (string-width str (string-cursor->index str nl-index))))
          (with! (col (+ c (string-width str))))))))

(define-computation-type make-show-env show-run
  (port (current-output-port))
  (col 0)
  (row 0)
  (width 78)
  (radix 10)
  (pad-char #\space)
  (output output-default)
  (string-width substring-length)
  (word-separator? char-whitespace?)
  (ellipsis "")
  (decimal-align #f)
  (decimal-sep #f)
  (comma-sep #f)
  (comma-rule #f)
  (sign-rule #f)
  (precision #f)
  (writer #f)
  )

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
      (show-run (sequence (with! (port out)) proc)))
     ((eq? #t out)
      (show-run (sequence (with! (port (current-output-port))) proc)))
     ((eq? #f out)
      (call-with-output-string
        (lambda (out)
          (show-run (sequence (with! (port out)) proc)))))
     (else
      (error "unknown output to show" out)))))


;;> Temporarily bind the parameters in the body \var{x}.

(define-syntax with
  (syntax-rules ()
    ((with params x ... y)
     (computation-with params (each x ... y)))))

;;> The noop formatter.  Generates no output and leaves the state
;;> unmodified.
(define nothing (fn () (with!)))

;;> Formats a displayed version of x - if a string or char, outputs the
;;> raw characters (as with `display'), if x is already a formatter
;;> defers to that, otherwise outputs a written version of x.
(define (displayed x)
  (cond
   ((procedure? x) x)
   ((string? x) (fn ((output1 output)) (output1 x)))
   ((char? x) (displayed (string x)))
   (else (written x))))

;;> Formats a written version of x, as with `write'.  The formatting
;;> can be updated with the \scheme{'writer} field.
(define (written x)
  (fn (writer) ((or writer written-default) x)))

;;> Takes a single list of formatters, combined in sequence with
;;> \scheme{each}.
(define (each-in-list args)
  (if (pair? args)
      (if (pair? (cdr args))
          (sequence (displayed (car args)) (each-in-list (cdr args)))
          (fn () (displayed (car args))))
      nothing))

;;> Combines each of the formatters in a sequence using
;;> \scheme{displayed}, so that strings and chars will be output
;;> directly and other objects will be \scheme{written}.
(define (each . args)
  (each-in-list args))

;;> Captures the output of \var{producer} and formats the result with
;;> \var{consumer}.
(define (call-with-output producer consumer)
  (let ((out (open-output-string)))
    (forked (with ((port out) (output output-default)) producer)
            (fn () (consumer (get-output-string out))))))
