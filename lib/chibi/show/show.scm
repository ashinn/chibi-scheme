;; show.scm -- additional combinator formatters
;; Copyright (c) 2013-2017 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> A library of procedures for formatting Scheme objects to text in
;;> various ways, and for easily concatenating, composing and
;;> extending these formatters.

;;> \section{Background}
;;>
;;> There are several approaches to text formatting.  Building strings
;;> to \scheme{display} is not acceptable, since it doesn't scale to
;;> very large output.  The simplest realistic idea, and what people
;;> resort to in typical portable Scheme, is to interleave
;;> \scheme{display} and \scheme{write} and manual loops, but this is
;;> both extremely verbose and doesn't compose well.  A simple concept
;;> such as padding space can't be achieved directly without somehow
;;> capturing intermediate output.
;;>
;;> The traditional approach is to use templates - typically strings,
;;> though in theory any object could be used and indeed Emacs'
;;> mode-line format templates allow arbitrary sexps.  Templates can
;;> use either escape sequences (as in C's \cfun{printf} and
;;> \hyperlink["http://en.wikipedia.org/wiki/Format_(Common_Lisp)"]{CL's}
;;> \scheme{format}) or pattern matching (as in Visual Basic's
;;> \cfun{Format},
;;> \hyperlink["http://search.cpan.org/~dconway/Perl6-Form-0.04/Form.pm"}{Perl6's}
;;> \cfun{form}, and SQL date formats).  The primary disadvantage of
;;> templates is the relative difficulty (usually impossibility) of
;;> extending them, their opaqueness, and the unreadability that
;;> arises with complex formats.  Templates are not without their
;;> advantages, but they are already addressed by other libraries such
;;> as
;;> \hyperlink["http://srfi.schemers.org/srfi-28/srfi-28.html"]{SRFI-28}
;;> and
;;> \hyperlink["http://srfi.schemers.org/srfi-48/srfi-48.html"]{SRFI-48}.
;;>
;;> This library takes a combinator approach.  Formats are nested chains
;;> of closures, which are called to produce their output as needed.
;;> The primary goal of this library is to have, first and foremost, a
;;> maximally expressive and extensible formatting library.  The next
;;> most important goal is scalability - to be able to handle
;;> arbitrarily large output and not build intermediate results except
;;> where necessary.  The third goal is brevity and ease of use.

;;> \section{Interface}

;;> \procedure{(show out [args ...])}
;;>
;;> The primary interface.  Analogous to CL's \scheme{format}, the first
;;> argument is either an output port or a boolean, with \scheme{#t}
;;> indicating \scheme{current-output-port} and \scheme{#f} indicating a
;;> string port.  The remaining arguments are formatters, combined as with
;;> \scheme{each}, run with output to the given destination.  If \var{out}
;;> is \scheme{#f} then the accumulated output is returned, otherwise
;;> the result is unspecified.

;;> \section{Formatters}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacing

;;> Output a single newline.
(define nl (displayed "\n"))

;;> "Fresh line" - output a newline iff we're not at the start of a
;;> fresh line.
(define fl
  (fn (col) (if (zero? col) nothing nl)))

;;> Move to a given tab-stop (using spaces, not tabs).
(define (tab-to . o)
  (fn (col pad-char)
    (let* ((tab-width (if (pair? o) (car o) 8))
           (rem (modulo col tab-width)))
      (if (positive? rem)
          (displayed (make-string (- tab-width rem) pad-char))
          nothing))))

;;> Move to an explicit column.
(define (space-to where)
  (fn (col pad-char)
    (displayed (make-string (max 0 (- where col)) pad-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String transformations

(define (with-string-transformer proc . ls)
  (fn (output)
    (let ((output* (lambda (str) (fn () (output (proc str))))))
      (with ((output output*)) (each-in-list ls)))))

;;> Show each of \var{ls}, uppercasing all generated text.
(define (upcased . ls) (apply with-string-transformer string-upcase ls))

;;> Show each of \var{ls}, lowercasing all generated text.
(define (downcased . ls) (apply with-string-transformer string-downcase ls))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Padding and trimming

;;> Pad the result of \scheme{(each-in-list ls)} to at least
;;> \var{width} characters, equally applied to the left and right,
;;> with any extra odd padding applied to the right.  Uses the value
;;> of \scheme{pad-char} for padding, defaulting to \scheme{#\\space}.
(define (padded/both width . ls)
  (call-with-output
   (each-in-list ls)
   (lambda (str)
     (fn (string-width pad-char)
       (let ((diff (- width (string-width str))))
         (if (positive? diff)
             (let* ((diff/2 (quotient diff 2))
                    (left (make-string diff/2 pad-char))
                    (right (if (even? diff)
                               left
                               (make-string (+ 1 diff/2) pad-char))))
               (each left str right))
             (displayed str)))))))

;;> As \scheme{padded/both} but only applies padding on the right.
(define (padded/right width . ls)
  (fn ((col1 col))
    (each (each-in-list ls)
          (fn ((col2 col) pad-char)
            (displayed (make-string (max 0 (- width (- col2 col1)))
                                    pad-char))))))

;;> As \scheme{padded/both} but only applies padding on the left.
(define (padded/left width . ls)
  (call-with-output
   (each-in-list ls)
   (lambda (str)
     (fn (string-width pad-char)
       (let ((diff (- width (string-width str))))
         (each (make-string (max 0 diff) pad-char) str))))))

;;> An alias for \scheme{padded/left}.
(define padded padded/left)

;; General buffered trim - capture the output apply a trimmer.
(define (trimmed/buffered width producer proc)
  (call-with-output
   producer
   (lambda (str)
     (fn (string-width)
       (let* ((str-width (string-width str))
              (diff (- str-width width)))
         (displayed (if (positive? diff)
                        (proc str str-width diff)
                        str)))))))

;;> Trims the result of \scheme{(each-in-list ls)} to at most
;;> \var{width} characters, removed from the right.  If any characters
;;> are removed, then the value of \scheme{ellipsis} (default empty)
;;> is used in its place (trimming additional characters as needed to
;;> be sure the final output doesn't exceed \var{width}).
(define (trimmed/right width . ls)
  (trimmed/buffered
   width
   (each-in-list ls)
   (lambda (str str-width diff)
     (fn (ellipsis string-width col)
       (let* ((ell (if (char? ellipsis) (string ellipsis) (or ellipsis "")))
              (ell-len (string-width ell))
              (diff (- (+ str-width ell-len) width)))
         (each (if (negative? diff)
                   nothing
                   (substring str 0 (- width ell-len)))
               ell))))))

;;> As \scheme{trimmed} but removes from the left.
(define (trimmed/left width . ls)
  (trimmed/buffered
   width
   (each-in-list ls)
   (lambda (str str-width diff)
     (fn (ellipsis string-width)
       (let* ((ell (if (char? ellipsis) (string ellipsis) (or ellipsis "")))
              (ell-len (string-width ell))
              (diff (- (+ str-width ell-len) width)))
         (each ell
               (if (negative? diff)
                   nothing
                   (substring str diff))))))))

;;> An alias for \scheme{trimmed/left}.
(define trimmed trimmed/left)

;;> As \scheme{trimmed} but removes equally from both the left and the
;;> right, removing extra odd characters from the right, and inserting
;;> \scheme{ellipsis} on both sides.
(define (trimmed/both width . ls)
  (trimmed/buffered
   width
   (each-in-list ls)
   (lambda (str str-width diff)
     (fn (ellipsis string-width)
       (let* ((ell (if (char? ellipsis) (string ellipsis) (or ellipsis "")))
              (ell-len (string-width ell))
              (diff (- (+ str-width ell-len ell-len) width))
              (left (quotient diff 2))
              (right (- (string-width str) (quotient (+ diff 1) 2))))
         (if (negative? diff)
             ell
             (each ell (substring str left right) ell)))))))

;;> A \scheme{trimmed}, but truncates and terminates immediately if
;;> more than \var{width} characters are generated by \var{ls}.  Thus
;;> \var{ls} may lazily generate an infinite amount of output safely
;;> (e.g. \scheme{write-simple} on an infinite list).  The nature of
;;> this procedure means only truncating on the right is meaningful.
(define (trimmed/lazy width . ls)
  (fn ((orig-output output) string-width)
    (call-with-current-continuation
     (lambda (return)
       (let ((chars-written 0)
             (output (or orig-output output-default)))
         (define (output* str)
           (let ((len (string-width str)))
             (set! chars-written (+ chars-written len))
             (if (> chars-written width)
                 (let* ((end (max 0 (- len (- chars-written width))))
                        (s (substring str 0 end)))
                   (each (output s)
                         (with! (output orig-output))
                         (fn () (return nothing))))
                 (output str))))
         (with ((output output*))
           (each-in-list ls)))))))

;;> Fits the result of \scheme{(each-in-list ls)} to exactly
;;> \var{width} characters, padding or trimming on the right as
;;> needed.
(define (fitted/right width . ls)
  (padded/right width (trimmed/right width (each-in-list ls))))

;;> As \scheme{fitted} but pads/trims from the left.
(define (fitted/left width . ls)
  (padded/left width (trimmed/left width (each-in-list ls))))

;;> An alias for \scheme{fitted/left}.
(define fitted fitted/left)

;;> As \scheme{fitted} but pads/trims equally from both the left and
;;> the right.
(define (fitted/both width . ls)
  (padded/both width (trimmed/both width (each-in-list ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joining and interspersing

(define (joined/general elt-f last-f dot-f init-ls sep)
  (fn ()
    (let lp ((ls init-ls))
      (cond
       ((pair? ls)
        (each (if (eq? ls init-ls) nothing sep)
              ((if (and last-f (null? (cdr ls))) last-f elt-f) (car ls))
              (lp (cdr ls))))
       ((and dot-f (not (null? ls)))
        (each (if (eq? ls init-ls) nothing sep) (dot-f ls)))
       (else
        nothing)))))

;;> \procedure{(joined elt-f ls [sep])}
;;>
;;> Joins the result of applying \var{elt-f} to each element of the
;;> list \var{ls} together with \var{sep}, which defaults to the empty
;;> string.
(define (joined elt-f ls . o)
  (joined/general elt-f #f #f ls (if (pair? o) (car o) "")))

;;> As \scheme{joined} but treats the separator as a prefix, inserting
;;> before every element instead of between.
(define (joined/prefix elt-f ls . o)
  (if (null? ls)
      nothing
      (let ((sep (if (pair? o) (car o) "")))
        (each sep (joined elt-f ls sep)))))

;;> As \scheme{joined} but treats the separator as a suffix, inserting
;;> after every element instead of between.
(define (joined/suffix elt-f ls . o)
  (if (null? ls)
      nothing
      (let ((sep (if (pair? o) (car o) "")))
        (each (joined elt-f ls sep) sep))))

;;> As \scheme{joined} but applies \var{last-f}, instead of
;;> \var{elt-f}, to the last element of \var{ls}, useful for
;;> e.g. commas separating a list with "and" before the final element.
(define (joined/last elt-f last-f ls . o)
  (joined/general elt-f last-f #f ls (if (pair? o) (car o) "")))

;;> As \scheme{joined} but if \var{ls} is a dotted list applies
;;> \var{dot-f} to the dotted tail as a final element.
(define (joined/dot elt-f dot-f ls . o)
  (joined/general elt-f #f dot-f ls (if (pair? o) (car o) "")))

;;> As \scheme{joined} but counts from \var{start} to \var{end}
;;> (exclusive), formatting each integer in the range.  If \var{end}
;;> is \scheme{#f} or unspecified, produces an infinite stream of
;;> output.
(define (joined/range elt-f start . o)
  (let ((end (and (pair? o) (car o)))
        (sep (if (and (pair? o) (pair? (cdr o))) (cadr o) "")))
    (let lp ((i start))
      (if (and end (>= i end))
          nothing
          (each (if (> i start) sep nothing)
                (elt-f i)
                (fn () (lp (+ i 1))))))))
