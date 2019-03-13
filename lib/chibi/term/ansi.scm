;; Copyright (c) 2010-2014 Alex Shinn. All rights reserved. BSD-style
;; license: http://synthcode.com/license.txt

;;> A library to use ANSI escape codes to format text and background
;;> color, font weigh, and underlining.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-simple-escape-procedure parameter)
  (let ((code (string-append "\x1B;[" (number->string parameter) "m")))
    (lambda () code)))

(define (make-wrap-procedure start-escape end-escape)
  (lambda (str)
    (if (not (string? str))
        (error "argument must be a string" str))
    (if (ansi-escapes-enabled?)
        (string-append start-escape str end-escape)
        str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some definitions are wrapped in begin in order to avoid Scribble
;; generating duplicate signatures.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Library}

(define black-escape
   (make-simple-escape-procedure 30))
(define red-escape
  (make-simple-escape-procedure 31))
(define green-escape
  (make-simple-escape-procedure 32))
(define yellow-escape
  (make-simple-escape-procedure 33))
(define blue-escape
  (make-simple-escape-procedure 34))
(define magenta-escape
  (make-simple-escape-procedure 35))
(define cyan-escape
  (make-simple-escape-procedure 36))
(define white-escape
  (make-simple-escape-procedure 37))

;;> Return a string consisting of an ANSI escape code to select the
;;> specified text color.
;;/

;;> Return a string consisting of an ANSI escape code to select the
;;> text color specified by the \var{red-level}, \var{green-level},
;;> and \var{blue-level} arguments, each of which must be an exact
;;> integer in the range [0, 5].
;;>
;;> The caller is resonsible for verifying that the terminal supports
;;> 256 colors.

(define (rgb-escape red-level green-level blue-level)
  (when (not (and (exact-integer? red-level) (<= 0 red-level 5)))
    (error "invalid red-level value" red-level))
  (when (not (and (exact-integer? green-level) (<= 0 green-level 5)))
    (error "invalid green-level value" green-level))
  (when (not (and (exact-integer? blue-level) (<= 0 blue-level 5)))
    (error "invalid blue-level value" blue-level))
  (string-append
   "\x1B;[38;5;"
   (number->string (+ (* 36 red-level) (* 6 green-level) blue-level 16))
   "m"))

;;> Return a string consisting of an ANSI escape code to select the
;;> text color specified by the \var{gray-level} argument, which must
;;> be an exact integer in the range [0, 23].
;;>
;;> The caller is resonsible for verifying that the terminal supports
;;> 256 colors.

(define (gray-escape gray-level)
  (when (not (and (exact-integer? gray-level) (<= 0 gray-level 23)))
    (error "invalid gray-level value" gray-level))
  (string-append "\x1B;[38;5;"
                 (number->string (+ gray-level 232))
                 "m"))

;;> The true-color equivalent of \scheme{rgb-escape}. Return a string
;;> consisting of an ANSI escape code to select the text color
;;> specified by the \var{red-level}, \var{green-level}, and
;;> \var{blue-level} arguments, each of which must be an exact integer
;;> in the range [0, 255].

(define (rgb24-escape red-level green-level blue-level)
  (when (not (and (exact-integer? red-level) (<= 0 red-level 255)))
    (error "invalid red-level value" red-level))
  (when (not (and (exact-integer? green-level) (<= 0 green-level 255)))
    (error "invalid green-level value" green-level))
  (when (not (and (exact-integer? blue-level) (<= 0 blue-level 255)))
    (error "invalid blue-level value" blue-level))
  (string-append
   "\x1B;[38;2;"
   (number->string red-level) ";"
   (number->string green-level) ";"
   (number->string blue-level)
   "m"))

;;> Return a string consisting of an ANSI escape code to select the
;;> default text color.

(define reset-color-escape
  (make-simple-escape-procedure 39))

(define black
  (make-wrap-procedure (black-escape)
                       (reset-color-escape)))
(define red
  (make-wrap-procedure (red-escape)
                       (reset-color-escape)))
(define green
  (make-wrap-procedure (green-escape)
                       (reset-color-escape)))
(define yellow
  (make-wrap-procedure (yellow-escape)
                       (reset-color-escape)))
(define blue
  (make-wrap-procedure (blue-escape)
                       (reset-color-escape)))
(define magenta
  (make-wrap-procedure (magenta-escape)
                       (reset-color-escape)))
(define cyan
  (make-wrap-procedure (cyan-escape)
                       (reset-color-escape)))
(define white
  (make-wrap-procedure (white-escape)
                       (reset-color-escape)))

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects specified text color
;;> and a suffix that selects the default text color.
;;>
;;> If ANSI escapes are not enabled, return \var{str}.
;;/

;;> Returns a procedure which takes a single argument, a string, and
;;> which when called behaves as follows.
;;>
;;> If ANSI escapes are enabled, the procedure returns a string
;;> consisting of its argument with a prefix that selects specified
;;> text color (obtained by calling the \scheme{rgb-escape} procedure
;;> with the values of the \var{red-level}, \var{green-level}, and
;;> \var{blue-level} arguments) and a suffix that selects the default
;;> text color.
;;>
;;> If ANSI escapes are not enabled, the procedure returns its argument.
;;>
;;> The caller is resonsible for verifying that the terminal supports
;;> 256 colors.

(define (rgb red-level green-level blue-level)
  (make-wrap-procedure (rgb-escape red-level green-level blue-level)
                       (reset-color-escape)))

;;> Returns a procedure which takes a single argument, a string, and
;;> which when called behaves as follows.
;;>
;;> If ANSI escapes are enabled, the procedure returns a string
;;> consisting of its argument with a prefix that selects specified
;;> text color (obtained by calling the \scheme{gray-escape} procedure
;;> with the values of the \var{gray-level} argument) and a suffix
;;> that selects the default text color.
;;>
;;> If ANSI escapes are not enabled, the procedure returns its argument.
;;>
;;> The caller is resonsible for verifying that the terminal supports
;;> 256 colors.

(define (gray gray-level)
  (make-wrap-procedure (gray-escape gray-level)
                       (reset-color-escape)))

;;> The true-color equivalent of \scheme{rbg}, extending the ranges
;;> to [0, 255].

(define (rgb24 red-level green-level blue-level)
  (make-wrap-procedure (rgb24-escape red-level green-level blue-level)
                       (reset-color-escape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define black-background-escape
  (make-simple-escape-procedure 40))
(define red-background-escape
  (make-simple-escape-procedure 41))
(define green-background-escape
  (make-simple-escape-procedure 42))
(define yellow-background-escape
  (make-simple-escape-procedure 43))
(define blue-background-escape
  (make-simple-escape-procedure 44))
(define magenta-background-escape
  (make-simple-escape-procedure 45))
(define cyan-background-escape
  (make-simple-escape-procedure 46))
(define white-background-escape
  (make-simple-escape-procedure 47))

;;> Return a string consisting of an ANSI escape code to select the
;;> specified background color.
;;/

;;> Return a string consisting of an ANSI escape code to select the
;;> background color specified by the \var{red-level}, \var{green-level},
;;> and \var{blue-level} arguments, each of which must be an exact
;;> integer in the range [0, 5].
;;>
;;> The caller is resonsible for verifying that the terminal supports
;;> 256 colors.

(define (rgb-background-escape red-level green-level blue-level)
  (when (not (and (exact-integer? red-level) (<= 0 red-level 5)))
    (error "invalid red-level value" red-level))
  (when (not (and (exact-integer? green-level) (<= 0 green-level 5)))
    (error "invalid green-level value" green-level))
  (when (not (and (exact-integer? blue-level) (<= 0 blue-level 5)))
    (error "invalid blue-level value" blue-level))
  (string-append
   "\x1B;[48;5;"
   (number->string (+ (* 36 red-level) (* 6 green-level) blue-level 16))
   "m"))

;;> Return a string consisting of an ANSI escape code to select the
;;> background color specified by the \var{gray-level} argument, which
;;> must be an exact integer in the range [0, 23].
;;>
;;> The caller is resonsible for verifying that the terminal supports
;;> 256 colors.

(define (gray-background-escape gray-level)
  (when (not (and (exact-integer? gray-level) (<= 0 gray-level 23)))
    (error "invalid gray-level value" gray-level))
  (string-append "\x1B;[48;5;"
                 (number->string (+ gray-level 232))
                 "m"))

;;> The true-color equivalent of \scheme{rgb-background-escape}.
;;> Return a string consisting of an ANSI escape code to select the
;;> text color specified by the \var{red-level}, \var{green-level},
;;> and \var{blue-level} arguments, each of which must be an exact
;;> integer in the range [0, 255].

(define (rgb24-background-escape red-level green-level blue-level)
  (when (not (and (exact-integer? red-level) (<= 0 red-level 255)))
    (error "invalid red-level value" red-level))
  (when (not (and (exact-integer? green-level) (<= 0 green-level 255)))
    (error "invalid green-level value" green-level))
  (when (not (and (exact-integer? blue-level) (<= 0 blue-level 255)))
    (error "invalid blue-level value" blue-level))
  (string-append
   "\x1B;[48;5;"
   (number->string red-level) ";"
   (number->string green-level) ";"
   (number->string blue-level)
   "m"))

;;> \procedure{(reset-background-color-escape)}
;;>
;;> Return a string consisting of an ANSI escape code to select the
;;> default background color.

(define reset-background-color-escape
  (make-simple-escape-procedure 49))

(define black-background
  (make-wrap-procedure (black-background-escape)
                       (reset-background-color-escape)))
(define red-background
  (make-wrap-procedure (red-background-escape)
                       (reset-background-color-escape)))
(define green-background
  (make-wrap-procedure (green-background-escape)
                       (reset-background-color-escape)))
(define yellow-background
  (make-wrap-procedure (yellow-background-escape)
                       (reset-background-color-escape)))
(define blue-background
  (make-wrap-procedure (blue-background-escape)
                       (reset-background-color-escape)))
(define magenta-background
  (make-wrap-procedure (magenta-background-escape)
                       (reset-background-color-escape)))
(define cyan-background
  (make-wrap-procedure (cyan-background-escape)
                       (reset-background-color-escape)))
(define white-background
  (make-wrap-procedure (white-background-escape)
                       (reset-background-color-escape)))

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects specified background
;;> color and a suffix that selects the default background color.
;;>
;;> If ANSI escapes are not enabled, return \var{str}.
;;/

;;> Returns a procedure which takes a single argument, a string, and
;;> which when called behaves as follows.
;;>
;;> If ANSI escapes are enabled, the procedure returns a string
;;> consisting of its argument with a prefix that selects specified
;;> background color (obtained by calling the \scheme{rgb-background-escape}
;;> procedure with the values of the \var{red-level}, \var{green-level},
;;> and \var{blue-level} arguments) and a suffix that selects the
;;> default background color.
;;>
;;> If ANSI escapes are not enabled, the procedure returns its argument.
;;>
;;> The caller is resonsible for verifying that the terminal supports
;;> 256 colors.

(define (rgb-background red-level green-level blue-level)
  (make-wrap-procedure (rgb-background-escape red-level green-level blue-level)
                       (reset-background-color-escape)))

;;> Returns a procedure which takes a single argument, a string, and
;;> which when called behaves as follows.
;;>
;;> If ANSI escapes are enabled, the procedure returns a string
;;> consisting of its argument with a prefix that selects specified
;;> background color (obtained by calling the \scheme{gray-background-escape}
;;> procedure with the values of the \var{gray-level} argument) and a
;;> suffix that selects the default background color.
;;>
;;> If ANSI escapes are not enabled, the procedure returns its argument.
;;>
;;> The caller is resonsible for verifying that the terminal supports
;;> 256 colors.

(define (gray-background gray-level)
  (make-wrap-procedure (gray-background-escape gray-level)
                       (reset-background-color-escape)))

;;> The true-color equivalent of \scheme{rbg-background}, extending
;;> the ranges to [0, 255].

(define (rgb24-background red-level green-level blue-level)
  (make-wrap-procedure
   (rgb24-background-escape red-level green-level blue-level)
   (reset-background-color-escape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> Return a string consisting of an ANSI escape code to select bold
;;> style.

(define bold-escape
  (make-simple-escape-procedure 1))

;;> Return a string consisting of an ANSI escape code to select non-bold
;;> style.

(define reset-bold-escape
  (make-simple-escape-procedure 22))

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects bold style and a suffix
;;> that selects non-bold style.
;;>
;;> If ANSI escapes are not enabled, return \var{str}.

(define bold (make-wrap-procedure (bold-escape)
                                  (reset-bold-escape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> Return a string consisting of an ANSI escape code to select
;;> underlined style.

(define underline-escape
  (make-simple-escape-procedure 4))

;;> Return a string consisting of an ANSI escape code to select
;;> non-underlined style.

(define reset-underline-escape
  (make-simple-escape-procedure 24))

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects underlined style and
;;> a suffix that selects non-underlined style.
;;>
;;> If ANSI escapes are not enabled, return \var{str}.

(define underline (make-wrap-procedure (underline-escape)
                                       (reset-underline-escape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> Return a string consisting of an ANSI escape code to select negative
;;> style (text in the background color and background in the text
;;> color).

(define negative-escape
  (make-simple-escape-procedure 7))

;;> Return a string consisting of an ANSI escape code to select positive
;;> style (text in the text color and background in the background
;;> color).

(define reset-negative-escape
  (make-simple-escape-procedure 27))

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects negative style (text
;;> in the background color and background in the text color) and a
;;> suffix that selects positive style (text in the text color and
;;> background in the background color).
;;>
;;> If ANSI escapes are not enabled, return \var{str}.

(define negative (make-wrap-procedure (negative-escape)
                                      (reset-negative-escape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> A parameter object that determines whether ANSI escapes are enabled
;;> in some of the preceding procedures. They are disabled if
;;> \scheme{(ansi-escapes-enabled?)} returns \scheme{#f}, and otherwise
;;> they are enabled.
;;>
;;> The initial value returned by \scheme{(ansi-escapes-enabled?)} is
;;> determined by the environment.
;;>
;;> If the environment variable \scheme{ANSI_ESCAPES_ENABLED} is set,
;;> its value determines the initial value returned by
;;> \scheme{(ansi-escapes-enabled?)}. If the value of
;;> \scheme{ANSI_ESCAPES_ENABLED} is \scheme{"0"}, the initial value
;;> is \scheme{#f}, otherwise the initial value is \scheme{#t}.
;;>
;;> If the environment variable \scheme{ANSI_ESCAPES_ENABLED} is not
;;> set, but the environment variable \scheme{TERM} is set, the value
;;> of the latter determines the initial value returned by
;;> \scheme{(ansi-escapes-enabled?)}. If the value of \scheme{TERM}
;;> is \scheme{"xterm"}, \scheme{"xterm-color"}, \scheme{"xterm-256color"},
;;> \scheme{"rxvt"}, \scheme{"rxvt-unicode-256color"}, \scheme{"kterm"},
;;> \scheme{"linux"}, \scheme{"screen"}, \scheme{"screen-256color"},
;;> or \scheme{"vt100"}, the initial value is \scheme{#t}, otherwise
;;> the initial value is \scheme{#f}.
;;>
;;> If neither of the environment variables \scheme{ANSI_ESCAPES_ENABLED}
;;> and \scheme{TERM} are set, the initial value returned by
;;> \scheme{(ansi-escapes-enabled?)} is \scheme{#f}.

(define ansi-escapes-enabled?
  (make-parameter
   (cond
    ((get-environment-variable "ANSI_ESCAPES_ENABLED")
     => (lambda (s) (not (equal? s "0"))))
    (else
     (member (get-environment-variable "TERM")
             '("xterm" "xterm-color" "xterm-256color" "rxvt" "kterm"
               "linux" "screen" "screen-256color" "vt100"
               "rxvt-unicode-256color"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Notes}
;;>
;;> It is important to remember that the formatting procedures apply
;;> a prefix to set a particular graphics parameter and a suffix to
;;> reset the parameter to its default value. This can lead to surprises.
;;> For example, on an ANSI terminal, one might mistakenly expect the
;;> following to display GREEN in green text and then RED in red text:
;;>
;;> \codeblock{(display (red (string-append (green "GREEN") "RED")))}
;;>
;;> However, it will actually display GREEN in green text and then RED
;;> in the default text color. This is a limitation of ANSI control
;;> codes; graphics attributes are not saved to and restored from a
;;> stack, but instead are simply set. One way to display GREEN in
;;> green text and then RED in red text is:
;;>
;;> \codeblock{(display (string-append (green "GREEN") (red "RED")))}
;;>
;;> On the other hand, text color, background color, font weight (bold
;;> or default), underline (on or off), image (positive or negative)
;;> are orthogonal. So, for example, on an ANSI terminal the following
;;> should display GREEN in green text and then RED in red text, with
;;> both in bold and GREEN underlined.
;;>
;;> \codeblock{(display (bold (string-append (underline (green "GREEN")) (red "RED"))))}
;;>
