;; Copyright (c) 2010-2014 Alex Shinn. All rights reserved. BSD-style
;; license: http://synthcode.com/license.txt

;;> A library to use ANSI escape codes to format text and background
;;> color, font weigh, and underlining.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code to bracket string str with ANSI escape codes to set the select
;; graphic rendition (SGR) parameters first to start-code and then to
;; end-code. This is a macro rather than, say, a procedure returning a
;; procedure to allow us to write the procedure definitions below such
;; that they are recognised by scribble.

(define-syntax bracket-with-sgr-parameters-body
  (syntax-rules ()
    ((bracket-with-sgr-parameters-body start-code str end-code)
     (begin
       (if (not (string? str))
         (error "argument must be a string" str))
       (if (ansi-escapes-enabled?)
         (string-append "\x1B;[" (number->string start-code) "m"
                        str
                        "\x1B;[" (number->string end-code) "m")
         str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (black str)
  (bracket-with-sgr-parameters-body 30 str 39))

(define (red str)
  (bracket-with-sgr-parameters-body 31 str 39))

(define (green str)
  (bracket-with-sgr-parameters-body 32 str 39))

(define (yellow str)
  (bracket-with-sgr-parameters-body 33 str 39))

(define (blue str)
  (bracket-with-sgr-parameters-body 34 str 39))

(define (magenta str)
  (bracket-with-sgr-parameters-body 35 str 39))

(define (cyan str)
  (bracket-with-sgr-parameters-body 36 str 39))

(define (white str)
  (bracket-with-sgr-parameters-body 37 str 39))

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects specified text color
;;> and a suffix that selects the default text color.
;;>
;;/ If ANSI escapes are not enabled, return \var{str}.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (background-black str)
  (bracket-with-sgr-parameters-body 40 str 49))

(define (background-red str)
  (bracket-with-sgr-parameters-body 41 str 49))

(define (background-green str)
  (bracket-with-sgr-parameters-body 42 str 49))

(define (background-yellow str)
  (bracket-with-sgr-parameters-body 43 str 49))

(define (background-blue str)
  (bracket-with-sgr-parameters-body 44 str 49))

(define (background-magenta str)
  (bracket-with-sgr-parameters-body 45 str 49))

(define (background-cyan str)
  (bracket-with-sgr-parameters-body 46 str 49))

(define (background-white str)
  (bracket-with-sgr-parameters-body 47 str 49))

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects specified background
;;> color and a suffix that selects the default background color.
;;>
;;/ If ANSI escapes are not enabled, return \var{str}.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects bold style
;;> and a suffix that selects non-bold style.
;;>
;;> If ANSI escapes are not enabled, return \var{str}.

(define (bold str)
  (bracket-with-sgr-parameters-body 1 str 22))

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects underlined
;;> style and a suffix that selects non-underlined style.
;;>
;;> If ANSI escapes are not enabled, return \var{str}.

(define (underline str)
  (bracket-with-sgr-parameters-body 4 str 24))

;;> If ANSI escapes are enabled, return a string consisting of the
;;> string \var{str} with a prefix that selects negative style (text in the background color and background in the text color)
;;> and a suffix that selects positive style.
;;>
;;> If ANSI escapes are not enabled, return \var{str}.

(define (negative str)
  (bracket-with-sgr-parameters-body 7 str 27))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> A parameter object that determines whether ANSI escapes are enabled
;;> in the preceding procedures. They are disabled if \scheme{(ansi-escapes-enabled?)}
;;> returns \scheme{#f}, and otherwise they are enabled.
;;>
;;> The initial value returned by \scheme{(ansi-escapes-enabled?)} is determined by the
;;> environment.
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
;;> \scheme{(ansi-escapes-enabled?)}. If the value of \scheme{TERM} is
;;> \scheme{"xterm"}, \scheme{"xterm-color"}, \scheme{"xterm-256color"},
;;> \scheme{"rxvt"}, \scheme{"kterm"}, \scheme{"linux"}, \scheme{"screen"},
;;> \scheme{"screen-256color"}, or \scheme{"vt100"}, the initial value
;;> is \scheme{#t}, otherwise the initial value is \scheme{#f}.
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
               "linux" "screen" "screen-256color" "vt100"))))))

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
