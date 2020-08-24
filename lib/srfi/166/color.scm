;; color.scm -- colored output
;; Copyright (c) 2006-2020 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (color->ansi x)
  (case x
    ((bold) "1")
    ((dark) "2")
    ((italic) "3")
    ((underline) "4")

    ((bold-off) "22")
    ((italic-off) "23")
    ((underline-off) "24")

    ((black) "30")
    ((red) "31")
    ((green) "32")
    ((yellow) "33")
    ((blue) "34")
    ((magenta) "35")
    ((cyan) "36")
    ((white) "37")
    ((default-fg) "39")

    ((on-black) "40")
    ((on-red) "41")
    ((on-green) "42")
    ((on-yellow) "43")
    ((on-blue) "44")
    ((on-magenta) "45")
    ((on-cyan) "46")
    ((on-white) "47")
    ((default-bg) "49")))

(define (ansi-escape color)
  (if (string? color)
      color
      (string-append "\x1B;[" (color->ansi color) "m")))

(define color (make-state-variable 'color 'default-fg #f))
(define background (make-state-variable 'background 'default-bg #f))
(define bold (make-state-variable 'bold 'bold-off #f))
(define italic (make-state-variable 'bold 'italic-off #f))
(define underline (make-state-variable 'bold 'underline-off #f))

(define (with-attr var new-attr . args)
  (fn ((orig-attr var))
      (with ((var new-attr))
            (each (ansi-escape new-attr)
                  (each-in-list args)
                  (ansi-escape orig-attr)))))

(define (as-bold . args) (with-attr bold 'bold (each-in-list args)))
(define (as-italic . args) (with-attr italic 'italic (each-in-list args)))
(define (as-underline . args) (with-attr underline 'underline (each-in-list args)))

(define (as-red . args) (with-attr color 'red (each-in-list args)))
(define (as-blue . args) (with-attr color 'blue (each-in-list args)))
(define (as-green . args) (with-attr color 'green (each-in-list args)))
(define (as-cyan . args) (with-attr color 'cyan (each-in-list args)))
(define (as-yellow . args) (with-attr color 'yellow (each-in-list args)))
(define (as-magenta . args) (with-attr color 'magenta (each-in-list args)))
(define (as-white . args) (with-attr color 'white (each-in-list args)))
(define (as-black . args) (with-attr color 'black (each-in-list args)))

(define (on-red . args) (with-attr background 'on-red (each-in-list args)))
(define (on-blue . args) (with-attr background 'on-blue (each-in-list args)))
(define (on-green . args) (with-attr background 'on-green (each-in-list args)))
(define (on-cyan . args) (with-attr background 'on-cyan (each-in-list args)))
(define (on-yellow . args) (with-attr background 'on-yellow (each-in-list args)))
(define (on-magenta . args) (with-attr background 'on-magenta (each-in-list args)))
(define (on-white . args) (with-attr background 'on-white (each-in-list args)))
(define (on-black . args) (with-attr background 'on-black (each-in-list args)))

(define (rgb-escape red-level green-level blue-level bg?)
  (when (not (and (exact-integer? red-level) (<= 0 red-level 5)))
    (error "invalid red-level value" red-level))
  (when (not (and (exact-integer? green-level) (<= 0 green-level 5)))
    (error "invalid green-level value" green-level))
  (when (not (and (exact-integer? blue-level) (<= 0 blue-level 5)))
    (error "invalid blue-level value" blue-level))
  (string-append
   (if bg? "\x1B;[48;5;" "\x1B;[38;5;")
   (number->string (+ (* 36 red-level) (* 6 green-level) blue-level 16))
   "m"))

(define (rgb24-escape red-level green-level blue-level bg?)
  (when (not (and (exact-integer? red-level) (<= 0 red-level 255)))
    (error "invalid red-level value" red-level))
  (when (not (and (exact-integer? green-level) (<= 0 green-level 255)))
    (error "invalid green-level value" green-level))
  (when (not (and (exact-integer? blue-level) (<= 0 blue-level 255)))
    (error "invalid blue-level value" blue-level))
  (string-append
   (if bg? "\x1B;[48;2;" "\x1B;[38;2;")
   (number->string red-level) ";"
   (number->string green-level) ";"
   (number->string blue-level)
   "m"))

(define (as-color red green blue . fmt)
  (with-attr color (rgb-escape red green blue #f) (each-in-list fmt)))

(define (as-true-color red green blue . fmt)
  (with-attr color (rgb24-escape red green blue #f) (each-in-list fmt)))

(define (on-color red green blue . fmt)
  (with-attr background (rgb-escape red green blue #t) (each-in-list fmt)))

(define (on-true-color red green blue . fmt)
  (with-attr background (rgb24-escape red green blue #t) (each-in-list fmt)))
