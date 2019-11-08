;; color.scm -- colored output
;; Copyright (c) 2006-2017 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (color->ansi x)
  (case x
    ((bold) "1")
    ((dark) "2")
    ((underline) "4")
    ((black) "30")
    ((red) "31")
    ((green) "32")
    ((yellow) "33")
    ((blue) "34")
    ((magenta) "35")
    ((cyan) "36")
    ((white) "37")
    (else "0")))

(define (ansi-escape color)
  (string-append (string (integer->char 27)) "[" (color->ansi color) "m"))

(define (colored new-color . args)
  (fn (color)
    (with ((color new-color))
      (each (ansi-escape new-color)
            (each-in-list args)
            (if (or (memq new-color '(bold underline))
                    (memq color '(bold underline)))
                (ansi-escape 'reset)
                nothing)
            (ansi-escape color)))))

(define (as-red . args) (colored 'red (each-in-list args)))
(define (as-blue . args) (colored 'blue (each-in-list args)))
(define (as-green . args) (colored 'green (each-in-list args)))
(define (as-cyan . args) (colored 'cyan (each-in-list args)))
(define (as-yellow . args) (colored 'yellow (each-in-list args)))
(define (as-magenta . args) (colored 'magenta (each-in-list args)))
(define (as-white . args) (colored 'white (each-in-list args)))
(define (as-black . args) (colored 'black (each-in-list args)))
(define (as-bold . args) (colored 'bold (each-in-list args)))
(define (as-underline . args) (colored 'underline (each-in-list args)))
