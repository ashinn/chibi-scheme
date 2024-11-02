(library (rnrs base)
  (export *
          +
          -
          ...
          /
          <
          <=
          =
          =>
          >
          >=
          _
          abs
          acos
          and
          angle
          append
          apply
          asin
          assert
          assertion-violation
          atan
          begin
          boolean=?
          boolean?
          caaaar
          caaadr
          caaar
          caadar
          caaddr
          caadr
          caar
          cadaar
          cadadr
          cadar
          caddar
          cadddr
          caddr
          cadr
          call-with-current-continuation
          call-with-values
          call/cc
          car
          case
          cdaaar
          cdaadr
          cdaar
          cdadar
          cdaddr
          cdadr
          cdar
          cddaar
          cddadr
          cddar
          cdddar
          cddddr
          cdddr
          cddr
          cdr
          ceiling
          char->integer
          char<=?
          char<?
          char=?
          char>=?
          char>?
          char?
          complex?
          cond
          cons
          cos
          define
          define-syntax
          denominator
          div
          div-and-mod
          div0
          div0-and-mod0
          dynamic-wind
          else
          eq?
          equal?
          eqv?
          error
          even?
          exact
          exact-integer-sqrt
          exact?
          exp
          expt
          finite?
          floor
          for-each
          gcd
          identifier-syntax
          if
          imag-part
          inexact
          inexact?
          infinite?
          integer->char
          integer-valued?
          integer?
          lambda
          lcm
          length
          let
          let*
          let*-values
          let-syntax
          let-values
          letrec
          letrec*
          letrec-syntax
          list
          list->string
          list->vector
          list-ref
          list-tail
          list?
          log
          magnitude
          make-polar
          make-rectangular
          make-string
          make-vector
          map
          max
          min
          mod
          mod0
          nan?
          negative?
          not
          null?
          number->string
          number?
          numerator
          odd?
          or
          pair?
          positive?
          procedure?
          quasiquote
          quote
          rational-valued?
          rational?
          rationalize
          real-part
          real-valued?
          real?
          reverse
          round
          set!
          sin
          sqrt
          string
          string->list
          string->number
          string->symbol
          string-append
          string-copy
          string-for-each
          string-length
          string-ref
          string<=?
          string<?
          string=?
          string>=?
          string>?
          string?
          substring
          symbol->string
          symbol=?
          symbol?
          syntax-rules
          tan
          truncate
          unquote
          unquote-splicing
          values
          vector
          vector->list
          vector-fill!
          vector-for-each
          vector-length
          vector-map
          vector-ref
          vector-set!
          vector?
          zero?)
  (import (rename (scheme base)
                  (error r7rs:error))
          (scheme cxr)
          (scheme inexact)
          (scheme complex)
          (rnrs conditions)
          (rename (srfi 141)
                  (euclidean-quotient div)
                  (euclidean-remainder mod)
                  (euclidean/ div-and-mod)
                  (balanced-quotient div0)
                  (balanced-remainder mod0)
                  (balanced/ div0-and-mod0))
          (except (chibi ast) error)
          (chibi show))

  (define-syntax assert
    (syntax-rules ()
      ((_ expr)
         (if (not expr)
             (assertion-violation #f "assertion failed" (quote expr))))))

  (define (%error make-base who message irritants)
    (assert (or (not who) (symbol? who) (string? who)))
    (assert (string? message))
    (raise (condition (make-base)
                      (if who (make-who-condition who) (condition))
                      (make-message-condition message)
                      (make-irritants-condition irritants))))
  (define (error who message . irritants)
    (%error make-error who message irritants))
  (define (assertion-violation who message . irritants)
    (%error make-assertion-violation who message irritants))

  (define (real-valued? n) (zero? (imag-part n)))
  (define (rational-valued? n)
    (and (real-valued? n)
         (not (nan? n))
         (not (infinite? n))))
  (define (integer-valued? n)
    (and (rational-valued? n)
         (integer? (real-part n)))))
