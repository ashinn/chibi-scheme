(import (chibi)
        (only (scheme base) parameterize)
        (chibi test)
        (chibi term ansi))

(test-begin "term.ansi")

(test-assert (procedure? ansi-escapes-enabled?))
(test-assert
 (let ((tag (cons #t #t)))
   (eqv? tag
         (parameterize ((ansi-escapes-enabled? tag))
           (ansi-escapes-enabled?)))))

(define-syntax test-term-ansi
  (syntax-rules ()
    ((test-term-ansi p s)
     (begin
       (test-assert (procedure? p))
       (test-error (p))
       (test-error (p #f))
       (test-error (p "" #f))
       (test (p "FOO")
             "FOO"
             (parameterize ((ansi-escapes-enabled? #f)) (p "FOO")))
       (test (p "FOO")
             s
             (parameterize ((ansi-escapes-enabled? #t)) (p "FOO")))))))

(test-term-ansi black              "\x1b;[30mFOO\x1b;[39m")
(test-term-ansi red                "\x1b;[31mFOO\x1b;[39m")
(test-term-ansi yellow             "\x1b;[33mFOO\x1b;[39m")
(test-term-ansi green              "\x1b;[32mFOO\x1b;[39m")
(test-term-ansi blue               "\x1b;[34mFOO\x1b;[39m")
(test-term-ansi cyan               "\x1b;[36mFOO\x1b;[39m")
(test-term-ansi magenta            "\x1b;[35mFOO\x1b;[39m")
(test-term-ansi white              "\x1b;[37mFOO\x1b;[39m")

(test-term-ansi background-black   "\x1b;[40mFOO\x1b;[49m")
(test-term-ansi background-red     "\x1b;[41mFOO\x1b;[49m")
(test-term-ansi background-yellow  "\x1b;[43mFOO\x1b;[49m")
(test-term-ansi background-green   "\x1b;[42mFOO\x1b;[49m")
(test-term-ansi background-blue    "\x1b;[44mFOO\x1b;[49m")
(test-term-ansi background-cyan    "\x1b;[46mFOO\x1b;[49m")
(test-term-ansi background-magenta "\x1b;[45mFOO\x1b;[49m")
(test-term-ansi background-white   "\x1b;[47mFOO\x1b;[49m")

(test-term-ansi bold               "\x1b;[1mFOO\x1b;[22m")
(test-term-ansi underline          "\x1b;[4mFOO\x1b;[24m")
(test-term-ansi negative           "\x1b;[7mFOO\x1b;[27m")

(test-end)
