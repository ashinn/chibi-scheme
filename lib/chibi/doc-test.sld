(define-library (chibi doc-test)
  (export run-tests)
  (import (scheme base) (chibi doc) (chibi test))
  (begin
    (define (run-tests)
      (test-begin "doc")
      (test '(spec (args config))
          (get-optionals-signature
           '(spec . o)
           '((let ((args (or (and (pair? o) (car o)) (command-line)))
                   (config (and (pair? o) (pair? (cdr o)) (cadr o))))
               (foo)))))
      (test '(filename (port len))
          (get-optionals-signature
           '(filename . o)
           '((let ((port (if (pair? o) (car o) (open-input-file filename)))
                   (len (if (and (pair? o) (pair? (cdr o))) (cadr o) 4096)))
               (foo)))))
      (test '(f kons knil source (index))
          (get-optionals-signature
           '(f kons knil source . o)
           '((let lp ((p (if (string? source)
                             (string->parse-stream source)
                             source))
                      (index (if (pair? o) (car o) 0))
                      (acc knil))
               (f p index fk)))))
      (test "hello" (ansi->sxml "hello"))
      (test '(span "[ " (span (@ (style . "color:red")) "FAIL") "]")
          (ansi->sxml "[ \x1B;[31mFAIL\x1B;[39m]"))
      (test '(span (u "under " (span (@ (style . "color:red")) "red") " line"))
          (ansi->sxml "\x1B;[4munder \x1B;[31mred\x1B;[39m line\x1B;[24m"))
      (test '(span "plain "
                   (u "under "
                      (span (@ (style . "color:red")) "red")
                      " line"))
          (ansi->sxml
           "plain \x1B;[4munder \x1B;[31mred\x1B;[39m line\x1B;[24m"))
      (test-end))))
