;; unicode.scm -- Unicode character width and ANSI escape support
;; Copyright (c) 2006-2020 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (unicode-char-width ch ambiguous-is-wide?)
  (let ((ci (char->integer ch)))
    (cond
     ((char-set:zero-width? ci)
      0)
     ((char-set:full-width? ci)
      2)
     ((and ambiguous-is-wide? (char-set:ambiguous-width? ci))
      2)
     (else
      1))))

(define (unicode-terminal-width/aux str start end ambiguous-is-wide?)
  (let lp1 ((sc start) (width 0))
    (if (string-cursor>=? sc end)
        width
        (let ((c (string-ref/cursor str sc)))
          (cond
           ;; ANSI escapes
           ;; TODO: maintain a state machine so the escape can be
           ;; spread across multiple strings
           ((and (= 27 (char->integer c)) ; esc
                 (string-cursor<? (string-cursor-next str sc) end)
                 (eqv? #\[ (string-ref/cursor str (string-cursor-next str sc))))
            (let lp2 ((sc (string-cursor-forward str sc 2)))
              (cond ((string-cursor>=? sc end) width)
                    ((memv (string-ref/cursor str sc) '(#\m #\newline))
                     (lp1 (string-cursor-next str sc) width))
                    (else (lp2 (string-cursor-next str sc))))))
           ;; fast-path ASCII
           ((char<=? c #\~)
            (lp1 (string-cursor-next str sc) (+ width 1)))
           ;; unicode
           (else
            (lp1 (string-cursor-next str sc)
                 (+ width (unicode-char-width c ambiguous-is-wide?)))))))))

(define (cursor-arg str x)
  (if (string-cursor? x) x (string-index->cursor str x)))

;; convert args to cursors internally for efficiency

(define (unicode-terminal-width str . o)
  (let ((start (cursor-arg str (if (pair? o)
                                   (car o)
                                   (string-cursor-start str))))
        (end (cursor-arg str (if (and (pair? o) (pair? (cdr o)))
                                 (cadr o)
                                 (string-cursor-end str)))))
    (unicode-terminal-width/aux str start end #f)))

(define (unicode-terminal-width/wide str . o)
  (let ((start (cursor-arg str (if (pair? o)
                                   (car o)
                                   (string-cursor-start str))))
        (end (cursor-arg str (if (and (pair? o) (pair? (cdr o)))
                                 (cadr o)
                                 (string-cursor-end str)))))
    (unicode-terminal-width/aux str start end #t)))

(define (as-unicode . args)
  (fn (ambiguous-is-wide?)
    (with ((string-width (if ambiguous-is-wide?
                             unicode-terminal-width/wide
                             unicode-terminal-width)))
      (each-in-list args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String transformations

(define (with-string-transformer proc . ls)
  (fn ((orig-output output))
    (let ((output* (lambda (str) (orig-output (proc str)))))
      (with ((output output*))
        (each-in-list ls)))))

;;> Show each of \var{ls}, uppercasing all generated text.
(define (upcased . ls) (apply with-string-transformer string-upcase ls))

;;> Show each of \var{ls}, lowercasing all generated text.
(define (downcased . ls) (apply with-string-transformer string-downcase ls))
