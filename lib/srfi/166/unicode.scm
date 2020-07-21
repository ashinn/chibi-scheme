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

(define (string-terminal-width/aux str start end ambiguous-is-wide?)
  (let lp1 ((sc start) (width 0))
    (if (string-cursor>=? sc end)
        width
        (let ((c (string-ref/cursor str sc))
              (sc2 (string-cursor-next str sc)))
          (cond
           ;; ANSI escapes
           ;; TODO: consider maintaining a state machine so the escape
           ;; can be spread across multiple strings (not needed if
           ;; assuming all escapes come from (srfi 166 color)).
           ((and (= 27 (char->integer c)) ; esc
                 (string-cursor<? sc2 end)
                 (eqv? #\[ (string-ref/cursor str sc2)))
            (let lp2 ((sc (string-cursor-forward str sc 2)))
              (cond ((string-cursor>=? sc end) width)
                    ((memv (string-ref/cursor str sc) '(#\m #\newline))
                     (lp1 (string-cursor-next str sc) width))
                    (else (lp2 (string-cursor-next str sc))))))
           ;; fast-path ASCII
           ((char<=? c #\~)
            (lp1 sc2 (+ width 1)))
           ;; unicode
           (else
            (lp1 sc2 (+ width (unicode-char-width c ambiguous-is-wide?)))
            ))))))

(define (cursor-arg str x)
  (if (string-cursor? x) x (string-index->cursor str x)))

;; convert args to cursors internally for efficiency

(define (string-terminal-width str . o)
  (let ((start (cursor-arg str (if (pair? o)
                                   (car o)
                                   (string-cursor-start str))))
        (end (cursor-arg str (if (and (pair? o) (pair? (cdr o)))
                                 (cadr o)
                                 (string-cursor-end str)))))
    (string-terminal-width/aux str start end #f)))

(define (string-terminal-width/wide str . o)
  (let ((start (cursor-arg str (if (pair? o)
                                   (car o)
                                   (string-cursor-start str))))
        (end (cursor-arg str (if (and (pair? o) (pair? (cdr o)))
                                 (cadr o)
                                 (string-cursor-end str)))))
    (string-terminal-width/aux str start end #t)))

(define (substring-terminal-width/aux str lo hi ambiguous-is-wide?)
  (let ((start (string-cursor-start str))
        (end (string-cursor-end str)))
    (let lp1 ((sc start)
              (from (and (negative? lo) start))
              (width 0))
      (if (string-cursor>=? sc end)
          (if from (substring/cursors str from end) str)
          (let ((c (string-ref/cursor str sc)))
            (cond
             ((and (= 27 (char->integer c)) ; esc
                   (string-cursor<? (string-cursor-next str sc) end)
                   (eqv? #\[ (string-ref/cursor
                              str
                              (string-cursor-next str sc))))
              (let lp2 ((sc2 (string-cursor-forward str sc 2)))
                (cond ((string-cursor>=? sc2 end)
                       (lp1 sc2 from width))
                      ((memv (string-ref/cursor str sc2) '(#\m #\newline))
                       (lp1 (string-cursor-next str sc2) from width))
                      (else (lp2 (string-cursor-next str sc2))))))
             (else
              (let ((width2 (+ width
                               (unicode-char-width c ambiguous-is-wide?))))
                (cond
                 ((> width2 hi)
                  (if from
                      (substring/cursors str from sc)
                      ""))
                 ((and (not from) (> width2 lo))
                  (lp1 (string-cursor-next str sc) sc width2))
                 (else
                  (lp1 (string-cursor-next str sc) from width2)
                  ))))))))))

(define (substring-terminal-width str lo hi)
  (substring-terminal-width/aux str lo hi #f))

(define (substring-terminal-width/wide str lo hi)
  (substring-terminal-width/aux str lo hi #t))

;; The BiDi control characters - trimming these would result in the
;; remaining text rendered in the wrong direction.
;; Other characters for consideration would be language tags or
;; interlinear annotation, but use of these is discouraged.
;; Similarly, we might want to preserve the BOM only at the start of
;; text, but this is a file-level encoding mechanism and not likely
;; appropriate to formatting in-memory strings.
(define non-local-controls
  '(#\x061C #\x200E #\x200F #\x202A #\x202B #\x202C
    #\x202D #\x202E #\x2066 #\x2067 #\x2068 #\x2069))

(define (substring-terminal-preserve str)
  (let ((start (string-cursor-start str))
        (end (string-cursor-end str)))
    (let lp1 ((sc start) (escapes '()))
      (if (string-cursor>=? sc end)
          (string-concatenate-reverse escapes)
          (let ((c (string-ref/cursor str sc))
                (sc2 (string-cursor-next str sc)))
            (cond
             ((and (= 27 (char->integer c))
                   (string-cursor<? sc2 end)
                   (eqv? #\[ (string-ref/cursor str sc2)))
              (let lp2 ((sc2 (string-cursor-next str sc2)))
                (if (string-cursor>=? sc2 end)
                    (string-concatenate-reverse escapes)
                    (let ((c2 (string-ref/cursor str sc2))
                          (sc3 (string-cursor-next str sc2)))
                      (if (eqv? #\m c2)
                          (lp1 sc3
                               (cons (substring/cursors str sc sc3)
                                     escapes))
                          (lp2 sc3))))))
             ((and (memv c non-local-controls))
              (lp1 sc2 (cons (string c) escapes)))
             (else
              (lp1 sc2 escapes))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (terminal-aware . args)
  (fn (ambiguous-is-wide?)
    (with ((string-width (if ambiguous-is-wide?
                             string-terminal-width/wide
                             string-terminal-width))
           (substring/width (if ambiguous-is-wide?
                                substring-terminal-width/wide
                                substring-terminal-width))
           (substring/preserve substring-terminal-preserve))
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
