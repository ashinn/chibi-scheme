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
              (from #f)
              (width 0)
              (escapes '()))
      ;; need to pick up trailing ansi escapes
      (define (finish res sc)
        (let ((res (if (pair? escapes)
                       (string-concatenate-reverse (cons res escapes))
                       res))
              (end-1 (string-cursor-prev str end)))
          (let lp ((sc sc) (right-escapes '()))
            (define (finish2 right-escapes)
              (if (pair? right-escapes)
                  (string-append res
                                 (string-concatenate-reverse
                                  right-escapes))
                  res))
            (if (string-cursor>=? sc end-1)
                (finish2 right-escapes)
                (let ((c (string-ref/cursor str sc))
                      (sc2 (string-cursor-next str sc)))
                  (if (and (= 27 (char->integer c))
                           (eqv? #\[ (string-ref/cursor str sc2)))
                      (let lp2 ((sc2 (string-cursor-next str sc2)))
                        (if (string-cursor>=? sc2 end)
                            (finish2 right-escapes)
                            (let ((c2 (string-ref/cursor str sc2))
                                  (sc3 (string-cursor-next str sc2)))
                              (if (eqv? #\m c2)
                                  (lp sc3
                                      (cons (substring/cursors str sc sc3)
                                            right-escapes))
                                  (lp2 sc3)))))
                      (lp sc2 right-escapes)))))))
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
                       (lp1 sc2 from width escapes))
                      ((memv (string-ref/cursor str sc2) '(#\m #\newline))
                       (let* ((sc3 (string-cursor-next str sc2))
                              (escapes
                               (if (not from)
                                   (cons (substring/cursors str sc sc3)
                                         escapes)
                                  escapes)))
                         (lp1 sc3 from width escapes)))
                      (else (lp2 (string-cursor-next str sc2))))))
             (else
              (let ((width2 (+ width
                               (unicode-char-width c ambiguous-is-wide?))))
                (cond
                 ((> width2 hi)
                  (finish (substring/cursors str (or from start) sc) sc))
                 ((and (not from) (> width2 lo))
                  (lp1 (string-cursor-next str sc) sc width2 escapes))
                 (else
                  (lp1 (string-cursor-next str sc) from width2 escapes)
                  ))))))))))

(define (substring-terminal-width str lo hi)
  (substring-terminal-width/aux str lo hi #f))

(define (substring-terminal-width/wide str lo hi)
  (substring-terminal-width/aux str lo hi #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (terminal-aware . args)
  (fn (ambiguous-is-wide?)
    (with ((string-width (if ambiguous-is-wide?
                             string-terminal-width/wide
                             string-terminal-width))
           (substring/width (if ambiguous-is-wide?
                                substring-terminal-width/wide
                                substring-terminal-width)))
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
