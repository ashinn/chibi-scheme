;;> A library for Sanskrit Prosody.
;;>
;;> Sanskrit poetry classifies syllables as "light" or "heavy".
;;> Patterns of three syllables are called a "gana", of which there
;;> are 2^3 = 8 combinations of light and heavy.  This library allows
;;> looking up gana by number, and inquiring on their syllable
;;> pattern from name or number.
;;>
;;> See \hyperlink["http://en.wikipedia.org/wiki/Sanskrit_prosody"]{Sanskrit Poetry}.

(define-library (pingala prosody)
  (export ganas ganas-pattern)
  (import (scheme base) (scheme file) (scheme lazy) (scheme process-context))
  (begin
    ;; String utilities.
    (define (string-find str ch start)
      (let ((end (string-length str)))
        (let lp ((i start))
          (cond ((>= i end) end)
                ((eqv? ch (string-ref str i)) i)
                (else (lp (+ i 1)))))))
    (define (string-split str ch)
      (let ((end (string-length str)))
        (let lp ((i 0) (res '()))
          (let* ((j (string-find str ch i))
                 (res (cons (substring str i j) res)))
            (if (>= j end)
                (reverse res)
                (lp (+ j 1) res))))))
    ;; Filesystem utilities.
    (define (find-in-path base dirs)
      (let lp ((ls dirs))
        (and (pair? ls)
             (let ((path (string-append (car ls) "/" base)))
               (if (file-exists? path)
                   path
                   (lp (cdr ls)))))))
    ;; We install data files alongside source files, but have no way
    ;; to determine where this is.  We need a SRFI providing an API to
    ;; determine standard system directories relative to the host
    ;; Scheme implementations install prefix.  For now, for testing,
    ;; we use an env var hack.
    (define ganas-path
      (let ((prefix (or (get-environment-variable "SNOW_TEST_DATA_DIR") ".")))
        (map (lambda (f) (string-append prefix "/" f))
             (string-split
              (or (get-environment-variable "PINGALA_GANAS_PATH")
                  ".:/usr/local/share/pingala")
              #\:))))
    ;; This data file is tiny - we keep it separate only for testing
    ;; purposes.
    (define ganas-data
      (delay
        (let ((file (find-in-path "ganas.txt" ganas-path)))
          (if (not file)
              (error "couldn't find ganas.txt in " ganas-path))
          (call-with-input-file file
            (lambda (in)
              (let lp ((res '()))
                (let ((line (read-line in)))
                  (if (or (eof-object? line) (equal? "" line))
                      (list->vector (reverse res))
                      (lp (cons line res))))))))))
    ;;> Lookup a ganas by number.
    (define (ganas x)
      (if (integer? x)
          (vector-ref (force ganas-data) x)
          x))
    ;;> Returns the pattern as a string such as "H-L-L".
    (define (ganas-pattern x)
      (define (weight n)
        (if (zero? n) "H" "L"))
      (if (integer? x)
          (string-append
           (weight (modulo x 2)) "-"
           (weight (modulo (quotient x 2) 2)) "-"
           (weight (quotient x 4)))
          (let lp ((i 0))
            (and (< i (vector-length (force ganas-data)))
                 (if (equal? x (vector-ref (force ganas-data) i))
                     (ganas-pattern i)
                     (lp (+ i 1)))))))
    ))
