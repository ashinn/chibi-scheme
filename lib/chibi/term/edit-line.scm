;;;; edit-line.scm - pure scheme line editor
;;
;; Copyright (c) 2011-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vt100 terminal utilities

(define (terminal-escape out ch arg)
  (write-char (integer->char 27) out)
  (write-char #\[ out)
  (if arg (display arg out))
  (write-char ch out))

;; we use zero-based columns
(define (terminal-goto-col out n)  (terminal-escape out #\G (+ n 1)))
(define (terminal-up out n)        (terminal-escape out #\A n))
(define (terminal-down out n)      (terminal-escape out #\B n))
(define (terminal-clear-below out) (terminal-escape out #\J #f))
(define (terminal-clear-right out) (terminal-escape out #\K #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; history

(define maximum-history-size 512)

(define-record-type History
  (%make-history remaining past future filter)
  history?
  (remaining history-remaining history-remaining-set!)
  (past history-past history-past-set!)
  (future history-future history-future-set!)
  (filter history-filter history-filter-set!))

(define (make-history . o)
  (%make-history (if (pair? o) (car o) maximum-history-size)
                 '()
                 '()
                 (and (pair? o) (pair? (cdr o)) (cadr o))))

(define (history-current h)
  (let ((p (history-past h)))
    (and (pair? p) (car p))))

(define (history->list h)
  (let ((past (history-past h)) (future (history-future h)))
    (if (pair? past) (cons (car past) (append future (cdr past))) future)))

(define (list->history ls . o)
  (%make-history (max maximum-history-size (length ls)) ls '()
                 (and (pair? o) (car o))))

(define (history-flatten! h)
  (history-past-set! h (history->list h))
  (history-future-set! h '()))

(define (drop-last ls) (reverse (cdr (reverse ls))))

(define (history-past-push! h x)
  (if (positive? (history-remaining h))
      (history-remaining-set! h (- (history-remaining h) 1))
      (if (pair? (history-past h))
          (history-past-set! h (drop-last (history-past h)))
          (history-future-set! h (drop-last (history-future h)))))
  (history-past-set! h (cons x (history-past h))))

(define (history-insert! h x)
  (history-flatten! h)
  (if (not (and (history-filter h) ((history-filter h) x)))
      (history-past-push! h x)))

(define (history-reset! h)
  (cond
   ((pair? (history-future h))
    (history-past-set! h (append (drop-last (history-future h))
                                 (history-past h)))
    (history-future-set! h '()))))

(define (history-commit! h x)
  (history-reset! h)
  (history-insert! h x))

(define (history-prev! h)
  (let ((past (history-past h)))
    (and (pair? past)
         (pair? (cdr past))
         (begin
           (history-future-set! h (cons (car past) (history-future h)))
           (history-past-set! h (cdr past))
           (cadr past)))))

(define (history-next! h)
  (let ((future (history-future h)))
    (and (pair? future)
         (begin
           (history-past-set! h (cons (car future) (history-past h)))
           (history-future-set! h (cdr future))
           (car future)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; char and string utils

(define (char-word-constituent? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)
      (memv ch '(#\_ #\- #\+ #\:))))

(define (char-non-word-constituent? ch) (not (char-word-constituent? ch)))

(define (string-copy! dst dstart src start end)
  (if (>= start dstart)
      (do ((i start (+ i 1)) (j dstart (+ j 1)))
          ((= i end))
        (string-set! dst j (string-ref src i)))
      (do ((i (- end 1) (- i 1)) (j (+ dstart (- end start 1)) (- j 1)))
          ((< i start))
        (string-set! dst j (string-ref src i)))))

(define (string-index ch x)
  (let ((len (string-length x)))
    (let lp ((i 0))
      (cond ((>= i len) #f)
            ((eqv? ch (string-ref x i)))
            (else (lp (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffers

(define-record-type Buffer
  (%make-buffer refresh? min pos row max-row col gap start width string
                history complete? single-line?)
  buffer?
  (refresh? buffer-refresh? buffer-refresh?-set!)
  (min buffer-min buffer-min-set!)
  (pos buffer-pos buffer-pos-set!)
  (row buffer-row buffer-row-set!)
  (max-row buffer-max-row buffer-max-row-set!)
  (col buffer-col buffer-col-set!)
  (gap buffer-gap buffer-gap-set!)
  (start buffer-start buffer-start-set!)
  (width buffer-width buffer-width-set!)
  (string buffer-string buffer-string-set!)
  (kill-ring buffer-kill-ring buffer-kill-ring-set!)
  (history buffer-history buffer-history-set!)
  (complete? buffer-complete? buffer-complete?-set!)
  (single-line? buffer-single-line? buffer-single-line?-set!))

(define default-buffer-size 256)
(define default-buffer-width 80)

(define (make-buffer)
  (%make-buffer #f 0 0 0 0 0 default-buffer-size 0 default-buffer-width
                (make-string default-buffer-size) '() #f #f))

(define (buffer->string buf)
  (let ((str (buffer-string buf)))
    (string-append (substring str (buffer-min buf) (buffer-pos buf))
                   (substring str (buffer-gap buf) (string-length str)))))

(define (buffer-right-length buf)
  (- (string-length (buffer-string buf)) (buffer-gap buf)))
(define (buffer-length buf)
  (+ (buffer-pos buf) (buffer-right-length buf)))
(define (buffer-free-space buf)
  (- (buffer-gap buf) (buffer-pos buf)))

(define (buffer-clamp buf n)
  (max (buffer-min buf) (min n (buffer-length buf))))

(define (buffer-resize buf n)
  (cond ((<= (buffer-free-space buf) n)
         (let* ((right-len (buffer-right-length buf))
                (new-len (* 2 (max n (buffer-length buf))))
                (new-gap (- new-len right-len))
                (new (make-string new-len))
                (old (buffer-string buf)))
           (string-copy! new 0 old 0 (buffer-pos buf))
           (string-copy! new new-gap old (buffer-gap buf) (string-length old))
           (buffer-string-set! buf new)
           (buffer-gap-set! buf new-gap)))))

(define (buffer-update-position! buf)
  (let ((pos (buffer-pos buf))
        (gap (buffer-gap buf))
        (str (buffer-string buf))
        (end (string-length (buffer-string buf)))
        (width (buffer-width buf)))
    ;; TODO: Support double and zero-width chars and ANSI escapes.
    (cond
     ((buffer-single-line? buf)
      ;; The "start" is the last left-most column of the buffer when
      ;; we overflow and need to scroll horizontally.  This defaults
      ;; to 0 and increments as we move past the last column.  We
      ;; update it when we find that (via movement or insertion) the
      ;; point would no longer be visible from "start" to the end of
      ;; the line, by shifting the start to the rightmost column that
      ;; would show the point.  Thus, after scrolling off the
      ;; beginning of the buffer, successive movements left will first
      ;; go to the 0th column, then scroll to the start one character
      ;; at a time.  A beginning-of-line command will restore the
      ;; "start" to 0 immediately.
      ;; We assume no embedded newlines in this case.
      (let ((start (buffer-start buf)))
        (cond
         ((> start pos)
          (buffer-start-set! buf pos))
         ((> (+ 1 (buffer-min buf) (- pos start)) (buffer-width buf))
          (buffer-start-set! buf (max 0 (- (+ 1 (buffer-min buf) pos)
                                           (buffer-width buf))))))
        (buffer-col-set! buf (+ (buffer-min buf) (- pos (buffer-start buf))))))
     (else
      ;; Otherwise, in a multi-line editor we need to scan for
      ;; newlines to determine the current (relative) row and column.
      (let lp ((i 0) (row 0) (col 0)) ;; update row/col
        (cond ((= i pos)
               (buffer-row-set! buf row)
               (buffer-col-set! buf col)
               (lp gap row col))      ;; skip from pos->gap
              ((>= i end)
               (buffer-max-row-set!
                buf (if (and (zero? col) (> row 0)) (- row 1) row)))
              ((eqv? #\newline (string-ref str i))
               (lp (+ i 1) (+ row 1) 0))
              ((= (+ col 1) width)
               (lp (+ i 1) (+ row 1) 0))
              (else
               (lp (+ i 1) row (+ col 1)))))))))

(define (buffer-clear buf out)
  ;; goto start of input
  (terminal-goto-col out 0)
  (if (positive? (buffer-row buf))
      (terminal-up out (buffer-row buf)))
  ;; clear below
  (terminal-clear-below out))

(define (buffer-draw buf out)
  (let* ((gap (buffer-gap buf))
         (str (buffer-string buf))
         (end (string-length str))
         (old-row (buffer-row buf))
         (old-col (buffer-col buf)))
    ;; update position and clear the current input
    (buffer-clear buf out)
    (buffer-update-position! buf)
    (let ((left (if (buffer-single-line? buf)
                    (buffer-start buf)
                    (buffer-min buf)))
          (right
           (if (buffer-single-line? buf)
               (min end (+ (buffer-gap buf)
                           (- (buffer-width buf) (buffer-col buf))))
               end)))
      (display (substring str 0 (buffer-min buf)) out)
      (display (substring str left (buffer-pos buf)) out)
      (display (substring str (buffer-gap buf) right) out))
    (cond
     ((not (buffer-single-line? buf))
      ;; move to next line if point at eol
      (if (and (zero? (buffer-col buf)) (positive? (buffer-row buf)))
          (write-char #\space out))
      ;; move to correct row then col
      (if (< (buffer-row buf) (buffer-max-row buf))
          (terminal-up out (- (buffer-max-row buf) (buffer-row buf))))))
    (terminal-goto-col out (buffer-col buf))
    (flush-output out)))

(define (buffer-refresh buf out)
  (cond ((buffer-refresh? buf)
         (buffer-draw buf out)
         (buffer-refresh?-set! buf #f))))

(define (buffer-goto! buf out n)
  (let ((pos (buffer-pos buf))
        (gap (buffer-gap buf))
        (str (buffer-string buf))
        (n (buffer-clamp buf n)))
    (cond ((not (= n pos))
           (buffer-update-position! buf) ;; necesary?
           (if (< n pos)
               (string-copy! str (- gap (- pos n)) str n pos)
               (string-copy! str pos str gap (+ gap (- n pos))))
           (buffer-pos-set! buf n)
           (buffer-gap-set! buf (+ gap (- n pos)))
           (cond
            ((not (buffer-refresh? buf))
             (let ((old-row (buffer-row buf))
                   (old-start (buffer-start buf)))
               (buffer-update-position! buf)
               (cond
                ((not (= old-start (buffer-start buf)))
                 (buffer-refresh?-set! buf #t))
                (else
                 (let ((row-diff (- old-row (buffer-row buf))))
                   (cond ((> row-diff 0) (terminal-up out row-diff))
                         ((< row-diff 0) (terminal-down out (- row-diff)))))
                 (terminal-goto-col out (buffer-col buf)))))))))))

(define (buffer-insert! buf out x)
  (let ((len (if (char? x) 1 (string-length x)))
        (pos (buffer-pos buf)))
    (buffer-resize buf len)
    (if (char? x)
        (string-set! (buffer-string buf) pos x)
        (string-copy! (buffer-string buf) pos x 0 len))
    (buffer-pos-set! buf (+ (buffer-pos buf) len))
    (cond
     ((buffer-refresh? buf))
     ((and (= (buffer-gap buf) (string-length (buffer-string buf)))
           (< (+ (buffer-col buf) len) (buffer-width buf))
           (if (char? x)
               (not (eqv? x #\newline))
               (not (string-index #\newline x))))
      ;; fast path - append to end of buffer w/o wrapping to next line
      (display x out)
      (flush-output out)
      (buffer-col-set! buf (+ (buffer-col buf) len)))
     (else
      (buffer-refresh?-set! buf #t)))))

(define (buffer-delete! buf out start end)
  (let ((pos (buffer-pos buf))
        (gap (buffer-gap buf))
        (str (buffer-string buf))
        (start (buffer-clamp buf start))
        (end (buffer-clamp buf end)))
    (if (not (buffer-refresh? buf))
        (if (and (= start pos) (>= end (buffer-length buf)))
            (terminal-clear-below out)
            (buffer-refresh?-set! buf #t)))
    (cond ((< end pos)
           (string-copy! str start str end pos)
           (buffer-pos-set! buf (+ start (- pos end))))
          ((> start gap)
           (string-copy! str start str gap (+ gap (- end start)))
           (buffer-gap-set! buf (+ gap (- end start))))
          (else
           (buffer-pos-set! buf (min pos start))
           (buffer-gap-set! buf (max gap (+ pos (- gap pos) (- end pos))))))))

(define (buffer-skip buf pred)
  (let* ((str (buffer-string buf)) (end (string-length str)))
    (let lp ((i (buffer-gap buf)))
      (if (or (>= i end) (not (pred (string-ref str i))))
          (+ (- i (buffer-gap buf)) (buffer-pos buf))
          (lp (+ i 1))))))

(define (buffer-skip-reverse buf pred)
  (let ((str (buffer-string buf)))
    (let lp ((i (- (buffer-pos buf) 1)))
      (if (or (< i 0) (not (pred (string-ref str i)))) i (lp (- i 1))))))

(define (buffer-previous-word buf)
  (let ((i (buffer-skip-reverse buf char-word-constituent?)))
    (substring (buffer-string buf) (+ i 1) (buffer-pos buf))))

(define (buffer-format-list buf out words)
  (let ((width (buffer-width buf)))
    (define (write-rows num-cols widths)
      (let lp ((ls words) (i 0))
        (cond
         ((pair? ls)
          (let ((diff (- (vector-ref widths i) (string-length (car ls)))))
            (display (car ls) out)
            (if (= (+ i 1) num-cols)
                (newline out)
                (display (make-string (+ 1 diff) #\space) out))
            (lp (cdr ls) (modulo (+ i 1) num-cols))))
         ((< i num-cols)
          (newline out)))))
    (let try-cols ((num-cols (length words)))
      (cond
       ((<= num-cols 1)
        (newline out)
        (for-each (lambda (x) (display x out) (newline out)) words))
       (else
        (let ((widths (make-vector num-cols 0)))
          (let lp ((ls words) (i 0) (avail (- num-cols 1)))
            (cond
             ((null? ls)
              (write-rows num-cols widths))
             (else
              (let ((diff (- (string-length (car ls)) (vector-ref widths i))))
                (if (positive? diff)
                    (let ((avail (+ avail diff)))
                      (cond
                       ((> avail width)
                        (try-cols (- num-cols 1)))
                       (else
                        (vector-set! widths i (string-length (car ls)))
                        (lp (cdr ls) (modulo (+ i 1) num-cols) avail))))
                    (lp (cdr ls) (modulo (+ i 1) num-cols) avail))))))))))))

(define (buffer-make-completer generate)
  (lambda (ch buf out return)
    (let* ((word (buffer-previous-word buf))
           (ls (generate buf word)))
      (cond
       ((null? ls)
        (command/beep ch buf out return))
       ((= 1 (length ls))
        (buffer-insert! buf out (substring (car ls) (string-length word))))
       (else
        (newline out)
        (buffer-format-list buf out ls)
        (buffer-draw buf out))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymaps

(define keymap? pair?)

(define (make-keymap . o)
  (cons (make-vector 256 #f) (and (pair? o) (car o))))

(define (make-sparse-keymap . o)
  (cons '() (and (pair? o) (car o))))

(define (make-printable-keymap)
  (let* ((keymap (make-keymap))
         (v (car keymap)))
    (do ((i #x20 (+ i 1))) ((= i #x7F) keymap)
      (vector-set! v i command/self-insert))))

(define (make-standard-escape-bracket-keymap)
  (let* ((keymap (make-keymap))
         (v (car keymap)))
    (vector-set! v 65 command/backward-history)
    (vector-set! v 66 command/forward-history)
    (vector-set! v 67 command/forward-char)
    (vector-set! v 68 command/backward-char)
    keymap))

(define (make-standard-escape-keymap)
  (let* ((keymap (make-keymap))
         (v (car keymap)))
    (vector-set! v   8 command/backward-delete-word)
    (vector-set! v  91 (make-standard-escape-bracket-keymap))
    (vector-set! v  98 command/backward-word)
    (vector-set! v 100 command/forward-delete-word)
    (vector-set! v 102 command/forward-word)
    (vector-set! v 127 command/backward-delete-word)
    keymap))

(define (make-standard-keymap)
  (let* ((keymap (make-printable-keymap))
         (v (car keymap)))
    (vector-set! v   0 command/enter)   ;; for telnet
    (vector-set! v   1 command/beginning-of-line)
    (vector-set! v   2 command/backward-char)
    (vector-set! v   3 command/cancel)
    (vector-set! v   4 command/forward-delete-char)
    (vector-set! v   5 command/end-of-line)
    (vector-set! v   6 command/forward-char)
    (vector-set! v   8 command/backward-delete-char)
    (vector-set! v  10 command/enter)
    (vector-set! v  11 command/forward-delete-line)
    (vector-set! v  12 command/refresh)
    (vector-set! v  13 command/skip)
    (vector-set! v  21 command/backward-delete-line)
    (vector-set! v  27 (make-standard-escape-keymap))
    (vector-set! v 127 command/backward-delete-char)
    keymap))

(define (keymap-lookup keymap n)
  (let ((table (car keymap)))
    (or (if (vector? table)
            (and (< -1 n (vector-length table)) (vector-ref table n))
            (cond ((assv n table) => cdr) (else #f)))
        (if (keymap? (cdr keymap))
            (keymap-lookup (cdr keymap) n)
            (cdr keymap)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands

(define (command/self-insert ch buf out return)
  (buffer-insert! buf out ch))

(define (command/enter ch buf out return)
  (guard (exn (else
               (buffer-clear buf out)
               (print-exception exn out)
               (buffer-draw buf out)))
    (cond
     (((buffer-complete? buf) buf)
      (command/end-of-line ch buf out return)
      (display "\r\n" out)
      (flush-output out)
      (return))
     (else
      (command/self-insert ch buf out return)))))

(define (command/cancel ch buf out return)
  (command/end-of-line ch buf out return)
  (display "^C" out)
  (newline out)
  (buffer-delete! buf out 0 (buffer-length buf))
  (buffer-draw buf out))

(define (command/beep ch buf out return)
  (write-char (integer->char 7) out))

(define (command/skip ch buf out return)
  #f)

(define (command/refresh ch buf out return)
  (buffer-draw buf out))

(define (command/beginning-of-line ch buf out return)
  (buffer-goto! buf out 0))

(define (command/end-of-line ch buf out return)
  (buffer-goto! buf out (buffer-length buf)))

(define (command/forward-char ch buf out return)
  (buffer-goto! buf out (+ (buffer-pos buf) 1)))

(define (command/backward-char ch buf out return)
  (buffer-goto! buf out (- (buffer-pos buf) 1)))

(define (command/forward-delete-char ch buf out return)
  (cond
   ((zero? (- (buffer-length buf) (buffer-min buf)))
    (newline out)
    (return 'eof))
   (else
    (buffer-delete! buf out (buffer-pos buf) (+ (buffer-pos buf) 1)))))

(define (command/backward-delete-char ch buf out return)
  (buffer-delete! buf out (- (buffer-pos buf) 1) (buffer-pos buf)))

(define (command/forward-delete-line ch buf out return)
  (buffer-delete! buf out (buffer-pos buf) (buffer-length buf)))

(define (command/backward-delete-line ch buf out return)
  (buffer-delete! buf out 0 (buffer-pos buf)))

(define (command/backward-history ch buf out return)
  (let ((history (buffer-history buf)))
    (cond
     ((and (history? history) (pair? (history-past history)))
      (if (null? (history-future history))
          (history-insert! history (buffer->string buf)))
      (cond
       ((pair? (cdr (history-past history)))
        (buffer-delete! buf out 0 (buffer-length buf))
        (buffer-insert! buf out (history-prev! history))))))))

(define (command/forward-history ch buf out return)
  (let ((history (buffer-history buf)))
    (cond
     ((and (history? history) (pair? (history-future history)))
      (buffer-delete! buf out 0 (buffer-length buf))
      (let ((res (buffer-insert! buf out (history-next! history))))
        (if (null? (history-future history))
            (history-past-set! history (cdr (history-past history))))
        res)))))

(define (command/forward-word ch buf out return)
  (buffer-goto! buf out (buffer-skip buf char-non-word-constituent?))
  (buffer-goto! buf out (buffer-skip buf char-word-constituent?)))

(define (command/backward-word ch buf out return)
  (buffer-goto! buf out (buffer-skip-reverse buf char-non-word-constituent?))
  (buffer-goto! buf out (+ (buffer-skip-reverse buf char-word-constituent?) 1)))

(define (command/forward-delete-word ch buf out return)
  (let ((start (buffer-pos buf)))
    (buffer-goto! buf out (buffer-skip buf char-non-word-constituent?))
    (buffer-delete! buf out start (buffer-skip buf char-word-constituent?))))

(define (command/backward-delete-word ch buf out return)
  (let ((end (buffer-pos buf)))
    (buffer-goto! buf out (buffer-skip-reverse buf char-non-word-constituent?))
    (let ((start (buffer-skip-reverse buf char-word-constituent?)))
      (buffer-delete! buf out (+ start 1) end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-editing

(define standard-keymap (make-standard-keymap))

(define (get-key ls key . o)
  (let ((x (memq key ls)))
    (if (and x (pair? (cdr x))) (cadr x) (and (pair? o) (car o)))))

(define (with-leading-ports ls proc)
  (if (and (pair? ls) (input-port? (car ls)))
      (if (and (pair? (cdr ls)) (output-port? (cadr ls)))
          (proc (car ls) (cadr ls) (cddr ls))
          (proc (car ls) (current-output-port) (cdr ls)))
      (proc (current-input-port) (current-output-port) ls)))

(define (make-line-editor . args)
  (let* ((prompter (get-key args 'prompt: "> "))
         (history (get-key args 'history:))
         (complete? (get-key args 'complete?: (lambda (buf) #t)))
         (completion (get-key args 'completion: #f))
         (terminal-width (get-key args 'terminal-width:))
         (single-line? (get-key args 'single-line?: #f))
         (no-stty? (get-key args 'no-stty?: #f))
         (keymap0 (get-key args 'keymap: standard-keymap))
         (keymap (if completion
                     (cons (list (cons 9 completion)) keymap0)
                     keymap0))
         (buf (or (get-key args 'buffer: #f) (make-buffer))))
    (lambda (in out)
      (let* ((width (or terminal-width (get-terminal-width out) 80))
             (prompt (if (procedure? prompter) (prompter) prompter))
             (done? #f)
             (return (lambda o (set! done? (if (pair? o) (car o) #t)))))
        ;; Clear buffer and reset prompt.
        (buffer-refresh?-set! buf #t)
        (buffer-min-set! buf 0)
        (buffer-delete! buf out 0 (buffer-length buf))
        (buffer-width-set! buf width)
        (buffer-insert! buf out prompt)
        (buffer-min-set! buf (string-length prompt))
        (buffer-history-set! buf history)
        (buffer-complete?-set! buf complete?)
        (buffer-single-line?-set! buf single-line?)
        (if single-line? (buffer-start-set! buf (buffer-min buf)))
        (buffer-refresh buf out)
        (flush-output out)
        ((if no-stty? (lambda (out f) (f)) with-raw-io)
         out
         (lambda ()
           (let lp ((kmap keymap))
             (let ((ch (read-char in)))
               (if (eof-object? ch)
                   (let ((res (buffer->string buf)))
                     (if (equal? res "") ch res))
                   (let ((x (keymap-lookup kmap (char->integer ch))))
                     (cond
                      ((keymap? x)
                       (lp x))
                      ((procedure? x)
                       (guard (exn (else
                                    (buffer-clear buf out)
                                    (print-exception exn out)
                                    (buffer-draw buf out)))
                         (x ch buf out return))
                       (flush-output out)
                       (buffer-refresh buf out)
                       (if done?
                           (and (not (eq? done? 'eof)) (buffer->string buf))
                           (lp keymap)))
                      (else
                       ;;(command/beep ch buf out return)
                       (lp keymap)))))))))))))

(define (edit-line . args)
  (with-leading-ports
   args
   (lambda (in out rest) ((apply make-line-editor rest) in out))))

(define (edit-line-repl . args)
  (with-leading-ports
   args
   (lambda (in out rest)
     (let ((eval (get-key rest 'eval: (lambda (x) x)))
           (print (get-key rest 'write: write))
           (history (or (get-key rest 'history:) (make-history))))
       (let ((edit-line
              (apply make-line-editor 'no-stty?: #t 'history: history rest)))
         ((if (get-key args 'no-stty?:) (lambda (out f) (f)) with-raw-io)
          out
          (lambda ()
            (let lp ()
              (let ((line (edit-line in out)))
                (if (pair? (history-future history))
                    (history-past-set! history (cdr (history-past history))))
                (history-commit! history line)
                (print (eval line) out)
                (newline out)
                (lp))))))))))
