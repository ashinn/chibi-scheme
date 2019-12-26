
(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (display-to-string x)
  (cond ((string? x) x)
        ((char? x) (string x))
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        (else (error "don't know how to display as sxml" x))))

(define (html-display-escaped-attr str . o)
  (let ((start 0)
        (end (string-length str))
        (out (if (pair? o) (car o) (current-output-port))))
    (let lp ((from start) (to start))
      (if (>= to end)
          (display (substring str from to) out)
          (let ((c (string-ref str to)))
            (cond
             ((eq? c #\<)
              (display (substring str from to) out)
              (display "&lt;" out)
              (lp (+ to 1) (+ to 1)))
             ((eq? c #\&)
              (display (substring str from to) out)
              (display "&amp;" out)
              (lp (+ to 1) (+ to 1)))
             ((eq? c #\")
              (display (substring str from to) out)
              (display "&quot;" out)
              (lp (+ to 1) (+ to 1)))
             (else
              (lp from (+ to 1)))))))))

(define (html-escape-attr str)
  (call-with-output-string
    (lambda (out) (html-display-escaped-attr (display-to-string str) out))))

(define (html-attr->string attr)
  (if (cdr attr)
      (let ((val (if (pair? (cdr attr)) (cadr attr) (cdr attr))))
        (string-append (symbol->string (car attr))
                       "=\"" (html-escape-attr val) "\""))
      (symbol->string (car attr))))

(define (html-tag->string tag attrs)
  (let lp ((ls attrs) (res (list (symbol->string tag) "<")))
    (if (null? ls)
        (apply string-append (reverse (cons ">" res)))
        (lp (cdr ls) (cons (html-attr->string (car ls)) (cons " " res))))))

(define (html-display-escaped-string x . o)
  (let* ((str (display-to-string x))
         (start 0)
         (end (string-length str))
         (out (if (pair? o) (car o) (current-output-port))))
    (let lp ((from start) (to start))
      (if (>= to end)
          (display (substring str from to) out)
          (let ((c (string-ref str to)))
            (cond
             ((eq? c #\<)
              (display (substring str from to) out)
              (display "&lt;" out)
              (lp (+ to 1) (+ to 1)))
             ((eq? c #\&)
              (display (substring str from to) out)
              (display "&amp;" out)
              (lp (+ to 1) (+ to 1)))
             (else
              (lp from (+ to 1)))))))))

(define (html-escape str)
  (call-with-output-string
    (lambda (out) (html-display-escaped-string str out))))

;;> Render (valid, expanded) \var{sxml} as html.
;;> \var{@raw} tag is considered safe text and not processed or escaped.
(define (sxml-display-as-html sxml . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (let lp ((sxml sxml))
      (cond
       ((pair? sxml)
        (let ((tag (car sxml)))
          (if (symbol? tag)
              (let ((rest (cdr sxml)))
                (cond
                 ((and (pair? rest)
                       (pair? (car rest))
                       (eq? '@ (caar rest)))
                  (display (html-tag->string tag (cdar rest)) out)
                  (for-each lp (cdr rest))
                  (display "</" out) (display tag out) (display ">" out))
                 ((and (eq? '@raw tag)
                       (string? (car rest)))
                  (display (car rest) out))
                 (else
                  (display (html-tag->string tag '()) out)
                  (for-each lp rest)
                  (display "</" out) (display tag out) (display ">" out))))
              (for-each lp sxml))))
       ((null? sxml))
       (else (html-display-escaped-string sxml out))))))

;;> Render \var{sxml} as \var{xml}.
;;> \var{@raw} tag is considered safe text and not processed or escaped.
(define (sxml->xml sxml)
  (call-with-output-string
    (lambda (out) (sxml-display-as-html sxml out))))

;;> Render \var{sxml} as simple text, stripping all tags.
(define (sxml-strip sxml)
  (call-with-output-string
    (lambda (out)
      (let strip ((x sxml))
        (cond
         ((pair? x)
          (for-each
           strip
           (if (and (pair? (cdr x)) (eq? '@ (cadr x))) (cddr x) (cdr x))))
         ((string? x)
          (display x out)))))))

;;> Render \var{sxml} as text for viewing in a terminal.
(define (sxml-display-as-text sxml . o)
  (let ((out (if (pair? o) (car o) (current-output-port))))
    (let lp ((sxml sxml))
      (cond
       ((pair? sxml)
        (let ((tag (car sxml)))
          (cond
           ;; skip headers and the menu
           ((or (memq tag '(head style script))
                (and (eq? 'div tag)
                     (pair? (cdr sxml))
                     (pair? (cadr sxml))
                     (eq? '@ (car (cadr sxml)))
                     (equal? '(id . "menu") (assq 'id (cdr (cadr sxml)))))))
           ;; recurse other tags, appending newlines for new sections
           ((symbol? tag)
            (for-each
             lp
             (if (and (pair? (cdr sxml)) (eq? '@ (cadr sxml)))
                 (cddr sxml)
                 (cdr sxml)))
            (if (memq tag '(p li br h1 h2 h3 h4 h5 h6))
                (newline out)))
           (else
            (for-each lp sxml)))))
       ((null? sxml))
       (else (display sxml out))))))
