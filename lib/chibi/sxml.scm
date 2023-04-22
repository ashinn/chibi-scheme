
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
  (if (null? (cdr attr))
      (symbol->string (car attr))
      (let ((val (if (pair? (cdr attr)) (cadr attr) (cdr attr))))
        (string-append (symbol->string (car attr))
                       "=\"" (html-escape-attr val) "\""))))

(define (html-tag->string tag attrs)
  (let lp ((ls attrs) (res (list (symbol->string tag) "<")))
    (if (null? ls)
        (apply string-append (reverse (cons ">" res)))
        (lp (cdr ls) (cons (html-attr->string (car ls)) (cons " " res))))))

(define void-elements
  '(area base br col embed hr img input keygen link meta param source track wbr))

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

(define indentable-elements
  '(address article aside base blockquote body dd details dialog
    div dl dt fieldset figcaption figure footer form h1 h2 h3 h4
    h5 h6 head header hgroup hr li link main meta nav ol p pre
    script section style table title ul))

(define (indent i out)
  (do ((j (* 2 i) (- j 1))) ((= j 0)) (write-char #\space out)))

;;> Render (valid, expanded) \var{sxml} as html.
;;> \var{@raw} tag is considered safe text and not processed or escaped.
(define (sxml-display-as-html sxml . args)
  (let* ((out (if (null? args) (current-output-port) (car args)))
         (args (if (null? args) args (cdr args)))
	 (indent? (if (null? args) #f (car args)))
         (args (if (null? args) args (cdr args))))
    (unless (null? args) (error "too many args"))
    (let lp ((sxml (if (and (pair? sxml) (eq? '*TOP* (car sxml)))
                       (cdr sxml)
                       sxml))
	     (depth 0))
      (cond
       ((pair? sxml)
        (let ((tag (car sxml))
              (rest (cdr sxml)))
          (cond
           ((symbol? tag)
            (cond
             ((eqv? #\! (string-ref (symbol->string tag) 0))
              (display "<" out) (display tag out)
              (for-each (lambda (x) (display " " out) (display x out)) rest)
              (display ">\n" out))
             ((and (eq? '@raw tag)
                   (string? (car rest)))
              (if (not (null? (cdr rest)))
                  (error "@raw takes only one value" sxml))
              (display (car rest) out))
             ((and (pair? rest)
                   (pair? (car rest))
                   (eq? '@ (caar rest)))
	      (when (and indent? (memq tag indentable-elements))
		(newline out)
		(indent depth out))
	      (display (html-tag->string tag (cdar rest)) out)
	      (for-each (lambda (x) (lp x (+ 1 depth))) (cdr rest))
	      (unless (and (null? (cdr rest)) (memq tag void-elements))
                (display "</" out) (display tag out) (display ">" out)))
             (else
	      (when (and indent? (memq tag indentable-elements))
		(newline out)
		(indent depth out))
	      (display (html-tag->string tag '()) out)
	      (for-each (lambda (x) (lp x (+ 1 depth))) rest)
	      (unless (and (null? rest) (memq tag void-elements))
                (display "</" out) (display tag out) (display ">" out)))))
           (else
            (for-each (lambda (x) (lp x (+ 1 depth))) sxml)))))
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
  (let ((out (if (pair? o) (car o) (current-output-port)))
        (sxml (if (and (pair? sxml) (null? (cddr sxml)) (eq? '*TOP* (car sxml)))
                       (cadr sxml)
                       sxml)))
    (let lp ((sxml sxml))
      (cond
       ((pair? sxml)
        (let ((tag (car sxml)))
          (cond
           ;; skip headers and the menu
           ((or (memq tag '(head style script !DOCTYPE))
                (and (eq? 'div tag)
                     (pair? (cdr sxml))
                     (pair? (cadr sxml))
                     (eq? '@ (car (cadr sxml)))
                     (equal? '(id . "menu") (assq 'id (cdr (cadr sxml)))))))
           ;; recurse other tags, appending newlines for new sections
           ((symbol? tag)
            (if (memq tag '(h1 h2 h3 h4 h5 h6))
                (newline out))
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
