;; config.scm -- general configuration management
;; Copyright (c) 2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> This is a library for unified configuration management.
;;> Essentially it provides an abstract collection data type for
;;> looking up named values, two or more of which can be chained
;;> together.  Values from more recent collections can be preferred as
;;> with an environment, or the values at multiple levels can be
;;> flattened together.  Convenience routines are provided from
;;> loading these collections from files while allowing extensions
;;> such as configurations from command-line options.

;;> @subsubsection{Background}
;;>
;;> As any application grows to sufficient complexity, it acquires
;;> options and behaviors that one may want to modify at startup or
;;> runtime.  The traditional approach is a combination of
;;> command-line options, config files, environment variables, and/or
;;> other specialized settings.  These all have various pros and cons:
;;>
;;> @table[(^ (border 1) (style border-collapse:collapse) (width "100%"))]{
;;> @tr{@th{name} @th{pros} @th{cons}}
;;> @tr{@td{environment variables}
;;>     @td{implicit - no need to retype; can share between applications}
;;>     @td{unclear when set; unexpected differences between users; limited size}}
;;> @tr{@td{command-line options}
;;>     @td{explicit - visible each time a command is run; }
;;>     @td{verbose; limited size}}
;;> @tr{@td{config files}
;;>     @td{implicit; preserved - can be shared and version controlled}
;;>     @td{requires a parser}}
;;> }
;;>
;;> Environment variables are convenient for broad preferences, used
;;> by many different applications, and unlikely to change per user.
;;> Command-line options are best for settings that are likely to
;;> change between invocations of a program.  Anything else is best
;;> stored in a config file.  If there are settings that multiple
;;> users of a group or whole system are likely to want to share, then
;;> it makes sense to cascade multiple config files.

;;> @subsubsection{Syntax}
;;>
;;> With any other language there is a question of config file syntax,
;;> and a few popular choices exist such as .ini syntax.  With Scheme
;;> the obvious choice is sexps, generally as an alist.  We use a
;;> single alist for the whole file, with symbols for keys and
;;> arbitrary sexps for values.  The alists are intended primarily for
;;> editing by hand and need not be dotted, but the interface allows
;;> dotted values.  Disambiguation is handled as with two separate
;;> functions, @scheme{(conf-get config key)} and
;;> @scheme{(conf-get-list config key)}, which both retrieve the value
;;> associated with @var{key} from @var{config}, in the latter case
;;> coercing to a list.  The result is determined according to the
;;> structure of the alist cell as follows:
;;>
;;> @table[(^ (border 1) (style border-collapse:collapse) (width "100%"))]{
;;> @tr{@th{Cell} @th{@scheme{conf-get} result} @th{@scheme{conf-get-list} result}}
;;> @tr{@td{@scheme{(key)}} @td{@scheme{()}} @td{@scheme{()}}}
;;> @tr{@td{@scheme{(key . non-list-value)}} @td{@scheme{non-list-value}} @td{@scheme{(non-list-value)}}}
;;> @tr{@td{@scheme{(key non-list-value)}} @td{@scheme{non-list-value}} @td{@scheme{(non-list-value)}}}
;;> @tr{@td{@scheme{(key (value1 value2 ...))}} @td{@scheme{(value1 value2 ...)}} @td{@scheme{(value1 value2 ...)}}}
;;> @tr{@td{@scheme{(key value1 value2 ...)}} @td{@scheme{(value1 value2 ...)}} @td{@scheme{(value1 value2 ...)}}}
;;> }
;;>
;;> Thus writing the non-dotted value will always do what you want.
;;> Specifically, the only thing to be careful of is if you want a
;;> single-element list value, even with @scheme{conf-get}, you should
;;> write @scheme{(key (value))}.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> @subsubsection{Interface}

;;> Returns true iff @var{x} is a config object.

(define-record-type Config
  (make-conf alist parent source timestamp)
  conf?
  (alist conf-alist conf-alist-set!)
  (parent conf-parent conf-parent-set!)
  (source conf-source conf-source-set!)
  (timestamp conf-timestamp conf-timestamp-set!))

(define (assq-tail key alist)
  (let lp ((ls alist))
    (and (pair? ls)
         (if (and (pair? (car ls)) (eq? key (caar ls)))
             ls
             (lp (cdr ls))))))

(define (assq-chain key alist)
  (let ((x (assq-tail (car key) alist)))
    (and x
         (if (null? (cdr key))
             (car x)
             (or (assq-chain (cdr key) (cdar x))
                 (assq-chain key (cdr x)))))))

(define (assq-split key alist)
  (let lp ((ls alist) (rev '()))
    (cond
     ((null? ls) #f)
     ((and (pair? (car ls)) (eq? key (caar ls))) (cons (reverse rev) ls))
     (else (lp (cdr ls) (cons (car ls) rev))))))

(define (read-from-file file . opt)
  (guard (exn (else (and (pair? opt) (car opt))))
    (call-with-input-file file read)))

(define (alist? x)
  (and (list? x) (every pair? x)))

;;> Returns just the base of @var{config} without any parent.

(define (conf-head config)
  (make-conf
   (conf-alist config) #f (conf-source config) (conf-timestamp config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading from files.

;;> @subsubsubsection{@rawcode{(conf-load file [conf])}}

;;> Loads the config file @var{file}, prepending to @var{conf} if
;;> provided.

(define (conf-load file . o)
  (make-conf
   (read-from-file file '())
   (if (pair? o) (car o) '())
   file
   (current-second)))

;;> Search for and load any files named @var{file} in the
;;> @var{config-path}, which should be a list of strings.

(define (conf-load-in-path config-path file)
  (cond
   ((equal? file "")
    (error "can't load from empty filename" file))
   ((eqv? #\/ (string-ref file 0))
    (conf-load file))
   (else
    (let lp ((ls (reverse config-path)) (res #f))
      (if (null? ls)
          (or res (make-conf '() #f #f (current-second)))
          (let ((path (string-append (car ls) "/" file)))
            (if (file-exists? path)
                (lp (cdr ls) (conf-load path res))
                (lp (cdr ls) res))))))))

;;> @subsubsubsection{@rawcode{(conf-load-cascaded config-path file [include-keyword])}}

;;> Similar to conf-load-in-path, but also recursively loads any
;;> "include" config files, indicated by a top-level
;;> @var{include-keyword} with either a string or symbol value.
;;> Includes are loaded relative to the current file, and cycles
;;> automatically ignored.

(define (conf-load-cascaded config-path file . o)
  (define (path-directory file)
    (let lp ((i (string-length file)))
      (cond ((zero? i) "./")
            ((eqv? #\/ (string-ref file (- i 1))) (substring file 0 i))
            (else (lp (- i 1))))))
  (define (path-relative file from)
    (if (eqv? #\/ (string-ref file 0))
        file
        (string-append (path-directory from) file)))
  (let ((include-keyword (if (pair? o) (car o) 'include)))
    (let load ((ls (list (cons file (and (pair? o) (pair? (cdr o)) (cadr o)))))
               (seen '())
               (res '()))
      (cond
       ((null? ls)
        res)
       (else
        (let ((file (if (symbol? (caar ls))
                        (symbol->string (caar ls))
                        (caar ls)))
              (depth (cdar ls)))
          (cond
           ((member file seen)
            (load (cdr ls) seen res))
           ((and (number? depth) (<= depth 0))
            (load (cdr ls) seen res))
           (else
            (let* ((config (conf-load-in-path config-path file))
                   (includes (conf-get-list config include-keyword)))
              (load (append (cdr ls)
                            (map (lambda (x)
                                   (cons (path-relative x file)
                                         (and (number? depth) (- depth 1))))
                                 includes))
                    (cons file seen)
                    (append res config)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (conf-get-cell config key)
  (cond
   ((pair? key)
    (if (null? (cdr key))
        (conf-get-cell config (car key))
        (any (lambda (x) (assq-chain key x)) config)))
   (else
    (let search ((config config))
      (and config
           (or (assq key (conf-alist config))
               (search (conf-parent config))))))))

;;> @subsubsubsection{@rawcode{(conf-get config key [default])}}

;;> Basic config lookup - retrieves the value from @var{config}
;;> associated with @var{key}.  If not present, return @var{default}.
;;> In @scheme{conf-get} and related accessors @var{key} can be either
;;> a symbol, or a list of symbols.  In the latter case, each symbol
;;> is used as a key in turn, with the value taken as an alist to
;;> further lookup values in.

(define (conf-get config key . opt)
  (let ((cell (conf-get-cell config key)))
    (if (not cell)
        (and (pair? opt) (car opt))
        (if (and (pair? (cdr cell)) (null? (cddr cell)))
            (cadr cell)
            (cdr cell)))))

;;> @subsubsubsection{@rawcode{(conf-get-list config key [default])}}

;;> Equivalent to @scheme{conf-get} but coerces its result to a list
;;> as described in the syntax section.

(define (conf-get-list config key . opt)
  (let ((res (conf-get config key)))
    (if res
        (if (or (pair? res) (null? res)) res (list res))
        (if (pair? opt) (car opt) '()))))

;;> Equivalent to @scheme{conf-get-list} but returns a list of all

(define (conf-get-multi config key)
  (if (not config)
      '()
      (append (conf-get-list (conf-head config))
              (conf-get-multi (conf-parent config) key))))

;;> Extends the config with anadditional alist.

(define (conf-extend config alist . o)
  (if (pair? alist)
      (make-conf alist config (and (pair? o) (car o)) (current-second))
      config))

;;> Joins two configs.

(define (conf-append a b)
  (let ((parent (if (conf-parent a) (conf-append (conf-parent a) b) b)))
    (make-conf (conf-alist a) parent (conf-source a) (conf-timestamp a))))

;;> Utility to create an alist cell representing the chained key
;;> @var{key} mapped to @var{value}.

(define (conf-unfold-key key value)
  (if (null? (cdr key))
      (cons (car key) value)
      (list (car key) (conf-unfold-key (cdr key) value))))

;;> Replace a new definition into the first config alist.

(define (conf-set config key value)
  (make-conf
   (let lp ((key (if (not (list? key)) (list key) key))
            (alist (conf-alist config)))
     (cond
      ((null? (cdr key))
       (cons (cons (car key) value)
             (remove (lambda (x) (and (pair? x) (eq? (car key) (car x))))
                     alist)))
      ((assq-split (car key) alist)
       => (lambda (x)
            (let ((left (car x))
                  (right (cdr x)))
              (append left
                      (cons (cons (car key) (lp (cdr key) (cdar right)))
                            (cdr right))))))
      (else
       (cons (conf-unfold-key key value) alist))))
   (conf-parent config)
   (conf-source config)
   (conf-timestamp config)))

;;> Lift specialized sections to the top-level of a config.

(define (conf-specialize config key name)
  (let lp ((ls config) (res '()))
    (cond
     ((null? ls) (reverse res))
     ((assq key (car ls))
      => (lambda (specialized)
           (let ((named (assq name (cdr specialized))))
             (if named
                 (lp (cdr ls) (cons (car ls) (cons (cdr named) res)))
                 (lp (cdr ls) (cons (car ls) res))))))
     (else (lp (cdr ls) (cons (car ls) res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> @subsubsection{Config Verification}

(define (conf-default-warn . args)
  (for-each
   (lambda (a) ((if (string? a) display write) a (current-error-port)))
   args)
  (newline (current-error-port))
  #f)

(define (conf-verify-symbol->predicate sym)
  (case sym
    ((integer) integer?)
    ((number) number?)
    ((list) list?)
    ((alist) alist?)
    ((boolean) boolean?)
    ((char) char?)
    ((string) string?)
    ((symbol) symbol?)
    ((pair) pair?)
    ((filename dirname) string?)
    (else (error "no known conf predicate for" sym))))

;; non-short-circuit versions to report all warnings

(define (and* . args)
  (every (lambda (x) x) args))

(define (every* pred ls)
  (apply and* (map pred ls)))

(define (conf-verify-match def cell warn)
  (define (cell-value)
    (if (and (pair? (cdr cell)) (null? (cddr cell))) (cadr cell) (cdr cell)))
  (define (cell-list)
    (if (and (pair? (cdr cell)) (null? (cddr cell)) (not (pair? (cadr cell))))
        (list (cadr cell))
        (cdr cell)))
  (cond
   ((procedure? def)
    (or (def (cell-value))
        (warn "bad conf value for " (car cell) ": " (cell-value))))
   ((symbol? def)
    (case def
      ((existing-filename)
       (cond
        ((not (string? (cell-value)))
         (warn "bad conf value for " (car cell)
               ": expected a filename but got " (cell-value)))
        ((not (file-exists? (cell-value)))
         (warn "conf setting ~S references a non-existent file: ~S"
               (car cell) (cell-value)))
        (else
         #t)))
      ((existing-dirname)
       (cond
        ((not (string? (cell-value)))
         (warn "bad conf value for " (car cell)
               ": expected a dirname but got " (cell-value)))
        ((not (file-directory? (cell-value)))
         (cond
          ((file-exists? (cell-value))
           (warn "conf setting " (car cell)
                 " expected a directory but found a file: " (cell-value)))
          (else
           (warn "conf setting " (car cell)
                 " references a non-existent directory: " (cell-value)))))
        (else
         #t)))
      ((integer number char string symbol filename dirname boolean pair)
       (or ((conf-verify-symbol->predicate def) (cell-value))
           (warn "bad conf value for " (car cell)
                 ": expected " def " but got " (cell-value))))
      ((list alist)
       (or ((conf-verify-symbol->predicate def) (cell-list))
           (warn "bad conf value for " (car cell)
                 ": expected " def " but got " (cell-list))))
      (else
       (warn "bad conf spec list: " def))))
   ((pair? def)
    (case (car def)
      ((cons)
       (and*
        (conf-verify-match
         (cadr def) (cons `(car ,(car cell)) (car (cell-list))) warn)
        (conf-verify-match
         (car (cddr def)) (cons `(car ,(car cell)) (cdr (cell-list))) warn)))
      ((list)
       (and (list? (cell-list))
            (every* (lambda (x)
                      ;; (cons `(list ,(car cell)) x)
                      (conf-verify-match (cadr def) x warn))
                    (cell-list))))
      ((alist)
       (let ((key-def (cadr def))
             (val-def (if (pair? (cddr def)) (car (cddr def)) (lambda (x) #t))))
         (and (alist? (cell-list))
              (every* (lambda (x)
                        (and (pair? x)
                             (conf-verify-match key-def (car x) warn)
                             (conf-verify-match val-def (cell-value x) warn)))
                      (cell-list)))))
      ((conf)
       (and (alist? (cell-list))
            (conf-verify (cdr def) (list (cell-list)) warn)))
      ((or)
       (or (any (lambda (x) (conf-verify-match x cell (lambda (x) x)))
                (cdr def))
           (warn "bad spec value for " (car cell)
                 ": expected " def " but got " (cell-value))))
      ((member)
       (or (member (cell-value) (cdr def))
           (warn "bad spec value " (cell-value)
                 " for " (car cell) ", expected one of " (cdr def))))
      ((quote)
       (or (equal? (cadr def) (cell-value))
           (warn "bad conf value for " (car cell)
                 ": expected '" (cadr def) " but got " (cell-value))))
      (else
       (warn "bad conf list spec name: " (car def)))))
   (else
    (or (equal? def (cell-value))
        (warn "bad conf value for " (car cell)
              ": expected " def " but got " (cell-value))))))

(define (conf-verify-one spec cell warn)
  (cond
   ((not (pair? cell))
    (warn "bad config entry: " cell))
   ((not (symbol? (car cell)))
    (warn "non-symbol config entry name: " (car cell)))
   (else
    (let ((def (assq (car cell) spec)))
      (cond
       ((not def)
        (warn "unknown config entry: " (car cell)))
       ((null? (cdr def)))
       (else (conf-verify-match (cadr def) cell warn)))))))

(define (conf-verify spec config . o)
  (let ((warn (if (pair? o)
                  (lambda args (apply (car o) args) #f)
                  conf-default-warn)))
    (let lp ((config config))
      (cond
       (config
        (for-each
         (lambda (cell) (conf-verify-one spec cell warn))
         (conf-alist config))
        (lp (conf-parent config)))))))
