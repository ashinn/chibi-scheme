;; memoize.scm -- caching and memoization utilities
;; Copyright (c) 2003-2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Memory and persistent caching with various levels of control, based
;;> on a combination of lru-cache from Hato and an older memoization
;;> library for Gauche.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types

(define-record-type Lru-Cache
  (%make-lru-cache table front back size size-limit compute-size)
  lru-cache?
  (table lru-table)
  (front lru-front lru-front-set!)
  (back lru-back lru-back-set!)
  (size lru-size lru-size-set!)
  (size-limit lru-size-limit)
  (compute-size lru-compute-size))

(define-record-type Lru-Entry
  (make-lru-entry key value size prev)
  lru-entry?
  (key lru-entry-key)
  (value lru-entry-value lru-entry-value-set!)
  (size lru-entry-size lru-entry-size-set!)
  (prev lru-entry-prev lru-entry-prev-set!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-level interface

;;> Analagous to the procedure form of \scheme{define} but
;;> automatically memoizes the function.  Uses \scheme{equal?} for
;;> equality comparisons and reasonable defaults - for finer grained
;;> control use \scheme{memoize}.

(define-syntax define-memoized
  (syntax-rules ()
    ((define-memoized (proc x ...) . body)
     (define proc
       (make-memoizer (lambda (x ...) . body) (length '(x ...)) (make-lru-cache))))
    ((define-memoized (proc . x) . body)
     (define proc
       (make-memoizer (lambda x . body) #f (make-lru-cache ))))))

(define (make-memoizer proc arity cache)
  (let ((ref! (if (lru-cache? cache) lru-ref! hash-table-ref!)))
    (case arity
      ((0)
       proc)
      ((1)
       (lambda (x) (ref! cache x proc)))
      ((2)
       (lambda (x y)
         (ref! cache (cons x y) (lambda (xy) (proc (car xy) (cdr xy))))))
      (else
       (lambda args
         (ref! cache args (lambda (args) (apply proc args))))))))

;;> Returns a memoized version of the procedure \var{proc}.  By
;;> default uses a least-recently-used (LRU) cache, which can be tuned
;;> with the following keyword arguments:
;;>
;;> \items[
;;> \item{cache: an explicit pre-existing cache (LRU or hash-table)}
;;> \item{equal: an equality predicate defaulting to \scheme{equal?}}
;;> \item{hash: a hash function to match the equality predicate, defaulting to \scheme{hash} from \scheme{(srfi 69)}}
;;> \item{init-size: a hint for the initial size of the backing hash table}
;;> \item{size-limit: the maximum size of the cache}
;;> \item{compute-size: compute the size of a cache entry}
;;> ]
;;>
;;> \var{compute-size} is a procedure of two arguments, the key and
;;> value to be stored, and defaults to a constant 1 per entry.  After
;;> every insertion the oldest elements will be removed until the size
;;> is under \var{size-limit}.  You may find
;;>
;;>   \scheme{(lambda (k v) (+ (object-size k) (object-size v)))}
;;>
;;> using \scheme{object-size} from \scheme{(chibi ast)} to be a
;;> useful \var{compute-size}.
;;>
;;> If \var{size-limit} is \scheme{#f} then the cache is unlimited,
;;> and a simple hash-table will be used in place of an LRU cache.

(define (memoize proc . o)
  (let-keywords* o
      ((equal equal: equal?)
       (hash hash: hash)
       (arity arity: (and (not (procedure-variadic? proc))
                          (procedure-arity proc)))
       (init-size init-size: 31)
       (limit size-limit: 1000)
       (compute-size compute-size: (lambda (k v) 1))
       (cache-init cache: '()))
    (let ((cache (cond ((lru-cache? cache-init)
                        cache-init)
                       (limit
                        (make-lru-cache 'equal: equal
                                        'hash: hash
                                        'init-size: init-size
                                        'size-limit: limit
                                        'compute-size: compute-size))
                       (else
                        (make-hash-table equal hash)))))
      ;; allow an alist initializer for the cache
      (if (pair? cache-init)
          (for-each (lambda (x) (lru-add! cache (car x) (cdr x)))
                    cache-init))
      (make-memoizer proc arity cache))))

;;> Equivalent to memoize except that the procedure's first argument
;;> must be a pathname.  If the corresponding file has been modified
;;> since the memoized value, the value is recomputed.  Useful to
;;> automatically reflect external changes to a file-backed resource.
;;> The additional keyword argument \scheme{reloader?:}, if true,
;;> indicates that the result of loading is itself a procedure which
;;> should check for updates on each call.

(define (memoize-file-loader proc . o)
  (let* ((f (lambda (file . rest)
              (let ((mtime (file-modification-time file)))
                (cons mtime (apply proc file rest)))))
         (g (apply memoize f o))
         (reloader? (cond ((memq 'reloader?: o) => cdr) (else #f))))
    (lambda (file . rest)
      (let ((cell (apply g file rest)))
        (let-syntax ((update!
                      (syntax-rules ()
                        ((update! default)
                         (let ((mtime (file-modification-time file)))
                           (if (> mtime (car cell))
                               (let ((res (apply proc file rest)))
                                 (set-car! cell mtime)
                                 (set-cdr! cell res)
                                 res)
                               default))))))
          (update! (if (and reloader? (procedure? (cdr cell)))
                       (lambda args (apply (update! (cdr cell)) args))
                       (cdr cell))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent memoization

(define (get-memo-directory proc-name)
  (let ((uid (current-user-id)))
    (if (zero? uid)
        (make-path "/var/run/memo.d" proc-name)
        (make-path (user-home (user-information uid)) ".memo.d" proc-name))))

(define (encode-file-name str)
  (define (file-name-safe-char? ch)
    (or (char-alphabetic? ch) (char-numeric? ch)
        (memv ch '(#\_ #\- #\+ #\. #\,))))
  (define (encode-char ch)
    (let* ((i (char->integer ch))
           (hex (number->string i 16)))
      (if (< i 16)
          (string-append "%0" hex)
          (string-append "%" hex))))
  (define (collect str from to res)
    (if (>= from to)
        res
        (cons (substring-cursor str from to) res)))
  (let ((end (string-cursor-end str)))
    (let lp ((from 0) (to 0) (res '()))
      (if (string-cursor>=? to end)
          (if (zero? from)
              str
              (string-concatenate (reverse (collect str from to res))))
          (let* ((ch (string-cursor-ref str to))
                 (next (string-cursor-next str to)))
            (if (file-name-safe-char? ch)
                (lp from next res)
                (lp next next (cons (encode-char ch)
                                    (collect str from to res)))))))))

(define (default-args-encoder args)
  (encode-file-name
   (string-append (call-with-output-string (lambda (out) (write/ss args out)))
                  ".memo")))

;;> Returns a memoized version of the procedure \var{proc} which
;;> stores the memoized results persistently in a file.  Garbage
;;> collection of the files is left as an external task for monitoring
;;> tools or cron jobs.
;;>
;;> Accepts the following keyword arguments:
;;>
;;> \items[
;;> \item{args-encoder: procedure which takes the arguments as a single list, and returns a string representation suitable for use as a (base) file name}
;;> \item{proc-name: the name of the procedure, to use a a subdir of memo-dir to distinguish from other memoized procedures}
;;> \item{memo-dir: the directory to store results in, defaulting to ~/.memo/}
;;> \item{file-validator: validator to run on the existing file - if it returns false, the file is considered bad and the result recomputed}
;;> \item{validator: validator to run on the result of reading the file}
;;> \item{read: the read procedure to extract the result from the file}
;;> \item{write: the write procedure to write the result to the file}
;;> ]

(define (memoize-to-file proc . o)
  (let-keywords* o
      ((args-encoder args-encoder: default-args-encoder)
       (proc-name proc-name: (or (procedure-name proc) "lambda"))
       (memo-dir memo-dir: (get-memo-directory proc-name))
       (file-validator file-validator: (lambda args #t))
       (validator validator: (lambda args #t))
       (read read: read/ss)
       (write write: write/ss))
    (lambda args
      (let ((file (make-path memo-dir (apply args-encoder args))))
        (define (compute)
          (let ((res (apply proc args)))
            (create-directory* (path-directory file))
            (call-with-output-file file
              (lambda (out) (write res out)))
            res))
        (if (and (file-exists? file)
                 (apply file-validator file args))
            (let ((res (call-with-input-file file read)))
              (if (validator res)
                  res
                  (compute)))
            (compute))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; low-level utilities

;;> Creates a new empty LRU object.  The same keyword arguments as in
;;> \scheme{memoize} are available, except of course for \var{cache}.

(define (make-lru-cache . o)
  (let-keywords* o ((equal equal: equal?)
                    (hash hash: hash)
                    (init-size init-size: 31)
                    (compute-size compute-size: (lambda (k v) 1))
                    (size-limit size-limit: 1000))
    (let ((tab (make-hash-table equal hash init-size)))
      (%make-lru-cache tab '() '() 0 size-limit compute-size))))

;; add entry to the back of the queue
(define (lru-enq! lru entry)
  (let ((cell (list entry)))
    (if (null? (lru-front lru))         ; empty
        (lru-front-set! lru cell)
        (set-cdr! (lru-back lru) cell))
    (lru-back-set! lru cell)))

;; pop the front of the queue
(define (lru-deq! lru)
  (let ((cell (lru-front lru)))
    (if (null? cell)
        (error "lru queue is empty")
        (let ((rest (cdr cell)))
          (lru-front-set! lru rest)
          (if (null? rest)
              (lru-back-set! lru '()))
          (car cell)))))

;; shift the given entry, anywhere in the queue, to the end
(define (lru-shift-to-last! lru entry)
  (let ((prev (lru-entry-prev entry))
        (last-pair (lru-back lru)))
    (cond
     ((null? prev)
      ;; first entry, just pop and re-queue it, and update prev pointers
      (lru-enq! lru (lru-deq! lru))
      (lru-entry-prev-set! entry last-pair)
      (lru-entry-prev-set! (car (lru-front lru)) '()))
     ((eq? (cdr prev) last-pair)
      ;; already at the end, nothing to do
      )
     (else
      ;; a middle element, splice it out and re-queue
      (let ((cell (cdr prev)))
        (set-cdr! prev (cdr cell))      ; splice out
        (if (pair? (cdr cell))
            (lru-entry-prev-set! (cadr cell) prev))
        (lru-enq! lru entry)            ; reinsert at end
        (lru-entry-prev-set! entry last-pair))))))

(define (lru-shrink! lru)
  (let ((size-limit (lru-size-limit lru))
        (size (lru-size lru)))
    (if (> size size-limit)
        (let lp ((size size))
          (if (> size size-limit)
              (let ((x (lru-deq! lru)))
                (let ((next (lru-front lru)))
                  (if (pair? next)
                      (lru-entry-prev-set! (car next) '())))
                (hash-table-delete! (lru-table lru) (lru-entry-key x))
                (lp (- size (lru-entry-size x))))
              (lru-size-set! lru size))))))

;;> Looks up \var{key} in the cache LRU.  If not found returns #f,
;;> unless \var{compute} is given in which case \var{compute} is
;;> applied to \var{key} to determine the return value.  This does not
;;> update the cache.

(define (lru-ref lru key . o)
  (let ((entry (hash-table-ref/default (lru-table lru) key #f)))
    (cond (entry
           (lru-shift-to-last! lru entry)
           (lru-entry-value entry))
          ((pair? o)
           ((car o) key))
          (else
           (error "no lru entry for" key)))))

;;> Identical to lru-ref except that it updates the cache on a miss.

(define (lru-ref! lru key compute)
  (cond ((hash-table-ref/default (lru-table lru) key #f)
         => (lambda (entry)
              (lru-shift-to-last! lru entry)
              (lru-entry-value entry)))
        (else
         (let ((value (compute key)))
           (lru-add! lru key value)
           value))))

(define (lru-add! lru key value)
  (let* ((size ((lru-compute-size lru) key value))
         (last-pair (lru-back lru))
         (entry (make-lru-entry key value size last-pair)))
    (hash-table-set! (lru-table lru) key entry)
    (lru-enq! lru entry)
    (lru-size-set! lru (+ size (lru-size lru)))
    (lru-shrink! lru)))

;;> Directly set a value in the cache.

(define (lru-set! lru key value)
  (let ((entry (hash-table-ref/default (lru-table lru) key #f)))
    (cond (entry
           (lru-shift-to-last! lru entry)
           (lru-entry-value-set! entry value)
           (let ((prev-size (lru-entry-size entry))
                 (size ((lru-compute-size lru) key value)))
             (lru-entry-size-set! entry size)
             (lru-size-set! lru (+ (lru-size lru) (- size prev-size)))))
          (else
           (lru-add! lru key value)))
    (lru-shrink! lru)))

(define (hash-table-ref! table key proc)
  (hash-table-ref table key
                  (lambda ()
                    (let ((res (proc key)))
                      (hash-table-set! table key res)
                      res))))
