;; Copyright (c) 2012 Alan Watson. All rights reserved. BSD-style
;; license: http://synthcode.com/license.txt

;; This library implements procedures that give the TAI to UTC offset a
;; specified instant in the UTC or TAI timescales.

(define-library (scheme time tai-to-utc-offset)
  
  (export tai-to-utc-offset-at-utc-day
          tai-to-utc-offset-at-tai-second
          set-open-leap-seconds-list-port!
          set-update-exception-handler!)
  
  (import (scheme base))
  (import (scheme file))
  (import (scheme read))
  (import (scheme process-context))
  
  (cond-expand
   (threads
    (import (srfi 18)))
   (else
    (begin
      (define (make-thread thunk) #f)
      (define (thread-start! th) #f)
      (define (thread-sleep! secs) #f))))
  
  (cond-expand
   (chibi
    (begin
      (define *file-name-environment-variable* "SEXP_LEAP_SECONDS_LIST_FILE")))
   (else
    (begin
      (error "Need to define *file-name-environment-variable*."))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (cond-expand
   
   ;; We ensure thread-safe atomic access to mutated bindings using
   ;; atomic boxes.
   
   (chibi
    (begin
      
      ;; This implementation relies on record accessors and mutators
      ;; being atomic in Chibi Scheme.
      
      (define-record-type atomic-box-record-type
        (make-atomic-box value)
        atomic-box?
        (value atomic-box-value atomic-box-value-set!))))
   
   (else
    (begin
      
      ;; This implementation uses SRFI-18 mutexes.
      
      (define (make-atomic-box value)
        (let ((mutex (make-mutex)))
          (mutex-specific-set! mutex value)
          mutex))
      
      (define atomic-box? mutex?)
      
      (define (atomic-box-value mutex)
        (mutex-lock! mutex)
        (let ((value (mutex-specific mutex)))
          (mutex-unlock! mutex)
          value))
      
      (define (atomic-box-value-set! mutex value)
        (mutex-lock! mutex)
        (mutex-specific-set! mutex value)
        (mutex-unlock! mutex)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (begin
    
    (define seconds-per-day 86400)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; First, a remark on timescales.
    ;;
    ;; The TAI timescale used in this library has an epoch of 1970-01-01
    ;; 00:00:00 TAI. The epoch used by the current-second procedure in
    ;; the draft R7RS (scheme time) library is the "TAI-10" timescale
    ;; with an epoch of 1970-01-01 00:00:10 TAI.
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; The library maintains a cache of values derived from the leap
    ;; seconds list.
    ;;
    ;; The first cached value is the utc-day-alist. This is an alist
    ;; whose cars are the UTC days since 1970-01-01 00:00:00 UTC and
    ;; whose cdrs are the corresponding TAI to UTC offsets.
    ;;
    ;; The second cached value is the tai-second-alist. This is an alist
    ;; whose cars are the TAI seconds since 1970-01-01 00:00:00 TAI and
    ;; whose cdrs are the corresponding TAI to UTC offsets.
    
    (define-record-type cache-record-type
      (make-cache utc-day-alist tai-second-alist)
      cache?
      (utc-day-alist cache-utc-day-alist)
      (tai-second-alist cache-tai-second-alist))
    
    (define (make-cache-from-port port)
      (let ((utc-day-alist (read-leap-seconds-list port)))
        (make-cache utc-day-alist
                    (utc-day-alist->tai-second-alist utc-day-alist))))
    
    ;; The utc-day-alist->tai-second-alist procedure converts an alist
    ;; indexed by the number of UTC day since 1970-01-01 00:00:00 UTC
    ;; into an equivalent alist indexed by the number of TAI seconds
    ;; since 1970-01-01 00:00:00 TAI.
    ;;
    ;; This procedure does not have to worry about the complications of
    ;; transforming UTC dates prior to 1972-01-01 00:00:00 UTC to TAI,
    ;; since there were no leap seconds prior to this date.
    
    (define (utc-day-alist->tai-second-alist utc-day-alist)
      (map
       (lambda (p)
         (let* ((utc-day      (car p))
                (leap-seconds (cdr p))
                (tai-second   (+ (* utc-day seconds-per-day) leap-seconds)))
           (cons tai-second leap-seconds)))
       utc-day-alist))
    
    ;; The library updates the cache: when the library is loaded; when
    ;; the set-open-leap-seconds-list-port! is called; and once per day.
    ;; These automatic periodic updates are useful in long-running
    ;; programs.
    ;;
    ;; The choice of daily updates is motivated by the following
    ;; considerations. ITU-R TF.460-6, which contains the current
    ;; definition of UTC, requires that the IERS should announce leap
    ;; seconds with at least eight weeks in advance. However, NIST,
    ;; which maintains the leap-seconds.list file, only guarantees one
    ;; month. Recently the IERS and NIST have managed six months of
    ;; notice, but we should not rely on this.
    ;;
    ;; During updates, we install the update-exception-handler.
    
    (define *cache-lifetime* (* 1 seconds-per-day))
    
    (define (update-cache! open-port)
      (with-exception-handler
          (update-exception-handler)
        (lambda ()
          (let ((port (open-port)))
            (when port
              (set-cache! (make-cache-from-port port)))))))
    
    (thread-start!
     (make-thread
      (lambda ()
        (let loop ()
          (thread-sleep! *cache-lifetime*)
          (update-cache! (open-leap-seconds-list-port))
          (loop)))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; The read-leap-seconds-list procedure reads text from the port
    ;; argument. The text must follow the format of a NIST leap seconds
    ;; file, for example,
    ;;
    ;;   ftp://time.nist.gov/pub/leap-seconds.list
    ;;
    ;; The procedure returns an alist. The cars of the pairs are the
    ;; number of whole UTC days since 1970-01-01 00:00:00 UTC and the
    ;; cdrs of the pairs are the corresponding TAI to UTC offset. The
    ;; alist is ordered by decreasing car. The cars and cdrs are exact
    ;; integers.
    ;;
    ;; TODO: Do not rely on the input file being correctly ordered. Do
    ;; not rely on the input data being exact integers.
    ;;
    ;; TODO: Check known leap seconds.
    
    (define (read-leap-seconds-list port)
      
      (define (ntp-second->utc-day ntp-second)
        ;; The NTP initial epoch is 1900-01-01 00:00:00 UTC. The UTC
        ;; initial epoch is 1970-01-01 00:00:00 UTC. There are 70 years,
        ;; containing 17 leap days, between these epochs.
        (- (quotient ntp-second seconds-per-day) (* 70 365) 17))
      
      (define (leap-seconds-line->pair line)
        (let* ((line-port         (open-input-string line))
               (ntp-second        (read line-port))
               (tai-to-utc-offset (read line-port))
               (utc-day           (ntp-second->utc-day ntp-second))
               (leap-seconds      tai-to-utc-offset))
          (cons utc-day leap-seconds)))
      
      (define (leap-seconds-comment-line? line)
        (char=? #\# (string-ref line 0)))
      
      (define (read-leap-seconds-line port)
        (let ((line (read-line port)))
          (cond
           ((eof-object? line) line)
           ((leap-seconds-comment-line? line) (read-leap-seconds-line port))
           (else line))))
      
      (let loop ((alist '()))
        (let ((line (read-leap-seconds-line port)))
          (if (eof-object? line)
            alist
            (loop (cons (leap-seconds-line->pair line) alist))))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; (set-open-leap-seconds-list-port! p)
    ;;
    ;; The set-open-leap-seconds-list-port! procedure sets the value of
    ;; procedure called to obtain the leap second list is read to its
    ;; argument p and then performs a cache update. The procedure should
    ;; return either #f, signifying that no leap second list is
    ;; available, or an input port, from which a leap-second list,
    ;; following the format of the NIST leap-second list, will be read.
    ;; The NIST leap-second file can be found here:
    ;;
    ;;   ftp://time.nist.gov/pub/leap-seconds.list
    ;;
    ;; The default procedure attempts to open the file named by the
    ;; environment variable SEXP_LEAP_SECOND_LIST_FILE and returns the
    ;; port. If the environment variable is not set or if the file does
    ;; not exist, it return #f.
    
    (define *open-leap-seconds-list-port*
      (make-atomic-box
       (lambda ()
         (when *file-name-environment-variable*
           (let ((file-name
                  (get-environment-variable *file-name-environment-variable*)))
             (if file-name
               (open-input-file file-name)
               #f))))))
    
    (define (open-leap-seconds-list-port)
      (atomic-box-value *open-leap-seconds-list-port*))
    
    (define (set-open-leap-seconds-list-port! p)
      (atomic-box-value-set! *open-leap-seconds-list-port* p)
      (update-cache! (open-leap-seconds-list-port)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; (set-update-exception-handler! p)
    ;;
    ;; The set-update-exception-handler! procedure sets the value of the
    ;; error handler installed during cache updates. The default error
    ;; handler simply raises the exception again.
    
    (define *update-exception-handler*
      (make-atomic-box raise))
    
    (define (update-exception-handler)
      (atomic-box-value *update-exception-handler*))
    
    (define (set-update-exception-handler! p)
      (atomic-box-value-set! *update-exception-handler* p))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define *cache*
      (make-atomic-box #f))
    
    (define (cache)
      (atomic-box-value *cache*))
    
    (define (set-cache! value)
      (atomic-box-value-set! *cache* value))
    
    (update-cache!
     (lambda ()
       (open-input-string
        (string-append
         
         ;; These strings are lines extracted from:
         ;;
         ;;   ftp://time.nist.gov/pub/leap-seconds.3535228800
         ;;
         ;; The original file contains extensive comments on the format
         ;; and provenance of the data, which have been removed from
         ;; this version.
         
         "2272060800 10  # 1 Jan 1972\n"
         "2287785600 11  # 1 Jul 1972\n"
         "2303683200 12  # 1 Jan 1973\n"
         "2335219200 13  # 1 Jan 1974\n"
         "2366755200 14  # 1 Jan 1975\n"
         "2398291200 15  # 1 Jan 1976\n"
         "2429913600 16  # 1 Jan 1977\n"
         "2461449600 17  # 1 Jan 1978\n"
         "2492985600 18  # 1 Jan 1979\n"
         "2524521600 19  # 1 Jan 1980\n"
         "2571782400 20  # 1 Jul 1981\n"
         "2603318400 21  # 1 Jul 1982\n"
         "2634854400 22  # 1 Jul 1983\n"
         "2698012800 23  # 1 Jul 1985\n"
         "2776982400 24  # 1 Jan 1988\n"
         "2840140800 25  # 1 Jan 1990\n"
         "2871676800 26  # 1 Jan 1991\n"
         "2918937600 27  # 1 Jul 1992\n"
         "2950473600 28  # 1 Jul 1993\n"
         "2982009600 29  # 1 Jul 1994\n"
         "3029443200 30  # 1 Jan 1996\n"
         "3076704000 31  # 1 Jul 1997\n"
         "3124137600 32  # 1 Jan 1999\n"
         "3345062400 33  # 1 Jan 2006\n"
         "3439756800 34  # 1 Jan 2009\n"
         "3550089600 35  # 1 Jul 2012\n"))))
    
    (update-cache! (open-leap-seconds-list-port))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; (tai-to-utc-offset-at-utc-day utc-day)
    ;;
    ;; The tai-to-utc-offset-at-utc-day procedure returns the TAI to UTC
    ;; offset at the instant specified by its argument utc-day. The
    ;; instant is specified by the number of UTC days since 1970-01-01
    ;; 00:00:00 UTC.
    ;;
    ;; To convert a UTC time since 1972-01-01 00:00:00 UTC to number of
    ;; TAI seconds since 1972-01-01 00:00:00 TAI, first find the number
    ;; of whole UTC days D since 1970-01-01 00:00:00 UTC and the number
    ;; of UTC seconds S since the start of the current day. The number
    ;; of TAI seconds since 1972-01-01 00:00:00 TAI is then
    ;;
    ;;   (+ (* D 86400) S (tai-to-utc-offset-at-utc-day D))
    ;;
    ;; This implementation does not return the correct result for
    ;; instants prior to 1972-01-01 00:00:00 UTC.
    ;;
    ;; Converting a UTC time prior to 1972-01-01 00:00:00 UTC to TAI is
    ;; more involved, since prior to this date UTC and TAI seconds were
    ;; not equal. See,
    ;;
    ;;   http://hpiers.obspm.fr/eop-pc/index.php?index=TAI-UTC_tab
    
    (define (tai-to-utc-offset-at-utc-day-loop utc-day alist)
      (cond ((null? alist) 10)
            ((>= utc-day (caar alist)) (cdar alist))
            (else (tai-to-utc-offset-at-utc-day-loop utc-day (cdr alist)))))
    
    (define (tai-to-utc-offset-at-utc-day utc-day)
      (tai-to-utc-offset-at-utc-day-loop
       utc-day
       (cache-utc-day-alist (cache))))
    
    ;; (tai-to-utc-offset-at-tai-second tai-second)
    ;;
    ;; The tai-to-utc-offset-at-tai-second procedure returns TAI to UTC
    ;; offset at the instant specified by its argument tai-second. The
    ;; instant is specified by the number of TAI seconds since
    ;; 1970-01-01 00:00:00 TAI.
    ;;
    ;; This implementation does not return the correct result for
    ;; instants prior to 1972-01-01 00:00:00 UTC.
    
    (define (tai-to-utc-offset-at-tai-second-loop tai-second alist)
      (cond
       ((null? alist) 10)
       ((>= tai-second (caar alist)) (cdar alist))
       (else (tai-to-utc-offset-at-tai-second-loop tai-second (cdr alist)))))
    
    (define (tai-to-utc-offset-at-tai-second tai-second)
      (tai-to-utc-offset-at-tai-second-loop
       tai-second
       (cache-tai-second-alist (cache))))))
