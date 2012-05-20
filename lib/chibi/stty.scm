;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> A high-level interface to stty and ioctl.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symbolic representation of attributes

(define stty-lookup (make-hash-table eq?))

(for-each
 (lambda (c)
   (let ((type (cadr c))
         (value (car (cddr c))))
     (hash-table-set! stty-lookup (car c) (cdr c))))

 ;; ripped from the stty man page, then trimmed down to what seemed
 ;; available on most systems

 `(;; characters
   ;;(dsusp    char     ,VDSUSP)   ; CHAR will send a terminal stop signal
   (eof      char     ,VEOF)     ; CHAR will send an EOF (terminate input)
   (eol      char     ,VEOL)     ; CHAR will end the line
   (eol2     char     ,VEOL2)    ; alternate CHAR for ending the line
   (erase    char     ,VERASE)   ; CHAR will erase the last character typed
   (intr     char     ,VINTR)    ; CHAR will send an interrupt signal
   (kill     char     ,VKILL)    ; CHAR will erase the current line
   (lnext    char     ,VLNEXT)   ; CHAR will enter the next character quoted
   (quit     char     ,VQUIT)    ; CHAR will send a quit signal
   (rprnt    char     ,VREPRINT) ; CHAR will redraw the current line
   (start    char     ,VSTART)   ; CHAR will restart output after stopping it
   (stop     char     ,VSTOP)    ; CHAR will stop the output
   (susp     char     ,VSUSP)    ; CHAR will send a terminal stop signal
   (werase   char     ,VWERASE)  ; CHAR will erase the last word typed

   ;; special settings
   (cols     special  #f) ; tell the kernel that the terminal has N columns
   (columns  special  #f) ; same as cols N
   (ispeed   special  #f) ; set the input speed to N
   (line     special  #f) ; use line discipline N
   (min      special  #f) ; with -icanon, set N characters minimum for a completed read
   (ospeed   special  #f) ; set the output speed to N
   (rows     special  #f) ; tell the kernel that the terminal has N rows
   (size     special  #f) ; print the number of rows and columns according to the kernel
   (speed    special  #f) ; print the terminal speed
   (time     special  #f) ; with -icanon, set read timeout of N tenths of a second

   ;; control settings
   (clocal   control  ,CLOCAL)  ; disable modem control signals
   (cread    control  ,CREAD)   ; allow input to be received
   (crtscts  control  ,CRTSCTS) ; enable RTS/CTS handshaking
   (cs5      control  ,CS5)     ; set character size to 5 bits
   (cs6      control  ,CS6)     ; set character size to 6 bits
   (cs7      control  ,CS7)     ; set character size to 7 bits
   (cs8      control  ,CS8)     ; set character size to 8 bits
   (cstopb   control  ,CSTOPB)  ; use two stop bits per character (one with `-')
   (hup      control  ,HUPCL)   ; send a hangup signal when the last process closes the tty
   (hupcl    control  ,HUPCL)   ; same as [-]hup
   (parenb   control  ,PARENB)  ; generate parity bit in output and expect parity bit in input
   (parodd   control  ,PARODD)  ; set odd parity (even with `-')

   ;; input settings
   (brkint   input    ,BRKINT)  ; breaks cause an interrupt signal
   (icrnl    input    ,ICRNL)   ; translate carriage return to newline
   (ignbrk   input    ,IGNBRK)  ; ignore break characters
   (igncr    input    ,IGNCR)   ; ignore carriage return
   (ignpar   input    ,IGNPAR)  ; ignore characters with parity errors
   (imaxbel  input    ,IMAXBEL) ; * beep and do not flush a full input buffer on a character
   (inlcr    input    ,INLCR)   ; translate newline to carriage return
   (inpck    input    ,INPCK)   ; enable input parity checking
   (istrip   input    ,ISTRIP)  ; clear high (8th) bit of input characters
   ;;(iuclc    input    ,IUCLC)   ; * translate uppercase characters to lowercase
   (ixany    input    ,IXANY)   ; * let any character restart output, not only start character
   (ixoff    input    ,IXOFF)   ; enable sending of start/stop characters
   (ixon     input    ,IXON)    ; enable XON/XOFF flow control
   (parmrk   input    ,PARMRK)  ; mark parity errors (with a 255-0-character sequence)
   (tandem   input    ,IXOFF)   ; same as [-]ixoff

   ;; output settings
   ;;(bs0      output   ,BS0) ; backspace delay style, N in [0..1]
   ;;(bs1      output   ,BS1) ; backspace delay style, N in [0..1]
   ;;(cr0      output   ,CR0) ; carriage return delay style, N in [0..3]
   ;;(cr1      output   ,CR1) ; carriage return delay style, N in [0..3]
   ;;(cr2      output   ,CR2) ; carriage return delay style, N in [0..3]
   ;;(cr3      output   ,CR3) ; carriage return delay style, N in [0..3]
   ;;(ff0      output   ,FF0) ; form feed delay style, N in [0..1]
   ;;(ff1      output   ,FF1) ; form feed delay style, N in [0..1]
   ;;(nl0      output   ,NL0) ; newline delay style, N in [0..1]
   ;;(nl1      output   ,NL1) ; newline delay style, N in [0..1]
   (ocrnl    output   ,OCRNL) ; translate carriage return to newline
   ;;(ofdel    output   ,OFDEL) ; use delete characters for fill instead of null characters
   ;;(ofill    output   ,OFILL) ; use fill (padding) characters instead of timing for delays
   ;;(olcuc    output   ,OLCUC) ; translate lowercase characters to uppercase
   (onlcr    output   ,ONLCR) ; translate newline to carriage return-newline
   (onlret   output   ,ONLRET) ; newline performs a carriage return
   (onocr    output   ,ONOCR) ; do not print carriage returns in the first column
   (opost    output   ,OPOST) ; postprocess output
   (tab0     output   #f) ; horizontal tab delay style, N in [0..3]
   (tab1     output   #f) ; horizontal tab delay style, N in [0..3]
   (tab2     output   #f) ; horizontal tab delay style, N in [0..3]
   (tab3     output   #f) ; horizontal tab delay style, N in [0..3]
   (tabs     output   #f) ; same as tab0
   ;;(-tabs    output   #f) ; same as tab3
   ;;(vt0      output   ,VT0) ; vertical tab delay style, N in [0..1]
   ;;(vt1      output   ,VT1) ; vertical tab delay style, N in [0..1]

   ;; local settings
   (crterase local    ,ECHOE)   ; echo erase characters as backspace-space-backspace
   (crtkill  local    ,ECHOKE)  ; kill all line by obeying the echoprt and echoe settings
   ;;(-crtkill local    #f) ; kill all line by obeying the echoctl and echok settings
   (ctlecho  local    ,ECHOCTL) ; echo control characters in hat notation (`^c')
   (echo     local    ,ECHO)    ; echo input characters
   (echoctl  local    ,ECHOCTL) ; same as [-]ctlecho
   (echoe    local    ,ECHOE)   ; same as [-]crterase
   ;;(echok    local    ,ECHOK)   ; echo a newline after a kill character
   (echoke   local    ,ECHOKE)  ; same as [-]crtkill
   (echonl   local    ,ECHONL)  ; echo newline even if not echoing other characters
   ;;(echoprt  local    ,ECHOPRT) ; echo erased characters backward, between `\' and '/'
   (icanon   local    ,ICANON)  ; enable erase, kill, werase, and rprnt special characters
   ;;(iexten   local    ,IEXTEN)  ; enable non-POSIX special characters
   (isig     local    ,ISIG)    ; enable interrupt, quit, and suspend special characters
   (noflsh   local    ,NOFLSH)  ; disable flushing after interrupt and quit special characters
   ;;(prterase local    ,ECHOPRT) ; same as [-]echoprt
   (tostop   local    ,TOSTOP)  ; stop background jobs that try to write to the terminal
   ;;(xcase    local    ,XCASE)   ; with icanon, escape with `\' for uppercase characters

   ;; combination settings
   (LCASE    combine  (lcase))
   (cbreak   combine  (not icanon))
   (cooked   combine  (brkint ignpar istrip icrnl ixon opost isig icanon))
                                        ; also eof and eol characters
                                        ; to their default values
   (crt      combine  (echoe echoctl echoke))
   (dec      combine  (echoe echoctl echoke (not ixany)))
                                        ; also intr ^c erase 0177 kill ^u
   (decctlq  combine  (ixany))
   (ek       combine  ()) ; erase and kill characters to their default values
   (evenp    combine  (parenb (not parodd) cs7))
   ;;(-evenp combine  #f) ; same as -parenb cs8
   (lcase    combine  (xcase iuclc olcuc))
   (litout   combine  (cs8 (not parenb istrip opost)))
   ;;(-litout  combine  #f) ; same as parenb istrip opost cs7
   (nl       combine  (not icrnl onlcr))
   ;;(-nl      combine  #f) ; same as icrnl -inlcr -igncr onlcr -ocrnl -onlret
   (oddp     combine  (parenb parodd cs7))
   (parity   combine  (evenp)) ; same as [-]evenp
   (pass8    combine  (cs8 (not parenb istrip)))
   ;;(-pass8   combine  #f) ; same as parenb istrip cs7
   (raw      combine  (not ignbrk brkint ignpar parmrk
                           inpck istrip inlcr igncr icrnl))
   (ixon     combine  (ixoff ixany imaxbel opost isig icanon)) ;; xcase iuclc
   ;;(time     combine  #f) ; 0
   ;;(-raw     combine  #f) ; same as cooked
   (sane     combine  (cread brkint icrnl imaxbel opost onlcr
                       isig icanon ;; nl0 cr0 bs0 vt0 ff0 ; tab0
                       echo echoe echoctl echoke ;; iexten echok
                       (not ignbrk igncr ixoff ixany inlcr ;; iuclc
                            ocrnl onocr onlret ;; olcuc ofill ofdel
                            echonl noflsh tostop echoprt))) ;; xcase
                                        ; plus all special characters to
                                        ; their default values
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-level interface

;;> @subsubsubsection{@scheme{(stty [port] args ...)}}

;;> Set the terminal attributes for @var{port} (default
;;> @scheme{(current-output-port)}) to @var{attrs}.
;;> Attributes are specified symbolically using the
;;> names from the @rawcode{stty(1)} command.  In addition,
;;> (not args ...) may be used to negate the listed symbols.

(define (stty . args)
  (let* ((port (if (and (pair? args) (port? (car args)))
                   (car args)
                   (current-output-port)))
         (attr (get-terminal-attributes port)))
    ;; parse change requests
    (let lp ((lst (if (and (pair? args) (port? (car args))) (cdr args) args))
             (iflag (term-attrs-iflag attr))
             (oflag (term-attrs-oflag attr))
             (cflag (term-attrs-cflag attr))
             (lflag (term-attrs-lflag attr))
             (invert? #f)
             (return (lambda (iflag oflag cflag lflag)
                       (term-attrs-iflag-set! attr iflag)
                       (term-attrs-oflag-set! attr oflag)
                       (term-attrs-cflag-set! attr cflag)
                       (term-attrs-lflag-set! attr lflag)
                       (set-terminal-attributes! port TCSANOW attr))))
      (define (join old new)
        (if invert? (bitwise-and old (bitwise-not new)) (bitwise-ior old new)))
      (cond
       ((pair? lst)
        (let ((command (car lst)))
          (cond
           ((pair? command) ;; recurse on sub-expr
            (lp command iflag oflag cflag lflag invert?
                (lambda (i o c l) (lp (cdr lst) i o c l invert? return))))
           ((eq? command 'not) ;; toggle current setting
            (lp (cdr lst) iflag oflag cflag lflag (not invert?) return))
           (else
            (let ((x (hash-table-ref/default stty-lookup command #f)))
              (case (and x (car x))
                ((input)
                 (lp (cdr lst) (join iflag (cadr x)) oflag cflag lflag invert? return))
                ((output)
                 (lp (cdr lst) iflag (join oflag (cadr x)) cflag lflag invert? return))
                ((control)
                 (lp (cdr lst) iflag oflag (join cflag (cadr x)) lflag invert? return))
                ((local)
                 (lp (cdr lst) iflag oflag cflag (join lflag (cadr x)) invert? return))
                ((char)
                 ;;(term-attrs-cc-set! attr (cadr x) (or (cadr lst) 0))
                 (lp (cddr lst) iflag oflag cflag lflag invert? return))
                ((combine)
                 (lp (cadr x) iflag oflag cflag lflag invert?
                     (lambda (i o c l) (lp (cdr lst) i o c l invert? return))))
                ((special)
                 (error "special settings not yet supported" command))
                (else
                 (error "unknown stty command" command))))))))
       (else
        (return iflag oflag cflag lflag))))))

;;> Run @var{thunk} with the @scheme{stty} @var{setting}s in effect
;;> during its dynamic extent, resetting the original settings
;;> when it returns.

(define (with-stty setting thunk . o)
  (let* ((port (if (pair? o) (car o) (current-input-port)))
         (orig-attrs (get-terminal-attributes port)))
    (cond
     (orig-attrs
      (dynamic-wind
        (lambda () (stty port setting))
        thunk
        (lambda () (set-terminal-attributes! port TCSANOW orig-attrs))))
     (else
      ;; No terminal attributes means this isn't a tty.
      (thunk)))))

;;> Run @var{thunk} with the "raw" (no canonical or echo) options
;;> needed for a terminal application.

(define (with-raw-io port thunk)
  (with-stty '(not icanon isig echo) thunk port))

;;> Returns the current terminal width in characters of @var{x},
;;> which must be a port or a file descriptor.

(define (get-terminal-width x)
  (let ((ws (ioctl x TIOCGWINSZ)))
    (and ws (winsize-col ws))))

;;> Returns the current terminal dimensions, as a list of character width
;;> and height, of @var{x}, which must be a port or a file descriptor.

(define (get-terminal-dimensions x)
  (let ((ws (ioctl x TIOCGWINSZ)))
    (and ws (list (winsize-col ws) (winsize-row ws)))))
