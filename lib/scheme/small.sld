(define-library (scheme small)
  (import (scheme base) (scheme char) (scheme complex) (scheme cxr)
          (scheme eval) (scheme file) (scheme inexact) (scheme lazy)
          (scheme load) (scheme process-context) (scheme read)
          (scheme repl) (scheme time) (scheme write))
  (export
   ;; base
   * + - ... / < <= = => > >= _ abs and append apply assoc assq assv begin
   binary-port?  boolean?  boolean=?  bytevector bytevector-append
   bytevector-copy bytevector-copy! bytevector-length
   bytevector-u8-ref bytevector-u8-set!  bytevector?  caar cadr
   call-with-current-continuation call-with-port call-with-values
   call/cc car case cdr cdar cddr ceiling char->integer
   char-ready? char<=?  char<?  char=?  char>=?  char>?  char?
   close-input-port close-output-port close-port complex?  cond cond-expand
   cons current-error-port current-input-port current-output-port define
   define-record-type define-syntax define-values denominator do
   dynamic-wind else eof-object? eof-object  eq?  equal?  eqv?  error
   error-object-irritants error-object-message error-object?  even?
   exact exact-integer-sqrt exact-integer?  exact?  expt features
   file-error? floor
   flush-output-port for-each gcd get-output-bytevector get-output-string
   guard if include include-ci inexact inexact?  input-port?
   integer->char
   integer?  lambda lcm length let let* let*-values let-syntax let-values
   letrec letrec* letrec-syntax list list->string list->vector list-copy
   list-ref list-set!  list-tail list?  make-bytevector make-list
   make-parameter make-string make-vector map max member memq memv min
   modulo negative?  newline not null?  number->string number?  numerator
   odd?  open-input-bytevector open-input-string open-output-bytevector
   open-output-string or output-port?  pair?  parameterize peek-char
   peek-u8 input-port-open? output-port-open?  port?  positive?
   procedure?  quasiquote quote
   quotient raise raise-continuable rational?  rationalize read-bytevector
   read-bytevector!  read-char read-error? read-line read-string read-u8
   real?  remainder
   reverse round set!  set-car!  set-cdr!  square string string->list
   string->number string->symbol string->utf8 string->vector string-append
   string-copy string-copy! string-fill!  string-for-each string-length
   string-map
   string-ref string-set!  string<=?  string<?  string=?  string>=?
   string>?  string?  substring symbol->string symbol?  symbol=?  syntax-error
   syntax-rules textual-port?  truncate u8-ready?  unless unquote
   unquote-splicing utf8->string values vector vector-append
   vector->list vector->string
   vector-copy vector-copy! vector-fill!  vector-for-each vector-length
   vector-map vector-ref vector-set!  vector?  when with-exception-handler
   write-bytevector write-char write-string write-u8 zero?
   truncate-quotient truncate-remainder truncate/
   floor-quotient floor-remainder floor/
   ;; char
   char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
   char-downcase char-foldcase char-lower-case? char-numeric?
   char-upcase char-upper-case? char-whitespace? digit-value
   string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
   string-downcase string-foldcase string-upcase
   ;; complex
   angle imag-part magnitude make-polar make-rectangular real-part
   ;; cxr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ;; eval
   eval environment
   ;; file
   call-with-input-file call-with-output-file
   delete-file file-exists?
   open-binary-input-file open-binary-output-file
   open-input-file open-output-file
   with-input-from-file with-output-to-file
   ;; inexact
   acos asin atan cos exp finite? infinite? log nan? sin sqrt tan
   ;; lazy
   delay force delay-force make-promise promise?
   ;; load
   load
   ;; process-context
   get-environment-variable get-environment-variables
   command-line exit emergency-exit
   ;; read
   read
   ;; repl
   interaction-environment
   ;; time
   current-second current-jiffy jiffies-per-second
   ;; write
   display write write-shared write-simple
   ))
