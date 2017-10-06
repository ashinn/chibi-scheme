
(define-library (scheme red)
  (import
   (scheme base)
   (scheme box)
   (scheme case-lambda)
   (scheme char)
   (scheme charset)
   (scheme comparator)
   (scheme complex)
   (scheme cxr)
   (scheme ephemeron)
   (scheme eval)
   (scheme file)
   (scheme generator)
   (scheme hash-table)
   (scheme ilist)
   (scheme inexact)
   (scheme lazy)
   (scheme list-queue)
   (scheme list)
   (scheme load)
   (scheme process-context)
   (scheme read)
   (scheme repl)
   (scheme sort)
   (scheme time)
   (scheme vector)
   (scheme write))
  (export
   *                               +
   -                               ->char-set
   ...                             /
   <                               <=
   <=?                             <?
   =                               =>
   =?                              >
   >=                              >=?
   >?                              _
   abs                             acos
   alist->hash-table
   alist-cons                      alist-copy
   alist-delete                    alist-delete!
   and                             angle
   any                             append
   append!
   append-map                      append-map!
   append-reverse                  append-reverse!
   apply
   asin
   assoc                           assq
   assv                            atan
   begin
   binary-port?
   boolean-hash                    boolean=?
   boolean?                        box
   box?                            break
   break!                          bytevector
   bytevector->generator           bytevector-append
   bytevector-copy                 bytevector-copy!
   bytevector-length               bytevector-u8-ref
   bytevector-u8-set!              bytevector?
   caaaar                          caaadr
   caaar                           caadar
   caaddr                          caadr
   caar                            cadaar
   cadadr                          cadar
   caddar                          cadddr
   caddr                           cadr
   call-with-current-continuation  call-with-input-file
   call-with-output-file
   call-with-port
   call-with-values                call/cc
   car                             car+cdr
   case                            case-lambda
   cdaaar                          cdaadr
   cdaar                           cdadar
   cdaddr                          cdadr
   cdar                            cddaar
   cddadr                          cddar
   cdddar                          cddddr
   cdddr                           cddr
   cdr                             ceiling
   char->integer                   char-alphabetic?
   char-ci-hash                    char-ci<=?
   char-ci<?                       char-ci=?
   char-ci>=?                      char-ci>?
   char-downcase                   char-foldcase
   char-hash                       char-lower-case?
   char-numeric?                   char-ready?
   char-set                        char-set->list
   char-set->string                char-set-adjoin
   char-set-adjoin!                char-set-any
   char-set-complement             char-set-complement!
   char-set-contains?              char-set-copy
   char-set-count                  char-set-cursor
   char-set-cursor-next            char-set-delete
   char-set-delete!                char-set-diff+intersection
   char-set-diff+intersection!     char-set-difference
   char-set-difference!            char-set-every
   char-set-filter                 char-set-filter!
   char-set-fold                   char-set-for-each
   char-set-hash                   char-set-intersection
   char-set-intersection!          char-set-map
   char-set-ref                    char-set-size
   char-set-unfold                 char-set-unfold!
   char-set-union                  char-set-union!
   char-set-xor                    char-set-xor!
   char-set:ascii                  char-set:blank
   char-set:digit                  char-set:empty
   char-set:full                   char-set:graphic
   char-set:hex-digit              char-set:iso-control
   char-set:letter                 char-set:letter+digit
   char-set:lower-case             char-set:printing
   char-set:punctuation            char-set:symbol
   char-set:title-case             char-set:upper-case
   char-set:whitespace             char-set<=
   char-set=                       char-upcase
   char-upper-case?                char-whitespace?
   char<=?                         char<?
   char=?                          char>=?
   char>?                          char?
   circular-list                   circular-list?
   close-input-port                close-output-port
   close-port
   command-line                    comparator-check-type
   comparator-equality-predicate   comparator-hash
   comparator-hash-function        comparator-hashable?
   comparator-if<=>                comparator-ordered?
   comparator-ordering-predicate   comparator-register-default!
   comparator-test-type            comparator-type-test-predicate
   comparator?
   complex?                        concatenate
   concatenate!                    cond
   cond-expand                     cons
   cons*
   cos
   count
   current-error-port
   current-input-port              current-jiffy
   current-output-port
   current-second                  default-hash
   define
   define-record-type              define-syntax
   define-values                   delay
   delay-force                     delete
   delete!                         delete-duplicates
   delete-duplicates!              delete-file
   denominator
   digit-value                     display
   do                              dotted-list?
   drop                            drop-right
   drop-right!                     drop-while
   dynamic-wind                    eighth
   else                            emergency-exit
   end-of-char-set?
   environment
   eof-object                      eof-object?
   ephemeron-broken?               ephemeron-key
   ephemeron-datum                 ephemeron?
   eq?                             equal?
   eqv?
   error                           eval
   even?                           every
   exact
   exact-integer-sqrt
   exact-integer?
   exact?
   exit                            exp
   expt                            features
   fifth                           file-error?
   file-exists?
   filter                          filter!
   filter-map                      find
   find-tail
   finite?                         first
   floor                           floor-quotient
   floor-remainder                 floor/
   flush-output-port
   fold                            fold-right
   for-each                        force
   fourth                          gappend
   gcd
   gcombine                        gcons*
   gdelete                         gdelete-neighbor-dups
   gdrop                           gdrop-while
   generator
   generator->list                 generator->reverse-list
   generator->string               generator->vector
   generator->vector!              generator-any
   generator-count                 generator-every
   generator-find                  generator-fold
   generator-for-each              generator-unfold
   get-environment-variable        get-environment-variables
   get-output-bytevector           get-output-string
   gfilter                         gindex
   gremove                         gselect
   gtake                           gtake-while
   gtree->itree                    gtree->tree
   guard                           hash
   hash-bound                      hash-by-identity
   hash-salt                       hash-table
   hash-table->alist               hash-table-clear!
   hash-table-contains?            hash-table-copy
   hash-table-count                hash-table-delete!
   hash-table-difference!          hash-table-empty-copy
   hash-table-empty?               hash-table-entries
   hash-table-equivalence-function hash-table-exists?
   hash-table-find                 hash-table-fold
   hash-table-for-each             hash-table-hash-function
   hash-table-intern!              hash-table-intersection!
   hash-table-keys                 hash-table-map
   hash-table-map!                 hash-table-map->list
   hash-table-merge!               hash-table-mutable?
   hash-table-pop!                 hash-table-prune!
   hash-table-ref                  hash-table-ref/default
   hash-table-set!                 hash-table-size
   hash-table-unfold               hash-table-union!
   hash-table-update!              hash-table-update!/default
   hash-table-values               hash-table-walk
   hash-table-xor!                 hash-table=?
   hash-table?
   if                              ilist->list
   ilist-comparator                ilist-tail
   imag-part
   include                         include-ci
   inexact
   inexact?                        infinite?
   input-port-open?                input-port?
   integer->char                   integer?
   interaction-environment         iota
   ipair->pair                     ipair-comparator
   iq
   itree->tree
   jiffies-per-second              lambda
   last                            last-pair
   lcm
   length
   length+                         let
   let*                            let*-values
   let-syntax
   let-values                      letrec
   letrec*                         letrec-syntax
   list                            list->char-set
   list->char-set!                 list->generator
   list->ilist                     list->string
   list->vector                    list-copy
   list-delete-neighbor-dups       list-delete-neighbor-dups!
   list-index                      list-merge
   list-merge!                     list-queue
   list-queue-add-back!            list-queue-add-front!
   list-queue-append               list-queue-append!
   list-queue-back                 list-queue-concatenate
   list-queue-copy                 list-queue-empty?
   list-queue-first-last           list-queue-for-each
   list-queue-front                list-queue-list
   list-queue-map                  list-queue-map!
   list-queue-remove-all!          list-queue-remove-back!
   list-queue-remove-front!        list-queue-set-list!
   list-queue-unfold               list-queue-unfold-right
   list-queue?                     list-ref
   list-set!                       list-sort
   list-sort!                      list-sorted?
   list-stable-sort                list-stable-sort!
   list-tabulate                   list-tail
   list=                           list?
   load
   log
   lset-adjoin
   lset-diff+intersection          lset-diff+intersection!
   lset-difference                 lset-difference!
   lset-intersection               lset-intersection!
   lset-union                      lset-union!
   lset-xor                        lset-xor!
   lset<=                          lset=
   magnitude                       make-bytevector
   make-comparator
   make-coroutine-generator        make-default-comparator
   make-ephemeron
   make-eq-comparator              make-equal-comparator
   make-eqv-comparator
   make-for-each-generator
   make-hash-table                 make-icar-comparator
   make-icdr-comparator            make-improper-ilist-comparator
   make-iota-generator             make-list
   make-list-comparator            make-list-queue
   make-pair-comparator            make-parameter
   make-polar                      make-promise
   make-range-generator            make-rectangular
   make-string
   make-unfold-generator           make-vector
   make-vector-comparator          map
   map!                            map-in-order
   max                             member
   memq                            memv
   min                             modulo
   nan?                            negative?
   newline                         ninth
   not                             not-ipair?
   not-pair?
   null-list?                      null?
   number->string                  number-hash
   number?                         numerator
   odd?
   open-binary-input-file          open-binary-output-file
   open-input-bytevector           open-input-file
   open-input-string
   open-output-bytevector          open-output-file
   open-output-string
   or                              output-port-open?
   output-port?                    pair->ipair
   pair-fold                       pair-fold-right
   pair-for-each
   pair?
   parameterize                    partition
   partition!                      peek-char
   peek-u8
   port?                           positive?
   procedure?
   promise?                        proper-list?
   quasiquote                      quote
   quotient                        raise
   raise-continuable
   rational?                       rationalize
   read                            read-bytevector
   read-bytevector!                read-char
   read-error?                     read-line
   read-string                     read-u8
   real-part
   real?                           reduce
   reduce-right
   remainder
   remove                          remove!
   replace-icar                    replace-icdr
   reverse                         reverse!
   reverse-list->vector            reverse-vector->generator
   reverse-vector->list
   round
   second                          set!
   set-box!                        set-car!
   set-cdr!
   seventh                         sin
   sixth
   span
   span!                           split-at
   split-at!                       sqrt
   square                          string
   string->char-set                string->char-set!
   string->generator               string->list
   string->number                  string->symbol
   string->utf8                    string->vector
   string-append                   string-ci-hash
   string-ci<=?                    string-ci<?
   string-ci=?                     string-ci>=?
   string-ci>?
   string-copy                     string-copy!
   string-downcase                 string-fill!
   string-foldcase                 string-for-each
   string-hash
   string-length                   string-map
   string-ref                      string-set!
   string-upcase
   string<=?                       string<?
   string=?                        string>=?
   string>?                        string?
   substring
   symbol->string                  symbol-hash
   symbol=?                        symbol?
   syntax-error
   syntax-rules
   take
   take!                           take-right
   take-while                      take-while!
   tan                             tenth
   textual-port?                   third
   tree->itree
   truncate                        truncate-quotient
   truncate-remainder              truncate/
   u8-ready?
   ucs-range->char-set             ucs-range->char-set!
   unbox                           unfold
   unfold-right                    unless
   unquote                         unquote-splicing
   unzip1                          unzip2
   unzip3                          unzip4
   unzip5                          utf8->string
   values                          vector
   vector->generator               vector->list
   vector->string                  vector-any
   vector-append                   vector-append-subvectors
   vector-binary-search            vector-concatenate
   vector-copy                     vector-copy!
   vector-count                    vector-cumulate
   vector-delete-neighbor-dups     vector-delete-neighbor-dups!
   vector-empty?                   vector-every
   vector-fill!                    vector-find-median
   vector-find-median!             vector-fold
   vector-fold-right               vector-for-each
   vector-index                    vector-index-right
   vector-length                   vector-map
   vector-map!                     vector-merge
   vector-merge!                   vector-partition
   vector-ref                      vector-reverse!
   vector-reverse-copy             vector-reverse-copy!
   vector-select!                  vector-separate!
   vector-set!                     vector-skip
   vector-skip-right               vector-sort
   vector-sort!                    vector-sorted?
   vector-stable-sort              vector-stable-sort!
   vector-swap!                    vector-unfold
   vector-unfold!                  vector-unfold-right
   vector-unfold-right!            vector=
   vector?
   when                            with-exception-handler
   with-input-from-file
   with-output-to-file             write
   write-bytevector                write-char
   write-string                    write-u8
   xcons
   zero?
   zip                             
   ))
