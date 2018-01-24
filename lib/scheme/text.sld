
(define-library (scheme text)
  (import (srfi 135))
  (export
   ;; Predicates
   text?                 textual?
   textual-null? 
   textual-every         textual-any
   ;; Constructors
   make-text             text
   text-tabulate
   text-unfold           text-unfold-right
   ;; Conversion
   textual->text
   textual->string       textual->vector      textual->list
   string->text          vector->text         list->text    reverse-list->text
   textual->utf8         textual->utf16be
   textual->utf16        textual->utf16le
   utf8->text            utf16be->text
   utf16->text           utf16le->text
   ;; Selection
   text-length           textual-length
   text-ref              textual-ref
   subtext               subtextual
   textual-copy
   textual-take          textual-take-right
   textual-drop          textual-drop-right
   textual-pad           textual-pad-right 
   textual-trim          textual-trim-right   textual-trim-both
   ;; Replacement
   textual-replace
   ;; Comparison
   textual=?             textual-ci=?
   textual<?             textual-ci<?
   textual>?             textual-ci>?
   textual<=?            textual-ci<=?
   textual>=?            textual-ci>=?
   ;; Prefixes & suffixes
   textual-prefix-length textual-suffix-length
   textual-prefix?       textual-suffix?    
   ;; Searching
   textual-index         textual-index-right
   textual-skip          textual-skip-right
   textual-contains      textual-contains-right
   ;; Case conversion
   textual-upcase        textual-downcase
   textual-foldcase      textual-titlecase
   ;; Concatenation
   textual-append        textual-concatenate  textual-concatenate-reverse
   textual-join
   ;; Fold & map & friends
   textual-fold          textual-fold-right
   textual-map           textual-for-each
   textual-map-index     textual-for-each-index
   textual-count
   textual-filter        textual-remove
   ;; Replication & splitting
   textual-replicate     textual-split
   ))
