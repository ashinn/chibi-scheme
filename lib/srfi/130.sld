(define-library (srfi 130)
  (export
   ;; Cursor operations
   string-cursor?
   string-cursor-start    string-cursor-end
   string-cursor-next     string-cursor-prev
   string-cursor-forward  string-cursor-backward
   string-cursor=?
   string-cursor<?        string-cursor>?
   string-cursor<=?       string-cursor>=?
   string-cursor-diff
   string-cursor->index   string-index->cursor
   ;; Predicates
   string-null? string-every string-any
   ;; Constructors
   string-tabulate string-unfold string-unfold-right
   ;; Conversion
   string->list/cursors string->vector/cursors
   reverse-list->string string-join
   ;; Selection
   string-ref/cursor
   substring/cursors  string-copy/cursors
   string-take        string-take-right
   string-drop        string-drop-right
   string-pad         string-pad-right 
   string-trim        string-trim-right string-trim-both
   ;; Prefixes & suffixes
   string-prefix-length    string-suffix-length
   string-prefix?          string-suffix?    
   ;; Searching
   string-index string-index-right
   string-skip  string-skip-right
   string-contains string-contains-right
   ;; The whole string
   string-reverse
   string-concatenate  string-concatenate-reverse
   string-fold         string-fold-right
   string-for-each-cursor
   string-replicate    string-count
   string-replace      string-split   string-split-right
   string-filter       string-remove)
  (import (scheme base)
          (scheme char) (scheme write)
          (rename (chibi string)
                  (string-fold %string-fold)
                  (string-fold-right %string-fold-right)
                  (string-contains %string-contains)
                  (string-join %string-join)
                  (string-prefix? %string-prefix?)
                  (string-suffix? %string-suffix?)))
  (include "130.scm"))
