
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary Records

;;> \macro{(define-binary-record-type <name> [<bindings> ...] (block: <fields> ...))}
;;>
;;> Defines a new record type that supports serializing to and from
;;> binary ports.  The generated procedures accept keyword-style
;;> arguments:
;;>
;;> \itemlist[
;;> \item{\scheme{(make: <constructor-name>)}}
;;> \item{\scheme{(pred: <predicate-name>)}}
;;> \item{\scheme{(read: <reader-name>)}}
;;> \item{\scheme{(write: <writer-name>)}}
;;> \item{\scheme{(block: <fields> ...)}}
;;> ]
;;>
;;> The fields are also similar to \scheme{define-record-type} but
;;> with an additional type:
;;>
;;> \scheme{(field (type args ...) getter setter)}
;;>
;;> Built-in types include:
;;>
;;> \itemlist[
;;> \item{\scheme{(u8)} - a single byte in [0, 255]}
;;> \item{\scheme{(u16/le)} - a little-endian short integer}
;;> \item{\scheme{(u16/be)} - a big-endian short integer}
;;> \item{\scheme{(fixed-string <length>)} - a fixed-length utf-8 string}
;;> \item{\scheme{(padded-string <length> (pad <pad-char>))} - a utf-8 string padded to a given length}
;;> \item{\scheme{(octal <length>)} - an integer in octal string format}
;;> \item{\scheme{(decimal <length>)} - an integer in decimal string format}
;;> \item{\scheme{(hexadecimal <length>)} - an integer in hexadecimal string format}
;;> ]
;;>
;;> In addition, the field can be a literal (char, string or
;;> bytevector), for instance as a file magic sequence or fixed
;;> separator.  The fields (and any constants) are serialized in the
;;> order they appear in the block.  For example, the header of a GIF
;;> file could be defined as:
;;>
;;> \example{
;;>  (define-binary-record-type gif-header
;;>    (make: make-gif-header)
;;>    (pred: gif-header?)
;;>    (read: read-gif-header)
;;>    (write: write-gif-header)
;;>    (block:
;;>     "GIF89a"
;;>     (width (u16/le) gif-header-width)
;;>     (height (u16/le) gif-header-height)
;;>     (gct (u8) gif-header-gct)
;;>     (bgcolor (u8) gif-header-gbcolor)
;;>     (aspect-ratio (u8) gif-header-aspect-ratio)
;;>     ))
;;> }
;;>
;;> For a more complex example see the \scheme{(chibi tar)}
;;> implementation.
;;>
;;> The binary type itself is a macro used to expand to a predicate
;;> and reader/writer procedures, which can be defined with
;;> \scheme{define-binary-type}.  For example,
;;>
;;> \example{
;;>  (define-binary-type (u8)
;;>    (lambda (x) (and (exact-integer? x) (<= 0 x 255)))
;;>    read-u8
;;>    write-u8)
;;> }

(define-syntax define-binary-record-type
  (syntax-rules ()
    ((define-binary-record-type name x ...)
     (defrec (x ...) name hidden-make hidden-pred hidden-read hidden-write
       () () ()))))

(define-syntax defrec
  (syntax-rules (make: pred: read: write: block:)
    ((defrec () n m p r w
       ((field-tmp field-read field-read-expr field-write field-write-expr field-get) ...)
       ((field getter . s) ...)
       (def-setter ...))
     (begin
       (define-record-type n (m field ...) p
         (field getter . s) ...)
       (define r
         (let ((field-read field-read-expr) ...)
           (lambda (in)
             (let* ((field-tmp (field-read in)) ...)
               (m field ...)))))
       (define w
         (let ((field-write field-write-expr) ...)
           (lambda (x out)
             (field-write (field-get x) out) ...)))
       def-setter ...)
     ;; workaround for impls which strip hygiene from top-level defs
     ;; for some reason, works in chicken but not across libraries
     ;;
     ;; (begin
     ;;   (define-values (n m p getter ... setter ...)
     ;;     (let ()
     ;;       (define-record-type n (m field ...) p
     ;;         (field getter . s) ...)
     ;;       (def setter val) ...
     ;;       (values (record-rtd n) m p getter ... setter ...)))
     ;;   (define r
     ;;     (let ((field-read field-read-expr) ...)
     ;;       (lambda (in)
     ;;         (let* ((field-tmp (field-read in)) ...)
     ;;           (m field ...)))))
     ;;   (define w
     ;;     (let ((field-write field-write-expr) ...)
     ;;       (lambda (x out)
     ;;         (field-write (field-get x) out) ...))))
     )
    ((defrec ((make: x) . rest) n m p r w b f s)
     (defrec rest n x p r w b f s))
    ((defrec ((pred: x) . rest) n m p r w b f s)
     (defrec rest n m x r w b f s))
    ((defrec ((read: x) . rest) n m p r w b f s)
     (defrec rest n m p x w b f s))
    ((defrec ((write: x) . rest) n m p r w b f s)
     (defrec rest n m p r x b f s))
    ((defrec ((block: (field (type . args) getter setter) . fields) . rest) n m p r w b f s)
     (defrec ((block: (field (type . args) getter setter tmp-setter) . fields) . rest) n m p r w b f s))
    ((defrec ((block: (field (type . args) getter setter tmp-setter) . fields) . rest) n m p r w
       (b ...) (f ...) (s ...))
     (defrec ((block: . fields) . rest) n m p r w
       (b ...
          (field read-tmp (type read: args) write-tmp (type write: args) getter))
       (f ...
          (field getter tmp-setter))
       (s ...
          (define setter
            (let ((pred? (type pred: args)))
              (lambda (x val)
                (if (not (pred? val))
                    (error "invalid val for" 'field val))
                (tmp-setter x val)))))))
    ((defrec ((block: (field (type . args) getter) . fields) . rest) n m p r w
       (b ...) (f ...) s)
     (defrec ((block: . fields) . rest) n m p r w
       (b ...
          (field read-tmp (type read: args) write-tmp (type write: args) getter))
       (f ...
          (field getter))
       s))
    ((defrec ((block: (field . x)) . rest) n m p r w b f s)
     (syntax-error "invalid field in block" (field . x)))
    ((defrec ((block: data . fields) . rest) n m p r w (b ...) f s)
     (defrec ((block: . fields) . rest) n m p r w
       (b ...
          (tmp-data read-tmp (read-literal 'data) write-tmp (write-literal 'data) (lambda (x) x)))
       f
       s))
    ((defrec ((block:) . rest) n m p r w b f s)
     (defrec rest n m p r w b f s))
    ))
