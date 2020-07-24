
;;> Library for weak data structures.

;;> \procedure{(make-ephemeron key value)}

;;> Returns a new ephemeron.  This ephemeron holds a weak reference to
;;> \var{key}, such that \var{value} will only be traced by the GC if
;;> \var{key} is referenced from an external object.

;;> \procedure{(ephemeron? x)}
;;> Returns true iff \var{x} is an ephemeron.

;;> \procedure{(ephemeron-broken? ephemeron)}
;;> Returns true iff \var{ephemeron}s \var{key} has been GCed.

;;> \procedure{(ephemeron-key ephemeron)}
;;> Returns \var{ephemeron}s \var{key}, or \scheme{#f} if it has been GCed.

;;> \procedure{(ephemeron-value ephemeron)}
;;> Returns \var{ephemeron}s \var{value}.

(define-library (chibi weak)
  (export make-ephemeron ephemeron? ephemeron-broken?
          ephemeron-key ephemeron-value
          ;; make-weak-vector weak-vector? weak-vector-length
          ;; weak-vector-ref weak-vector-set!
          )
  (include-shared "weak"))
