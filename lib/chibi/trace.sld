
;;> @subsubsubsection{(trace proc)}

;;> Write a trace of all calls to the procedure @var{proc} to
;;> @scheme{(current-error-port)}.

;;> @subsubsubsection{(untrace proc)}

;;> Remove any active traces on the procedure @var{proc}.

;;> @subsubsubsection{(untrace-all)}

;;> Remove all active procedure traces.

(define-library (chibi trace)
  (export trace untrace untrace-all)
  (import (chibi) (chibi ast) (srfi 38) (srfi 39) (srfi 69))
  (include "trace.scm"))
