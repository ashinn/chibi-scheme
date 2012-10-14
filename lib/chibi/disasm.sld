
;;> @subsubsubsection{(disasm f [out])}

;;> Write a human-readable disassembly for the procedure @var{f} to
;;> the port @var{out}, defaulting to @scheme{(current-output-port)}.

(define-library (chibi disasm)
  (export disasm)
  (import (chibi))
  (include-shared "disasm"))
