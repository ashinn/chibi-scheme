
;;> Utilities for gathering statistics on the heap.  Just measuring
;;> runtime memory usage doesn't give a good idea of how to optimize
;;> that usage, so this module is provided for profiling.

;;> @subsubsubsection{(heap-stats)}

;;> Returns an alist summarizing all heap allocated objects.  The
;;> @var{car} of each cell is the type-name, and the @var{cdr} is the
;;> count of objects of that type in the heap.  Garbage is collected
;;> before the counts are taken.

;;> @subsubsubsection{(heap-dump [depth])}

;;> Returns the same value as @scheme{(heap-stats)}, but also prints
;;> all objects on the heap as it runs.  @var{depth} indicates the
;;> printing depth for compound objects and defaults to 1.

;;> These functions just return @scheme{'()} when using the Boehm GC.

(define-library (chibi heap-stats)
  (export heap-stats heap-dump)
  (import (chibi))
  (include-shared "heap-stats"))
