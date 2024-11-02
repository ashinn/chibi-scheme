
(define-library (chibi csv)
  (import (scheme base) (srfi 227))
  (export csv-grammar csv-parser csv-grammar?
          default-csv-grammar default-tsv-grammar
          csv-read->list csv-read->vector  csv-read->fixed-vector
          csv-read->sxml
          csv-fold csv-map csv->list csv-for-each csv->sxml)
  (include "csv.scm"))
