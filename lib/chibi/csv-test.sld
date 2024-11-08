
(define-library (chibi csv-test)
  (import (scheme base)
          (srfi 227)
          (chibi csv)
          (chibi test))
  (export run-tests)
  (begin
    (define string->csv
      (opt-lambda (str (reader (csv-read->list)))
        (reader (open-input-string str))))
    (define csv->string
      (opt-lambda (row (writer (csv-writer)))
        (let ((out (open-output-string)))
          (writer row out)
          (get-output-string out))))
    (define (run-tests)
      (test-begin "(chibi csv)")
      (test-assert (eof-object? (string->csv "")))
      (test '("1997" "Ford" "E350")
          (string->csv "1997,Ford,E350"))
      (test '("1997" "Ford" "E350")
          (string->csv "\n1997,Ford,E350"))
      (test '(" ")
          (string->csv " \n1997,Ford,E350"))
      (test '("" "")
          (string->csv ",\n1997,Ford,E350"))
      (test '("1997" "Ford" "E350")
          (string->csv "\"1997\",\"Ford\",\"E350\""))
      (test '("1997" "Ford" "E350" "Super, luxurious truck")
          (string->csv "1997,Ford,E350,\"Super, luxurious truck\""))
      (test '("1997" "Ford" "E350" "Super, \"luxurious\" truck")
          (string->csv "1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\""))
      (test '("1997" "Ford" "E350" "Go get one now\nthey are going fast")
          (string->csv "1997,Ford,E350,\"Go get one now
they are going fast\""))
      (test '("1997" "Ford" "E350")
          (string->csv
           "# this is a comment\n1997,Ford,E350"
           (csv-read->list
            (csv-parser (csv-grammar '((comment-chars #\#)))))))
      (let ((parser (csv-parser (csv-grammar '((quote-non-numeric? . #t))))))
        (test-error (string->csv "1997,\"Ford\",E350" (csv-read->list parser)))
        (test '(1997 "Ford" "E350")
            (string->csv "1997,\"Ford\",\"E350\"" (csv-read->list parser))))
      (test '("1997" "Fo\"rd" "E3\"50")
          (string->csv "1997\tFo\"rd\tE3\"50"
                            (csv-read->list (csv-parser default-tsv-grammar))))
      (test '#("1997" "Ford" "E350")
          (string->csv "1997,Ford,E350" (csv-read->vector)))
      (test '#("1997" "Ford" "E350")
          (string->csv "1997,Ford,E350" (csv-read->fixed-vector 3)))
      (test-error
          (string->csv "1997,Ford,E350" (csv-read->fixed-vector 2)))
      (let ((city-csv "Los Angeles,34°03′N,118°15′W
New York City,40°42′46″N,74°00′21″W
Paris,48°51′24″N,2°21′03″E"))
        (test '(*TOP*
                (row (col-0 "Los Angeles")
                     (col-1 "34°03′N")
                     (col-2 "118°15′W"))
                (row (col-0 "New York City")
                     (col-1 "40°42′46″N")
                     (col-2 "74°00′21″W"))
                (row (col-0 "Paris")
                     (col-1 "48°51′24″N")
                     (col-2 "2°21′03″E")))
            ((csv->sxml) (open-input-string city-csv)))
       (test '(*TOP*
               (city (name "Los Angeles")
                     (latitude "34°03′N")
                     (longitude "118°15′W"))
               (city (name "New York City")
                     (latitude "40°42′46″N")
                     (longitude "74°00′21″W"))
               (city (name "Paris")
                     (latitude "48°51′24″N")
                     (longitude "2°21′03″E")))
           ((csv->sxml 'city '(name latitude longitude))
            (open-input-string city-csv)))
       (test 3 (csv-num-rows default-csv-grammar (open-input-string city-csv)))
       (test 0 (csv-num-rows default-csv-grammar (open-input-string "")))
       (test 1 (csv-num-rows default-csv-grammar (open-input-string "x"))))
      (test "1997,Ford,E350\n"
          (csv->string '("1997" "Ford" "E350")))
      (test "1997,Ford,E350,\"Super, luxurious truck\"\n"
          (csv->string '("1997" "Ford" "E350" "Super, luxurious truck")))
      (test "1997,Ford,E350,\"Super, \"\"luxurious\"\" truck\"\n"
          (csv->string '("1997" "Ford" "E350" "Super, \"luxurious\" truck")))
      (test "1997,Ford,E350,\"Go get one now\nthey are going fast\"\n"
          (csv->string
           '("1997" "Ford" "E350" "Go get one now\nthey are going fast")))
      (test "1997,Ford,E350\n"
          (csv->string '(1997 "Ford" E350)))
      (test "1997,\"Ford\",\"E350\"\n"
          (csv->string '(1997 "Ford" E350)
                       (csv-writer (csv-grammar '((quote-non-numeric? . #t))))))
      (test-end))))
