(define-library (chibi show-test)
  (export run-tests)
  (import (scheme base) (scheme read) (chibi test)
          (chibi show) (chibi show base) (chibi show pretty))
  (begin
    (define-syntax test-pretty
      (syntax-rules ()
        ((test-pretty str)
         (let ((sexp (read (open-input-string str))))
           (test str (show #f (pretty sexp)))))))
    (define (run-tests)
      (test-begin "show")

      ;; basic data types

      (test "hi" (show #f "hi"))
      (test "\"hi\"" (show #f (written "hi")))
      (test "\"hi \\\"bob\\\"\"" (show #f (written "hi \"bob\"")))
      (test "\"hello\\nworld\"" (show #f (written "hello\nworld")))
      (test "#(1 2 3)" (show #f (written '#(1 2 3))))
      (test "(1 2 3)" (show #f (written '(1 2 3))))
      (test "(1 2 . 3)" (show #f (written '(1 2 . 3))))
      (test "ABC" (show #f (upcased "abc")))
      (test "abc" (show #f (downcased "ABC")))

      (test "abc     def" (show #f "abc" (tab-to) "def"))
      (test "abc  def" (show #f "abc" (tab-to 5) "def"))
      (test "abcdef" (show #f "abc" (tab-to 3) "def"))

      ;; numbers

      (test "-1" (show #f -1))
      (test "0" (show #f 0))
      (test "1" (show #f 1))
      (test "10" (show #f 10))
      (test "100" (show #f 100))
      (test "-1" (show #f (numeric -1)))
      (test "0" (show #f (numeric 0)))
      (test "1" (show #f (numeric 1)))
      (test "10" (show #f (numeric 10)))
      (test "100" (show #f (numeric 100)))
      (test "57005" (show #f #xDEAD))
      (test "#xdead" (show #f (with ((radix 16)) #xDEAD)))
      (test "#xdead1234" (show #f (with ((radix 16)) #xDEAD) 1234))
      (test "de.ad"
          (show #f (with ((radix 16) (precision 2)) (numeric (/ #xDEAD #x100)))))
      (test "d.ead"
          (show #f (with ((radix 16) (precision 3)) (numeric (/ #xDEAD #x1000)))))
      (test "0.dead"
          (show #f (with ((radix 16) (precision 4)) (numeric (/ #xDEAD #x10000)))))
      (test "1g"
          (show #f (with ((radix 17)) (numeric 33))))

      (test "3.14159" (show #f 3.14159))
      (test "3.14" (show #f (with ((precision 2)) 3.14159)))
      (test "3.14" (show #f (with ((precision 2)) 3.14)))
      (test "3.00" (show #f (with ((precision 2)) 3.)))
      (test "1.10" (show #f (with ((precision 2)) 1.099)))
      (test "0.00" (show #f (with ((precision 2)) 1e-17)))
      (test "0.0000000010" (show #f (with ((precision 10)) 1e-9)))
      (test "0.0000000000" (show #f (with ((precision 10)) 1e-17)))
      (test "0.000004" (show #f (with ((precision 6)) 0.000004)))
      (test "0.0000040" (show #f (with ((precision 7)) 0.000004)))
      (test "0.00000400" (show #f (with ((precision 8)) 0.000004)))

      (test "   3.14159" (show #f (with ((decimal-align 5)) (numeric 3.14159))))
      (test "  31.4159" (show #f (with ((decimal-align 5)) (numeric 31.4159))))
      (test " 314.159" (show #f (with ((decimal-align 5)) (numeric 314.159))))
      (test "3141.59" (show #f (with ((decimal-align 5)) (numeric 3141.59))))
      (test "31415.9" (show #f (with ((decimal-align 5)) (numeric 31415.9))))
      (test "  -3.14159" (show #f (with ((decimal-align 5)) (numeric -3.14159))))
      (test " -31.4159" (show #f (with ((decimal-align 5)) (numeric -31.4159))))
      (test "-314.159" (show #f (with ((decimal-align 5)) (numeric -314.159))))
      (test "-3141.59" (show #f (with ((decimal-align 5)) (numeric -3141.59))))
      (test "-31415.9" (show #f (with ((decimal-align 5)) (numeric -31415.9))))

      (cond
       ((exact? (/ 1 3)) ;; exact rationals
        (test "333.333333333333333333333333333333"
            (show #f (with ((precision 30)) (numeric 1000/3))))
        (test  "33.333333333333333333333333333333"
            (show #f (with ((precision 30)) (numeric 100/3))))
        (test   "3.333333333333333333333333333333"
            (show #f (with ((precision 30)) (numeric 10/3))))
        (test   "0.333333333333333333333333333333"
            (show #f (with ((precision 30)) (numeric 1/3))))
        (test   "0.033333333333333333333333333333"
            (show #f (with ((precision 30)) (numeric 1/30))))
        (test   "0.003333333333333333333333333333"
            (show #f (with ((precision 30)) (numeric 1/300))))
        (test   "0.000333333333333333333333333333"
            (show #f (with ((precision 30)) (numeric 1/3000))))
        (test   "0.666666666666666666666666666667"
            (show #f (with ((precision 30)) (numeric 2/3))))
        (test   "0.090909090909090909090909090909"
            (show #f (with ((precision 30)) (numeric 1/11))))
        (test   "1.428571428571428571428571428571"
            (show #f (with ((precision 30)) (numeric 10/7))))
        (test "0.123456789012345678901234567890"
            (show #f (with ((precision 30))
                       (numeric (/  123456789012345678901234567890
                                    1000000000000000000000000000000)))))
        (test  " 333.333333333333333333333333333333"
            (show #f (with ((precision 30) (decimal-align 5)) (numeric 1000/3))))
        (test  "  33.333333333333333333333333333333"
            (show #f (with ((precision 30) (decimal-align 5)) (numeric 100/3))))
        (test  "   3.333333333333333333333333333333"
            (show #f (with ((precision 30) (decimal-align 5)) (numeric 10/3))))
        (test  "   0.333333333333333333333333333333"
            (show #f (with ((precision 30) (decimal-align 5)) (numeric 1/3))))
        ))

      (test "11.75" (show #f (with ((precision 2)) (/ 47 4))))
      (test "-11.75" (show #f (with ((precision 2)) (/ -47 4))))

      (test "(#x11 #x22 #x33)" (show #f (with ((radix 16)) '(#x11 #x22 #x33))))

      (test "299792458" (show #f (with ((comma-rule 3)) 299792458)))
      (test "299,792,458" (show #f (with ((comma-rule 3)) (numeric 299792458))))
      (test "-29,97,92,458"
          (show #f (with ((comma-rule '(3 . 2))) (numeric -299792458))))
      (test "299.792.458"
          (show #f (with ((comma-rule 3) (comma-sep #\.)) (numeric 299792458))))
      (test "299.792.458,0"
          (show #f (with ((comma-rule 3) (decimal-sep #\,)) (numeric 299792458.0))))

      (test "100,000" (show #f (with ((comma-rule 3)) (numeric 100000))))
      (test "100,000.0"
          (show #f (with ((comma-rule 3) (precision 1)) (numeric 100000))))
      (test "100,000.00"
          (show #f (with ((comma-rule 3) (precision 2)) (numeric 100000))))

      (cond-expand
       (complex
        (test "1+2i" (show #f (string->number "1+2i")))
        (test "1.00+2.00i"
            (show #f (with ((precision 2)) (string->number "1+2i"))))
        (test "3.14+2.00i"
            (show #f (with ((precision 2)) (string->number "3.14159+2i"))))))

      ;; padding/trimming

      (test "abc  " (show #f (padded 5 "abc")))
      (test "  abc" (show #f (padded/left 5 "abc")))
      (test " abc " (show #f (padded/both 5 "abc")))
      (test "abcde" (show #f (padded 5 "abcde")))
      (test "abcdef" (show #f (padded 5 "abcdef")))

      (test "abc" (show #f (trimmed 3 "abcde")))
      (test "abc" (show #f (trimmed 3 "abcd")))
      (test "abc" (show #f (trimmed 3 "abc")))
      (test "ab" (show #f (trimmed 3 "ab")))
      (test "a" (show #f (trimmed 3 "a")))
      (test "cde" (show #f (trimmed/left 3 "abcde")))
      (test "bcd" (show #f (trimmed/both 3 "abcde")))
      (test "bcdef" (show #f (trimmed/both 5 "abcdefgh")))
      (test "abc" (show #f (trimmed/lazy 3 "abcde")))
      (test "abc" (show #f (trimmed/lazy 3 "abc\nde")))

      (test "prefix: abc" (show #f "prefix: " (trimmed 3 "abcde")))
      (test "prefix: cde" (show #f "prefix: " (trimmed/left 3 "abcde")))
      (test "prefix: bcd" (show #f "prefix: " (trimmed/both 3 "abcde")))
      (test "prefix: abc" (show #f "prefix: " (trimmed/lazy 3 "abcde")))
      (test "prefix: abc" (show #f "prefix: " (trimmed/lazy 3 "abc\nde")))

      (test "abc :suffix" (show #f (trimmed 3 "abcde") " :suffix"))
      (test "cde :suffix" (show #f (trimmed/left 3 "abcde") " :suffix"))
      (test "bcd :suffix" (show #f (trimmed/both 3 "abcde") " :suffix"))
      (test "abc :suffix" (show #f (trimmed/lazy 3 "abcde") " :suffix"))
      (test "abc :suffix" (show #f (trimmed/lazy 3 "abc\nde") " :suffix"))

      (test "abcde"
          (show #f (with ((ellipsis "...")) (trimmed 5 "abcde"))))
      (test "ab..."
          (show #f (with ((ellipsis "...")) (trimmed 5 "abcdef"))))
      (test "abc..."
          (show #f (with ((ellipsis "...")) (trimmed 6 "abcdefg"))))
      (test "abcde"
          (show #f (with ((ellipsis "...")) (trimmed/left 5 "abcde"))))
      (test "...ef"
          (show #f (with ((ellipsis "...")) (trimmed/left 5 "abcdef"))))
      (test "...efg"
          (show #f (with ((ellipsis "...")) (trimmed/left 6 "abcdefg"))))
      (test "abcdefg"
          (show #f (with ((ellipsis "...")) (trimmed/both 7 "abcdefg"))))
      (test "...d..."
          (show #f (with ((ellipsis "...")) (trimmed/both 7 "abcdefgh"))))
      (test "...e..."
          (show #f (with ((ellipsis "...")) (trimmed/both 7 "abcdefghi"))))

      (test "abc  " (show #f (fitted 5 "abc")))
      (test "  abc" (show #f (fitted/left 5 "abc")))
      (test " abc " (show #f (fitted/both 5 "abc")))
      (test "abcde" (show #f (fitted 5 "abcde")))
      (test "abcde" (show #f (fitted/left 5 "abcde")))
      (test "abcde" (show #f (fitted/both 5 "abcde")))
      (test "abcde" (show #f (fitted 5 "abcdefgh")))
      (test "defgh" (show #f (fitted/left 5 "abcdefgh")))
      (test "bcdef" (show #f (fitted/both 5 "abcdefgh")))

      (test "prefix: abc   :suffix"
          (show #f "prefix: " (fitted 5 "abc") " :suffix"))
      (test "prefix:   abc :suffix"
          (show #f "prefix: " (fitted/left 5 "abc") " :suffix"))
      (test "prefix:  abc  :suffix"
          (show #f "prefix: " (fitted/both 5 "abc") " :suffix"))
      (test "prefix: abcde :suffix"
          (show #f "prefix: " (fitted 5 "abcde") " :suffix"))
      (test "prefix: abcde :suffix"
          (show #f "prefix: " (fitted/left 5 "abcde") " :suffix"))
      (test "prefix: abcde :suffix"
          (show #f "prefix: " (fitted/both 5 "abcde") " :suffix"))
      (test "prefix: abcde :suffix"
          (show #f "prefix: " (fitted 5 "abcdefgh") " :suffix"))
      (test "prefix: defgh :suffix"
          (show #f "prefix: " (fitted/left 5 "abcdefgh") " :suffix"))
      (test "prefix: bcdef :suffix"
          (show #f "prefix: " (fitted/both 5 "abcdefgh") " :suffix"))

      ;; joining

      (test "1 2 3" (show #f (joined each '(1 2 3) " ")))

      (test ":abc:123"
          (show #f (joined/prefix
                    (lambda (x) (trimmed 3 x))
                    '("abcdef" "123456")
                    ":")))

      (test "abc\n123\n"
          (show #f (joined/suffix
                    (lambda (x) (trimmed 3 x))
                    '("abcdef" "123456")
                    nl)))

      (test "lions, tigers, and bears"
          (show #f (joined/last
                    each
                    (lambda (x) (each "and " x))
                    '(lions tigers bears)
                    ", ")))

      (test "lions, tigers, or bears"
          (show #f (joined/dot
                    each
                    (lambda (x) (each "or " x))
                    '(lions tigers . bears)
                    ", ")))

      ;; shared structures

      (test "#0=(1 . #0#)"
          (show #f (written (let ((ones (list 1))) (set-cdr! ones ones) ones))))
      (test "(0 . #0=(1 . #0#))"
          (show #f (written (let ((ones (list 1)))
                              (set-cdr! ones ones)
                              (cons 0 ones)))))
      (test "(sym . #0=(sym . #0#))"
          (show #f (written (let ((syms (list 'sym)))
                              (set-cdr! syms syms)
                              (cons 'sym syms)))))
      (test "(#0=(1 . #0#) #1=(2 . #1#))"
          (show #f (written (let ((ones (list 1))
                                  (twos (list 2)))
                              (set-cdr! ones ones)
                              (set-cdr! twos twos)
                              (list ones twos)))))
      (test "(#0=(1 . #0#) #0#)"
          (show #f (written (let ((ones (list 1)))
                              (set-cdr! ones ones)
                              (list ones ones)))))
      (test "((1) (1))"
          (show #f (written (let ((ones (list 1)))
                              (list ones ones)))))

      (test "(#0=(1) #0#)"
          (show #f (written-shared (let ((ones (list 1)))
                                     (list ones ones)))))

      ;; cycles without shared detection

      (test "(1 1 1 1 1"
          (show #f (trimmed/lazy
                    10
                    (written-simply
                     (let ((ones (list 1))) (set-cdr! ones ones) ones)))))

      (test "(1 1 1 1 1 "
          (show #f (trimmed/lazy
                    11
                    (written-simply
                     (let ((ones (list 1))) (set-cdr! ones ones) ones)))))

      ;; pretty printing

      (test-pretty "(foo bar)\n")

      (test-pretty
       "((self . aquanet-paper-1991)
 (type . paper)
 (title . \"Aquanet: a hypertext tool to hold your\"))
")

      (test-pretty
       "(abracadabra xylophone
             bananarama
             yellowstonepark
             cryptoanalysis
             zebramania
             delightful
             wubbleflubbery)\n")

      '(test-pretty
        "#(0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
  26 27 28 29 30 31 32 33 34 35 36 37)\n")

      '(test-pretty
        "(0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
 26 27 28 29 30 31 32 33 34 35 36 37)\n")

      (test-pretty
       "(define (fold kons knil ls)
  (define (loop ls acc)
    (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc))))
  (loop ls knil))\n")

      (test-pretty
       "(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))\n")

      (test-pretty
       "(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec)
  (vector-set! vec i 'supercalifrajalisticexpialidocious))\n")

      (test-pretty
       "(do ((my-vector (make-vector 5)) (index 0 (+ index 1)))
    ((= index 5) my-vector)
  (vector-set! my-vector index index))\n")

      (test-pretty
       "(define (fold kons knil ls)
  (let loop ((ls ls) (acc knil))
    (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc)))))\n")

      (test-pretty
       "(define (file->sexp-list pathname)
  (call-with-input-file pathname
    (lambda (port)
      (let loop ((res '()))
        (let ((line (read port)))
          (if (eof-object? line) (reverse res) (loop (cons line res))))))))\n")

      (test-pretty
       "(design
 (module (name \"\\\\testshiftregister\") (attributes (attribute (name \"\\\\src\"))))
 (wire (name \"\\\\shreg\") (attributes (attribute (name \"\\\\src\")))))\n")

      '(test-pretty
       "(design
 (module (name \"\\\\testshiftregister\")
         (attributes
          (attribute (name \"\\\\src\") (value \"testshiftregister.v:10\"))))
 (wire (name \"\\\\shreg\")
       (attributes
        (attribute (name \"\\\\src\") (value \"testshiftregister.v:15\")))))\n")

      (test "(let ((ones '#0=(1 . #0#))) ones)\n"
          (show #f (pretty (let ((ones (list 1)))
                             (set-cdr! ones ones)
                             `(let ((ones ',ones)) ones)))))

      '(test
           "(let ((zeros '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      (ones '#0=(1 . #0#)))
  (append zeros ones))\n"
           (show #f (pretty
                     (let ((ones (list 1)))
                       (set-cdr! ones ones)
                       `(let ((zeros '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                              (ones ',ones))
                          (append zeros ones))))))

      (test-end))))
