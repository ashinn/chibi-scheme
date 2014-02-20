
(import (chibi) (chibi test)
        (chibi char-set) (chibi char-set ascii)
        (chibi parse) (chibi parse common))

(test-begin "parse")

;; basic

(test-assert (parse parse-epsilon ""))
(test-assert (parse-fully parse-epsilon ""))
(test-error (parse-fully parse-epsilon "a"))

(test-not (parse parse-anything ""))
(test-assert (parse-fully parse-anything "a"))
(test-error (parse-fully parse-anything "ab"))

(test-not (parse parse-nothing ""))
(test-not (parse parse-nothing "a"))

(test-not (parse (parse-char #\a) ""))
(test-assert (parse-fully (parse-char #\a) "a"))
(test-not (parse (parse-char #\a) "b"))
(test-error (parse-fully (parse-char #\a) "ab"))

(let ((f (parse-seq (parse-char #\a) (parse-char #\b))))
  (test-not (parse f "a"))
  (test-not (parse f "b"))
  (test-assert (parse f "ab"))
  (test-error (parse-fully f "abc")))

(let ((f (parse-or (parse-char #\a) (parse-char #\b))))
  (test-not (parse f ""))
  (test-assert (parse f "a"))
  (test-assert (parse f "b"))
  (test-error (parse-fully f "ab")))

(let ((f (parse-not (parse-char #\a))))
  (test-assert (parse f ""))
  (test-error (parse-fully f "a"))
  (test-assert (parse f "b")))

(let ((f (parse-repeat (parse-char #\a))))
  (test-assert (parse-fully f ""))
  (test-assert (parse-fully f "a"))
  (test-assert (parse-fully f "aa"))
  (test-assert (parse-fully f "aaa"))
  (test-assert (parse f "b"))
  (test-assert (parse f "aab"))
  (test-error (parse-fully f "aab")))

;; grammars

(let ()
  (define-grammar calc
    (space ((* ,char-set:whitespace)))
    (number ((=> n (+ ,char-set:digit))
             (string->number (list->string n))))
    (simple ((=> n ,number) n)
            ((: "(" (=> e1 ,term) ")") e1))
    (term-op ("*" *)
             ("/" /)
             ("%" modulo))
    (term ((: (=> e1 ,simple) ,space (=> op ,term-op) ,space (=> e2 ,term))
           (op e1 e2))
          ((=> e1 ,simple)
           e1)))
  (test 88 (parse term "4*22"))
  (test 42 (parse term "42"))
  ;; partial match (grammar isn't checking end)
  (test 42 (parse term "42*")))

(define calculator
  (grammar expr
    (space ((: ,char-set:whitespace ,space))
           (() #f))
    (digit ((=> d ,char-set:digit) d))
    (number ((=> n (+ ,digit))
             (string->number (list->string n))))
    (simple ((=> n ,number) n)
            ((: "(" (=> e1 ,expr) ")") e1))
    (term-op ("*" *)
             ("/" /)
             ("%" modulo))
    (term ((: (=> e1 ,simple) ,space (=> op ,term-op) ,space (=> e2 ,term))
           (op e1 e2))
          ((=> e1 ,simple)
           e1))
    (expr-op ("+" +) ("-" -))
    (expr ((: ,space (=> e1 ,term) ,space (=> op ,expr-op) ,space (=> e2 ,expr))
           (op e1 e2))
          ((: ,space (=> e1 ,term))
           e1))))

(test 42 (parse calculator "42"))
(test 4 (parse calculator "2 + 2"))
(test 23 (parse calculator "2 + 2*10 + 1"))
(test 25 (parse calculator "2+2 * 10+1 * 3"))
(test 41 (parse calculator "(2 + 2) * 10 + 1"))

(define prec-calc
  (grammar expr
    (simple (,(parse-integer))
            ((: "(" (=> e1 ,expr) ")") e1))
    (op
     ("+" '+) ("-" '-) ("*" '*) ("/" '/) ("^" '^))
    (expr
     (,(parse-binary-op op `((+ 5) (- 5) (* 3) (/ 3) (^ 1 right)) simple)))))

(test 42 (parse prec-calc "42"))
(test '(+ 2 2) (parse prec-calc "2 + 2"))
(test '(+ (+ 2 2) 2) (parse prec-calc "2 + 2 + 2"))
(test '(+ (+ 2 (* 2 10)) 1) (parse prec-calc "2 + 2*10 + 1"))
(test '(+ (+ 2 (* 2 10)) (* 1 3)) (parse prec-calc "2+2 * 10+1 * 3"))
(test '(+ (* (+ 2 2) 10) 1) (parse prec-calc "(2 + 2) * 10 + 1"))
(test '(^ 2 (^ 2 2)) (parse prec-calc "2 ^ 2 ^ 2"))
(test '(+ (+ (+ 1 (* (* 2 (^ 3 (^ 4 5))) 6)) (^ 7 8)) 9)
    (parse prec-calc "1 + 2 * 3 ^ 4 ^ 5 * 6 + 7 ^ 8 + 9"))

;; this takes exponential time without memoization
(define explode
  (grammar start
    (start ((: ,S eos) #t))
    (S ((+ ,A) #t))
    (A ((: "a" ,S "b") #t)
       ((: "a" ,S "c") #t)
       ((: "a") #t))))

(test-assert (parse explode "aaabb"))
(test-not (parse explode "bbaa"))
(test-assert
 (parse explode (string-append (make-string 10 #\a) (make-string 8 #\c))))

(test-end)
