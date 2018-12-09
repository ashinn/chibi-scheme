(define-library (chibi syntax-case-test)
  (export run-tests)
  (import (chibi)
	  (chibi syntax-case)
	  (chibi test))
  (begin
    (define (run-tests)
      (test-begin "Syntax Case")

      (test "syntax constant list"
	    '(+ 1 2)
	    #'(+ 1 2))

      (test "pattern variable"
	    'foo
	    (syntax-case 'foo ()
	      (x #'x)))

      (test "syntax-case pair"
	    '(a b)
	    (syntax-case '(a . b) ()
	      ((x . y) #'(x y))))

      (test "syntax-case var"
	    'a
	    (syntax-case '(a . b) (b)
	      ((b . y) #f)
	      ((x . b) #'x)))

      (test "syntax-case simple ellipsis"
	    '(a b c)
	    (syntax-case '(a b c) ()
	      ((a ...) #'(a ...))))

      (test "syntax-case ellipsis with tail"
	    '(a b x c)
	    (syntax-case '(a b c) ()
	      ((a ... b) #'(a ... x b))))

      (test "syntax-case ellipsis with dotted tail"
	    '(a b x c y d)
	    (syntax-case '(a b c . d) ()
	      ((a ... b . c) #'(a ... x b y c))))

      (test "syntax-case nested ellipsis"
	    '((a b) (d e) c f)
	    (syntax-case '((a b c) (d e f)) ()
	      (((x ... y) ...) #'((x ...) ... y ...))))

      (test "with-ellipsis"
	    '((a b))
	    (with-ellipsis :::
	      (syntax-case '(a) ()
		((... :::) #'((... b) :::)))))   

      (test-end))))
