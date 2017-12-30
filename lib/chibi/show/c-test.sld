
(define-library (chibi show c-test)
  (import (scheme base) (chibi show) (chibi show c) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "(chibi show c)")

      (test "if (1) {
    2;
} else {
    3;
}
"
          (show #f (c-if 1 2 3)))

      (test "if (x ? y : z) {
    2;
} else {
    3;
}
"
          (show #f (c-if (c-if 'x 'y 'z) 2 3)))

      (test "if (x ? y : z) {
    2;
} else {
    3;
}
"
          (show #f (c-expr '(if (if x y z) 2 3))))

      (test "if (x ? y : z) {
    2;
} else {
    3;
}
"
          (show #f (c-expr '(%begin (if (if x y z) 2 3)))))

      (test "if (x ? y : z) {
    2;
} else {
    if (w) {
        3;
    } else {
        4;
    }
}
"
          (show #f (c-expr '(if (if x y z) 2 (if w 3 4)))))

      (test "if (x ? y : z) {
    2;
} else {
    if (w) {
        if (u) {
            3;
        } else {
            4;
        }
    } else {
        5;
    }
}
"
          (show #f (c-expr '(if (if x y z) 2 (if w (if u 3 4) 5)))))

      (test "int square (int x) {
    return x * x;
}
"
          (show #f (c-fun 'int 'square '((int x)) (c* 'x 'x))))

      (test "int foo (int x, int y, int z) {
    if (x ? y : z) {
        return 2;
    } else {
        return 3;
    }
}
"
          (show #f (c-fun 'int 'foo '((int x) (int y) (int z))
                          (c-if (c-if 'x 'y 'z) 2 3))))

      (test "some_function(shape, x, y + 1, z);\n"
          (show #f (c-expr '(some_function shape x (+ y 1) z))))

      (test "if (y < 255 && pred(shape, x, y, z) == 0) {
    2;
} else {
    3;
}
"
          (show #f (c-expr '(if (&& (< y 255) (== (pred shape x y z) 0)) 2 3))))

      (test "heightmap[i + k * 16];\n"
          (show #f (c-expr '(vector-ref heightmap (+ i (* k 16))))))

      (test "void bar (int mode, const char *msg, unsigned int arg) {
    if (mode == 1) {
        printf(msg);
    } else {
        printf(msg, arg);
    }
}
"
          (show #f (c-fun 'void 'bar
                          '((int mode)
                            ((%pointer (const char)) msg)
                            ((unsigned int) arg))
                          (c-if (c== 'mode 1) '(printf msg) '(printf msg arg)))))

      (test "while ((line = readline()) != EOF) {
    printf(\"%s\", line);
}
"
          (show #f (c-while (c!= (c= 'line '(readline)) 'EOF)
                            '(printf "%s" line))))

      (test "switch (y) {
case 1:
    x = 1;
    break;
case 2:
    x = 4;
    break;
default:
    x = 5;
    break;
}
"
          (show #f (c-switch 'y
                             (c-case 1 (c= 'x 1))
                             (c-case 2 (c= 'x 4))
                             (c-default (c= 'x 5)))))

      (test "switch (y) {
case 1:
    x = 1;
    break;
case 2:
    x = 4;
default:
    x = 5;
    break;
}
"
          (show #f (c-switch 'y
                             (c-case 1 (c= 'x 1))
                             (c-case/fallthrough 2 (c= 'x 4))
                             (c-default (c= 'x 5)))))

      (test "switch (y) {
case 1:
    x = 1;
    break;
case 2:
    x = 4;
    break;
default:
    x = 5;
    break;
}
"
          (show #f (c-switch 'y '((1) (= x 1)) '((2) (= x 4)) '(else (= x 5)))))

      (test "switch (y) {
case 1:
    x = 1;
    break;
case 2:
    x = 4;
    break;
default:
    x = 5;
    break;
}
"
          (show #f (c-expr '(switch y ((1) (= x 1)) ((2) (= x 4)) (else (= x 5))))))

      (test "int q (int x) {
    switch (x) {
    case 1:
        return 1;
    case 2:
        return 4;
    default:
        return 5;
    }
}
"
          (show #f (c-fun 'int 'q '(x) (c-switch 'x '((1) 1) '((2) 4) '(else 5)))))

      (test "switch (x) {
case 1:
case 2:
    foo();
    break;
default:
    bar();
    break;
}
"
          (show #f (c-expr '(switch x ((1 2) (foo)) (else (bar))))))

      (test "switch (x) {
case 1:
    foo();
    break;
case 2:
case 3:
    bar();
    break;
default:
    baz();
    break;
}
"
          (show #f (c-expr
                    '(switch x (case 1 (foo)) (case (2 3) (bar)) (else (baz))))))

      (test "switch (x) {
case 1:
case 2:
    foo();
default:
    bar();
    break;
}
"
          (show #f (c-expr '(switch x (case/fallthrough (1 2) (foo)) (else (bar))))))

      (test "switch (x) {
case 1:
case 2:
    foo();
    break;
default:
    bar();
    break;
}
"
          (show #f (c-expr '(switch x ((1 2) (foo)) (default (bar))))))

      (test "switch (x) {
default:
    bar();
case 1:
case 2:
    foo();
    break;
}
"
          (show #f (c-expr '(switch x (else/fallthrough (bar)) ((1 2) (foo))))))

      (test "for (i = 0; i < n; i++) {
    printf(\"i: %d\");
}
"
          (show #f (c-for (c= 'i 0) (c< 'i 'n) (c++/post 'i) '(printf "i: %d"))))

      (test "a * x + b * y == c;\n"
          (show #f (c== (c+ (c* 'a 'x) (c* 'b 'y)) 'c)))
      (test "a * x + b * y == c;\n"
          (show #f (c-expr '(== (+ (* a x) (* b y)) c))))

      (test "(a + x) * (b + y) == c;\n"
          (show #f (c-expr '(== (* (+ a x) (+ b y)) c))))

      (test "1 - (3 + 2);\n"
          (show #f (c-expr '(- 1 (+ 3 2)))))
      (test "1 - (3 - 2);\n"
          (show #f (c-expr '(- 1 (- 3 2)))))
      (test "1 - 3 - 2;\n"
          (show #f (c-expr '(- 1 3 2))))
      (test "1 + (3 + 2);\n"
          (show #f (c-expr '(+ 1 (+ 3 2)))))
      (test "1 + 3 + 2;\n"
          (show #f (c-expr '(+ 1 3 2))))

      (test "x == 0 && (y == 2 || y == 3);\n"
          (show #f (c-expr '(%and (== x 0) (%or (== y 2) (== y 3))))))

      (test
          "(abracadabra!!!! + xylophone????)
  * (bananarama____ + yellowstonepark~~~~)
  * (cryptoanalysis + zebramania);\n"
          (show #f (c-expr '(* (+ abracadabra!!!! xylophone????)
                               (+ bananarama____ yellowstonepark~~~~)
                               (+ cryptoanalysis zebramania)))))

      (test
          "abracadabra(xylophone,
            bananarama,
            yellowstonepark,
            cryptoanalysis,
            zebramania,
            delightful,
            wubbleflubbery);\n"
          (show #f (c-expr '(abracadabra xylophone
                                         bananarama
                                         yellowstonepark
                                         cryptoanalysis
                                         zebramania
                                         delightful
                                         wubbleflubbery))))

      (test "#define foo(x, y) ((x) + (y))\n"
          (show #f (cpp-define '(foo (int x) (int y)) (c+ 'x 'y))))

      (test "#define foo(x, y) (2 * ((x) + (y) + z))\n"
          (show #f (cpp-define '(foo (int x) (int y)) '(* 2 (+ x y z)))))

      (test "#define min(x, y) ((x) < (y) ? (x) : (y))\n"
          (show #f (cpp-define '(min x y) (c-if (c< 'x 'y) 'x 'y))))

      (test
          "#define foo(x, y) abracadabra((x) + (y),
                              xylophone,
                              bananarama,
                              yellowstonepark,
                              cryptoanalysis,
                              zebramania,
                              delightful,
                              wubbleflubbery)
"
          (show #f (cpp-define '(foo x y)
                               '(abracadabra (+ x y)
                                             xylophone
                                             bananarama
                                             yellowstonepark
                                             cryptoanalysis
                                             zebramania
                                             delightful
                                             wubbleflubbery))))

      (test "#ifndef FOO_H
#define FOO_H

extern int foo ();

#endif  /* ! FOO_H */
"
          (show #f (cpp-wrap-header
                    'FOO_H
                    (c-extern (c-prototype 'int 'foo '())))))

      (test "#if foo
1
#elif bar
2
#elif baz
3
#else 
4
#endif
"
          (show #f (cpp-if 'foo 1 'bar 2 'baz 3 4)))

      (test "/* this is a /\\* nested *\\/ comment */"
          (show #f (c-comment " this is a /* nested */ comment ")))

      (test "/* this is a /\\* nested *\\/ comment */"
          (show #f (c-comment " this is a " (c-comment " nested ") " comment ")))

      ;; the initial leading space is annoying but hard to remove at the
      ;; moment - the important thing is we preserve indentation in the body
      (test "switch (y) {
case 1:
    x = 1;
    break;
#ifdef H_TWO
case 2:
    x = 4;
    break;
#endif  /* H_TWO */
default:
    x = 5;
    break;
}
"
          (show #f (c-expr
                    `(switch y
                             ((1) (= x 1))
                             ,(cpp-ifdef 'H_TWO (c-case '(2) '(= x 4)))
                             (else (= x 5))))))

      (test "#define eprintf(...) fprintf(stderr, __VA_ARGS__)\n"
          (show #f (c-expr '(%define (eprintf . args) (fprintf stderr args)))))

      (test "struct point {
    int x;
    int y;
};
"
          (show #f (c-expr `(struct point (x y)))))

      (test "struct employee {
    short age;
    char *name;
    struct {
        int year;
        int month;
        int day;
    } dob;
} __attribute__ ((packed));
"
          (show #f (c-expr `(struct employee
                                    ((short age)
                                     ((%pointer char) name)
                                     ((struct (year month day)) dob))
                                    (%attribute packed)
                                    ))))

      (test "class employee {
    short age;
    char *name;
    struct {
        int year;
        int month;
        int day;
    } dob;
} __attribute__ ((packed));
"
          (show #f (c-class 'employee
                            '((short age)
                              ((%pointer char) name)
                              ((struct (year month day)) dob))
                            (c-attribute 'packed)
                            )))

      (test "union object {
    char tag;
    struct {
        char tag;
        char *data;
    } string;
    struct {
        char tag;
        void *car;
        void *cdr;
    } pair;
    struct {
        char tag;
        unsigned int length;
        void *data;
    } vector;
};
"
          (show #f (c-expr
                    '(union object
                            ((char tag)
                             ((struct ((char tag) ((* char) data))) string)
                             ((struct ((char tag)
                                       ((* void) car)
                                       ((* void) cdr)))
                              pair)
                             ((struct ((char tag)
                                       ((unsigned int) length)
                                       ((* void) data)))
                              vector)
                             )))))

      (test "enum type_tags {
    TYPE_CHAR = 1,
    TYPE_FIXNUM,
    TYPE_BOOLEAN,
    TYPE_NULL,
    TYPE_EOF,
    TYPE_STRING,
    TYPE_PAIR,
    TYPE_VECTOR
};
"
          (show #f (c-expr '(enum type_tags ((TYPE_CHAR 1) TYPE_FIXNUM TYPE_BOOLEAN TYPE_NULL TYPE_EOF TYPE_STRING TYPE_PAIR TYPE_VECTOR)))))

      (test "#define OP_EVAL 0xFE\n" (show #f (with ((radix 16)) (cpp-define 'OP_EVAL 254))))

      (test "unsigned long table[SIZE] = {1, 2, 3, 4};\n"
          (show #f (c-var '(%array (unsigned long) SIZE) 'table '#(1 2 3 4))))

      (test "int *array_of_ptr[];\n"
          (show #f (c-var '(%array (* int)) 'array_of_ptr)))

      (test "int (*ptr_to_array)[];\n"
          (show #f (c-var '(* (%array int)) 'ptr_to_array)))

      (test "foo **table = {{1, \"foo\"}, {2, \"bar\"}, {3, \"baz\"}, {4, \"qux\"}};\n"
          (show #f (c-var '(* (* foo)) 'table
                          '#(#(1 "foo") #(2 "bar") #(3 "baz") #(4 "qux")))))

      (test "sexp (*f)(sexp, sexp) = NULL;\n"
          (show #f (c-var '(%fun sexp (sexp sexp)) 'f 'NULL)))

      (test "sexp (*)(sexp) (*f)(sexp, sexp) = NULL;\n"
          (show #f (c-var '(%fun (%fun sexp (sexp)) (sexp sexp)) 'f 'NULL)))

      (test "typedef double (*f)(double *, double, int);\n"
          (show #f (c-typedef '(%fun double ((* double) double int)) 'f)))

      (test "\"foo\\tbar\";\n"
          (show #f (c-expr "foo\tbar")))

      (test "static int i;\n"
          (show #f (c-expr '(static (%var int i)))))

      (test-end))))
