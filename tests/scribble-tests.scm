
(cond-expand
 (modules (import (chibi scribble) (only (chibi test) test-begin test test-end)))
 (else (load "lib/chibi/scribble.scm")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-begin "scribble")

(define (test-scribble expect str)
  (test (call-with-output-string (lambda (out) (write str out)))
      expect
    (call-with-input-string str scribble-parse)))

(test-scribble '((foo "blah blah blah")) "@foo{blah blah blah}")
(test-scribble '((foo "blah \"blah\" (`blah'?)")) "@foo{blah \"blah\" (`blah'?)}")
(test-scribble '((foo 1 2 "3 4")) "@foo[1 2]{3 4}")
(test-scribble '((foo 1 2 3 4)) "@foo[1 2 3 4]")
(test-scribble '((foo width: 2 "blah blah")) "@foo[width: 2]{blah blah}")
(test-scribble '((foo "blah blah" "\n" "     yada yada")) "@foo{blah blah
     yada yada}")
(test-scribble '((foo "  blah blah" "\n" "  yada yada" "\n")) "@foo{
  blah blah
  yada yada
}")

(test-scribble '((foo "bar " (baz "3") "\n" "     blah")) "@foo{bar @baz{3}
     blah}")
(test-scribble '((foo (b (u 3) " " (u "4")) "\n" "     blah")) "@foo{@b{@u[3] @u{4}}
     blah}")
(test-scribble '((C "while (*(p++))" "\n" "     *p = '\\n';")) "@C{while (*(p++))
     *p = '\\n';}")
(test-scribble '(("blah blah")) "@{blah blah}")
(test-scribble '(("blah " (3))) "@{blah @[3]}")
(test-scribble '(("foo" "\n" "   bar" "\n" "   baz")) "@{foo
   bar
   baz}")
(test-scribble '(foo) "@foo")
(test-scribble '(("blah " foo " blah")) "@{blah @foo blah}")
(test-scribble '(("blah " foo: " blah")) "@{blah @foo: blah}")
(test-scribble '(("blah " foo ": blah")) "@{blah @|foo|: blah}")
(test-scribble '((foo "(+ 1 2) -> " (+ 1 2) "!")) "@foo{(+ 1 2) -> @(+ 1 2)!}")
(test-scribble '((foo "A string escape")) "@foo{A @\"string\" escape}")
(test-scribble '((foo "eli@barzilay.org")) "@foo{eli@\"@\"barzilay.org}")
(test-scribble '((foo "A { begins a block")) "@foo{A @\"{\" begins a block}")
(test-scribble '((C "while (*(p++)) {" "\n" "     *p = '\\n';" "\n" "   }"))
 "@C{while (*(p++)) {
     *p = '\\n';
   }}")
(test-scribble '((foo "bar}@{baz")) "@foo|{bar}@{baz}|")
(test-scribble '((foo "bar " (x "X") " baz")) "@foo|{bar |@x{X} baz}|")
(test-scribble '((foo "bar " (x "@") " baz")) "@foo|{bar |@x|{@}| baz}|")

(test-scribble '((foo "bar}@|{baz")) "@foo|--{bar}@|{baz}--|")
(test-scribble '((foo "bar}@|{baz")) "@foo|<<{bar}@|{baz}>>|")

(test-scribble '((foo "bar " (baz 2 3) " {4 5}")) "@foo{bar @baz[2 3] {4 5}}")

(test-scribble '(`',@(foo "blah")) "@`',@foo{blah}")
;;(test-scribble '(#`#'#,@(foo "blah")) "@#`#'#,@foo{blah}")
(test-scribble '(((lambda (x) x) "blah")) "@(lambda (x) x){blah}")
(test-scribble '(`(,foo  "blah")) "@`(unquote foo){blah}")

(test-scribble '(("foo bar" "\n" "  baz")) "@{foo bar
  baz}")
(test-scribble '('("foo bar" "\n" "       baz")) "@'{foo bar
       baz}")
(test-scribble '((foo "bar      baz     blah")) "@foo{bar @; comment
     baz@;
     blah}")

(test-scribble '((foo "x " y " z")) "@foo{x @y z}")
(test-scribble '((foo "x " (* y 2) " z")) "@foo{x @(* y 2) z}")
(test-scribble '((foo " bar")) "@{@foo bar}")
(test-scribble '(((foo "bar") "baz")) "@@foo{bar}{baz}")

(test-scribble '((foo 1 (* 2 3) "bar")) "@foo[1 (* 2 3)]{bar}")
(test-scribble '((foo (bar "...") "blah")) "@foo[@bar{...}]{blah}")
(test-scribble '((foo bar)) "@foo[bar]")
(test-scribble '((foo "bar " (f x) " baz")) "@foo{bar @f[x] baz}")
(test-scribble '((foo "bar")) "@foo[]{bar}")
(test-scribble '((foo)) "@foo[]")
(test-scribble '(foo) "@foo")
(test-scribble '((foo)) "@foo{}")

(test-scribble '((foo 'style: 'big  "bar")) "@foo['style: 'big]{bar}")

(test-scribble '((foo "f{o}o")) "@foo{f{o}o}")
(test-scribble '((foo "{{}}{}")) "@foo{{{}}{}}")
(test-scribble '((foo "bar")) "@foo{bar}")
(test-scribble '((foo " bar ")) "@foo{ bar }")
(test-scribble '((foo 1 " bar ")) "@foo[1]{ bar }")

(test-scribble '((foo "a " (bar "b") " c")) "@foo{a @bar{b} c}")
(test-scribble '((foo "a " bar " c")) "@foo{a @bar c}")
(test-scribble '((foo "a " (bar 2) " c")) "@foo{a @(bar 2) c}")
(test-scribble '((foo "A } marks the end")) "@foo{A @\"}\" marks the end}")
(test-scribble '((foo "The prefix: @.")) "@foo{The prefix: @\"@\".}")
(test-scribble '((foo "@x{y} --> (x \"y\")")) "@foo{@\"@x{y}\" --> (x \"y\")}")

(test-scribble '((foo "...")) "@foo|{...}|")
(test-scribble '((foo "\"}\" follows \"{\"")) "@foo|{\"}\" follows \"{\"}|")
(test-scribble '((foo "Nesting |{is}| ok")) "@foo|{Nesting |{is}| ok}|")

(test-scribble '((foo "Maze" "\n" "      " (bar "is") "\n" "      Life!"))
 "@foo|{Maze
      |@bar{is}
      Life!}|")
(test-scribble '((t "In " (i "sub@s") " too")) "@t|{In |@i|{sub|@\"@\"s}| too}|")
(test-scribble '((foo "@x{foo} |@{bar}|.")) "@foo|<<<{@x{foo} |@{bar}|.}>>>|")
(test-scribble '((foo "X " (b "Y") "...")) "@foo|!!{X |!!@b{Y}...}!!|")

(test-scribble '((foo "foo" bar.)) "@foo{foo@bar.}")
(test-scribble '((foo "foo" bar ".")) "@foo{foo@|bar|.}")
(test-scribble '((foo "foo" 3.0)) "@foo{foo@3.}")
(test-scribble '((foo "foo" 3 ".")) "@foo{foo@|3|.}")
(test-scribble '((foo "foo" (f 1) "{bar}")) "@foo{foo@|(f 1)|{bar}}")
(test-scribble '((foo "foo" bar "[1]{baz}")) "@foo{foo@|bar|[1]{baz}}")
(test-scribble '((foo "xyz")) "@foo{x@\"y\"z}")
(test-scribble '((foo "x" "y" "z")) "@foo{x@|\"y\"|z}")
(test-scribble '((foo "x" 1 (+ 2 3) 4 "y")) "@foo{x@|1 (+ 2 3) 4|y}")
(test-scribble '((foo "x" * * "y")) "@foo{x@|*
        *|y}")
(test-scribble '((foo "Alice" "Bob" "Carol")) "@foo{Alice@||Bob@|
     |Carol}")
(test-scribble '((blah)) "@|{blah}|")
(test-scribble '((blah blah)) "@|{blah blah}|")

(test-scribble '((foo "First line" "\n" "     Second line")) "@foo{First line@;{there is still a
                  newline here;}
     Second line}")
(test-scribble '((foo "A long      single-     string arg.")) "@foo{A long @;
     single-@;
     string arg.}")

(test-scribble '((foo "bar")) "@foo{bar}")
(test-scribble '((foo " bar ")) "@foo{ bar }")
(test-scribble '((foo " bar" "\n" "     baz ")) "@foo{ bar
     baz }")

(test-scribble '((foo "bar" "\n")) "@foo{bar
}")
(test-scribble '((foo "  bar" "\n") "\n") "@foo{
  bar
}
")
(test-scribble '((foo "  bar" "\n" "\n")) "@foo{

  bar

}")
(test-scribble '((foo "  bar" "\n" "\n" "  baz" "\n")) "@foo{
  bar

  baz
}")
(test-scribble '((foo)) "@foo{
}")
(test-scribble '((foo)) "@foo{

}")
(test-scribble '((foo " bar" "\n" "     baz ")) "@foo{ bar
     baz }")

(test-scribble '((foo "  bar" "\n" "  baz" "\n" "  blah" "\n")) "@foo{
  bar
  baz
  blah
}")
(test-scribble '((foo "  begin" "\n" "    x++;" "\n" "  end")) "@foo{
  begin
    x++;
  end}")
(test-scribble '((foo "    a" "\n" "   b" "\n" "  c")) "@foo{
    a
   b
  c}")

(test-scribble '((foo "bar" "\n" "       baz" "\n" "     bbb")) "@foo{bar
       baz
     bbb}")
(test-scribble '((foo " bar" "\n" "        baz" "\n" "      bbb")) "@foo{ bar
        baz
      bbb}")
(test-scribble '((foo "bar" "\n" "   baz" "\n" "   bbb")) "@foo{bar
   baz
   bbb}")
(test-scribble '((foo " bar" "\n" "   baz" "\n" "   bbb")) "@foo{ bar
   baz
   bbb}")
(test-scribble
 '((foo " bar" "\n" "   baz" "\n" "     bbb"))
 "@foo{ bar
   baz
     bbb}")
(test-scribble
 '((text "Some " (b "bold" "\n" "\n" "  text")", and" "\n" "\n" "  more text."))
 "@text{Some @b{bold

  text}, and

  more text.}")

(test-scribble '((foo "  " " bar " "\n" "  " " baz")) "@foo{
  @|| bar @||
  @|| baz}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-end)
