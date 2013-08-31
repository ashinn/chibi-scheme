
(import (chibi) (chibi pathname) (chibi test))

(test-begin "pathname")

;; tests from the dirname(3) manpage

(test "dirname(3)" "/usr" (path-directory "/usr/lib"))
(test "lib"  (path-strip-directory "/usr/lib"))

(test "/"    (path-directory "/usr/"))
(test ""     (path-strip-directory "/usr/"))

(test "."    (path-directory "usr"))
(test "usr"  (path-strip-directory "usr"))

(test "/"    (path-directory "/"))
(test ""     (path-strip-directory "/"))

(test "."    (path-directory "."))
(test "."    (path-strip-directory "."))

(test "."    (path-directory ".."))
(test ".."   (path-strip-directory ".."))

;; additional tests (should match GNU dirname/basename behavior)

(test "path-directory:border"
            "/"    (path-directory "//"))
(test ""     (path-strip-directory "//"))

(test "."    (path-directory ""))
(test ""     (path-strip-directory ""))

(test "."    (path-directory "../"))
(test ""     (path-strip-directory "../"))

(test ".."   (path-directory "../.."))
(test ".."   (path-strip-directory "../.."))

(test "path-directory:extra"
            "/usr/local" (path-directory "/usr/local/lib"))
(test "lib" (path-strip-directory "/usr/local/lib"))

(test "/usr" (path-directory "/usr/local/"))
(test "" (path-strip-directory "/usr/local/"))

(test "usr"    (path-directory "usr/local"))
(test "local"  (path-strip-directory "usr/local"))

(test "/" (path-directory "//usr"))
(test "usr" (path-strip-directory "//usr"))

(test "/" (path-directory "//usr/"))
(test "" (path-strip-directory "//usr/"))

(test "path-directory:small"
            "/a" (path-directory "/a/b"))
(test "b" (path-strip-directory "/a/b"))

(test "a" (path-directory "a/b"))
(test "b" (path-strip-directory "a/b"))

(test "a" (path-directory "a/b/"))
(test "" (path-strip-directory "a/b/"))

(test "/a/b/c" (path-directory "/a/b/c/d"))
(test "d" (path-strip-directory "/a/b/c/d"))

(test "/a/b/c" (path-directory "/a/b/c/d/"))
(test "" (path-strip-directory "/a/b/c/d/"))

(test "a/b/c"    (path-directory "a/b/c/d"))
(test "d"  (path-strip-directory "a/b/c/d"))

(test "/a/b" (path-directory "/a/b/c.d"))
(test "c.d" (path-strip-directory "/a/b/c.d"))

(test "/a/b" (path-directory "/a/b/c.d/"))
(test "" (path-strip-directory "/a/b/c.d/"))

(test "/a/b/c" (path-directory "/a/b/c/."))
(test "." (path-strip-directory "/a/b/c/."))

(test "/a/b/c" (path-directory "/a/b/c/.."))
(test ".." (path-strip-directory "/a/b/c/.."))

(test "/a/b/." (path-directory "/a/b/./c"))
(test "c" (path-strip-directory "/a/b/./c"))

(test "/a/b/.." (path-directory "/a/b/../c"))
(test "c" (path-strip-directory "/a/b/../c"))

(test "/a/b" (path-directory "/a/b/c//"))
(test "" (path-strip-directory "/a/b/c//"))

(test "/a/b" (path-directory "/a/b//c///"))
(test "" (path-strip-directory "/a/b//c///"))

;; extensions

(test "path-extension" "scm" (path-extension "foo.scm"))
(test "foo" (path-strip-extension "foo.scm"))

(test "c" (path-extension "foo.scm.c"))
(test "foo.scm" (path-strip-extension "foo.scm.c"))

(test "scm" (path-extension "/home/me/foo.scm"))
(test "/home/me/foo" (path-strip-extension "/home/me/foo.scm"))

(test "scm" (path-extension "foo..scm"))
(test "foo." (path-strip-extension "foo..scm"))

(test "s" (path-extension "foo.s"))
(test "foo" (path-strip-extension "foo.s"))

(test #f (path-extension "foo."))
(test "foo." (path-strip-extension "foo."))

(test #f (path-extension "foo.scm."))
(test "foo.scm." (path-strip-extension "foo.scm."))

(test #f (path-extension "."))
(test "." (path-strip-extension "."))

(test #f (path-extension "a."))
(test "a." (path-strip-extension "a."))

(test #f (path-extension "/."))
(test "/." (path-strip-extension "/."))

(test #f (path-extension "foo.scm/"))
(test "foo.scm/" (path-strip-extension "foo.scm/"))

(test "path-replace-extension"
            "foo.c" (path-replace-extension "foo.scm" "c"))
(test "foo.c" (path-replace-extension "foo" "c"))

;; absolute paths

(test-assert (path-absolute? "/"))
(test-assert (path-absolute? "//"))
(test-assert (path-absolute? "/usr"))
(test-assert (path-absolute? "/usr/"))
(test-assert (path-absolute? "/usr/."))
(test-assert (path-absolute? "/usr/.."))
(test-assert (path-absolute? "/usr/./"))
(test-assert (path-absolute? "/usr/../"))

(test-assert (not (path-absolute? "")))
(test-assert (not (path-absolute? ".")))
(test-assert (not (path-absolute? "usr")))
(test-assert (not (path-absolute? "usr/")))

;; normalization & building

(test "path-normalize" "/a/b/c/d/e" (path-normalize "/a/b/c/d/./e"))
(test "/a/b/c/d/e" (path-normalize "/a/b//.///c//d/./e"))
(test "/a/b/c/d/e/" (path-normalize "/a/b//.///c//d/./e/"))
(test "/a/c/d/e" (path-normalize "/a/b/../c/d/e"))
(test "/a/b/c/e" (path-normalize "/a/b//.///c//d/../e"))
(test "/a/c/e" (path-normalize "/a/b//..///c//d/../e"))
(test "/a/b/c/d/e/"
            (path-normalize "/a/b//./../c/d/../../b//c/d/e/f/.."))

(test "path-normalize:border" "" (path-normalize ""))
(test "." (path-normalize "."))
(test "/" (path-normalize "/"))

(test "path-normalize:overflow"
            "/" (path-normalize "/a/b/c/../../../../.."))
(test "../.." (path-normalize "a/b/c/../../../../.."))
(test "../../.." (path-normalize "../a/b/c/../../../../.."))

(test "" (path-strip-leading-parents ".."))
(test "" (path-strip-leading-parents "../"))
(test "a" (path-strip-leading-parents "../a"))
(test "a/b" (path-strip-leading-parents "../../a/b"))
(test "a/b" (path-strip-leading-parents "../../../a/b"))
(test "a/../b" (path-strip-leading-parents "../../../a/../b"))

(test "path-relative-to" "c" (path-relative-to "/a/b/c" "/a/b"))
(test "c" (path-relative-to "/a/b/c" "/a/b/"))
(test "." (path-relative-to "/a/b/" "/a/b/"))
(test "." (path-relative-to "/a/b/" "/a/b"))
(test "." (path-relative-to "/a/b" "/a/b/"))
(test "." (path-relative-to "/a/b" "/a/b"))
(test-not (path-relative-to "/d/a/b/c" "/a/b"))

(test "make-path" "a/b" (make-path "a" "b"))
(test "a/b" (make-path "a/" "b"))
(test "a/b/./c" (make-path "a" "b" "." "c"))
(test "a/b/../c" (make-path "a" "b" ".." "c"))
(test "a/b/c" (make-path "a" '("b" "c")))

(test-end)
