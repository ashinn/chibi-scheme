(import (scheme base) (scheme write) (scheme process-context) (srfi 1)
        (chibi ast) (chibi config) (chibi filesystem) (chibi match)
        (chibi pathname) (chibi process) (chibi regexp) (chibi string)
        (chibi io) (chibi tar) (chibi test)
        (chibi snow package))

(test-begin "snow")

;; setup a temp root to install packages
(define install-prefix (make-path (current-directory) "tests/snow/tmp-root"))
(define install-libdir (make-path install-prefix "/share/snow/chibi"))
(if (file-exists? install-prefix)
    (delete-file-hierarchy install-prefix))
(create-directory install-prefix)

;; setup chicken install directory with minimum required modules
(define chicken-lib-dir "/usr/local/lib/chicken/7")
(define chicken-install-dir (make-path install-prefix "lib/chicken/7"))
(create-directory* chicken-install-dir)
(if (file-exists? chicken-lib-dir)
    (let ((rx-required
           '(: (or "chicken" "csi" "data-structures" "extras" "files"
                   "foreign" "irregex" "lolevel" "make" "matchable"
                   "modules" "numbers" "ports" "posix" "r7rs" "scheme"
                   "srfi" "tcp" "types" "utils")
               (or "." "-")
               (* any))))
      (for-each
      (lambda (file)
        (if (regexp-matches? rx-required file)
            (system 'cp
                    (make-path chicken-lib-dir file)
                    chicken-install-dir)))
      (directory-files chicken-lib-dir))))
(setenv "CHICKEN_REPOSITORY" chicken-install-dir)

;; ignore any personal config settings
(setenv "SNOW_CHIBI_CONFIG" "no-such-file")

;; run snow-chibi command as a separate process with test defaults
(define chibi-path "./chibi-scheme")
(define (snow-command . args)
  `(,chibi-path -A ,install-libdir "tools/snow-chibi"
                --always-no
                --implementations "chibi"
                --chibi-path ,(string-append chibi-path " -A" install-libdir)
                --install-prefix ,install-prefix
                --local-user-repository "tests/snow/repo-cache"
                ,@args))

(define-syntax snow
  (syntax-rules ()
    ((snow args ...)
     (match (process->output+error+status (apply snow-command `(args ...)))
       ((output error (? zero?))
        ;; (display output)
        ;; (display error)
        )
       ((output error status)
        (display "Snow failed: ")
        (display status)
        (newline)
        (display output)
        (display error)
        (newline))
       (other
        (display "Snow error:\n")
        (display other)
        (newline))))))

(define-syntax snow->string
  (syntax-rules ()
    ((snow->string args ...)
     (process->string (apply snow-command `(args ...))))))

(define-syntax snow->sexp
  (syntax-rules ()
    ((snow->sexp args ...)
     (process->sexp (apply snow-command `(--sexp args ...))))))

(define-syntax snow-status
  (syntax-rules ()
    ((snow-status args ...)
     (snow->sexp args ... status))))

(define (installed-status status lib-name . o)
  (let* ((impl (if (pair? o) (car o) 'chibi))
         (impl-status (assq impl status)))
    (and impl-status
         (assoc lib-name (cdr impl-status)))))

(define (installed-version status lib-name . o)
  (cond ((apply installed-status status lib-name o) => cadr)
        (else #f)))

(define (snowball-test->sexp-list pkg file)
  (let ((path (make-path (package-file-top-directory file)
                         (assoc-get pkg 'test))))
    (call-with-input-string
        (utf8->string (tar-extract-file (package-file-unzipped file) path))
      port->sexp-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basics

;; package
(snow package --output-dir tests/snow --authors "Édouard Lucas"
      --description "Lucas recurrence relation"
      tests/snow/repo0/edouard/lucas.sld)
(test-assert (file-exists? "tests/snow/edouard-lucas.tgz"))

;; install
(snow install tests/snow/edouard-lucas.tgz)
(define lucas-sld-path
  (make-path install-libdir "edouard/lucas.sld"))
(test-assert (file-exists? lucas-sld-path))
(delete-file "tests/snow/edouard-lucas.tgz")

;; status
(test-assert (installed-status (snow-status) '(edouard lucas)))

;; remove
(snow remove edouard.lucas)
(test-not (file-exists? lucas-sld-path))
(test-not (installed-version (snow-status) '(edouard lucas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install/upgrade via local repos

(define repo1 '(--repository-uri tests/snow/repo1/repo.scm))
(snow package --output-dir tests/snow/repo1/
      --version 1.0 --authors "Leonardo Fibonacci"
      --description "Fibonacci recurrence relation"
      --test tests/snow/repo1/leonardo/fibonacci-test.scm
      tests/snow/repo1/leonardo/fibonacci.sld)
(snow index ,(cadr repo1) tests/snow/repo1/leonardo-fibonacci-1.0.tgz)
(snow ,@repo1 install --show-tests leonardo.fibonacci)
(test "1.0" (installed-version (snow-status) '(leonardo fibonacci)))

(define repo2 '(--repository-uri tests/snow/repo2/repo.scm))
(snow package --output-dir tests/snow/repo2/
      --version 1.1 --authors "Leonardo Fibonacci"
      --description "Fibonacci recurrence relation"
      --test tests/snow/repo2/leonardo/fibonacci-test.scm
      tests/snow/repo2/leonardo/fibonacci.sld)
(snow index ,(cadr repo2))
(snow ,@repo2 upgrade leonardo.fibonacci)
(test "1.1" (installed-version (snow-status) '(leonardo fibonacci)))

(define repo3 '(--repository-uri tests/snow/repo3/repo.scm))
(setenv "PINGALA_GANAS_PATH"
        "pingala:tests/snow/tmp-root/share/snow/chibi/pingala")
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Pingala"
      --description "Factorial"
      tests/snow/repo3/pingala/factorial.scm)
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Pingala"
      --description "Binomial Coefficients"
      --test tests/snow/repo3/pingala/binomial-test.scm
      tests/snow/repo3/pingala/binomial.scm)
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Pingala"
      --description "Pingala's test framework"
      tests/snow/repo3/pingala/test-map.scm)
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Pingala"
      --description "Library for Sanskrit poetry"
      --test-library "tests/snow/repo3/pingala/prosody-test.sld"
      --data-files tests/snow/repo3/pingala/ganas.txt
      tests/snow/repo3/pingala/prosody.sld)
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Pingala" --name "(pingala triangle)"
      --description "Program to print a Sierpinski Triangle"
      --programs tests/snow/repo3/pingala/triangle.scm)
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Robert Recorde"
      --description "Equality implementation"
      --test-library "tests/snow/repo3/recorde/equal-test.sld"
      tests/snow/repo3/recorde/equal.sld)
(snow package --output-dir tests/snow/repo3/
      --version "2.7.1" --authors "Leonhard Euler"
      --description "Euler's Totient function"
      --test "tests/snow/repo3/totient-test.scm"
      tests/snow/repo3/totient.scm)
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Pythagoras"
      --description "Pythagoran Theorem"
      --test "tests/snow/repo3/pythagoras/hypotenuse-test.sch"
      tests/snow/repo3/pythagoras/hypotenuse.sch)
(snow index ,(cadr repo3))
(snow ,@repo3 install pingala.binomial)
(snow ,@repo3 install euler.totient)
(let ((status (snow-status)))
  (test-assert (installed-version status '(pingala binomial)))
  (test-assert (installed-version status '(pingala factorial)))
  (test "2.7.1" (installed-version status '(euler totient))))

;; programs
(snow ,@repo3 install pingala.triangle)
(test-assert (file-exists? "tests/snow/tmp-root/bin/triangle"))
(test "1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
"
    (process->string "tests/snow/tmp-root/bin/triangle"))
(test "1
1 1
1 2 1
1 3 3 1
"
    (process->string '("tests/snow/tmp-root/bin/triangle" "3")))

(snow ,@repo3 remove pingala.triangle)
(test-not (file-exists? "tests/snow/tmp-root/bin/triangle"))

;; data files
(snow ,@repo3 install pingala.prosody)
(test-assert (installed-version (snow-status) '(pingala prosody)))
(test-assert
    (file-exists? "tests/snow/tmp-root/share/snow/chibi/pingala/ganas.txt"))

;; chibi ffi
(snow ,@repo3 install pythagoras.hypotenuse)
(test-assert (installed-version (snow-status) '(pythagoras hypotenuse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other implementations

(snow ,@repo2 --implementations "gauche,kawa,larceny"
      install leonardo.fibonacci)
(let ((status (snow-status  --implementations "gauche,kawa,larceny")))
  (test "1.1" (installed-version status '(leonardo fibonacci) 'gauche))
  (test "1.1" (installed-version status '(leonardo fibonacci) 'kawa))
  (test "1.1" (installed-version status '(leonardo fibonacci) 'larceny)))

(snow ,@repo3 --implementations "chicken" --program-implementation "chicken"
      install pingala.triangle)
(let ((status (snow-status --implementations "chicken")))
  (test-assert (installed-version status '(pingala binomial) 'chicken))
  (test-assert (installed-version status '(pingala factorial) 'chicken))
  (test "1\n1 1\n1 2 1\n1 3 3 1\n"
      (process->string '("tests/snow/tmp-root/bin/triangle" "3"))))

(snow ,@repo3 --implementations "foment" install pingala.binomial)
(let ((status (snow-status --implementations "foment")))
  (test-assert (installed-version status '(pingala binomial) 'foment))
  (test-assert (installed-version status '(pingala factorial) 'foment)))

(snow ,@repo3 --implementations "gauche,kawa,larceny"
      install pingala.binomial)
(let ((status (snow-status --implementations "gauche,kawa,larceny")))
  (test-assert (installed-version status '(pingala binomial) 'gauche))
  (test-assert (installed-version status '(pingala factorial) 'gauche))
  (test-assert (installed-version status '(pingala binomial) 'kawa))
  (test-assert (installed-version status '(pingala factorial) 'kawa))
  (test-assert (installed-version status '(pingala binomial) 'larceny))
  (test-assert (installed-version status '(pingala factorial) 'larceny)))

;; this library is fine but the test fails, so this should't be installed
(snow ,@repo3 --implementations "chibi,chicken,gauche,kawa,larceny"
      install recorde.equal)
(let ((status
       (snow-status --implementations "chibi,chicken,gauche,kawa,larceny")))
  (test-not (installed-version status '(recorde equal) 'chibi))
  (test-not (installed-version status '(recorde equal) 'chicken))
  (test-not (installed-version status '(recorde equal) 'gauche))
  (test-not (installed-version status '(recorde equal) 'kawa))
  (test-not (installed-version status '(recorde equal) 'larceny)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart packaging

(define repo4 '(--repository-uri tests/snow/repo4/repo.scm))
(setenv "SNOW_CHIBI_CONFIG" "tests/snow/repo4/config.scm")

(snow ,@repo4 package --output-dir tests/snow/repo4/
      tests/snow/repo4/euler/interest.sld)
(snow index ,(cadr repo4))
(let* ((pkg-file "tests/snow/repo4/euler-interest-2.3.tgz")
       (pkg (package-file-meta pkg-file))
       (libs (package-libraries pkg)))
  (test 2 (length libs))
  (for-each
   (lambda (lib)
     (test "Leonhard Euler" (assoc-get lib 'author)))
   libs)
  (test 'bsd (assoc-get pkg 'license))
  (test "Library for computing (optionally continuously) compounded interest."
      (assoc-get pkg 'description))
  (test '((import (rename (euler interest-test)
                          (run-tests run-euler-interest-test-tests)))
          (run-euler-interest-test-tests))
      (snowball-test->sexp-list pkg pkg-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple repos

(define repo3+4
  '(--repository-uri "tests/snow/repo3/repo.scm,tests/snow/repo4/repo.scm"))

(let ((ls (snow->sexp ,@repo3+4 search euler)))
  (test-assert (assoc '(euler interest) ls))
  (test-assert (assoc '(euler totient) ls)))

(define repo5 '(--repository-uri tests/snow/repo5/repo.scm))

(let ((ls (snow->sexp ,@repo5 search euler)))
  (test-assert (assoc '(euler interest) ls))
  (test-assert (assoc '(euler totient) ls)))

(test-end)
