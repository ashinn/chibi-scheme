(import (scheme base) (scheme write) (scheme process-context) (srfi 1)
        (chibi ast) (chibi filesystem) (chibi match) (chibi pathname)
        (chibi process) (chibi string) (chibi test))

(test-begin "snow")

;; setup a temp root to install packages
(define install-prefix "tests/snow/tmp-root")
(define install-libdir (make-path install-prefix "/share/snow/chibi"))
(if (file-exists? install-prefix)
    (delete-file-hierarchy install-prefix))
(create-directory install-prefix)

;; ignore any personal config settings
(setenv "SNOW_CHIBI_CONFIG" "no-such-file")

;; run snow-chibi command as a separate process with test defaults
(define chibi-path "./chibi-scheme")
(define (snow-command . args)
  `(,chibi-path "tools/snow-chibi"
                --always-no
                --implementations "chibi"
                --chibi-path ,(string-append chibi-path " -A " install-libdir)
                --install-prefix ,install-prefix
                --local-user-repository "tests/snow/repo-cache"
                ,@args))

(define-syntax snow
  (syntax-rules ()
    ((snow args ...)
     (match (process->output+error+status (apply snow-command `(args ...)))
       ((output error (? zero?))
        ;;(display output)
        ;;(display error)
        )
       ((output error status)
        (display "Snow failed:\n")
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

(define (snow-status)
  (snow->sexp status))

(define (installed-status status lib-name . o)
  (let* ((impl (if (pair? o) (car o) 'chibi))
         (impl-status (assq impl status)))
    (and impl-status
         (assoc lib-name (cdr impl-status)))))

(define (installed-version status lib-name . o)
  (cond ((apply installed-status status lib-name o) => cadr)
        (else #f)))

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
      tests/snow/repo1/leonardo/fibonacci.sld)
(snow index ,(cadr repo1) tests/snow/repo1/leonardo-fibonacci-1.0.tgz)
(snow ,@repo1 update)
(snow ,@repo1 install --show-tests leonardo.fibonacci)
(test "1.0" (installed-version (snow-status) '(leonardo fibonacci)))

(define repo2 '(--repository-uri tests/snow/repo2/repo.scm))
(snow package --output-dir tests/snow/repo2/
      --version 1.1 --authors "Leonardo Fibonacci"
      --description "Fibonacci recurrence relation"
      tests/snow/repo2/leonardo/fibonacci.sld)
(snow index ,(cadr repo2))
(snow ,@repo2 update)
(snow ,@repo2 upgrade leonardo.fibonacci)
(test "1.1" (installed-version (snow-status) '(leonardo fibonacci)))

(define repo3 '(--repository-uri tests/snow/repo3/repo.scm))
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Pingala"
      --description "Factorial"
      tests/snow/repo3/pingala/factorial.scm)
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Pingala"
      --description "Binomial Coefficients"
      tests/snow/repo3/pingala/binomial.scm)
(snow package --output-dir tests/snow/repo3/
      --version 1.0 --authors "Pingala"
      --description "Pingala's test framework"
      tests/snow/repo3/pingala/test-map.scm)
(snow index ,(cadr repo3))
(snow ,@repo3 update)
(snow ,@repo3 install pingala.binomial)
(let ((status (snow-status)))
  (test-assert (installed-version status '(pingala binomial)))
  (test-assert (installed-version status '(pingala factorial))))

(test-end)
