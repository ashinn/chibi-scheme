(define-library (chibi app-test)
  (import (scheme base) (chibi app) (chibi config) (chibi test))
  (export run-tests)
  (begin
    (define (feed cfg spec . args)
      (let ((animals (conf-get-list cfg 'animals '())))
        (cons (if (conf-get cfg 'lions) (cons 'lions animals) animals) args)))
    (define (wash cfg spec . args)
      (let ((animals (conf-get-list cfg 'animals '())))
        (cons (cons 'soap (conf-get cfg '(command wash soap))) animals)))
    (define zoo-app-spec
      `(zoo
        "Zookeeper Application"
        (@
         (animals (list symbol) "list of animals to act on (default all)")
         (lions boolean (#\l) "also apply the action to lions"))
        (or
         (feed "feed the animals" (,feed animals ...))
         (wash "wash the animals" (@ (soap boolean)) (,wash animals ...))
         (help "print help" (,app-help-command)))
        ))
    (define (run-tests)
      (test-begin "app")
      (test '((camel elephant) "today")
          (run-application
           zoo-app-spec
           '("zoo" "--animals" "camel,elephant" "feed" "today")))
      (test '((lions camel elephant) "tomorrow")
          (run-application
           zoo-app-spec
           '("zoo" "--animals" "camel,elephant" "--lions" "feed" "tomorrow")))
      (test '((soap . #f) rhino)
          (run-application zoo-app-spec '("zoo" "--animals" "rhino" "wash")))
      (test '((soap . #t) rhino)
          (run-application zoo-app-spec
                           '("zoo" "--animals" "rhino" "wash" "--soap")))
      (test '((soap . #t) rhino)
          (run-application zoo-app-spec
                           '("zoo" "wash" "--soap" "--animals" "rhino")))
      (test 'error
          (guard (exn (else 'error))
            (run-application zoo-app-spec
                           '("zoo" "--soap" "wash" "--animals" "rhino"))))
      (test-end))))
