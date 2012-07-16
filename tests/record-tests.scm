
(cond-expand
 (modules
  (import (srfi 99)
          (only (chibi test) test-begin test-assert test test-end)))
 (else #f))

(test-begin "records")

(define-record-type organism
  (make-organism name)
  organism?
  (name name-of set-name-of!))

;; kingdom
(define-record-type (animal organism)
  (make-animal name food)
  animal?
  ;; all animals eat
  (food food-of set-food-of!))

;; phylum
(define-record-type (chordate animal)
  (make-chordate name food)
  chordate?)

;; class
(define-record-type (mammal chordate)
  (make-mammal name food num-nipples)
  mammal?
  ;; all mammals have nipples
  (num-nipples num-nipples-of set-num-nipples-of!))

;; order
(define-record-type (carnivore mammal)
  (make-carnivore name food num-nipples)
  carnivore?)

(define-record-type (rodent mammal)
  (make-rodent name food num-nipples)
  rodent?)

;; family
(define-record-type (felidae carnivore)
  (make-felidae name food num-nipples)
  felidae?)

(define-record-type (muridae rodent)
  (make-muridae name food num-nipples)
  muridae?)

;; genus
(define-record-type (felis felidae)
  (make-felis name food num-nipples)
  felis?)

(define-record-type (mus muridae)
  (make-mus name food num-nipples)
  mus?)

;; species
(define-record-type (cat felis)
  (make-cat name food num-nipples breed color)
  cat?
  (breed breed-of set-breed-of!)
  (color color-of set-color-of!))

(define-record-type (mouse mus)
  (make-mouse name food num-nipples)
  mouse?)

(define mickey (make-mouse "Mickey" "cheese" 10))
(define felix (make-cat "Felix" mickey 8 'mixed '(and black white)))

(test-assert (organism? mickey))
(test-assert (animal? mickey))
(test-assert (chordate? mickey))
(test-assert (mammal? mickey))
(test-assert (rodent? mickey))
(test-assert (muridae? mickey))
(test-assert (mus? mickey))
(test-assert (mouse? mickey))

(test-assert (not (carnivore? mickey)))
(test-assert (not (felidae? mickey)))
(test-assert (not (felis? mickey)))
(test-assert (not (cat? mickey)))

(test-assert (organism? felix))
(test-assert (animal? felix))
(test-assert (chordate? felix))
(test-assert (mammal? felix))
(test-assert (carnivore? felix))
(test-assert (felidae? felix))
(test-assert (felis? felix))
(test-assert (cat? felix))

(test-assert (not (rodent? felix)))
(test-assert (not (muridae? felix)))
(test-assert (not (mus? felix)))
(test-assert (not (mouse? felix)))

(test "Mickey" (name-of mickey))
(test "cheese" (food-of mickey))
(test 10 (num-nipples-of mickey))

(test "Felix" (name-of felix))
(test mickey (food-of felix))
(test 8 (num-nipples-of felix))
(test 'mixed (breed-of felix))
(test '(and black white) (color-of felix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type person #t #t name sex age)
(define-record-type (employee person) #t #t department salary)

(define bob (make-employee "Bob" 'male 28 'hr 50000.0))
(define alice (make-employee "Alice" 'female 32 'research 100000.0))

(test-assert (person? bob))
(test-assert (employee? bob))
(test "Bob" (person-name bob))
(test 'male (person-sex bob))
(test 28 (person-age bob))
(test 'hr (employee-department bob))
(test 50000.0 (employee-salary bob))

(test-assert (person? alice))
(test-assert (employee? alice))
(test "Alice" (person-name alice))
(test 'female (person-sex alice))
(test 32 (person-age alice))
(test 'research (employee-department alice))
(test 100000.0 (employee-salary alice))

;; After a trip to Thailand...
(person-sex-set! bob 'female)
(person-name-set! bob "Roberta")

;; Then Roberta quits!
(employee-department-set! bob #f)
(employee-salary-set! bob 0.0)

(test "Roberta" (person-name bob))
(test 'female (person-sex bob))
(test 28 (person-age bob))
(test #f (employee-department bob))
(test 0.0 (employee-salary bob))

;;;; SRFI-99 forbids this, but we currently do it anyway.
;;(test-assert (equal? (make-employee "Chuck" 'male 20 'janitorial 50000.0)
;;                     (make-employee "Chuck" 'male 20 'janitorial 50000.0)))

(test-assert (record? alice))
(test 'person (rtd-name person))
(let* ((constructor (rtd-constructor person))
       (trent (constructor "Trent" 'male 44)))
  (test "Trent" (person-name trent))
  (test 'male (person-sex trent))
  (test 44 ((rtd-accessor person 'age) trent))
  ((rtd-mutator person 'age) trent 45)
  (test 45 (person-age trent)))

(test-assert (rtd-field-mutable? employee 'department))

;;; We do not retain mutability information ATM.
;; (define-record-type foo
;;   (make-foo x)
;;   foo?
;;   (x foo-x))

;; (test-assert (not (rtd-field-mutable? foo 'x)))

(define point (make-rtd "point" #(x y)))
(define make-point (rtd-constructor point #(x y)))
(define point-x (rtd-accessor point 'x))
(test 3 (point-x (make-point 3 2)))

;; Name conflicts - make sure we rename 

(define-record-type example make-example #t example)
(test-assert (example? (make-example 3)))
(test 3 (example-example (make-example 3)))

(test-end)
