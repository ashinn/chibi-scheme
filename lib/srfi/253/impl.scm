;; SPDX-FileCopyrightText: 2024 Artyom Bologov
;; SPDX-License-Identifier: MIT

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(define (check-impl? type)
  (lambda (x)
    (eq? type (type-of x))))

(define-syntax check-arg
  (syntax-rules ()
    ((_ pred val caller)
     (assert (pred val) val caller))
    ((_ pred val)
     (check-arg pred val 'check-arg))))

(define-syntax values-checked
  (syntax-rules ()
    ((_ (predicate) value)
     (let ((v value))
       (check-arg predicate v 'values-checked)
       v))
    ((_ (predicate ...) value ...)
     (values (values-checked (predicate) value) ...))))

(define-syntax %check-case
  (syntax-rules (else)
    ((_ val (clause ...) (else body ...))
     (cond
      clause ...
      (else body ...)))
    ((_ val ((clause-check clause-body ...) ...))
     (cond
      (clause-check clause-body ...)
      ...
      (else (assume (or clause-check ...)
              "at least one branch of check-case should be true"
              'clause-check ...))))
    ((_ val (clause ...) (pred body ...) rest ...)
     (%check-case
      val
      (clause ... ((pred val) body ...))
      rest ...))))
(define-syntax check-case
  (syntax-rules ()
    ((_ value clause ...)
     (let ((v value))
       (%check-case v () clause ...)))))

(define-syntax %lambda-checked
  (syntax-rules (=>)
    ((_ name (=> (returns ...) body ...) args (checks ...))
     (lambda args
       checks ...
       (values-checked
        (returns ...)
        (begin body ...))))
    ((_ name (body ...) args (checks ...))
     (lambda args
       checks ...
       body ...))
    ((_ name body (args ...) (checks ...) (arg pred) . rest)
     (%lambda-checked
      name body
      (args ... arg) (checks ... (check-arg pred arg 'name)) . rest))
    ((_ name body (args ...) (checks ...) arg . rest)
     (%lambda-checked
      name body
      (args ... arg) (checks ...) . rest))
    ((_ name body (args ...) (checks ...) . last)
     (%lambda-checked
      name body
      (args ... . last) (checks ...)))))

(define-syntax lambda-checked
  (syntax-rules ()
    ((_ () body ...)
     (lambda () body ...))
    ((_ (arg . args) body ...)
     (%lambda-checked lambda-checked (body ...) () () arg . args))
    ;; Case of arg->list lambda, no-op.
    ((_ arg body ...)
     (lambda arg body ...))))

(define-syntax %case-lambda-checked
  (syntax-rules (=>)
    ;; Terminal case, generate the actual lambda
    ((_ (clauses-so-far ...)
        ()
        args-so-far (checks-so-far ...) (body ...))
     (case-lambda
       clauses-so-far ...
       (args-so-far
        checks-so-far ...
        body ...)))
    ;; Empty args with returns
    ((_ (clauses-so-far ...)
        ((() => (returns ...) body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () ((values-checked (returns ...) (begin body-to-process ...)))))
    ;; Empty args without returns
    ((_ (clauses-so-far ...)
        ((() body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () (body-to-process ...)))
    ;; Regular args with returns
    ((_ (clauses-so-far ...)
        (((arg . args-to-process) => (returns ...) body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () ((values-checked (returns ...) (begin body-to-process ...))) arg . args-to-process))
    ;; Regular args without returns
    ((_ (clauses-so-far ...)
        (((arg . args-to-process) body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () (body-to-process ...) arg . args-to-process))
    ;; Rest arg with returns
    ((_ (clauses-so-far ...)
        ((arg-to-process => (returns ...) body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      arg-to-process () ((values-checked (returns ...) (begin body-to-process ...)))))
    ;; Rest arg without returns
    ((_ (clauses-so-far ...)
        ((arg-to-process body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...))
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      arg-to-process () (body-to-process ...)))
    ;; Consume arg with predicate / check
    ((_ (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ...) (checks-so-far ...) (body ...) (arg pred) . args)
     (%case-lambda-checked
      (clauses-so-far ...) (clauses-to-process ...)
      (args-so-far ... arg)
      (checks-so-far ... (check-arg pred arg 'case-lambda-checked))
      (body ...) . args))
    ;; Consume regular arg
    ((_ (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ...) (checks-so-far ...) (body ...) arg . args)
     (%case-lambda-checked
      (clauses-so-far ...) (clauses-to-process ...)
      (args-so-far ... arg) (checks-so-far ...) (body ...) . args))
    ;; Consume rest arg
    ((_ (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ...) (checks-so-far ...) (body ...) . arg)
     (%case-lambda-checked
      (clauses-so-far ...) (clauses-to-process ...)
      (args-so-far ... . arg) (checks-so-far ...) (body ...)))))

(define-syntax case-lambda-checked
  (syntax-rules (=>)
    ((_ (() => (returns ...) first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () ()
                           ((values-checked (returns ...) (begin first-body ...)))))
    ((_ (() first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () () (first-body ...)))
    ((_ ((first-arg . first-args) => (returns ...) first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () ()
                           ((values-checked (returns ...) (begin first-body ...)))
                           first-arg . first-args))
    ((_ ((first-arg . first-args) first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () () (first-body ...)
                           first-arg . first-args))
    ((_ (args-var => (returns ...) first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) args-var ()
                           ((values-checked (returns ...) (begin first-body ...)))))
    ((_ (args-var first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) args-var () (first-body ...)))))

(define-syntax define-checked
  (syntax-rules ()
    ;; Procedure
    ((_ (name . args) body ...)
     (define name (%lambda-checked name (body ...) () () . args)))
    ;; Variable
    ((_ name pred value)
     (define name (values-checked (pred) value)))))

(define-syntax %define-record-type-checked
  (syntax-rules ()
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...))
     (begin
       (define-record-type
           type-name constructor predicate
           fields ...)
       field-wrappers ...))
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...) (field pred accessor modifier)
        fields-to-process ...)
     (%define-record-type-checked
      type-name constructor predicate
      (fields ... (field internal-accessor internal-modifier))
      (field-wrappers
       ...
       (define-checked (accessor (record predicate))
         (internal-accessor record))
       (define-checked (modifier (record predicate) (val pred))
         (internal-modifier record val)))
      fields-to-process ...))
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...) (field pred accessor)
        fields-to-process ...)
     (%define-record-type-checked
      type-name constructor predicate
      (fields ... (field internal-accessor))
      (field-wrappers
       ...
       (define-checked (accessor (record predicate))
         (internal-accessor record)))
      fields-to-process ...))))
(define-syntax %wrap-constructor
  (syntax-rules ()
    ((_ constructor internal-constructor (arg-names ...) (args ...))
     (define-checked (constructor args ...)
       (internal-constructor arg-names ...)))
    ((_ constructor internal-constructor (arg-names ...) (args ...)
        (name pred rest ...) fields-to-process ...)
     (%wrap-constructor constructor internal-constructor
                        (arg-names ... name) (args ... (name pred))
                        fields-to-process ...))))

(define-syntax define-record-type-checked
  (syntax-rules ()
    ((_ type-name (constructor constructor-args ...) predicate field ...)
     (begin
       (%define-record-type-checked
        type-name
        (internal-constructor constructor-args ...)
        predicate
        () () field ...)
       (%wrap-constructor constructor internal-constructor () () field ...)))))
