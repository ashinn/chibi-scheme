(library (rnrs conditions)
  (export &condition
          (rename make-compound-condition condition)
          simple-conditions
          condition-predicate
          condition-accessor
          (rename define-condition-type/constructor define-condition-type)

          ;; 7.3 Standard condition types
          &message
          make-message-condition
          message-condition?
          condition-message

          &warning
          make-warning
          warning?

          &serious
          make-serious-condition
          serious-condition?

          &error
          make-error
          error?

          &violation
          make-violation
          violation?

          &assertion
          make-assertion-violation
          assertion-violation?

          &irritants
          make-irritants-condition
          irritants-condition?
          condition-irritants

          &who
          make-who-condition
          who-condition?
          condition-who

          &non-continuable
          make-non-continuable-violation
          non-continuable-violation?

          &implementation-restriction
          make-implementation-restriction-violation
          implementation-restriction-violation?

          &lexical
          make-lexical-violation
          lexical-violation?

          &syntax
          make-syntax-violation
          syntax-violation?
          syntax-violation-form
          syntax-violation-subform

          &undefined
          make-undefined-violation
          undefined-violation?)
  (import (srfi 35 internal))

  (define-condition-type/constructor &warning &condition
    make-warning warning?)

  (define-condition-type/constructor &violation &serious
    make-violation violation?)

  (define-condition-type/constructor &assertion &violation
    make-assertion-violation assertion-violation?)

  (define-condition-type/constructor &irritants &condition
    make-irritants-condition irritants-condition?
    (irritants condition-irritants))

  (define-condition-type/constructor &who &condition
    make-who-condition who-condition?
    (who condition-who))

  (define-condition-type/constructor &non-continuable &violation
    make-non-continuable-violation non-continuable-violation?)

  (define-condition-type/constructor &implementation-restriction &violation
    make-implementation-restriction-violation
    implementation-restriction-violation?)

  (define-condition-type/constructor &lexical &violation
    make-lexical-violation lexical-violation?)

  (define-condition-type/constructor &syntax &violation
    make-syntax-violation syntax-violation?
    (form syntax-violation-form)
    (subform syntax-violation-subform))

  (define-condition-type/constructor &undefined &violation
    make-undefined-violation undefined-violation?))
