
(define-library (chibi ast)
  (export
   analyze optimize env-cell ast->sexp macroexpand type-of
   Object Input-Port Output-Port Opcode Procedure Bytecode Macro Env
   Number Bignum Flonum Integer Complex Char Boolean
   Symbol String Byte-Vector Vector Pair File-Descriptor
   Context Lam Cnd Set Ref Seq Lit Sc Exception Core
   syntactic-closure? lambda? cnd? set? ref? seq? lit? type? core?
   environment? bytecode? exception? macro? context? file-descriptor?
   syntactic-closure-expr syntactic-closure-env syntactic-closure-vars
   copy-lambda make-lambda make-cnd make-ref make-set make-seq make-lit
   make-macro
   lambda-name lambda-params lambda-body lambda-defs lambda-locals
   lambda-flags lambda-free-vars lambda-set-vars lambda-return-type
   lambda-param-types lambda-source
   lambda-name-set! lambda-params-set! lambda-body-set! lambda-defs-set!
   lambda-locals-set! lambda-flags-set! lambda-free-vars-set!
   lambda-set-vars-set! lambda-return-type-set! lambda-param-types-set!
   lambda-source-set!
   cnd-test cnd-pass cnd-fail
   cnd-test-set! cnd-pass-set! cnd-fail-set!
   set-var set-value set-var-set! set-value-set!
   ref-name ref-cell ref-name-set! ref-cell-set!
   seq-ls seq-ls-set! lit-value lit-value-set!
   exception-kind exception-message exception-irritants exception-source
   opcode-name opcode-num-params opcode-return-type opcode-param-type
   opcode-class opcode-code opcode-data opcode-variadic?
   macro-procedure macro-env macro-source
   procedure-code procedure-vars procedure-name procedure-name-set!
   bytecode-name bytecode-literals bytecode-source
   port-line port-line-set!
   extend-env env-parent env-parent-set! env-lambda env-lambda-set!
   env-define! env-push! env-syntactic? env-syntactic?-set! core-code
   type-name type-cpl type-parent type-slots type-num-slots type-printer
   object-size integer->immediate gc atomically
   string-contains integer->error-string
   flatten-dot update-free-vars!)
  (import (chibi))
  (include-shared "ast")
  (include "ast.scm"))
