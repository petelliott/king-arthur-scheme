(import (scheme base))

(define-record-type ast-ref
  (make-ast-ref symbol)
  ast-ref?
  (symbol ast-ref-symbol))

(define-record-type ast-define
  (make-ast-define symbol expr)
  ast-define?
  (symbol ast-define-symbol)
  (ref ast-define-ref)
  (expr ast-define-expr))

(define-record-type ast-set
  (make-ast-set symbol expr)
  ast-set?
  (symbol ast-set-symbol)
  (ref ast-set-ref)
  (expr ast-set-expr))

(define-record-type ast-lambda
  (make-ast-lambda lambda-list body)
  ast-lambda?
  (lambda-list ast-lambda-list)
  (body ast-lambda-body))

(define-record-type ast-if
  (make-ast-if condition tbranch fbranch)
  ast-if?
  (condition ast-if-condition)
  (tbranch ast-if-tbranch)
  (fbranch ast-if-fbranch))

(define-record-type ast-literal
  (make-ast-literal value)
  ast-literal?
  (value ast-literal-value))

(define-record-type ast-call
  (make-ast-call callee args)
  ast-call?
  (callee ast-call-callee)
  (args ast-call-args))

(define-record-type ast-quote
  (make-ast-quote value)
  ast-quote?
  (value ast-quote-value))
