(import (scheme base))

(define-record-type ast-ref
  (make-ast-ref symbol)
  ast-ref?
  (symbol ast-ref-symbol)
  (boxed ast-ref-boxed ast-ref-set-boxed!)
  (scope ast-ref-scope ast-ref-set-scope!))  ; nil -> toplevel

(define-record-type ast-define
  (make-ast-define ref expr)
  ast-define?
  (ref ast-define-ref ast-define-set-ref!)
  (expr ast-define-expr ast-define-set-expr!))

(define-record-type ast-set
  (make-ast-set ref expr)
  ast-set?
  (ref ast-set-ref ast-set-set-ref!)
  (expr ast-set-expr ast-set-set-expr!))

(define-record-type ast-lambda
  (private-make-ast-lambda lambda-list body scope closure)
  ast-lambda?
  (lambda-list ast-lambda-list ast-lambda-set-list!)
  (body ast-lambda-body ast-lambda-set-body!)
  (scope ast-lambda-scope ast-lambda-set-scope!)
  (parent ast-lambda-parent ast-lambda-set-parent!)
  (closure ast-lambda-closure ast-lambda-set-closure!))

(define (make-ast-lambda lambda-list body)
  (private-make-ast-lambda lambda-list body '() '()))

(define (ast-lambda-enclose! ast ref)
  (ast-lambda-set-closure! ast (cons ref (ast-lambda-closure ast))))

(define (ast-lambda-scope-intern! ast sym)
  (unless (assoc sym (ast-lambda-scope ast))
    (let ((ref (make-ast-ref sym)))
      (ast-ref-set-scope! ref ast)
      (ast-lambda-set-scope! ast (acons sym ref
                                        (ast-lambda-scope ast))))))

(define (ast-lambda-scope-ref ast sym)
  (let ((pair (and ast (assoc sym (ast-lambda-scope ast)))))
    (cond
     ((not ast) (make-ast-ref sym))
     ((not pair) (ast-lambda-scope-ref (ast-lambda-parent ast) sym))
     (else (cdr pair)))))

(define (ast-lambda-define ast sym)
  (if ast
      (begin
        (when (not (assoc sym (ast-lambda-scope ast)))
          (ast-lambda-scope-intern! ast sym))
        (cdr (assoc sym (ast-lambda-scope ast))))
      (make-ast-ref sym)))

(define-record-type ast-if
  (make-ast-if condition tbranch fbranch)
  ast-if?
  (condition ast-if-condition ast-if-set-condition!)
  (tbranch ast-if-tbranch ast-if-set-tbranch!)
  (fbranch ast-if-fbranch ast-if-set-fbranch!))

(define-record-type ast-literal
  (make-ast-literal value)
  ast-literal?
  (value ast-literal-value))

(define-record-type ast-call
  (make-ast-call callee args)
  ast-call?
  (callee ast-call-callee ast-call-set-callee!)
  (args ast-call-args ast-call-set-args!)
  (tail ast-call-tail? ast-call-set-tail!))

(define-record-type ast-quote
  (make-ast-quote value)
  ast-quote?
  (value ast-quote-value))

(define (ast->sexp ast)
  (cond
   ((ast-ref? ast)
    (if (ast-ref-scope ast)
        `(ref ,(hashq ast 1048576) ,(ast-ref-boxed ast))
        (ast-ref-symbol ast)))
   ((ast-define? ast) `(define ,(ast->sexp (ast-define-ref ast))
                         ,(ast->sexp (ast-define-expr ast))))
   ((ast-set? ast) `(set! ,(ast->sexp (ast-set-ref ast))
                      ,(ast->sexp (ast-set-expr ast))))
   ((ast-lambda? ast) `(lambda ,(improper-map ast->sexp (ast-lambda-list ast))
                         ,(map ast->sexp (ast-lambda-closure ast))
                         ,@(map ast->sexp (ast-lambda-body ast))))
   ((ast-if? ast) `(if ,(ast->sexp (ast-if-condition ast))
                       ,(ast->sexp (ast-if-tbranch ast))
                       ,(ast->sexp (ast-if-fbranch ast))))
   ((ast-literal? ast) (ast-literal-value ast))
   ((ast-call? ast) `(,(ast->sexp (ast-call-callee ast))
                      ,@(map ast->sexp (ast-call-args ast))))
   ((ast-quote? ast) `(quote ,(ast-quote-value ast)))))
