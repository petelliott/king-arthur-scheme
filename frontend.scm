(import (scheme base)
        (srfi srfi-1))

(define (traverse-ast ast pred proc)
  (cond
   ((pred ast)
    (proc ast (lambda (node) (traverse-ast node pred proc))))
   ((ast-define? ast)
    (ast-define-set-ref! ast (traverse-ast (ast-define-ref ast) pred proc))
    (ast-define-set-expr! ast (traverse-ast (ast-define-expr ast) pred proc))
    ast)
   ((ast-set? ast)
    (ast-set-set-ref! ast (traverse-ast (ast-set-ref ast) pred proc))
    (ast-set-set-expr! ast (traverse-ast (ast-set-expr ast) pred proc))
    ast)
   ((ast-lambda? ast)
    (ast-lambda-set-body! ast (map
                               (lambda (node)
                                 (traverse-ast node pred proc))
                               (ast-lambda-body ast)))
    ast)
   ((ast-if? ast)
    (ast-if-set-condition! ast (traverse-ast (ast-if-condition ast) pred proc))
    (ast-if-set-tbranch! ast (traverse-ast (ast-if-tbranch ast) pred proc))
    (ast-if-set-fbranch! ast (traverse-ast (ast-if-fbranch ast) pred proc))
    ast)
   ((ast-call? ast)
    (ast-call-set-callee! ast (traverse-ast (ast-call-callee ast) pred proc))
    (ast-call-set-args! ast (map
                             (lambda (node)
                               (traverse-ast node pred proc))
                             (ast-call-args ast)))
    ast)
   (else ast)))

(define (traverse-ast-scope ast pred scope proc)
  (traverse-ast ast (pred-or pred ast-lambda?)
                (lambda (ast recur)
                  (cond
                   ((pred ast)
                    (proc ast (lambda (node)
                                (if (ast-lambda? ast)
                                    (traverse-ast-scope node pred ast proc)
                                    (traverse-ast-scope node pred scope proc)))
                          scope))
                   ((ast-lambda? ast)
                    (ast-lambda-set-body! ast (map
                                               (lambda (node)
                                                 (traverse-ast-scope node pred
                                                                     ast proc))
                                               (ast-lambda-body ast)))
                    ast)))))

(define (form->ast form)
  (case (car form)
    ((define)
     (if (pair? (cadr form))
         (make-ast-define (make-ast-ref (caadr form))
                          (make-ast-lambda (cdadr form)
                                           (map object->ast (cddr form))))
         (make-ast-define (make-ast-ref (cadr form)) (object->ast (caddr form)))))
    ((set!) (make-ast-set (make-ast-ref (second form)) (object->ast (third form))))
    ((lambda) (make-ast-lambda (improper-map make-ast-ref (second form))
                               (map object->ast (cddr form))))
    ((if) (make-ast-if (second form) (object->ast (third form))
                       (object->ast (fourth form))))
    ((quote) (make-ast-quote (second form)))
    (else (make-ast-call (object->ast (car form))
                         (map object->ast (cdr form))))))


(define (object->ast object)
  (cond
   ((list? object) (form->ast object))
   ((symbol? object) (make-ast-ref object))
   (else (make-ast-literal object))))


;(define (add-scope-to-lambdas-inner ast scope)
;  (traverse-ast ast (pred-or ast-lambda? ast-ref?)
;                (lambda (node recur)
;                  (cond
;                   ((ast-lambda? node)
;                    (ast-lambda-set-parent! node scope)
;                    (map
;                     (lambda (newnode)
;                       (add-scope-to-lambdas-inner newnode node))
;                     (ast-lambda-body node)))
;                   ((ast-ref? node)
;                    (ast-ref-set-scope! node scope)))
;                  node)))
;
;(define (add-scope-to-lambdas ast)
;  (add-scope-to-lambdas-inner ast #f)
;  ast)


(define (add-scope-to-lambdas ast)
  (traverse-ast-scope
   ast (pred-or ast-lambda? ast-ref?) #f
   (lambda (node recur scope)
     (cond
      ((ast-lambda? node)
       (ast-lambda-set-parent! node scope)
       (map recur (ast-lambda-body node)))
      ((ast-ref? node)
       (ast-ref-set-scope! node scope)))
     node)))

(define (resolve-references ast)
  (traverse-ast ast (pred-or ast-ref? ast-define? ast-lambda?)
                (lambda (node recur)
                  (cond
                   ((ast-ref? node)
                    (ast-lambda-scope-ref (ast-ref-scope node)
                                          (ast-ref-symbol node)))
                   ((ast-define? node)
                    (ast-define-set-expr! node (recur (ast-define-expr node)))
                    (let ((ref (ast-define-ref node)))
                      (ast-define-set-ref! node
                                           (ast-lambda-define (ast-ref-scope ref)
                                                              (ast-ref-symbol ref))))
                    node)
                   ((ast-lambda? node)
                    (ast-lambda-set-list! node
                                          (improper-map
                                           (lambda (ref)
                                             (ast-lambda-define
                                              node (ast-ref-symbol ref)))
                                           (ast-lambda-list node)))
                    (ast-lambda-set-body! node (map recur (ast-lambda-body node)))
                    node)))))

(define (box-set-variables ast)
  (traverse-ast ast ast-set?
                (lambda (node recur)
                  (ast-ref-set-boxed! (ast-set-ref node) #t)
                  (recur (ast-set-expr node))
                  node)))

(define (enclose ref scope)
  (unless (or (not (ast-ref-scope ref)) (not scope)
              (eq? (ast-ref-scope ref) scope))
    (unless (memq ref (ast-lambda-closure scope))
      (ast-lambda-enclose! scope ref))
    (enclose ref (ast-lambda-parent scope))))

(define (calculate-closures ast)
  (traverse-ast-scope
   ast ast-ref? #f
   (lambda (ast recur scope)
     (enclose ast scope)
     ast)))

(define (mark-tail-position ast)
  (cond
   ((ast-call? ast) (ast-call-set-tail! ast #t))
   ((ast-if? ast)
    (mark-tail-position (ast-if-tbranch ast))
    (mark-tail-position (ast-if-fbranch ast)))))

(define (mark-tail-calls ast)
  (traverse-ast
   ast ast-lambda?
   (lambda (ast recur)
     (mark-tail-position (last (ast-lambda-body ast)))
     (map recur (ast-lambda-body ast))
     ast)))

;; #f if no valid unquote occurs in form
(define (never-unquoted? form)
  (cond
   ((not (pair? form)) #t)
   ((eq? (car form) 'unquote) #f)
   ((eq? (car form) 'unquote-splicing) #f)
   ((or (eq? (car form) 'quasiquote)
        (eq? (car form) 'quote))
    #t)
   (else (and (never-unquoted? (car form))
              (never-unquoted? (cdr form))))))

;; expands a quasiquoted value
(define (expand-1-quasiquote form)
  (cond ; TODO check tail for no more unquotes
   ((or (not (pair? form))
        (never-unquoted? form))
    `(quote ,form))
   ((eq? (car form) 'unquote)
    (expand-quasiquotes (cadr form)))
   ((and
     (list? (cadr form))
     (eq? (caadr form) 'unquote-splicing))
    `(append (list ,(expand-1-quasiquote (car form)))
            (list ,@(expand-quasiquotes (cadadr form)))))
   (else
    `(cons ,(expand-1-quasiquote (car form))
           ,(expand-1-quasiquote (cdr form))))))

;; expands quasiquote into cons and regular quote
(define (expand-quasiquotes form)
  (cond
   ((not (pair? form)) form)
   ((eq? (car form) 'quasiquote)
    (expand-1-quasiquote (cadr form)))
   (else (improper-map expand-quasiquotes form))))


(define (pipeline val fns)
  (if (null? fns)
      val
      (pipeline ((car fns) val) (cdr fns))))

(define (-> val . fns)
  (pipeline val fns))

(define (frontend form)
  (-> form
      expand-quasiquotes
      object->ast
      add-scope-to-lambdas
      resolve-references
      box-set-variables
      calculate-closures
      mark-tail-calls))
