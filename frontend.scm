(import (scheme base))

(define (form->ast form)
  (case (car form)
    ((define)
     (if (pair? (cadr form))
         (make-ast-define (caadr form)
                          (make-ast-lambda (cdadr form)
                                           (map object->ast (cddr form))))
         (make-ast-define (cadr form) (object->ast (cddr form)))))
    ((set!) (make-ast-set (second form) (object->ast (third form))))
    ((lambda) (make-ast-lambda (second form) (map object->ast (cddr form))))
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
