(define (improper-map proc lst)
  (cond
  ((pair? lst)
   (cons (proc (car lst))
         (improper-map proc (cdr lst))))
  ((null? lst) lst)
  (else (proc lst))))


(define-syntax pred-or
  (syntax-rules ()
    ((_ pred* ...)
     (lambda (arg) (or (pred* arg) ...)))))
