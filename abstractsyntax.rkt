#lang eopl
;;; begin -- abstract syntax definition

(define-datatype  program  program?
   (a-program (exp expression?)))

(define-datatype expression expression?
   (lit-exp (datum number?))
   (var-exp (id symbol?))
   (primapp-exp
     (prim  primitive?)
     (rands  (list-of expression?))) )

(define-datatype primitive primitive?
   (add-prim)
   (subtract-prim)
   (mult-prim)
   (incr-prim)
   (decr-prim))
;;; end -- abstract syntax definition

; program example: 2
(define a-program-test-1  
(a-program (lit-exp 2)))

; program example: add1(2)
(define a-program-test-2  
  '(a-program (primapp-exp (incr-prim) ((primapp-exp (add-prim) ((lit-exp 3) (var-exp x)))))))

;(a-program
;  (primapp-exp
;    (incr-prim)
;    ((primapp-exp (add-prim)  
;                  ((lit-exp 1)
;                   (lit-exp 3))))))

(provide (all-defined-out))