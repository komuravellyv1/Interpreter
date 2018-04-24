#lang eopl

(require "abstractsyntax.rkt")
;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    )
  )

(define the-grammar
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)let-exp)
    (expression
     ("if" expression "then" expression "else" expression)if-exp)
    (expression
     ("proc" "("(separated-list identifier ",")")" expression)proc-exp)
    (expression
     ("(" expression (arbno expression) ")")app-exp)
    (expression
     (primitive "(" (separated-list expression ",") ")")
     primapp-exp)
    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("/")     div-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    )
  )
(provide (all-defined-out))



;;;; ***********************Environment*********************

;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)    ; can use this for anything.
   (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((position (rib-find-position sym syms)))
                             (if (number? position)
                                 (vector-ref vals position)
                                 (apply-env env sym)))))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

(define init-env 
  (lambda ()
    (extend-env
     '(i v x)   '(1 5 10)   (empty-env))))

(provide (all-defined-out))

;;;;;;;;; ************************Interpreter Program*******************************

(define my-interpreter
  (lambda (program)
    (eval-program (parse program))))



(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes
     the-lexical-spec the-grammar)))
(define parse
  (sllgen:make-string-parser
   the-lexical-spec the-grammar))




(define eval-program
  (lambda(pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)) )
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (if-exp (test-exp true-exp false-exp)
              (if (true? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      
      #|  (proc-exp (ids body)
                (eopl:error "proc-exp: ~s" exp)
                )
      (app-exp (rator rands)
               (eopl:error "app-exp: ~s" exp) 
               ) |#
      ;(else (eopl:error 'eval-expression "Not here:~s" exp))

 ;;******************* Function evaluation ******************************
      
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procval proc args)
                     (eopl:error 'eval-expression "Attempt to apply non-procedure ~s" proc)))))))
;   (else (eopl:printf "this expression will be implemented: ~s" exp)))))

(define true?
  (lambda(x)
    (not (zero? x))))

;;********* Abstract syntax for function ********************************
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))
      
(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body
                                (extend-env ids args env))))))

;get and return the values of arguments
(define eval-rands
  (lambda (rands env)
    (map (lambda (x)(eval-rand x env)) rands))) ;map each argument to its value
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))
;;;apply-primitive Implementation
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)) )
      (subtract-prim () (- (car args) (cadr args)) )
      (mult-prim () (* (car args) (cadr args)) )
      (div-prim () (/ (car args) (cadr args)) )
      (incr-prim () (+ (car args) 1) )
      (decr-prim () (- (car args) 1) ) )))

;;;;;;;;;; Test case 1:: (my-interpreter "if +(5,-(x,15)) then i else v")
;;;;;;;;;; Test-case 2:: (my-interpreter "if  1 then let a=7 in add1(sub1(+(a,3))) else add1(sub1(-(x,3)))")
;;;;;;;;;; Test-case 3:: (my-interpreter  " let f = proc (a, b)  - (a, + (b,8)) in (f  5 7)")

;;(my-interpreter "let centigrade = proc (fahrenheit) /(*(5, -(fahrenheit, 32)), 9) in (centigrade 70)")

;;(my-interpreter "let pi = /(22, 7) in let radian = proc (degree) * (degree, / (pi, 180)) in (radian 90)")

;;(my-interpreter "let pi = /(22, 7) in let ConeVolume = proc (radius, height) /(*(pi, *(height, *(radius, radius))), 3) in (ConeVolume 4 6)")

;;(my-interpreter "if 1 then let f = proc (a, b)  - (a, + (b,8)) in (f  5 7) else let f = proc (a, b)  + (a, - (b,8)) in (f  5 7)")
