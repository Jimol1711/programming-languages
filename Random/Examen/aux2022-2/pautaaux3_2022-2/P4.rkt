#lang play
(require "first-class.rkt")
#|
       SUPER PAUTA - Auxiliar #3

================================================
P4
================================================
|#

;; Definition of a function 
;; <fundef> ::=  <id> <id> <expr> 
(deftype FunDef
  (fundef name arg body))


(define (parse-fun f-list prog)
  (match f-list
    [(list) (parse prog)]
    [(cons (fundef name arg body) rest)
     (app (fun name (parse-fun rest prog))
          (fun arg body))]))


;; run :: s-expr listof(FunDef) -> number
(define (new-run prog f-list)
  (eval (parse-fun f-list prog) empty-env))

(define my-funcs (list (fundef 'add1 'x (parse '{+ x 1}))
                       (fundef 'add5 'x (parse '{+ x 5}))))

(test (new-run '{add1 5} my-funcs) (numV 6))
  
                              
                         
