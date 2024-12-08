;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     FUNCTIONS WITH DEFERRED SUBSTITUTION      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <id> <expr> <expr>)
         | (id <id>)
         | (app <id> <expr>)
|#
;; Inductive type for representing arith-
;; metical expressions with conditionals,
;; identifiers and function calls
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with id named-expr body)
  (id x)
  (app f-name f-arg))


;; Definition of a function 
;; <fundef> ::=  <id> <id> <expr> 
(deftype FunDef
  (fundef name arg body))


;; look-up :: symbol listof(FunDef) -> FunDef
;; searches a function definition within a list of definitions
(define (look-up f-name l)
  (match l
    [ (list) (error 'lookup "Function ~a not found" f-name) ]
    [ (cons head tail) (if (symbol=? f-name  (fundef-name head))
                           head
                           (look-up f-name tail))]))


;; Interface of the Abstract Dada Type (ADT) for  
;; keeping track of the deferred substitutions

;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))


;; eval :: Expr listof(FunDef) Env -> number
;; evaluates an arithmetical expression with function calls
;; and local definitions deferring the substitutions,
;; using static scoping 
(define (eval expr f-list env)
  (match expr
    [(num n) n]
    [(add l r) (+ (eval l f-list env) (eval r f-list env))]
    [(sub l r) (- (eval l f-list env) (eval r f-list env))]
    [(if0 c t f) (if  (zero? (eval c f-list env))
                      (eval t f-list env)
                      (eval f f-list env))]
    [(with x e b) (def new-env (extend-env x (eval e f-list env) env))
       (eval b f-list new-env) ]
    [(id x) (env-lookup x env) ] 
    [(app f e) (def (fundef _ the-arg the-body) (look-up f f-list))
               (def new-env (extend-env the-arg (eval e f-list env) empty-env))
               (eval the-body f-list new-env) ]))


;; dyn-eval :: Expr listof(FunDef) Env -> number
;; evaluates an arithmetical expression with function calls
;; and local definitions deferring the substitutions,
;; using dynamic scoping 
(define (dyn-eval expr f-list env)
  (match expr
    [(num n) n]
    [(add l r) (+ (dyn-eval l f-list env) (dyn-eval r f-list env))]
    [(sub l r) (- (dyn-eval l f-list env) (dyn-eval r f-list env))]
    [(if0 c t f) (if  (zero? (dyn-eval c f-list env))
                      (dyn-eval t f-list env)
                      (dyn-eval f f-list env))]
    [(with x e b) (def new-env (extend-env x (dyn-eval e f-list env) env))
       (dyn-eval b f-list new-env) ]
    [(id x) (env-lookup x env) ] 
    [(app f e) (def (fundef _ the-arg the-body) (look-up f f-list))
               (def new-env (extend-env the-arg (dyn-eval e f-list env) env))
               (dyn-eval the-body f-list new-env) ]))


;; s-expressions used as concrete 
;; syntax for our programs
#|
<s-expr> ::= <num>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | <sym>
           | (list <sym> <s-expr>) 
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c)
                            (parse t)
                            (parse f))]
    [(list 'with (list x e) b) #:when (symbol? x)
         (with x (parse e) (parse b))]
    [(list f a) #:when (symbol? f) (app f (parse a))]))


;; run :: s-expr listof(FunDef) -> number
(define (run prog f-list)
  (eval (parse prog) f-list empty-env))


;; dyn-run :: s-expr listof(FunDef) -> number
(define (dyn-run prog f-list)
  (dyn-eval (parse prog) f-list empty-env))


;; Some testing to see the difference between
;; static and dynamic scoping
(define my-funcs (list (fundef 'f 'x (id 'n))))
(define my-expr '(with (n 5) (f 10)))
(test/exn (run my-expr my-funcs) "free identifier")
(test (dyn-run my-expr my-funcs) 5)
