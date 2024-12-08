;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        ARITHMETICAL LANGUAGE WITH             ;;
;;        FIRST CLASS FUNCTIONS                  ;;
;;        (call-by-value)                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
;; Inductive type for representing (the abstract syntax
;; of) an aritmetical language with first-class functions
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with id named-expr body)
  (id x)
  (fun arg body)
  (app f-name f-arg))



;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactical sugar
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]))



;; Interface of the Abstract Dada Type (ADT) for  
;; representing idenfifier environments

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


;; values of expressions
;; <value> ::= (numV <number>)
;;          |  (closureV <sym> <s-expr> <env>) 
(deftype Value
  (numV n)
  (closureV id body env))

;; Auxiliary functions handling numeric values
(define (num+ n1 n2)
  (def (numV v1) n1) (def (numV v2) n2) (numV (+ v1 v2)))

(define (num- n1 n2)
  (def (numV v1) n1) (def (numV v2) n2) (numV (- v1 v2)))

(define (num-zero? n)
  (def (numV v) n) (zero? v))


;; eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using static scoping 
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (num+ (eval l env) (eval r env))]
    [(sub l r) (num- (eval l env) (eval r env))]
    [(with x e b) (def new-env (extend-env x (eval e env) env))
       (eval b new-env) ]
    [(if0 c t f) (if  (num-zero? (eval c env))
                      (eval t env)
                      (eval f env))]
    [(app f e) (def (closureV the-arg the-body the-claus-env) (eval f env))
               (def the-ext-env (extend-env the-arg (eval e env) the-claus-env))
               (eval the-body the-ext-env)]))


;; dyn-eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using dynamic scoping 
(define (dyn-eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (num+ (dyn-eval l env) (dyn-eval r env))]
    [(sub l r) (num- (dyn-eval l env) (dyn-eval r env))]
    [(with x e b) (def new-env (extend-env x (dyn-eval e env) env))
       (dyn-eval b new-env) ]
    [(if0 c t f) (if  (num-zero? (dyn-eval c env))
                      (dyn-eval t env)
                      (dyn-eval f env))]
    [(app f e) (def (closureV the-arg the-body the-env) (dyn-eval f env))
               (def the-ext-env (extend-env the-arg (dyn-eval e env) env))
               ;;for dynamic scoping we changed above the-claus-env --> env 
               (dyn-eval the-body the-ext-env)]))


;; run :: s-expr -> value
;; evaluates an expression using static scoping 
(define (run prog)
  (eval (parse prog) empty-env))


;; dyn-run :: s-expr -> value
;; evaluates an expression using dynamic scoping 
(define (dyn-run prog)
  (dyn-eval (parse prog) empty-env))


;; some testing
(define expr1 '(with (f (fun (y) y)) (f 4)))
(test (run expr1) (numV 4))
(define expr2 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (f 4))))
(test (run expr2) (numV 7))
(define expr3 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (with (x 5) (+ x (f 4))))))
(test (run expr3) (numV 12))
(test (dyn-run expr3) (numV 14))
