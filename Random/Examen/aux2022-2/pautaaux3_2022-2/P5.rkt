#lang play

#|
       SUPER PAUTA - Auxiliar #3

================================================
P5
================================================

El propósito de este ejercicio es ver que no existe una única definición usando BNF.
Hay algunas formas de BNF que pueden ser útiles en ciertos casos, pero no en otros.

Al final del día depende del caso de uso lo que puede dictaminar si una forma es
mejor que otra.


Partimos con:
<expr> ::= <Expr> ::= <number?>
        |  {+ <Expr> <Expr>}
        |  {- <Expr> <Expr>}
        |  {with {<id> <Expr>} <Expr>}
        |  <id>
        | {<id> <Expr>}

<fundef> ::= {define {<id> <id>} <Expr>}

Para añadir los nuevos operadores, necesitaríamos agregar dos nuevos constructores
al deftype: (mul l r) y (div l r).
En todas las funciones donde hagamos match sobre la estructura de las expresiones,
necesitaremos agregar 2 casos nuevos, para manejar estos dos nuevos constructores.
Esta definición es válida, pero puede tener el inconveniente de que agregar una operación
nueva desencadena muchos otros cambios (lo que en general puede introducir bugs, por ejemplo).

Se puede agregar un nodo de tipo binop que agrupe todas las operaciones binarias primitivas.
En el deftype podemos cambiar todas las operaciones binarias por un único nodo para binop.
Luego, al momento de parsea basta con hacer match a un puro nodo y deferimos el parseo más
específico de binops a otra función.
De la misma manera, la interpretación de estas operaciones se puede unificar, tomando el
nodo binops y aplicando la función Racket correspondiente 
<Expr> ::= <number>
       |  {<binop> <Expr> <Expr>}
       |  {with {<id> <Expr>} <Expr>}
       |  <id>
       | {<id> <Expr>}
 
<binop> ::= + | - | * | /            
<fundef> ::= {define {<id> <id>} <Expr>}
            
Un posible beneficio de este enfoque es que podemos acotar (encapsular) las partes del código
que hacen referencia y manipulan los operadores binarios. 
Así, si queremos agregar nuevos operadores (como %), no desencadenamos cambios en todas partes,
solo en algunos lugares acotados. En general, esto puede ser buena práctica ingenieril (más allá
de lenguajes).
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     FUNCTIONS WITH DEFERRED SUBSTITUTION      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (bin <BinOp> <expr> <expr>)
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
  (bin op l r)
  (if0 c t f)
  (with id named-expr body)
  (id x)
  (app f-name f-arg))

#|
<binop> ::= (add) | (sub) | (mul) | (div) | ...
|#
(deftype BinOp
  (add)
  (sub)
  (mul)
  (div))


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

;; apply-op :: BinOp -> (Num Num -> Num)
;; Obtains the respective binary operator.
(define (apply-op op)
  (match op
    [(add) +]
    [(sub) -]
    [(mul) *]
    [(div) /]))


;; eval :: Expr listof(FunDef) Env -> number
;; evaluates an arithmetical expression with function calls
;; and local definitions deferring the substitutions,
;; using static scoping 
(define (eval expr f-list env)
  (match expr
    [(num n) n]
    [(bin op l r) ((apply-op op) (eval l f-list env) (eval r f-list env))]
    [(if0 c t f) (if  (zero? (eval c f-list env))
                      (eval t f-list env)
                      (eval f f-list env))]
    [(with x e b) (def new-env (extend-env x (eval e f-list env) env))
       (eval b f-list new-env) ]
    [(id x) (env-lookup x env) ] 
    [(app f e) (def (fundef _ the-arg the-body) (look-up f f-list))
               (def new-env (extend-env the-arg (eval e f-list env) empty-env))
               (eval the-body f-list new-env) ]))


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
    [(list 'if0 c t f) (if0 (parse c)
                            (parse t)
                            (parse f))]
    [(list 'with (list x e) b) #:when (symbol? x)
         (with x (parse e) (parse b))]
    [(list f a) #:when (symbol? f) (app f (parse a))]
    [(list op l r) (bin (parse-op op) (parse l) (parse r))]))

;; parse :: s-binop -> BinOp
;; converts s-binops into BinOps
(define (parse-op op)
  (match op
    ['+ (add)]
    ['- (sub)]
    ['* (mul)]
    ['/ (div)]))
    

;; run :: s-expr listof(FunDef) -> number
(define (run prog f-list)
  (eval (parse prog) f-list empty-env))



;; Some testing to see the difference between
;; static and dynamic scoping
(define my-funcs (list (fundef 'f 'x (id 'n))))
(define my-expr '(with (n 5) (f 10)))
(test/exn (run my-expr my-funcs) "free identifier")
(test (run '{with {x 5} {+ x 3}} my-funcs) 8)
(test (run '{with {y 8} {* 7 y}} my-funcs) 56)
(test (run '{/ 12 6} my-funcs) 2)
(test (run '{- 13 3} my-funcs) 10)
