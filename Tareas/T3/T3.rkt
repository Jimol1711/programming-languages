#lang play

;; Juan Ignacio Molina - Sección 1

#|

Hizo Ud uso de la whiteboard policy: NO
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------------;;
;; P1.a, P1.e, P2.a ;;
;;------------------;;


#|
Abstract syntax of expressions:

<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (nil)
         | (id <symbol>)
         | (conz <expr> <expr>)
         | (fun <pattern> <expr>)
         | (app <expr> <expr>) 
|#
(deftype Expr
  (num n)
  (add l r)
  (nil)
  (id x)
  (conz l r)
  (fun p body)
  (app f arg))

;;------------------;;
;; P1.b, P1.e, P2.b ;;
;;------------------;;


;; parse : s-expr -> Expr
;; parses a syntax expr int and expression
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'nil) (nil)]
    [(list 'cons l r) (conz (parse l) (parse r))]
    [(list 'list elems ...)
     (foldr (lambda (e acc) (conz (parse e) acc))
            (nil)
            elems)]
    [(list 'fun pattern body) (fun (parse-pattern pattern) (parse body))]
    [(list f-expr arg-expr) (app (parse f-expr) (parse arg-expr))]))

;;----- ;;
;; P1.c ;;
;;----- ;;

#|
<pattern> ::= (numP <num>)
            | (nilP)
            | (varP <symbol>)
            | (conzP <pattern> <pattern>)
|#
;; Datatype to represent a pattern on the language
(deftype Pattern
  (numP n)      
  (varP v)      
  (nilP)        
  (conzP p1 p2))

;;----- ;;
;; P1.d ;;
;;----- ;;

;; parse-pattern : s-expr -> Pattern
;; parses a syntax expr into a pattern
(define (parse-pattern s-expr)
  (match s-expr
    [(? number? n) (numP n)]                    
    [(list 'nil) (nilP)]                         
    [(? symbol? v) (varP v)]                     
    [(list 'cons e1 e2)                         
     (conzP (parse-pattern e1) (parse-pattern e2))]
    [(list 'list elems ...)                      
     (foldr (lambda (e acc) (conzP (parse-pattern e) acc))
            (nilP)
            elems)])) 

;;----- ;;
;; P1.f ;;
;;----- ;;

#|
<value> ::= (numV <num>)
          | (nilV)
          | (consV <value> <value>
          | (closureV <symbol> <value> <value>)
|#
(deftype Value
  (numV n)
  (nilV)
  (consV l r)
  (closureV id body env))

#|
BEGIN utility definitions
|#

#|
<env> ::= (mtEnv)
       | (extEnv <sym> <value> <env>)
|#
(deftype Env
  (mtEnv)
  (extEnv x v env))

;; extend-env : Symbol Value Env -> Env
(define (extend-env x v env)
  (extEnv x v env))

;; empty-env : Env
(define empty-env (mtEnv))

;; extend-env* : (Listof (Symbol * Value)) Env -> Env
(define (extend-env* bindings env)
  (foldr
   (lambda (binding env) (match binding [(cons x v) (extend-env x v env)]))
   env
   bindings))

;; lookup-env : Symbol Env -> Value
(define (lookup-env x env)
  (match env
    [(mtEnv) (error "LookupError: variable ~a not found" x)]
    [(extEnv y v env) (if (equal? x y) v (lookup-env x env))]))

;; num+ : Value Value -> Value
(define (num+ v1 v2)
  (match v1
    [(numV n) (match v2
                [(numV m) (numV (+ n m))]
                [_ (error "TypeError: expected a number")])]
    [_ (error "TypeError: expected a number")]))


#|
END utility definitions
|#


;;----- ;;
;; P1.g ;;
;;----- ;;

#|
<result> e v ::= (failure e)
               | (success v)
|#
(deftype Result
  (failure e)
  (success v))

;; generate-substs : Pattern Value -> (Result String (Listof (Symbol * Value)))
(define (generate-substs p v) '???)

;;------------;;
;; P1.h, P2.c ;;
;;------------;;

;; interp : Expr Env -> Value
(define (interp expr env) '???)


;;----- ;;
;; P2.d ;;
;;----- ;;

#|
En función de lo implementado en la pregunta anterior, argumente porqué es útil que la función
generate-subst no lance un error (cuando el valor no calza con el patrón) y, en cambio, retorne
un mensaje.

R: ...

|#
