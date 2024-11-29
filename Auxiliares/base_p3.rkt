#lang play

;<FunDef> ::= (fundef <sym> <listOf sym> <Expr>)
(deftype FunDef
  (fundef name args body))

;<Expr> ::= (num <num>)
; | (id <sym>)
; | (add <Expr> <Expr>)
; | (sub <Expr> <Expr>)
; | (mult <Expr> <Expr>)
; | (if0 <Expr> <Expr> <Expr>)
; | (app <sym> ListOf[<Expr>])
(deftype Expr
  (num n)
  (id x)
  (add l r)
  (sub l r)
  (mult l r)
  (if0 c t f)
  (app f args))

;;parse :: ListOf[Symbo] -> Fundef
;;parsea la expresion concreta a un fundef
(define (parse-fundef s-expr)
  (match s-expr
    [(list 'fundef f-name parameters body) (fundef f-name parameters (parse body))]
    ))

;;parse :: ListOf[Symbo] -> Expr
;;parsea la expresion concreta a una abstracta
(define (parse s-expr)
  (match s-expr
    [x #:when (number? x) (num x)]
    [x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mult (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list f args) (app f (map parse args))]
    ))

;;look-up :: Symbol ListOf[Fundef] -> Fundef/Error
(define (look-up f-name l)
  (match l
    [ (list) (error 'look-up "function ~a not found" f-name)]
    ;;acá al hacer fundef que es el accesor y ponemos el campo podemos obtener ese valor de ese campo
    ;; fundef es el accesor y name es el campo
    [ (cons head tail) (if (symbol=? f-name (fundef-name head)) head (look-up f-name tail)) ]
    ;;forma equivalente
    ;; [ (cons (fundef name _ _) tail) (if (symbol=? f-name name) head (look-up f-name tail)) ]
    ))




  