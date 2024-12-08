#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <id> <expr> <expr>)
         | (id <id>)
|#
;; Inductive type for representing arith-
;; metical expressions with conditionals
;; and identifiers
(deftype Expr
  (num n)
  (add l r)
  (with id named-expr body)
  (id x))


#|
<s-expr> ::= <num>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | <sym>
|#
;; s-expressions used as concrete syntax
;; for writing arithmetical expressions
;; with conditionals and identifiers


;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'with (list x e) b) #:when (symbol? x)
         (with x (parse e) (parse b))]))


; subst :: Sym Expr Expr -> Expr
; reemplaza todas las ocurrencias libres del identificador
; por la primera expresión en la segunda expr
(define (subst x e expr)
  (match expr
    [(num n) expr]
    [(add l r) (add (subst x e l) (subst x e r))]
    [(id i) (if (equal? i x) e expr)]
    [(with wx we wb)
     (if (equal? wx x)
         (with wx (subst x e we) wb) ; don't subst in nested body!
         (with wx (subst x e we) (subst x e wb)))]))