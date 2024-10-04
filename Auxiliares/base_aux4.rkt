#lang play

(print-only-errors #t)

;;sintaxis concreta

#|
 <expr> ::= (num <Number>)
           |(add <expr <expr>)
           |(sub <expr> <expr>)
           

|#

;parseado en el AST
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  )

#|
<s-expr> ::= <Num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)

|#


;; parse :: s-expr -> Expr
;; parsea el codigo a un AST
(define (parse s-expr)
  (match s-expr
    [ x #:when (number? x) (num x) ]
    ;;aritmeticas
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    ))

;; --- Calcualdora------

;; calc :: Expr (AST) -> Number/Boolean
;; Calcula el valor de la expresion
(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    ))

;; --- Chequeo de tipos ---

;; <Type> ::= ?

;;(deftype Type)

;; typecheck :: Expr -> Type o error
;; Realiza chequeo de tipos estatico sobre una expresion y devuelve su tipo
(define (typecheck expr)
  (void))


;; run :: ListOf[Symbol] -> Number/Boolean
;; el main del programa
(define (run prog)
  (void))




