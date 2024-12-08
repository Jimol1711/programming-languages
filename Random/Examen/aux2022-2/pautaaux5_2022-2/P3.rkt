#lang play
#|

       SUPER PAUTA - Auxiliar #5

================================================
P3
================================================
|#

(require "base-lang.rkt")

;; eval :: Expr -> number
;; evaluates arithmetical expressions
;; with conditionals and identifiers
(define (eval expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (eval l) (eval r))]
    [(id x) (error "Free id" x)]
    [(with x e b)
     (eval (subst x e b))]))

#|
El intérprete utiliza un régimen de evaluación perezosa,
esto se observa en el caso 'with' de eval, donde se llama
a subst con 'e'. Esto reemplaza en la expresión del cuerpo
del with 'b' cada aparición de 'x' por 'e'.
Para cambiar a evaluación temprana sería necesario aplicar
eval a 'e' antes de la llamada a subst.
|#


;; run :: s-expr -> number
;; evaluates an arithmetical expression with
;; conditionals given in concrete syntax
(define (run prog)
  (eval (parse prog)))

(test (run '{with {x {+ 5 y}} 1}) 1) ;; y no está definido pero funciona igual


;; eval-eager :: Expr -> number
;; evaluates arithmetical expressions
;; with conditionals and identifiers
(define (eval-eager expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (eval-eager l) (eval-eager r))]
    [(id x) (error "Free id" x)]
    [(with x e b)
     (eval-eager (subst x (eval-eager e) b))]))

;; run-eager :: s-expr -> number
;; evaluates an arithmetical expression with
;; conditionals given in concrete syntax
(define (run-eager prog)
  (eval-eager (parse prog)))

(test/exn (run-eager '{with {x {+ 5 y}} 1}) "Free id 'y") ; se cae con evaluación temprana

