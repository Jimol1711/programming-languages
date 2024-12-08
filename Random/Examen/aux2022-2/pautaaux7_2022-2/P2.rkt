#lang play

#|       SUPER PAUTA - Auxiliar #7

================================================
P2
================================================


Se utiliza un Continuation-Passing Style. Este consiste
en entregar una función que indique qué hacer una vez
que se llega a una rama terminal (no recursiva) de la
función.

Esta función puede irse componiendo a medida que se realizan
llamadas recursivas.
|#

;; <expr> ::= <num> 
;;          | (+ <num> <num>)
(deftype Expr
  (num n)
  (add l r))

;; interp :: Expr (Num -> Num) ->  Num
(define (interp expr cont)  
	(match expr    
		[(num n) (cont n)]    
		[(add l r) 
		 (interp l (λ (vl)
                             (interp r (λ (vr)
                                         (cont (+ vl vr))))))]))

(test (interp (add (num 1) (num 2)) identity) 3)
(test (interp (add (add (num 1) (num 2)) (num 2)) identity) 5)

#|
En el caso num, se aplica la función cont directamente, pues es una
rama terminal.

Por otro lado, en la suma no se pueden realizar 2 llamados a interp
simultáneos y luego sumar (dejaría de ser llamado por la cola). Es por
ello que se calcula el valor izquierdo, dejando a cont el trabajo de
evaluar la expresión derecha, y posterior a ello sumar ambos valores.
|#