#lang play

(require "base_aux6.rkt")

;;p3

;;1) con scope dinamico da 1000 ya que el cuerpo se calculará con el ultimo valor de 'a, ya que env lo guarda
;; y con scope lexico o estático da 2 ya que el cuerpo se calcula teniendo el ambiente de la clausura donde 'a es 1
(test (run '{with {a 1}
                  {with {f {fun {x} {+ x a}}}
                        {with {a 999}
                              {f 1}}}}) (numV 2)) ;; --> 1000!?? --> alcance dinámico

;; con scope estatico da 28 ya que el 'y' dentro del cuerpo de f tiene valor 5 y al evaluar f 10 da 15, a esto se le suma el y redefinido que tiene valor 13
;; con scope dinamico da 28 igual ya que el nuevo valor de y = 13 solo existe dentro del cuerpo del with del primer operando de la suma, por lo que este nuevo ambiente
;; no se extiende al evaluar f 10.
(test (run '{with {y 5}
                  {with {f {fun {x} {+ x y}}}
                        {+ {with {y {+ 8 y}} y} 
                           {f 10}}
                        }
                  }) (numV 28)) ;; --> 28 para estatico y dinamico

;;2)
;;--con el mal interprete da error ya que no encuentra el a, por que al momento de definir la función
;; (que es el ambiente de la clasura) no habia ningun identificador a
(run '{with {f {fun {x} {+ x 1}}}
                        {with {a 999}
                              {f a}}})

;;3)
;;-- interpretar el argumento esto usaría el a=1 y no a=999, ya que el env de la clausura si encuentra un
;; identificador 'a antes de definir 'f, pero debería usar el último. Así que erroniamente dara como resultado 2 cuando deberia ser 1000
(run '{with {a 1}
                  {with {f {fun {x} {+ x 1}}}
                        {with {a 999}
                              {f a}}}})