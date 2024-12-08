#lang play

#|       SUPER PAUTA - Auxiliar #7

================================================
P4
================================================
|#

;; Tenemos:
(define (new-array) (box (λ (n) 0)))
(define (lookup a n) ((unbox a) n))
(define (update a m v)
    (set-box! a 
        (λ (n) (if (= n m) v ((unbox a) n)))))

;; Al definir un arreglo de este modo se ocupa menos
;; espacio en memoria. Esto dado que si creo un arreglo:
(define Arr (new-array))

;; Todos sus elementos son 0
(test (lookup Arr 1000) 0)

;; Por ejemplo, podemos pedir el elemento 1000 del arreglo,
;; y obtendremos 0, ya que en sí el arreglo es una función
;; que siempre devuelve 0, salvo en casos especificados.

;; Asignemos ahora un valor a la posición 8 (por ejemplo):
(update Arr 8 10)             ; A[8] = 10
(test (lookup Arr 8) 10)

;; Acabamos de setear A[8] como 10, normalmente para esto
;; necesitaríamos un arreglo de tamaño 8, pero sólo estaríamos
;; usando un único espacio, es decir, todo el resto se pierde!

;; Usando esta representación podemos asignar índices deliberadamente
;; usando un espacio proporcional a los elementos insertados. Al final
;; lo que sucede es que se añaden nuevos 'if' a la lambda interna.

