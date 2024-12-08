#lang play

;; P1 El lenguaje con funciones recursivas definido en clases. ¿Tiene mutación?
;;
;; No. El lenguaje usa mutación para implementar un cache y no tener que evaluar cada expresión dos veces y para definir los ambientes ciclicos,
;; pero no le entrega al programador la capacidad de utilizarla.


;  P2 ¿Que se tiene que hacer al nivel del interprete para poder soportar funciones recursivas en los siguientes lenguajes?
;; P2.a
;; No se necesita nada adicional,
;; el lenguaje soporta ya funciones recursivas porque todas las funciones
;; están previamente definidas y poseen visibilidad global entre ellas.

;; P2.b
;; No se necesita nada adicional para soportar recursión, debido a que al momento de definir una función, el binding
;; de su nombre ya se encuentra en el ambiente al hacer un llamado recursivo.

;; P2.c
;; Se necesita tener un ambiente cíclico, esto permite
;; que al definir una función que hace llamados recursivos en su cuerpo, el binding del identificador de la función ya se
;; encuentre en el ambiente. 


;  P3 
(with (fib 
  (fun f
    (fun n (if n
            1
            (if (- n 1) 
              1
              (+ ((f f) (- n 2) ((f f) (- n 1))))))
    )
  ))
  ((fib fib) 4)
)

; P4
; Qué problemas tiene este intérprete?
; > las clausuras se crean con el ambiente vacío en vez de con el ambiente actual y la condición de las expresiónes if-then-else no se interpreta

; Piense en una expresión que exponga el error
; > (with n 5 (with (f (fun x n)) (f 42)))
; > (if (+ 1 1) 1 1)

; Qué cambios habría que hacer para corregir el error?
; > cambiar la linea 
; [ (fun id body) (closureV id body empty-env)] por 
; [ (fun id body) (closureV id body env)]
; y cambiar la linea
; ( if (num-zero? c)) por
; ( if (num-zero? (interp c env)))