#lang play

; P1.
; No. El lenguaje usa mutación para implementar un cache y no tener que evaluar cada expresión dos veces y para definir los ambientes ciclicos,
; pero no le entrega al programador la capacidad de utilizarla.

; P2.
; caso 1: nada, ya que las funciones están previamente definidas y poseen visibilidad global entre ellas
; caso 2: nada, ya que el binding del nombre de la función se encuentra en su entorno
; caso 3: ambientes cíclicos, ya que los bindings no se van a quedar siempre en el scope, por lo que es necesario que se mantenga en el scope mediante un ambiente cpiclico

; P3.
; Basicamente el recursion via self application

; P4.
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