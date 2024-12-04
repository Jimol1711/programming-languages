#lang play

; P1.
; a) Sirven para cuando en un programa se quiere implementar evaluación perezosa, este puede retornar la clausura de una expresión. Por lo tanto, para poder reducir lo que retorna un programa
; y seguir implementando este tipo de evaluación siempre es necesario establecer puntos de evaluación estricta (Que siempre se evaluan). Estos son los strictness points. Un ejemplo es en el
; retorno de una función al ser aplicada. Este se puede reducir a un exprV (Ya que puede ser una operación combinada) y por ende es necesario reducirla estrictamente. Otro ejemplo es en lo que
; recibe una operación como add. Esto debe ser un numV, por lo que es necesario que se reduzca estrictamente a un numV.

; b)
;  a) El problema es que no se esta reduciendo further, es decir, solo se reduce 1 nivel, pero se puede seguir obteniendo un exprV y esto arrojaría un error.

;  b)
; {with {x {+ 1 2}}
;    {with {y x}
;          {+ y 1}}}
; x será una promesa de evaluar la suma entre 1 y 2, y esa promesa no se puede sumar a 1 por lo que daría un error

; c) Haskell es lazy por defecto.

; P2-
; a)
(define (sum n)
  (if (= n 1)
      1
      (+ n (sum (- n 1)))))
; el frame crecería n niveles.
; b)
(define (sum-tr n acc)
  (if (= n 1)
      acc
      (sum-tr (- n 1) (+ acc n))))

; P3.
; Expandir parte por parte
; en el env está el symbol con la dirección
; en el sto la dirección y el valor al que apunta
; las funciones son closures
; las cajas guardan una dir y la dir apunta al valor de la caja