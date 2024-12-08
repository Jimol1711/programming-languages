#lang play

; P1. Hecha antes

; P2. Hecha antes

; P3. Hecha antes

; P4.
; La misma implementación de CLASS pero hay que reimplementar una class adentro y definirla como variable local vacía, y la macro sería la lambda con el (msg . vals) pero con un
; match que si detecta que el mensaje es create, da un error de que no se puede instanciar y si no le aplica la macro definida localmente a msg args (apply c (cons msg vals))

; P5.
; Instanciar la clase con la macro