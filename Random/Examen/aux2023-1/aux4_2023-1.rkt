#lang play

; P1.
; a) No se cual es
; b) Porque es mejor para detectar errores leyendo el código y no requiere conocer los valores que tienen las variables en tiempo de ejecución.
; c) Si se quiere acceder a una variable del sistema

; P2.
; Básicamente usar la sintaxis para definir las funciones en cuestión

; P3.
; a)
(define (fundef a b c) (void))
(define (parse a) (void))
(define (run a b) (void))

(define my-funcs (list (fundef 'f 'y (parse '{+ y x}))))

(run '{with {x 5}
            {+ {with {x 8} x}
               {f 3}}} my-funcs)
; Hechas en ipad
