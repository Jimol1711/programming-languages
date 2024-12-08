#lang play

; P1.
; Implementa call by value. Habría que hacer que saque la locación de la variable que se use como argumento

; P2. y P3. MATERIA NO VISTA

; P4. Porque and debe implementar el short circuit, y el problema es que Racket es eager y evalua todos los argumentos, por lo que evaluaria todas las cosas del and y podría dar un
; un true aunque haya un false después
; Mejor respuesta: Se puede implementar, pero no se puede añadir corto-circuito. Esto rompería con la semántica de call by value