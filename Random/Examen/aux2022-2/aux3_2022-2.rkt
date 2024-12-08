#lang play

; P1.
; a)
; Substitución directa
; SUbtitución diferida
; La idea es ir mostrando la ejecución pero en el caso de diferida ir con un ambiente

; b)
; Sí dan lo mismo. La diferencia es que la diferida si encuentra el mismo identificador, en lugar de reasignarlo los saca del enviroment. La diferencia recae en la eficiencia.

;
; P2.
; a) En ambos da 10
; b) En scope estático da error y en dinámico da 0
; c) En dinámico error y en estático 48
; d) En estático error y en dinámico 24
; e) En estático error y en dinámico -15

; P3.
; a) Porque el scope dinámico puede generar comportamientos extraños cuando se tienen variables con identificadores iguales por ejemplo a los utilizados para parámetros de funciones.
; También facilita el trabajo al programador porque le permite hacerse una idea del scope de variables solo leyendo el programa sin preocuparse de su ejecución por detrás.
; b) SI se quieren acceder a variables del sistema es mejor tener un scope dinámico, ya que en caso contrario habría que darlas como parámetro a las funciones.

; P4.
; NO ENTENDÍ LA PREGUNTA

; P5.
; a) Ponerlos como {* <Expr> <Expr>} y {/ <Expr> <Expr>} y en el interprete agregar esos casos en el patrón del pattern matching
; b) Sí podríamos, agregando un nodo <binop> que agregue todas las operaciones binarias en la BNF