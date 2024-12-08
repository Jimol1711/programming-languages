#lang play

; P1.
; a) No, de hecho puede tomar menos, ya que podría no evaluar un argumento que no se usa
; b) No, se debe implementar mediante una macro, ya que si no se evaluarían las ramas t y f antes de la condición
; c) No, es una macro
(and
        (begin (print "tru") #t)
	(begin (print "false") #f)
	(begin (print "tru") #t)) 
; d) Utiliza eager evaluation
(define (eager a b)
  (void))

(eager (display "a") (display "b"))

; Error porque se evalua el argumento (+ 1 #f) aunque no se use en la función
; ((lambda (x) 1) (+ 1 #f)) 
; e) Lazy
; f)
; Lazy: No pasa nada, porque el argumento y no se evalua, se devuelve 47
; Eager: Da un error porque el id y es libre
; g) Debería ser lazy, porque si fuera eager se evaluaría una sola vez la condición del while y no de nuevo, con lo que se podría quedar en un loop infinito
; h) Las clausuras corresponden a la definición de la función más su entorno al momento de su aplicación. Estas wrapean la función en el momento de su aplicación con el entorno que
; contiene los identificadores hasta ese momento. Se utilizan para poder implementar evaluación perezosa y mantener scope estático.

; P2.
; a)
; En temprana llega e imprime todo en orden
; En lazy call by name z -> y -> x -> z -> y
; En lazy call by need z -> y -> x

; b)
; En temprana bip
; En lazy call by name bip bip bip bip
; En lazy call by need bip

; c)
; En temprana Oh No Yeah
; En lazy call by name Yeah Yeah
; En lazy call by need Yeah

; P3.
; Implementa evaluación perezosa, para que sea temprana hay que aplicarle eval a e en el with