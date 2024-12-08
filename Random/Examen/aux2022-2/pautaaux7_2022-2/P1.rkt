#lang play

#|       SUPER PAUTA - Auxiliar #7

================================================
P1
================================================


--- A ---

Llamado por la cola (o tail call ocurre cuando la última
instrucción de una función es una llamada a alguna otra
función, si el llamado es recursivo entonces hablamos de
recursión por la cola (o tail recursion).

Por ejemplo:

-- TAIL CALL --         -- TAIL REC --              -- None --
(define (foo x)         (define (bar x)             (define (uwu x)
	...                     ...                         ... 
	(bar y))                (bar (-1 z))                (* x (uwu (-1 z)))

Más en: https://users.dcc.uchile.cl/~etanter/recursion/Recursion__Efficiency_Considerations.html


--- B ---

Funciones de Racket que:

>> 1 - No pueda optimizarse con TRO ni TCO.

Puede ser cualquier función que no posea llamada por la cola.
(define (fact n)
	(if (= 0 n)
		1
		(* n (fact (- n 1))))
La función factorial debe multiplicar posterior al llamado recursivo,
por lo que no es llamada por la cola.


>> 2 - Se pueda aplicar directamente TRO.

Se necesita una función que sea recursiva por la cola.
(define (even n)
	(cond
		( [= n 0] #t)
		( [= n 1] #f)
		( else (even (- n 2)))))
En este caso, el llamado recursivo es la última instrucción
que ejecuta la función, de modo que no es necesario recordar
nada del llamado previo.


>> 3 - Se pueda aplicar TCO, pero no TRO directamente.

Se requiere una función que posea un llamado por la cola no
recursivo. Por ejemplo:

(define (even n)
	(if (= n 0)
		#t
		(odd (- n 1))))

(define (odd n) ...)

En este caso, el llamado a odd es la última instrucción
que ejecuta even.
|#
