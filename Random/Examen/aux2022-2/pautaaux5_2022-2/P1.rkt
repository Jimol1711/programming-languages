#lang play
#|

       SUPER PAUTA - Auxiliar #5

================================================
P1
================================================


-- 1.a 
Un lenguaje con evaluación perezosa ¿siempre toma más tiempo 
de ejecución que uno con evaluación temprana?

No necesariamente, puede existir un programa como:
(let ([x (expt 10 100000)])
     1)
donde una evaluación temprana calcularía el valor de 10^100000
para luego devolver 1, mientras que con evaluación perezosa se
omite tal cálculo.



-- 1.b
En un lenguaje con evaluación temprana, ¿Un if puede ser función?

Si consideramos if como función, la evaluación temprana exigiría
evaluar tanto la rama verdadera como la falsa. Por lo que:
(if (< 5 4)
	(error "chale")
	(+ 5 4))
produciría un error cuando claramente 5 no es menor que 4.



-- 1.c
En Racket, ¿and es una función?

No lo es, se implementan atajos de evaluación donde si el primer
argumento es falso, no se evalúa el siguiente (recordemos que un 
and solo es verdadero si ambos argumentos lo son).
(and 
	(begin (print "false") #f)
	(begin (print "tru") #t)) 


-- 1.d 
¿Qué semántica de evaluación usa Racket por defecto?
Idee un programa que lo demuestre.

Eager, el siguiente programa produce un error:
((lambda (x) 1) (+ 1 #f))  
aunque x jamás se instancia, se evalúa por semántica eager.



-- 1.e
Misma pregunta anterior para Haskell, Java, C++ y Python.

Haskell es lazy, utiliza call-by-need, esto permite las 
estructuras infinitas por ejemplo. Los 3 restantes son todos 
eager.


-- 1.f
¿Cuál es la diferencia entre aplicar evaluación temprana y 
perezosa en el siguiente programa?
{with {f {fun {x} 47}}
      {f y}}

Evaluación temprana produce un error, ya que 'y' no está definido 
mientras que se intenta evaluar su valor al llamar a f. Por otro 
lado, utilizando evaluación perezosa el argumento de la función 'x' 
jamás es instanciado, por lo que nunca es necesario obtener su 
valor (y por ende, el de 'y'), devolviendo 47.



-- 1.g
Consideremos una función while, ¿qué semántica de evaluación debería 
implementar para funcionar correctamente? Justifique.
(define (while condition body)
  ... Do Something ...)

Si utilizamos evaluación temprana, la condición se evaluará de 
inmediato y no en cada ciclo, por lo que potencialmente tendríamos 
loops infinitos. Por otro lado, si se considera call-by-need, el 
argumento es similar, se evalúa la condición la primera vez y se
cachea el resultado. La única manera de un correcto funcionamiento
sería mediante call-by-name.

1.h
Las clausuras de expresiones se utilizan para implementar lenguajes con
evaluación perezosa. La idea está en que dejamos para un futuro la
evaluación de expresiones, pero el contenido del ambiente puede variar
mucho desde ahora hasta el momento de evaluación. Es por ello que, al
igual que con funciones de primera clase, necesitamos una clausura
para guardar el ambiente actual y poder realizar la evaluación sin problemas.

|#