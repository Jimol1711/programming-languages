#lang play
#|

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
	(error "no funciona el if :c ")
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



#|

================================================
P2
================================================

-- 2.a
(define (kim-possible)
  (let ([x (begin (print "Call Me ") 1)]
        [y (begin (print "Beep Me ") 2)]
        [z (begin (print "If you wanna reach me ") 3)])
    (+ z y x (+ z y))))

Temprana:     Call Me Beep Me If you wanna reach me 11
Call-by-name: If you wanna reach me Beep Me Call Me If you wanna reach me Beep Me 11
Call-by-need: If you wanna reach me Beep Me Call Me 11



-- 2.b
(define (correcaminos)
  (let ([x (begin (print "bip ") 1)])
    (+ x x x x)))

Temprana:     bip 4
Call-by-name: bip bip bip bip 4
Call-by-need: bip 4



-- 2.c
(define (oh-no?)
  (let ([x (begin (error "Oh no") 1)]
        [y (begin (print "Yeah") 2)])
    (+ y y)))

Temprana:     Error: Oh no
Call-by-name: Yeah Yeah 4
Call-by-need: Yeah 4


|#


#|
================================================
P3
================================================

El primer error encontrado leyendo el interprete desde arriba hacia abajo es en la función strict.
Ya que ésta no es recursiva y si llegara a haber una (exprV) dentro de otra (exprV) esta no podría resolver el problema

un ejemplo que verifica el error sería el siguiente:

(run '{with {a 3}
                  { {fun {x} {+ x x}} a}})

acá al tener un with con a -> 3 se pasa a la aplicación de una función anónima donde se guarda en el ambiente
que env1 = [a -> (exprV (num 3) (mtEnv) (box #f)) ], es decir a se guarda como una promesa. Luego para aplicar la función
anónima también se tiene el valor de a denuevo, entonces para añadir x al ambiente para evaluar el cuerpo se debe
[x -> (exprV (id 'a) env1 (box #f))], pero al interpretar luego el cuerpo, en cada operando se llama a strict e interpreta
y por lo tanto interpretaria (id 'a) y esto da como resultado (exprV (num 3) (mtEnv) (box #f) y por eso falla.



El segundo error es en el if0, donde en la condición no se usa strict <->

(run ' {if0 {{fun {x} x} 3}
            0
            1})

Como sabemos al evaluar una función anonima identidad con un valor obtenemos una promesa, ya que el el cuerpo de la función es
(id 'x) y cuando hace el lookup en el env devuelve la promesa (exprV (num 3) ..) y debería hacerse un strict en ese punto, pero como no
se hace, al hacerse el numV-Zero? falla



El tercer error es en el uso del ambiente vacio en la promesa en vez del env <-> (exprV e (mtEnv) (box #f)) en vez de (exprV e env (box #f))

(run '{with {a 3}
            {{fun {x} x} {+ a 5}}})

como se usa el ambiente vacio en vez del env, no se guarda la información de que [a -> (exprV (num 3) ...)] y por lo tanto al momento
de evaluar el argumento de la función anónima no se sabe quien es (id 'a), por lo tanto se da un free-identifier.



El cuarto error es la falta de strict en el run, ya que cuando se ejecuta una función anonima identidad se devuelve la promesa y no se le hace un strict
por lo que debemos asegurar que siempre devolvamos un valor valido del lenguaje, es decir, un numero o una clausura, para esto lo más fácil es usar strict en
run, aunque también se puede usar strict en (id ... ) (strict (env-lookup ...))

un ejemplo sería:

(run '{{fun {x} x} 3})

que entrega (exprV (num 3) (mtEnv) '#&#f) en ves de 3, por lo que dijimos antes



|#
