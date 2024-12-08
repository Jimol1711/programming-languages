#lang play

#|       SUPER PAUTA - Auxiliar #7

================================================
P5
================================================

--- A ---

(with (b (newbox 5))
    (+ (with (n (openbox b))
            (seqn
                (setbox b (+ n n)) n))
        (openbox b)))

Si analizamos paso a paso este programa, primero se define
una caja cuyo contenido es un entero 5.
Luego el primer sumando:
 - Toma como "n" el contenido de la caja (ie: n=5)
 - Muta el interior de la caja a (+ n n), es decir, la caja
   contiene ahora un 10.
 - Retorna n, que recordemos es 5.

De momento la suma es (+ 5 _), y el contenido de la caja
es 10.

El segundo sumando abre la caja encontrándose con un 10.
Por tanto, el resultado final es (+ 5 10) que es 15.


--- B ---

(with (acum (newbox 0))
    (with (sum (fun (n) (seqn 
                            (setbox acum (+ n (openbox acum)))
                            (openbox acum))))
            (+ (sum 5) (sum 10))))

Primero creamos una caja cuyo contenido es 0.
Luego, se define una función "sum" cuyo efecto es sumarle su
argumento al contenido de la caja. Esta función devuelve el
contenido de la caja (después de modificarla).

Entonces, la primera vez que se invoca (sum 5), se suma 5
al contenido inicial de la caja (que es 0), quedando ahora
en 5. Luego se retorna dicho valor.

Tenemos de momento (+ 5 (sum 10)), con el contenido de "acum"
igual a 5.

El segundo llamado suma 10 al contenido de la caja, que recordemos
es 5! quedando el contenido final en 15.

Por lo tanto, la suma final es (+ 5 15), que produce 20.


--- C ---

(with (b (box 1))
    (with (f (fun (x)
                (with (e (openbox b))
                    (seqn (setbox x 18) e))))
            (+ (f b) (f b))))

Este programa primero crea una caja "b" cuyo contenido es 1.
Luego, se define una función "f" que recibe una caja "x" como
argumento.

Dentro de "f", se asocia el contenido actual de la caja recibida
al identificador "e". Luego, se cambia el contenido de la caja "x"
por 18 y finalmente se devuelve "e".
    
En la primera llamada (f b), se recibe x = b (box 1), de modo que
el contenido de "b" pasa a ser 18, pero se devuelve su contenido
antiguo 1.

Tendríamos de momento (+ 1 (f b)) con b guardando un 18.

La segunda llamada vuelve a insertar 18 en la caja, y retorna el
contenido previo, pero ahora el contenido previo es 18 también.

Obtenemos como resultado final (+ 1 18), es decir 19.


--- D ---

(with (switch (newbox 0))
    (with (toggle (fun (dummy)
                        (if0 (openbox switch)
                            (seqn
                                (setbox switch 1) 1)
                            (seqn
                                (setbox switch 0) 0))))
            (+ (toggle 5) (toggle 6))))

Lo primero que debemos notar es que la función toggle
no utiliza jamás su argumento. Solo alterna el contenido
de switch de 0 a 1 y viceversa, retornando a qué valor
cambió el switch.

Considerando eso podemos saltarnos directo al final.
Recordemos que switch se encuentra inicialmente en 0,
luego:
(+ (toggle 5) (toggle 6))
Provoca un cambio a 1 y luego otro a 0, obteniendo
(+ 1 0)
Que es 1 (wii).
|#