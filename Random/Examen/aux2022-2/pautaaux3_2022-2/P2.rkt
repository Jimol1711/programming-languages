#lang play
(require "first-order.rkt")

#|
       SUPER PAUTA - Auxiliar #3

================================================
P2
================================================
|#

(define my-funcs (list (fundef 'f    'x (parse '{+ y x}))
                       (fundef 'g    'y (parse '{+ y {f y}}))
                       (fundef 'h    'z (parse '{- {g z} n}))
                       (fundef 'add5 'n (parse '{+ 5 n}))))
;; a)
#|
  LEXIC - 10
  Se define x como 5 y se pasa el argumento x a la
  función add5, no existen variables libres en la
  función por lo que el scope es irrelevante.
|#
(test (run '{with {x 5}
                  {add5 x}} my-funcs)
      10)
#|
  DYNAMIC - 10
  Idem.
|#
(test (dyn-run '{with {x 5}
                      {add5 x}} my-funcs)
      10)




;; b)
#|
  LEXIC - ERROR
  Se define y como -3, y se procede a llamar
  a la función f pasando un 3 como argumento.
  Dentro de f, se captura el valor de x como
  3, sin embargo la definición de y no tiene
  alcance en su interior. Por esto ultimo es
  que al instanciar y se produce un error.
|#
(test/exn (run '{with {y -3}
                      {f 3}} my-funcs)
          "free identifier: y")
#|
  DYNAMIC - 0
  Teniendo alcance dinámico, la definición
  de y pasa al interior de f, de modo que
  la suma {+ y x} produce {+ -3 3} que es 0.
|#
(test (dyn-run '{with {y -3}
                      {f 3}} my-funcs)
      0)


;; c)
#|
  LEXIC - ERROR
  Se define "y" como 5. Luego, tenemos una
  suma donde el primer sumando redefine "y".
  Así, nos queda {+ 13 {f 22}}.
  Estando en el interior del llamado a f,
  nuevamente no se reconoce y como un enlace
  válido, produciendo un error.
|#
(test/exn (run '{with {y 5}
                      {+ {with {y {+ 8 y}} y}
                         {f 22}}} my-funcs)
          "free identifier: y")
#|
  DYNAMIC - 40
  En el llamado a f de este caso, y si se
  reconoce como identificador enlazado a un
  valor. Ahora, ese valor de y es 5 o 13?
  La respuesta es 5! Ya que la actualización
  del valor de y solo es valida dentro del
  alcance de ese with, que termina fuera del
  llamado a x. Finalmente, {f 22} devuelve
  {+ 22 5} que es 27, y sumando los 13 de
  afuera nos queda en 40.
|#
(test (dyn-run '{with {y 5}
                      {+ {with {y {+ 8 y}} y}
                         {f 22}}} my-funcs)
      40)



;; d)
#|
  LEXIC - ERROR
  Basta ver que f posee una variable libre "y".
  Como g llama a f, habrá un error.
  Se invoca a g pasando como argumento el 18,
  creando el binding (y,18). Al interior se
  llama a {f y}, por lo que al interior de f
  se enlaza (x,18). Sin embargo el enlace
  de y no entra a f, generando un error.
|#
(test/exn (run '{g 18} my-funcs)
          "free identifier: y")
#|
  DYNAMIC - 54
  Se invoca la función g pasandole como argumento
  el valor 18. Al interior de la ejecución de g,
  se enlaza (y, 18). Al llamar a f, como el alcance
  es dinámico, reconoce el enlace (y, 18) y además
  enlaza a x el parámetro recibido (x, 18).
  El resultado final es {+ 18(y) {+ 18(y) 18(x)}}
  que da 54.
|#
(test (dyn-run '{g 18} my-funcs)
      54)


;; e)
#|
  LEXIC - ERROR
  Basta ver que f posee una variable libre "y".
  Como h llama a g y g llama a f, se producirá este error.
|#
(test/exn (run '{with {n 12}
                      {with {y -3}
                            {h {add5 {f y}}}}} my-funcs)
          "free identifier: y")
#|
  DYNAMIC - -15
  Se fija el enlace (n,12) e (y,-3). Luego,
  al llamar a {f y} se establece dentro de
  la función el enlace (x,-3) (el argumento).
  De este modo obtenemos -6, que va de input
  a add5, lo cual devuelve -1.
  Finalmente, el llamado {h -1} crea un enlace
  (z, -1). Recordemos que n tiene asociado el 12,
  y por scope dinámico el alcance llega dentro
  de la función.
  Tenemos:
    {- {g z} n} con (z=-1;y=-3;n=12)
  En el llamado a g, se genera un enlace (y,-1)
  (el argumento de g). Aquí tenemos:
    {+ y {f y}} con (y=-1;z=-1;y=-3;n=12)
  En el llamado a f, se genera un enlace (x,-1)
  obteniendo:
    {+ y x} con (x=-1;y=-1;z=-1;y=-3;n=12)
  Resolviendo, el llamado a f produce -2. Dejando
  el llamado a g como -3. Por último, en h
  obtenemos {- -3 12}=-15.
|#
(test (dyn-run '{with {n 12}
                      {with {y -3}
                            {h {add5 {f y}}}}} my-funcs)
      -15)
