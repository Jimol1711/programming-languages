#lang play

;;------------------P1-----------------------
#|
Solución:

(a) Si analizamos paso a paso este programa, primero se define una box cuyo contenido 
es un entero 5. Luego el primer sumando toma como n el contenido de la caja (ie: 
n = 5), y setea como nuevo contenido (+ n n), es decir, la caja contiene ahora un 10. 
Finalmente retorna n, que recordemos es 5. 

De momento la suma es (+ 5 __), con el segundo sumando, el contenido de la caja b, 
que siguiendo la mutación, debería ser 10. Por tanto, el resultado final es 5 + 10 = 
15. 

(b) Primero creamos una caja cuyo contenido es 0, y se define una función sum cuyo 
efecto es sumarle su argumento al contenido de la caja, y luego retornar el contenidode la caja.
Entonces, la primera vez que se invoca (sum 5), se suma 5 al contenido 
actual 0, quedando acum con un 5 en su interior. Luego se retorna su valor, de modo 
que obtenemos (+ 5 (sum 10)). Finalmente el segundo llamado suma 10 al contenido 
de la caja, que ya sabemos que es 5, quedando el contenido final en 15. Por lo tanto, 
la suma final es (+ 5 15), que produce 20. 

(c) Este programa primero crea una caja b cuyo contenido es 1. Luego, se define una 
función que recibe una caja x como argumento, en ella se define una id e cuyo valor 
es el contenido actual de la caja b, y luego se cambia el valor de la caja x recibida 
como argumento por 18 y retorna el contenido de b. Entonces, en la primera llamada 
(f b), se recibe x = b, de modo que el contenido de b pasa a ser 18, pero se devuelve 
su contenido antiguo 1. La suma entonces queda (+ 1 (f b)). La segunda llamada 
vuelve a insertar 18 en la caja, y retorna el contenido previo, pero ahora el contenido 
previo es 18 también, dado que lo modificamos en la primera llamada a f. De este 
modo, obtenemos como resultado final (+ 1 18), es decir, el resultado final es 19. 

(d) El resultado es 1, primero tenemos una box que guarda el valor 0, y definimos toggle 
como una función que recibe un argumento dummy (que no se utiliza). Por esto, de 
partida, no nos importa qué argumento le entregamos a toggle, lo importante es que 
cambia el switch de 0 a 1, o de 1 a 0. 

En el sumando izquierdo (toggle 5), efectivamente la caja contiene un 0, por lo que 
se cambia su contenido a 1 y se retorna 1. Obtenemos así: (+ 1 (toggle 6)). 

Luego, en el sumando derecho, se ve que el contenido de la box es 1, por lo que se 
cambia a 0 y se retorna 0. Finalmente la suma queda (+ 1 0) que es 1
|#


;;------------------P2--------------------------

#|
{with {m {newbox 1}} 
  {with {f {fun {b} {with {x {openbox b}} 
    {seqn {setbox b 18} x}}}} 
    {+ {f m} {f m}}}}}

Primero se tiene una variable 'm que tiene una direccion 0 que apunta hacia una caja que adentro tiene una direccion
1 que apunta hacia el (numV 1)

Recordemos que no puede ser simplemente 'm -> 0 y luego 0->(boxV (numV 1)), ya que las cajas guardan direcciones, no valores del lenguaje

asi tenemos
env1 = ['m -> 0]
store1 = [0 -> (boxV 1), 1 -> (numV 1) ]

con la reduccion nos queda

{with {f {fun {b} {with {x {openbox b}} 
    {seqn {setbox b 18} x}}}} 
    {+ {f m} {f m}}}}

luego seria 'f que se asinga a la siguiente dir libre, que sería 2
y a 2 se le asigna la clausura de la funcion

env2 = ['m -> 0, 'f -> 2]
store2 = [0 -> (boxV 1), 1 -> (numV 1), 2 -> (closureV 'b {with {x {openbox b}} {seqn {setbox b 18} x}}} env)]

como solo se guarda 'f ahora recien debemos evaluarlo al hacer la suma de {+ {f m} {f m}}

primer operando:

fenv = env2 = ['m -> 0, 'f -> 2]
f-store = store1 = [0 -> (boxV 1), 1 -> (numV 1) ]

(interp {with {x {openbox b}} 
           {seqn {setbox b 18} x}}} fenv f-store)

env3 = ['x -> (numV 1)]
y cambiamos el valor de la caja de b por 18 cuando antes tenía el valor 1
así -> store1 = [0 -> (boxV 1), 1 -> (numV18) ]

(interp (id 'x) env3 store1)

es decir el primer operando nos da (numV 1)


y en el segundo operando tendriamos lo mismo pero ahora al abriri la caja 'b nos daría el valor 18

así la suma nos da 19
|#







;;------------------P3----------------------

;;Primer error es que en (id x) no se hace el sto-lookup despues del env-lookup
;;esto genera que en vez de devolver un valor del lenguaje, se retorne una locacion de memoria, que es un
;; Int de Racket.

#|
(run '{with {x 3} x})

eso se transforma a
(app (fun 'x (id 'x)) (num 3))

1) en el nodo app primero se interpreta la funcion, que en este caso es anonima y simplemente queda la clausura
  (val*sto (closureV parameter body fenv) f-sto) <-> (interp (fun 'x (id 'x) mtEnv mtSto)
  -> parameter = 'x
  -> body = (id 'x)
  -> fenv = mtEnv

2) luego interpretamos el argumento
   (val*sto arg-value arg-sto) <-> (interp (num 3) mtEnv mtSto)
   -> arg-value = (numV 3)
   -> arg-sto = mtSto

3) Creamos una nueva dir de memoria
   (def location (next-location arg-sto)) <-> (def location (next-location mtSto))
   -> location = 0

4) interpretamos el cuerpo con el sto y el ambiente extendido
   (interp body
              (extend-env parameter location fenv)
              (extend-sto location arg-value arg-sto))

   <->
   (interp (id 'x)
           (extend-env 'x 0 mtEnv)
           (extend-env 0 (numV 3) mtSto))
   <->
   (interp (id 'x) ['x -> 0] [0 -> (numV 3)])


5) interpretamos el cuerpo y vamos al match de (id x)

   (val*sto (env-lookup 'x ['x -> 0]) [0 -> (numV 3)])

  -> (val*sto 0 [0-> (numV 3)])

cuando debería haber sido
  (val*sto (sto-lookup (env-lookup 'x ['x -> 0]) [0 -> (numV 3)]) [0 -> (numV 3)])

  -> (val*sto (sto-lookup 0 [0 -> (numV 3)]) [0 -> (numV 3)])

  -> (val*sto (numV 3) [0 -> (numV 3)])





;;Segundo error es en la propagación de las mutaciones luego de la interpretacion de la condicion del if0
;; ya que no pasa a las ramas

;;Veamos un ejemplo donde en la condicion se cambia una caja que es usada en la condicion false y como el store
de la condicion no pasa hacia abajo no llega la informacion actualizada y queda con el valor antiguo

{with {b {newbox 0}}
                  {if0 {seqn {setbox b 5} {openbox b}}
                       1
                       {openbox b}}}

cuando evaluemos la condicion del if0 vamos a tener que
env = ['b -> 0]
store = [0 -> (boxV 1), 1 -> (numV 0)]

esto ya que el with es una aplicacion de funcion anonima con parametro 'b que se le asocia
una direccion 0 que apunta hacia la caja, pero la caja no contiene el valor directamente, si no que contiene
una direccion que luego contiene el valor real

entonces en la condicion haremos del seqn cambiando el valor ed la direccion 1 por (numV 5)

env = ['b -> 0]
store = [0 -> (boxV 1), 1 -> (numV 0)]
c-store = [0 -> (boxV 1), 1 -> (numV 5)]

y como evaluamos las ramas con store en vez de c-store obtenemos el valro desactuaizado que es 0




Tercer error es que en (setbox ..) en vez de usar el expr-sto al retornar se retorna con
box-sto. Esto hace que las modificaciones que se realicen en la expr que se ocupa para setear no serán
visibles en lo que venga después del programa.

Por eso un buen ejemplo sería tener 2 cajas, una 'b y otra 'a donde con un setbox
se modifique 'a con el valor (seqn () 3) donde en () se hará una modificación en 'b que luego debería ser usada
para hacer (+ (openbox a) (openbox b)) luego

'{with {b {newbox 1}}
                 {with {a {newbox 2}}
                          {seqn {setbox a {seqn {setbox b 0} 3}}
                                {+ {openbox a} {openbox b}}}}}

entonces cuando evaluamos el cuerpo del segundo with tenemos que

env = ['b ->  0, 'a -> 2 ]
store = [0 -> (boxV 1), 1 -> (numV 1), 2 -> (boxV 3), 3 -> (numV 2)]

es decir, 'b es una caja con un 1 y 'a es una caja con un 2

luego hacemos un {seqn A B}

A -> {setbox 'a {seqn {setbox b 0}} 3}}

primero se interpreta la caja que no trae ninguna modificación en el storage (queda igual que el anterior)
box-sto = store

luego se interpreta la expresion {seqn {setbox b 0}} 3}} y ahí si cambía el valor de la caja 'b a zero

expr-sto = [0 -> (boxV 1), 1 -> (numV 0), 2 -> (boxV 3), 3 -> (numV 2)] != store

pero como usamos el box-sto para retornar tenemos que en la parte B usaremos el box-sto en vez del expr-sto

(interp '{+ {openbox a} {openbox b}} env box-store)

y con eso interpretamos a suma y obtenemos {+ 3 1} en vez de {+ 3 0}

|#