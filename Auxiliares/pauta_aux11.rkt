#lang play

(print-only-errors #t)

;;p1)
(define (concat-TRO l1 l2)
  (define (concat-aux l1* l2* acc)
    (match l1*
      ['() (append acc l2*)]
      [ (list x xs ...) (concat-aux xs l2* (append acc (list x)))]
      ))
  (concat-aux l1 l2 '()))

(test (concat-TRO '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
;;p2)
;;a)
(λ (f)
  (match f
    [ 'a 1]
    [ 'b 2]
    [ 'c 3]
    ; ; tambien pueden agregar un clause de error, pero no es necesario:
    [_ (error 'record " invalid key: ~a" f ) ]
    ))

;;b)
(defmac (record [field-name : field-value ] ... )
  #:keywords :
  (lambda (field)
    (match field
      [ ‘field-name field-value ] ...
      ; ; Aqui si es necesario el error clause:
      [_ (error 'record " invalid key: ~a" field ) ]
      ))
  )
;;c)
(defmac (--> record field ) (record ‘ field ))


;;P3)
#|
(a) La expresión va a fallar porque el primer componente del par intenta acceder al
valor de a, que no está en scope (dado que es definido en el segundo componente).

(b) No cambia el comportamiento; volverá a fallar. Esto se debe a que, a pesar de que
el store fluye del primer componente al segundo, la variable a solo está definida
dentro del with (scope léxico).

(c) Reduce a (numV 1) dado que el último (openbox a) usa un store donde a aún
apunta a 0. Esto se justifica opr la linea 21 del intérprete, donde se observa
que el valor que devuelve la proyección se acompaña no por pair-sto (el store
resultante de evaluar el par), sino por sto (el store inicial).

(d) Linea 23:
(v*s val1 pair-sto ) ]
|#


;;P4)
#|
a) Un interprete sintáctico es el que ocupa lo menos posibles los valores de lenguaje base, es decir, trata de implementar todo
por su cuenta de manera detallada. En cambio el meta-interprete ocupa en todo lo posible los valores, funciones y operaciones que
tenga el lenguaje base.

Por ejemplo en todo el curso se ha tratado lo más posible de hacer un interprete sintáctico, construyendo los valores de retorno
(numV) y (closureV), donde el primero ocupa los numeros de Racket  y el segundo NO ocupa las funciones de racket tal cual, si no que
ocupa un (closureV id body env)

b) Los wrappers son envulturas que les hacemos a los valores de retorno de lenguaje y la ventaja es que nos permiten tener más control
por ejemplo en (closureV id ...) podemos elegir los campos dentro y poder desarmarlo (unwrap) de la manera que queramos dentro del
interprete, en cambio sin unwrapper no podriamos tener ese control y nos tocaría confiar en el lenguaje base

c) Al usar funciones anonimas de racket para guardar un fun-val dentro de closureV, lo que haremos en la aplicación será simplemente INTERPRETAR
lo que ese closurev tenga, y en ese punto de programa solo tenemos acceso al ambiente donde se ejecuta la aplicación, no así la definición
de la función, por lo tanto, no hay manera de entrar a esa función anonima de Racket a obtener esa información.

|#


;;-------------P5-------;;;nuevo
(define empty-sto
  (λ (loc) (error "not found locaction ~a" loc)))

;; es más compleja por que debe seguir buscando
(define (sto-lookup loc sto)
  (let ([value (sto loc)])
    (if (not (procedure? value))
      value
      (sto-lookup loc value) ;;si value es un procedure entonces sigue buscando en el value que es una funcion que tiene la información que viene
      )))

(define (extend-sto new-loc value sto)
  (λ (loc)
    (if (equal? loc new-loc)
        value
        sto))) ;;--> cambio importante, no es (sto loc) si no solo sto, si no, no podriamos hacer next-location

(define sto1 (extend-sto 1 4 (extend-sto 0 2 empty-sto)))
(define sto2 (extend-sto 0 5 (extend-sto 1 4 (extend-sto 0 2 empty-sto))))
(define sto3 (extend-sto 1 3
                         (extend-sto 0 5
                                     (extend-sto 1 4
                                                 (extend-sto 0 2 empty-sto)))))
(define sto4 (extend-sto 3 3
                         (extend-sto 2 5
                                     (extend-sto 1 4
                                                 (extend-sto 0 2 empty-sto)))))

(test (sto-lookup 0 sto1) 2)
(test (sto-lookup 0 sto2) 5)
(test (sto-lookup 0 sto3) 5)
(test (sto-lookup 0 sto4) 2)

;;next-location :: Procedure -> Int
;; retorna la siguiente locacion disponible, sin contar que hayan repetidos, es decir, si solo 5 direcciones 0
;; con distintos valores, entonces retornará la direccion 5, ya que esto es sin (replace-or-add), eso sería más complejo
;;logica: si el sto es el vacio entonces retorna actual que al principio es cero, pero si no, revisa si al evaluar sto en actual
;;tenemos un valor del lenguaje o una funcion de racket, en el primer caso, tenemos dos posibilidaes, que tenemos es que al principio
;; del sto haya el valor del final por ejemplo en sto2 la dir está 2 veces, y la primera vez no retornamos, si no que debemos pasar a la dirección
;; que viene y sumar 1, es decir, que si justo el valor de 'actual es justo el de ese momento y nos retorna un valor que no sea función, debemos ignorarlo
;; y pasar a lo que viene hasta siosi llegar al empty-sto
(define (next-location sto)
  (define (next-location-aux sto* actual)
    (if (equal? sto* empty-sto)
        actual
        (if (not (procedure? (sto* actual)))
            (next-location-aux (sto* (+ 1 actual)) (+ 1 actual)) ;;valor no funcion
            (next-location-aux (sto* actual) (+ 1 actual)))))
  (next-location-aux sto 0))

(test (next-location sto1) 2)
(test (next-location sto2) 3)
(test (next-location sto3) 4)
(test (next-location sto4) 4)