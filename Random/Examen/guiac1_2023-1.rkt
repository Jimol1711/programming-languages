#lang play

; P1.
; a) Nos permite escribir programas fáciles de leer y crear intérpretes basados en patrones (Interpretar una sintaxis, de manera de establecer patrones para luego parsearlas)

; b) Esto es ya que puede reaparecer la variable en una expresión y si esto sucede, se debe recorrer toda la expresión para buscarla. La subtitución diferida nos permite guardar el valor de la
; variable de manera de solo tener que buscarla en un ambiente definido.

; c) El scope estático es un tipo de scope en el que los identificadores no se asignan dinámicamente en tiempo de ejecución, si no que toman el valor que estos tienen en el scope en el
; "texto". Los ambientes tienen la finalidad de preservar scope estático y al mismo tiempo permitir que el lenguaje tenga funciones de primera clase, por lo que son conceptos que van
; muy ligados. MALO

; Es un tipo de scope en el que los identificadores de variables definidas en un programa no permean dentro de una función al momento de aplicar la función, permitiendo que esta sea
; encapsulada a su propio "entorno": De ahí sale el concepto de ambiente, el cual es una estructura que está pensada para preservar el scope estático en un lenguaje con funciones de
; primera clase. La idea es que al tener un ambiente para clausuras particulares, estos permiten guardar los valores y hacer a las funciones más independientes.

; d) Un caso particular es si se quiere imprimir algún valor a un archivo temporal durante la ejecución de un programa para irlo comparando.

; e) Ver si es que las variables definidas en un programa pueden permear dentro de una función, como en el siguiente caso:
;; {define {f x} {+ x n}}
;;    {with {n 10}
;;           {f 10}}
; Si da un error es estatico, si da 20 es dinámico

; f) Porque las clausuras permiten que las funciones consideren ambientes solo al momento de su ejecución, no tomando en cuenta un ambiente que podría tener una definición que influya
; en su ejecución y en su output.

; P2.
; El y en el último with y el último z

; P3.
; a)
;; curry : int x f -> f
;; function that currifies f of n arguments
(define (curry n f )
  (define (curry-aux n f args)
    (if (equal? n 0)
        (apply f (reverse args))
        (lambda (x) (curry-aux (- n 1) f (cons x args)))))
  (curry-aux n f ' ()))

(test (((curry 2 +) 1) 2) (+ 1 2))
(test (((curry 2 -) 1) 2) (- 1 2))

; b)
;; uncurry-2 : f -> f
;; function that return the uncurrified version of f of 2 arguments
(define (uncurry-2 f)
  (lambda (x y) ((f x) y)))

; Para curry hacer una fn auxiliar que reciba una lista de argumentos y los meta, y después llamarla con lista vacía.
; Para uncurry devolver la lambda con los n argumentos y que la lambda devuelva la versión currificada.
