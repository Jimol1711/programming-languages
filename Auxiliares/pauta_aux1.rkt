#lang play
(print-only-errors #t) ;; Esto hace que solo se muestren los tests incorrectos al correr el programa
#|
Pauta Aux 1
|#

;;--------------P1-------------------

#|
a) 
¿Cual es la diferencia entre (cons 'a 'b ) y ( list 'a 'b')? Escriba el último en notación de pares.

La estructura cons corresponde a un par, mientras que list corresponde a una lista.
Con lo anterior, (cons 'a 'b) corresponde a un par donde el primer elemento es el símbolo 'a, y el segundo
elemento es el símbolo 'b; mientras que en (list 'a 'b) se tiene una lista donde nuevamente el primer elemento
es 'a y el segundo es 'b, pero además se tiene un elemento que está presente siempre al final de una lista,
correspondiente al elemento vacío. De esta forma, para representar (list 'a 'b) con notación de pares se
tendría que escribir como: (cons 'a (cons 'b '())), donde '() es la lista vacía.
|#
(test (cons 'a (cons 'b '())) (list 'a 'b))

#|
b) Hay varias formas de escribir '(1 (x (y . z) #f))
1. (list 1 (list 'x (cons 'y 'z) #f))
2. (cons 1 (cons (cons 'x (cons (cons 'y 'z) (cons #f '()))) '()))
|#
(test (list 1 (list 'x (cons 'y 'z) #f)) '(1 (x (y . z) #f)))
(test (cons 1 (cons (cons 'x (cons (cons 'y 'z) (cons #f '()))) '())) '(1 (x (y . z) #f)))

#|
c) Dado (define l (list '(a (b c)) '(d e f)))
1. Para acceder a b: (car (car (cdr (car l))))
2. Para acceder a f: (car (cdr (cdr (car (cdr l)))))
|#
(define l (list '(a (b c)) '(d e f)))
(test (car (car (cdr (car l)))) 'b)
(test (car (cdr (cdr (car (cdr l))))) 'f)

#|
d) Usando solo cons y '()
1. '(a . b) : (cons 'a 'b)
2. '(a b) : (cons 'a (cons 'b '()))
3. '(a (b . c) d) : (cons 'a (cons (cons 'b 'c) (cons 'd '())))
|#
(test (cons 'a 'b) '(a . b))
(test (cons 'a (cons 'b '())) '(a b))
(test (cons 'a (cons (cons 'b 'c) (cons 'd '()))) '(a (b . c) d))


;;--------------P2-------------------

;;a)
;;sum-coins :: Number Number Number -> Number
;;calcula el monto total a partir de monedas de 50, 100 y 500
(define (sum-coins c50 c100 c500)
  (+ (* c50 50) (* c100 100) (* c500 500))
  )
         
(test (sum-coins 1 1 1) 650)
(test (sum-coins 0 0 1) 500)
(test (sum-coins 0 1 0) 100)
(test (sum-coins 1 0 0) 50)

;;b)
;;pitatoria :: Number Number -> Number
;;calcula la pitatoria desde un inicio hasta un final dado

(define (pitatoria i j)
  (if (equal? i j)
      i
      (* j (pitatoria i (- j 1)))))

(test (pitatoria 1 2) 2)
(test (pitatoria 0 5) 0)
(test (pitatoria 2 4) 24)
(test (pitatoria 3 3) 3)


;;c)
;;lista_impares :: Number -> ListOf[Number]
;;crea la lista de largo n entregado de impares

(define (lista_impares n)
  (define (lista_impares_aux n* l)
    (cond [ (< n* 0) (error "Not negatives length allows")]
          [ (zero? n*) l ]
          [ else (lista_impares_aux (- n* 1) (append (list (- (* 2 n*) 1)) l)) ]
          ))
  (lista_impares_aux n '()))


(test (lista_impares 3) (list 1 3 5))
(test (lista_impares 0) '())
(test (lista_impares 5) (list 1 3 5 7 9))
(test/exn (lista_impares -1) "Not negatives length allows")

;;solucion alternativa sin usar una funcion auxiliar
(define (lista_impares2 n)
      (cond [ (< n 0) (error "Not negatives length allows")]
          [ (zero? n) '() ]
          [ else (append (lista_impares2 (- n 1)) (list (- (* 2 n) 1))) ]
          ))

(test (lista_impares2 3) (list 1 3 5))
(test (lista_impares2 0) '())
(test (lista_impares2 5) (list 1 3 5 7 9))
(test/exn (lista_impares2 -1) "Not negatives length allows")

