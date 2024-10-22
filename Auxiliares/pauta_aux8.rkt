#lang play

(print-only-errors #t)
(require "base_aux8.rkt")

;;P1)
;;a) clycic-list :: ListOf[A] -> ListOf[A/box(ListOf[..])]
;;funcion que se le pasa una lista y devuelve una lista ciclica con una caja dentro que contiene la misma direccion
(define (clycic-list l)
  (define place-holder (box #f))
  (define lista-ciclica (append l (list place-holder)))
  (set-box! place-holder lista-ciclica)
  lista-ciclica)

;;listas de ejemplo
(define l1 (list 1 2 3))
(define l2 (list #\d #\c #\c #\ ))

;;b) take-list-n ::  ListOf[A/box(ListOf[..])] -> ListOf[A]
;;recibe la lista ciclica y cuando llega a la caja con la lista guardada nuevamente le hace unbox y sigue al recursion
(define (take-list-n l n)
  (match n
    [ 0 '()]
    [ n (if (box? (car l))
                   (append (list (car (unbox (car l)))) (take-list-n (cdr (unbox (car l))) (- n 1)))
                   (append (list (car l)) (take-list-n (cdr l) (- n 1))))]
    ))

;;test de uso
(test (take-list-n (clycic-list l1) 8) '(1 2 3 1 2 3 1 2))
(test (list->string (take-list-n (clycic-list l2) 11)) "dcc dcc dcc")


;; c) Funcion que recibe la list que se quiere hacer cíclica, pero en vez de usar cajas usa mcons
;; o pares mutables

;;list-to-mcons :: ListOf[A] -> MPair(A,MPair(A..))
;;Transforma la lista a un pares mutables concatenados que al final en vez de tener '() contiene un #f
;;que es el placeholder para hacer la recursion 
(define (list-to-mcons l)
  (match l
    [(list x) #:when (not (list? x)) (mcons x #f)]
    [(list y rest ...) #:when (not (empty? rest)) (mcons y (list-to-mcons rest))]
    ))


;;set-placeholder ::  MPair(A,MPair(A..)) -> Void
;;busca el placeholder (#f) y lo setea para que sea toda la lista completa denuevo (genera el ciclo)
;; y debe hacerlo con una copia del original ya que de lo contrario, haria el ciclo solo con el ultimo elemento
(define (set-placeholder pares)
  (define (aux-set-placeholder pares original)
    (match pares
      [(mcons x #f) (set-mcdr! pares original)]
      [(mcons x rest) (aux-set-placeholder rest original)]))
  (aux-set-placeholder pares pares)
  )

;; clycic-list-to-mcons :: ListOf[A] -> MPair(A,MPair(A..))
(define (clycic-list-to-mcons l)
  ;;transformamos la lista a un par mutable
  (define par-mutable (list-to-mcons l))
  ;;cambiamos el placeholder por el llamado a si mismo, generamos el ciclo
  (set-placeholder par-mutable)
  par-mutable)

;;d) take-mcons :: MPair(A,MPair(A..)) Int -> ListOf[A]
;;toma el par mutable con la autorreferencia y un n y devuelve la cantidad de elmentos n en una lista
(define (take-mcons-n mpar n)
  (match n
    [0 '()]
    [ x #:when (> x 0) (append (list (mcar mpar)) (take-mcons-n (mcdr mpar) (- n 1) )) ]
    ))


;;tests
(test (take-mcons-n (clycic-list-to-mcons (list 1 2 3 4 5 6)) 12) '(1 2 3 4 5 6 1 2 3 4 5 6))
(test (list->string (take-mcons-n (clycic-list-to-mcons l2) 11)) "dcc dcc dcc")




;;------------------P2---------------------------
;; cyclic-env2 :: id expr id expr env -> env
(define (cyclic-env2 id1 fun-expr1 id2 fun-expr2 env)
  (def fun-val-holder1 (box 'dummy))
  (def fun-val-holder2 (box 'dummy))
  (def new-env (box-extend-env id2 fun-val-holder2
                               (box-extend-env id1 fun-val-holder1 env)))
  (def fun-val1 (interp fun-expr1 new-env))
  (def fun-val2 (interp fun-expr2 new-env))
  (begin
    (set-box! fun-val-holder1 fun-val1)
    (set-box! fun-val-holder2 fun-val2)   
    new-env))


(test
 (run '(rec2 (even (fun (n) (if0 n 1 (odd  (- n 1)))))
             (odd  (fun (n) (if0 n 0 (even (- n 1)))))
             (even 7)))
 0)

(test
 (run '(rec2 (even (fun (n) (if0 n 1 (odd  (- n 1)))))
             (odd  (fun (n) (if0 n 0 (even (- n 1)))))
             (odd 7)))
 1)


;;---------------P3----------------
#|
a)

Llamado por la cola (o tail call ocurre cuando la última
instrucción de una función es una llamada a alguna otra
función, si el llamado es recursivo entonces hablamos de
recursión por la cola (o tail recursion).

Por ejemplo:

-- TAIL CALL --         -- TAIL REC --              -- None --
(define (foo x)         (define (bar x)             (define (uwu x)
	...                     ...                         ... 
	(bar y))                (bar (-1 z))                (* x (uwu (-1 z)))

b)

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

;-----------------P4-----------------------
;; par? :: Int -> Boolean
;;retorna #t si un numero es par y #f si es impar
(define (par? n)
    (define (parTR? n acc)
      (cond
        ((= n 0) acc)
        ((= n 1) (not acc))
        (else (parTR? (- n 1) (not acc)))))
    (parTR? n #t))

(test (par? 5) #f)
(test (par? 4) #t)
(test (par? 1000000) #t)

;; interp :: Expr (Num -> Num) ->  Num
(define (interp expr cont)  
	(match expr    
		[(num n) (cont n)]    
		[(add l r) 
		 (interp l (λ (vl)
                             (interp r (λ (vr)
                                         (cont (+ vl vr))))))]))


(test (interp (add (num 1) (num 2)) identity) 3)
(test (interp (add (add (num 1) (num 2)) (num 2)) identity) 5)





