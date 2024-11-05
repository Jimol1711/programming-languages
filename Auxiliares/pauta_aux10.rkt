#lang play


;;---------------P1--------

#|
a). Al ejecutar en el intérprete de Racket una función como add1, este responde con un
#<procedure:add1> (la clausura). Por otro lado, al ejecutar and, se retorna un error
de "bad syntax", esto se debe a que and es una macro.
Las funciones en Racket tienen evaluación temprana, es decir, se evalúan ambas
expresiones antes de aplicar la función, en cambio, and, or etc, evalúan lo necesario,
implementando así atajos de evaluación.
Como and corresponde a una macro, significa que es mero azúcar sintáctico de una
expresión más compleja de escribir, en la que podrían pasarse funciones lambdas que
al ser llamadas ejecuten la instrucción, y no antes.
|#

;;b)
(defmac (for <id> in < list > do <body>)
  #:keywords in do
  ( let ( [ result (map (λ (<id>) <body>) <list>)])
     (void)))

;;c)
(defmac ( right-to-left f a b)
  ( let ( [b1 b] [a1 a])
     ( f a1 b1)))

; ; Produce outputs diferentes
( right-to-left + (begin (print "izq") 3) (begin (print "der") 5))

;;d)
(define i (box 0))

(define (while-def cond body)
  (if cond
      (begin body
             (while-def cond body))
      (void)))


;;cae en un bucle infinito luego de imprimir 0
;; (while-def (< (unbox i) 5) (begin (printf "~a" (unbox i))
;;                               (set-box! i (+ (unbox i) 1))
;;                               ))

(defmac (while-mac cond body)
  (letrec ([iter (λ ()
                   (if cond
                        (begin body
                               (iter))
                       (void)))])
   (iter)))


(while-mac (< (unbox i) 5) (begin (printf "~a" (unbox i))
                              (set-box! i (+ (unbox i) 1))
                              ))



;;-------------P2---------
#|
a) Obtenemos 10, dado que se recibe la caja b y se cambia su interior por el doble.

b) Al usar #:captures, se rompe la higiene para la variable "a", es decir, no le cambiará el nombre internamente, de modo que si o
si está esperando obtener una variable fuera (o antes) de la llamada a la macro que efectivamente se llama 'a, del modo que si no se
encuentra el programa fallará.

c) Acá pasa que capturamos "a", es decir, si o si necesitamos antes de llamar a la macro, una variable con este nombre, lo cual se
tiene, pero ahora el seteo se le hace a la caja b, pero la caja b no es argumento de la macro, como es que no falla? pasa que las
macros se expanden, y si por suerte hay una variable llamada "b" que es una caja, entonces no habrá problema, pero no es muy buena
práctica. Además notar que usamos (unbox a) pero el argumento que recibimos es un numero 1, es decir, en una función normal de Racket
estaríamos haciendo (unbox 1), pero como esto es una macro y capturamos "a", si o si se ocupa la variable "a" que está definida antes del
llamado.

Por lo tanto el resultado es cambiar la caja b con un valor de (* (unbox (box 5) 2), es decir, ahora b es (box 10) y da lo mismo el argumento
que le demos a (DOUBLE ) ya que no se ocupa.
|#



;;---------------P3-------
(define profile (make-hash))
; ; La macro define-profile cuenta cuantas veces es llamada una funcion
(defmac ( define-profile (fname arg) body ... )
; ; Llamamos a define internamente
  (define (fname arg) (begin
                      ; ; Pero antes de hacer cualquier calculo , aumentamos el contador
                      ( hash-set! profile 'fname (+ 1 (hash-ref profile 'fname 0)))
                      body ... )))

(define-profile ( fact n)
  (if (zero? n)
      1
      (* n ( fact (- n 1)))))


(define-profile (even n)
  ( if (zero? n)
       #t
       (odd (- n 1))))


( define-profile (odd n)
   ( if (zero? n)
        #f
        (even (- n 1))))


(even ( fact 10))
(hash-map profile (λ (x y) (printf "~a: ~a~n" x y)))

