#lang play

; P1 1)
; Al ejecutar en el intérprete de Racket una función como add1,
;       este responde con un "#<procedure:add1>" (la clausura).
;       Por otro lado, al ejecutar and, se retorna un error
;       de "bad syntax", esto se debe a que and es una macro.
;       Las funciones en Racket tienen evaluación temprana, es decir, se evalúan ambas
;       expresiones antes de aplicar la función, en cambio, and, or etc, evalúan lo necesario,
;       implementando así atajos de evaluación.
;       Como and corresponde a una macro, significa que es mero azúcar sintáctico de una
;       expresión más compleja de escribir, en la que podrían pasarse funciones lambdas que
;       al ser llamadas ejecuten la instrucción, y no antes.)


; P1 2)

(defmac (for <id> in < list > do <body>)
  #:keywords in do
  (let ([result (map (λ (<id>) <body>) <list>)])
    (void)))

; P3 3)
(defmac (right-to-left f a b)
  (let ([b1 b] [a1 a])
     (f a1 b1)))

; ; Produce outputs diferentes
(define (f a b) (- a b))
(right-to-left f (begin (print "hola") 3) (begin (print "adios") 5))


; P2

(define profile (make-hash))
; ; La macro define-profile cuenta cuantas veces es llamada una funcion
(defmac ( define-profile (fname arg) body ... )
; ; Llamamos a define internamente
  (define (fname arg) (begin
                        ; ; Pero antes de hacer cualquier calculo , aumentamos el contador
                        ( hash-set! profile 'fname (+ 1 (hash-ref profile 'fname 0)))
                        body ... )))

(begin
  (define-profile (fact n)
     ( if (zero? n)
          1
          (* n ( fact (- n 1)))))
  (define-profile (even n)
    (if (zero? n)
        #t
        (odd (- n 1))))
  (define-profile (odd n)
    (if (zero? n)
        #f
        (even (- n 1))))
  (even (fact 10)))
(hash-map profile (λ (x y) (printf "~a: ~a~n" x y)))


; P3 1
; se ocupa una nueva ubicación para el nuevo valor. Esto produce que el cambio no se vea
; reflejado de las expresiones que se ejecuten más adelante

; P3 2
; Para conservar el alcance estático, pero propagar dinámicamente las mutaciones en los programas

; P4 1
; Falso, en los casos de call by name y call by need pueden haber problemas de implementacíón.
; si no se computa un argumento porque no se usa, pero producia una mutación el programa reducirá a algo incorrecto.

; P4 2
; ...

; P5 1
; Falso, si se utiliza una estrategia de evaluación tardía y call by reference se pueden producir errores.
; por la misma razon que en P4 1

; P5 2
; No, por ejemplo en casos de aridad variable como 'or o '+.

; P5 3
; (with {b {newbox 5}}
;    {seqn ((fun (x) (setbox x 42)) b) (openbox b)}





