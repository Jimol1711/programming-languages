#lang play

; P1.
; 1. Porque Racket es un lenguaje eager, y evaluaría los argumentos altiro. Por esto, el and nos podría dar algo erroneo, como un error, sin implementar el cortocircuito en caso
; de que encuentre un false antes.
; 2.
(defmac (for <id> in <list> do <body>)
  #:keywords in do
  (let ([result (map (λ (<id>) <body>) <list>)])
    (void)))

; 3.
(defmac (right-to-left f a b)
  (f b a))

(- (begin (display "left\n") 1) (begin (display "right") 2)) ; daría -1
(right-to-left - (begin (display "left") 1) (begin (display "right\n") 2)) ; daría 1

; P2.
; Básicamente profile será un hashmap que mapea cada id de la función a su conteo. Se hace un begin en la macro donde antes se aumenta el contador el map y luego se llama la función.
(define profile (make-hash))
; ; La macro define-profile cuenta cuantas veces es llamada una funcion
(defmac (define-profile (fname arg) body ...)
; ; Llamamos a define internamente
  (define (fname arg) (begin
                        ;; Pero antes de hacer cualquier calculo , aumentamos el contador
                        (hash-set! profile 'fname (+ 1 (hash-ref profile 'fname 0)))
                        body ...)))

(begin
  (define-profile (fact n)
     (if (zero? n)
          1
          (* n (fact (- n 1)))))
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

; P3
; 1. se ocupa una nueva ubicación para el nuevo valor. Esto produce que el cambio no se vea
; reflejado de las expresiones que se ejecuten más adelante

; 2. Para conservar el alcance estático, pero propagar dinámicamente las mutaciones en los programas

; P4.
; 1. F, no tiene nada que ver
; 2. Hacer un programa que use if0, que cambie algo de la rama true y dé 0, que se meta a la rama true y ver que algo del store no cambia (Porque usa sto en vez de c-sto.

; P5. NO VIMOS ESTA MATERIA 