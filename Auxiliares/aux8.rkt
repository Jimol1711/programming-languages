#lang play

; (require "base_aux8.rkt")

; P1.
; a)
;; cyclic-list :: ListOf[A] -> ListOf[A/Box(L)]
;; returns list as a cyclic version of it
(define (cyclic-list the-list)
  (define placeholder (box #f))
  (define the-cyclic-list (append the-list (list placeholder)))
  (set-box! placeholder the-cyclic-list)
  the-cyclic-list)

; b)
;; take-list-n :: ListOf[A/L] Int -> ListOf[A]
(define (take-list-n l n)
  (match n
    [0 '()]
    [_ (if (box? (car l))
           (append (list (car (unbox (car l)))) (take-list-n (cdr (unbox (car l))) (- n 1)))
           (append (list (car l)) (take-list-n (cdr l))))])) ; TERMINAR

(test (take-list-n (cyclic-list (list 1 2 3) 6)) (list 1 2 3 1 2 3))

; c)
;; cyclic-list-to-mcons :: ListOf[A] -> Mcons(A ...)

(define (interp a b) (void)) ; interprete
(define box-extend-env (void)) ; debería ir aEnv

; P2.
(define (cyclic-env2 id1 fun-expr1 id2 fun-expr2 env)
  (define p1 (box #f))
  (define p2 (box #f))
  (define new-env (box-extend-env id1 p1
                                  (box-extend-env id2 p2 env)))
  (define fun-val1 (interp fun-expr1 new-env))
  (define fun-val2 (interp fun-expr2 new-env))
  (set-box! p1 fun-val1)
  (set-box! p2 fun-val2)
  new-env)

; P3.
; a)
; recursión por la cola es un caso particular de llamado por la cola, donde la función llamada en el retorno de la función es la misma función (recursión)

; b)
; no TCO ni TRO
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

; TRO
(define (factTRO n)
  (define (fact-auxTRO n acc)
    (if (zero? n)
        acc
        (fact-auxTRO (- n 1) (* n acc))))
  (fact-auxTRO n 1))

; TCO and not directly TRO
; el even y el odd del ejemplo

; P4.
(define (par? n)
  (define (parTRO n acc)
  (cond
    [(= n 0) #t]
    [else (parTRO (- n 1) (not acc))]))
  (parTRO n #t))

; transformación
