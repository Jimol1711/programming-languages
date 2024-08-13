#lang play

; Aux 1
; P1
; (a) cons representa un par de objeto, list es una lista encadenada de cons. En notación de pares: (cons a (cons b '()))

; (b)
(list 1 (list 'x (cons 'y 'z)) #f)

; (c)
(define l (list '(a (b c)) '(d e f)))
(car (car (cdr l)))
; acceder a b
(car (car (cdr (car l))))
; acceder a f
(car (cdr (cdr (car (cdr l)))))

; (d)
(cons 'a 'b)
(cons 'a (cons 'b '()))
(cons 'a (cons (cons 'b 'c) (cons 'd '())))

; P2
; (a)
(define (sum-coins a b c)
  (+ (* a 50)(* b 100)(* c 500)))

(test (sum-coins 1 1 1) 650)
(test (sum-coins 1 0 0) 50)
(test (sum-coins 1 1 0) 150)
(test (sum-coins 0 1 1) 600)

; (b)
(define (pitatoria x y)
  (if (equal? x y)
   x
  (* (pitatoria x (- y 1)) y)))

(test (pitatoria 2 4) 24)
(test (pitatoria 0 4) 0)

; (c)
(define (lista_impares n)
  (cond [(< n 0) (error "error largo negativo")]
        [(zero? n) '()]
        [else (append (lista_impares (- n 1)) (list (- (* 2 n) 1)))]))

(test (lista_impares 0) '())
(test/exn (lista_impares -1) "error largo negativo")
(test (lista_impares 3) '(1 3 5))