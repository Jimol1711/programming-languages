#lang slideshow

(let ([x 4]
      [y 5])
  (max x y))

(define (sum a b)
  (+ a b))

; elige elemento aleatorio entre x e y
(define (pick-random x y)
  (define rand (random))
  (if (< rand 1/2)
      x y))

; suma 1 a cada elemento del par p
(define (pair-add1 p)
  (define x (+ (car p) 1))
  (define y (+ (cdr p) 1))
  (cons x y))

; retorna elemento aleatorio de l
(define list-pick-random 1)
