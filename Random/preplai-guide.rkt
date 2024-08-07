#lang slideshow

; declaring to variables locally and returning the maximum one
(let ([x 4]
      [y 5])
  (max x y))

; sums a and b
(define (sum a b)
  (+ a b))

; chooses random element between x and y
(define (pick-random x y)
  (define rand (random))
  (if (< rand 1/2)
      x y))

; sums 1 to each element on pair p
(define (pair-add1 p)
  (define x (+ (car p) 1))
  (define y (+ (cdr p) 1))
  (cons x y))

; returns random element from l
(define (list-pick-random l)
  (define rand (random (length l)))
  (list-ref l rand))

; switches random value on v with given x
(define (vector-set-random! v x)
  (define rand (random (vector-length v)))
  (vector-set! v rand x))
