#lang slideshow
(define c (circle 10))
(define r (rectangle 10 20))

(define (square n)
  ; A semi-colon starts a line comment.
  ; The expression below is the function body.
  (filled-rectangle n n))

(display "Hello World!")

(define salutation (list-ref '("Hi" "Hello") (random 2)))