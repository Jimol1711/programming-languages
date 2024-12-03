#lang play

(define (swap x z)
  (let ([tmp x])
    (begin
      (set! x z)
      (set! z tmp))))

(let ([x 0][z 1])
  (begin
    (swap x z)
    x))