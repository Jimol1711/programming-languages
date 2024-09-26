#lang play

(let* ([b (mcons 4 (mcons 5 'dummy))]
       [c (mcdr b)]
       [d (mcons 1 b)])
  (begin (set-mcdr! c b) d))

(let ([fact box 'dummy])
  (let ([fact-fun (λ (n) (if (zero? n)
                          1
                          (* n ((unbox fact) (- n 1)))))])
  (begin (set-box! fact fact-fun)
  ((unbox fact) 5)))); ta mal