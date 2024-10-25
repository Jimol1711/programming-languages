#lang play

(define (my-time e)
  (let ([begin-time (current-milliseconds)])
    (begin
      e
      (- (current-milliseconds) begin-time))))

(define (my-time-thunk e-thunk)
  (let ([begin-time (current-milliseconds)])
    (begin
      (e-thunk)
      (- (current-milliseconds) begin-time))))

(defmac (my-time-macro e)
  (let ([begin-time (current-milliseconds)])
    (begin
      e
      (- (current-milliseconds) begin-time))))

(my-time-macro (expt 2 10000000))

(defmac (my-let-1 ([id e]) body)
  ((λ (id) body) e))

(my-let-1 ([x 5]) (* 3 x))

(defmac (my-let-n ([id e] ...) body)
  ((λ (id ...) body) e ...))

(my-let-n ([x 5] [y 3]) (* x y))

(defmac (check c then e1 else e2)
  #:keywords then else
  (if c e1 e2))

(defmac (my-or e1 e2)
  (let ([result e1])
    (if result
        result
        e2)))

(let ([result #t])
  (my-or #f result))
#|
(let ([result e1])
  (if result
      result
      e2))
|#




  