#lang play

(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

(define (tail-fact accum n)
  (if (zero? n)
      accum
      (tail-fact (* accum n) (- n 1))))

(define (even n)
  (match n
    [0 #t]
    [1 #f]
    [else (odd (- n 1))]))

(define (odd n)
  (match n
    [0 #f]
    [1 #t]
    [else (even (- n 1))]))


;; Trampoline
(deftype TailCall
  (call rest)
  (done val))

(define (even-tr n)
  (match n
    [0 (done #t)]
    [1 (done #f)]
    [else (call (lambda () (odd-tr (- n 1))))]))

(define (odd-tr n)
  (match n
    [0 (done #f)]
    [1 (done #t)]
    [else (call (lambda () (even-tr (- n 1))))]))

(define (result tc)
  (match tc
    [(done val) val]
    [(call rest) (result (rest))]))

    

