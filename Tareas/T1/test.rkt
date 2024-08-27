#lang play
(require "T1.rkt")

(print-only-errors #t)

;; tests for eval
(test (eval (compound 3 1 (compound 4 1 (compound 12 1 (simple 4))))) 649/200)
(test (eval (simple 0)) 0)
(test (eval (compound 5 1 (simple 23))) 116/23)
(test (eval (compound 1 8 (compound 5 1 (simple 12)))) 157/61)
(test/exn (eval (compound 5 1 (simple 0))) "zero division")

;; tests for degree
(test (degree (simple 1)) 0)
(test (degree (compound 3 1 (compound 4 1 (compound 12 1 (simple 4))))) 3)
(test (degree (compound -2 5 (compound 5 -7 (compound 7 0 (simple 9))))) 3)
(test (degree (compound 0 2 (compound 5 0 (compound 1 -6 (compound 2 5 (compound -5 7 (compound 7 8 (simple 9)))))))) 6)
(test/exn (eval (compound 5 1 (simple 0))) "zero division")

;; tests for fold-cfraction
(test ((fold-cfraction (lambda (value) value) (lambda (a b d) (+ a (/ b d)))) (simple 5)) 5)
(test ((fold-cfraction (lambda (value) value) (lambda (a b d) (+ a (/ b d)))) (compound 1 2 (simple 5))) 7/5)
(test ((fold-cfraction (lambda (value) value) (lambda (a b d) (+ a (/ b d)))) (compound 5 -7 (compound -1 23 (simple 8)))) 19/15)
(test ((fold-cfraction (lambda (value) value) (lambda (a b d) (+ a (/ b d)))) (compound -34 6 (compound 7 0 (compound -9 2 (simple 1))))) -232/7)
(test/exn ((fold-cfraction (lambda (value) value) (lambda (a b d) (if (= d 0)
                                                                  (error "zero division")
                                                                  (+ a (/ b d))))) (compound 0 0 (simple 0))) "zero division")

;; tests for eval2 (same as eval)
(test (eval2 (compound 3 1 (compound 4 1 (compound 12 1 (simple 4))))) 649/200)
(test (eval2 (simple 0)) 0)
(test (eval2 (compound 5 1 (simple 23))) 116/23)
(test (eval2 (compound 1 8 (compound 5 1 (simple 12)))) 157/61)
(test/exn (eval2 (compound 5 1 (simple 0))) "zero division")

;; tests for degree2 (same as degree)
(test (degree2 (simple 1)) 0)
(test (degree2 (compound 3 1 (compound 4 1 (compound 12 1 (simple 4))))) 3)
(test (degree2 (compound -2 5 (compound 5 -7 (compound 7 0 (simple 9))))) 3)
(test (degree2 (compound 0 2 (compound 5 0 (compound 1 -6 (compound 2 5 (compound -5 7 (compound 7 8 (simple 9)))))))) 6)
(test (degree2 (compound -4 -8 (compound 0 2 (compound 5 0 (compound 1 -6 (compound 2 5 (compound -5 7 (compound 7 8 (simple 9))))))))) 7)

;; tests for mysterious-cf
(test/exn (mysterious-cf -1) "Error: argumento negativo")
(test (mysterious-cf 0) (simple 6))
(test (mysterious-cf 2) (compound 6 (sqr 1) (compound 6 (sqr 3)(simple 6))))
(test (mysterious-cf 5) (compound 6 (sqr 1) (compound 6 (sqr 3)(compound 6 (sqr 5)(compound 6 (sqr 7)(compound 6 (sqr 9)(simple 6)))))))
(test (mysterious-cf 8) (compound 6 (sqr 1) (compound 6 (sqr 3)(compound 6 (sqr 5)(compound 6 (sqr 7)(compound 6 (sqr 9)(compound 6 (sqr 11) (compound 6 (sqr 13)(compound 6 (sqr 15)(simple 6))))))))))