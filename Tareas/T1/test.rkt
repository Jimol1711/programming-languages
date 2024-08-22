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