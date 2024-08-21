#lang play
(require "T1.rkt")

(print-only-errors #t)

; tests for eval
(test (eval (compound 3 1 (compound 4 1 (compound 12 1 (simple 4))))) 649/200)
(test (eval (simple 0)) 0)
(test (eval (compound 5 1 (simple 23))) 116/23)