#lang play
(require "T3.rkt")

(print-only-errors #t)

;; parse
(test (parse '1) (num 1))
(test (parse '+ 1 2) (add (num 1) (num 2)))
(test (parse '(nil)) (nil))
(test (parse '(cons 1 2)) (conz (num 1) (num 2)))
(test (parse '(list 1 2 3)) (conz (num 1) (conz (num 2) (conz (num 3) (nil)))))
(test (parse '(fun x x)) (fun (varP 'x) (id 'x)))
(test (parse '(fun (cons x xs) x)) (fun (conzP (varP 'x) (varP 'xs)) (id 'x)))

;; parse-pattern
(test (parse-pattern '3) (numP 3))
(test (parse-pattern '(nil)) (nilP))
(test (parse-pattern 'x) (varP 'x))
(test (parse-pattern '(cons 1 x)) (conzP (numP 1) (varP 'x)))
(test (parse-pattern '( list 1 x 3)) (conzP (numP 1) (conzP (varP 'x) (conzP (numP 3) (nilP)))))
