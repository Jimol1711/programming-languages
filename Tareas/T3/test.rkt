#lang play
(require "T3.rkt")

(print-only-errors #t)

;; parse
(test (parse '1) (num 1))
(test (parse '(+ 1 2)) (add (num 1) (num 2)))
(test (parse '(nil)) (nil))
(test (parse '(cons 1 2)) (conz (num 1) (num 2)))
(test (parse '(list 1 2 3)) (conz (num 1) (conz (num 2) (conz (num 3) (nil)))))
(test (parse '(fun x x)) (fun (varP 'x) (id 'x)))
(test (parse '(fun (cons x xs) x)) (fun (conzP (varP 'x) (varP 'xs)) (id 'x)))
(test (parse '(f x)) (app (id 'f) (id 'x)))

;; match
(test (parse '(match 2 [1 1] [x 3])) (pmatch (num 2) (list (cons (numP 1) (num 1)) (cons (varP 'x) (num 3)))))
(test (parse '(match (list 1 2) [(list 1 2) 42] [y (+ y 5)])) (pmatch (conz (num 1) (conz (num 2) (nil)))
                                                                              (list (cons (conzP (numP 1) (conzP (numP 2) (nilP))) (num 42))
                                                                                    (cons (varP 'y) (add (id 'y) (num 5))))))
(test/exn (parse '(match (list 1 2 3))) "SyntaxError: match expression must have at least one case")
(test/exn (parse '(match 2 [1 1] [x 3] [invalid-clause])) "SyntaxError: invalid match clause format")

;; parse-pattern
(test (parse-pattern '3) (numP 3))
(test (parse-pattern '(nil)) (nilP))
(test (parse-pattern 'x) (varP 'x))
(test (parse-pattern '(cons 1 x)) (conzP (numP 1) (varP 'x)))
(test (parse-pattern '(list 1 x 3)) (conzP (numP 1) (conzP (varP 'x) (conzP (numP 3) (nilP)))))

;; generate-substs
(test (generate-substs (numP 3) (numV 3)) (success '()))
(test (generate-substs (varP 'x) (numV 3)) (success (list (cons 'x (numV 3)))))
(test (generate-substs (nilP) (nilV)) (success '()))
(test (generate-substs (conzP (numP 1) (varP 'y)) (consV (numV 1) (numV 2))) (success (list (cons 'y (numV 2)))))
(test (generate-substs (conzP (varP 'x) (varP 'y)) (consV (numV 5) (numV 10))) (success (list (cons 'x (numV 5)) (cons 'y (numV 10)))))
(test (generate-substs (conzP (numP 1) (nilP)) (consV (numV 1) (nilV))) (success '()))
(test (generate-substs (conzP (conzP (varP 'x) (numP 2)) (nilP)) (consV (consV (numV 1) (numV 2)) (nilV))) (success (list (cons 'x (numV 1)))))

;; Error cases
(test (generate-substs (numP 3) (numV 4)) (failure "MatchError: given number does not match pattern"))
(test (generate-substs (numP 3) (nilV)) (failure "MatchError: expected a number"))
(test (generate-substs (nilP) (numV 5)) (failure "MatchError: expected nil"))
(test (generate-substs (conzP (numP 1) (nilP)) (consV (numV 1) (numV 2))) (failure "MatchError: expected nil"))
(test (generate-substs (conzP (varP 'x) (nilP)) (numV 3)) (failure "MatchError: expected a cons constructor"))
(test (generate-substs (conzP (nilP) (nilP)) (numV 4)) (failure "MatchError: expected a cons constructor"))

;; interp
;; basics
(test (interp (num 5) empty-env) (numV 5))
(test (interp (add (num 2) (num 3)) empty-env) (numV 5))
(test/exn (interp (add (nil) (num 3)) empty-env) "TypeError: expected a number")
(test (interp (nil) empty-env) (nilV))
(test (interp (conz (num 1) (nil)) empty-env) (consV (numV 1) (nilV)))

;; id
(test (interp (id 'x) (extend-env 'x (numV 10) empty-env)) (numV 10))
(test/exn (interp (id 'y) empty-env) "LookupError: variable ~a not found 'y")

;; fun and app
(test (interp (fun (varP 'x) (add (id 'x) (num 2))) empty-env) (closureV (varP 'x) (add (id 'x) (num 2)) empty-env))
(test (interp (app (fun (varP 'x) (add (id 'x) (num 2))) (num 3)) empty-env) (numV 5))
(test (interp (app (fun (conzP (varP 'x) (varP 'y)) (id 'x)) (conz (num 1) (num 2))) empty-env) (numV 1))

;; pmatch
(test (interp (pmatch (num 5) (list (cons (numP 5) (num 10)) (cons (numP 6) (num 20)))) empty-env) (numV 10))
(test (interp (pmatch (nil) (list (cons (nilP) (num 0)) (cons (numP 1) (num 1)))) empty-env) (numV 0))
(test (interp (pmatch (num 5) (list (cons (varP 'x) (add (id 'x) (num 10))))) empty-env) (numV 15))
(test (interp (pmatch (conz (num 1) (num 2)) (list (cons (conzP (numP 1) (numP 3)) (num 99)) (cons (conzP (numP 1) (numP 2)) (num 42)))) empty-env) (numV 42))

;; Errors
(test/exn (interp (app (fun (numP 3) (num 0)) (num 4)) empty-env) "MatchError: given number does not match pattern")
(test/exn (interp (app (fun (numP 2) (num 1)) (nil)) empty-env) "MatchError: expected a number")
(test/exn (interp (app (fun (nilP) (num 0)) (num 3)) empty-env) "MatchError: expected nil")
(test/exn (interp (app (fun (conzP (varP 'x) (varP 'y)) (id 'x)) (num 5)) empty-env) "MatchError: expected a cons constructor")
(test/exn (interp (pmatch (num 5) (list (cons (numP 6) (num 10)) (cons (nilP) (num 0)))) empty-env) "MatchError: expression does not match any pattern" )
