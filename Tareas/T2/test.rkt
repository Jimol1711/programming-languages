#lang play
(require "T2.rkt")

(print-only-errors #t)

;; tests for all top-level functions on T2.rkt

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

;; parse-prop
(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))
(test (parse-prop '(not true)) (p-not (tt)))
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))))
(test (parse-prop '(or true false)) (p-or (list (tt) (ff))))
(test (parse-prop '(false where [x true])) (p-where (ff) 'x (tt)))
(test (parse-prop '(x where [x true])) (p-where (p-id 'x) 'x (tt)))
(test (parse-prop '((and true false) where [x true])) (p-where (p-and (list (tt) (ff))) 'x (tt)))
; errors
(test/exn (parse-prop '(and true)) "parse-prop: and expects at least two operands")
(test/exn (parse-prop '(or false)) "parse-prop: or expects at least two operands")

;; from-Pvalue
(test (from-Pvalue (ttV)) (tt))
(test (from-Pvalue (ffV)) (ff))
(test (from-Pvalue (andV (list (ttV) (ffV) (ttV)))) (p-and (list (tt) (ff) (tt))))
(test (from-Pvalue (orV (list (ttV) (ffV) (ffV) (ffV)))) (p-or (list (tt) (ff) (ff) (ff))))
(test (from-Pvalue (idV 'x)) (p-id 'x))
(test (from-Pvalue (whereV (ttV) 'x (ffV))) (p-where (tt) 'x (ff)))
(test (from-Pvalue (whereV (idV 'y) 'x (ffV))) (p-where (p-id 'y) 'x (ff)))
; errors
(test/exn (from-Pvalue (andV (list (ffV)))) "from-Pvalue: andV expects at least two operands")
(test/exn (from-Pvalue (orV (list (ttV)))) "from-Pvalue: orV expects at least two operands")

;; p-subst
(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))
(test (p-subst (p-and (list (p-id 'x) (p-id 'y))) 'x (tt)) (p-and (list (tt) (p-id 'y))))
(test (p-subst (p-or (list (p-id 'x) (p-id 'x))) 'x (tt)) (p-or (list (tt) (tt))))
(test (p-subst (p-not (p-id 'x)) 'x (ff)) (p-not (ff)))

;; eval-or
(test (eval-or (list (tt))) (ttV))
(test (eval-or (list (ff))) (ffV))

;; added a value not from the language to test short circuit behaviour
(test (eval-or (list (ff) (tt) 1)) (ttV))
(test (eval-or (list (ff) (tt) (tt))) (ttV))
(test (eval-or (list (ff) (ff) (ff))) (ffV))
(test (eval-or (list (ff) (ff) (ff) (tt))) (ttV))

;; eval-and
(test (eval-and (list (tt))) (ttV))
(test (eval-and (list (ff))) (ffV))

;; added a value not from the language to test short circuit behaviour
(test (eval-and (list (tt) (ff) 1)) (ffV)) 
(test (eval-and (list (ff) (tt) (tt))) (ffV))
(test (eval-and (list (tt) (tt) (tt))) (ttV))
(test (eval-and (list (tt) (tt) (tt) (ff))) (ffV))

;; p-eval
;; boolean literals
(test (p-eval (tt)) (ttV))
(test (p-eval (ff)) (ffV))

;; not
(test (p-eval (p-not (tt))) (ffV))
(test (p-eval (p-not (ff))) (ttV))

;; and
(test (p-eval (p-and (list (tt) (ff)))) (ffV))
(test (p-eval (p-and (list (tt) (tt)))) (ttV))
(test (p-eval (p-and (list (ff) (tt)))) (ffV))

;; cases with other expressions inside
(test (p-eval (p-and (list (tt) (p-and (list (tt) (tt)))))) (ttV))
(test (p-eval (p-and (list (p-where (p-id 'x) 'x (tt)) (p-and (list (tt) (tt)))))) (ttV))

;; or
(test (p-eval (p-or (list (ff) (tt)))) (ttV))
(test (p-eval (p-or (list (ff) (ff)))) (ffV))
(test (p-eval (p-or (list (tt) (ff)))) (ttV))

;; cases with other expressions inside
(test (p-eval (p-or (list (p-or (list (tt) (ff))) (ff)))) (ttV))
(test (p-eval (p-or (list (p-where (p-id 'x) 'x (ff)) (p-and (list (ff) (tt)))))) (ffV))

;; where
(test (p-eval (p-where (p-and (list (p-id 'x) (tt))) 'x (tt))) (ttV))
(test (p-eval (p-where (p-or (list (p-id 'x) (ff))) 'x (ff))) (ffV))

;; unbound identifier errors
(test/exn (p-eval (p-id 'x)) "unbound identifier: x")
(test/exn (p-eval (p-where (p-id 'x) 'y (tt))) "unbound identifier: x")

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;; parse

;; real
(test (parse '1) (real 1))
(test (parse '2) (real 2))

;; imaginary
(test (parse '(1 i)) (imaginary 1))
(test (parse '(3 i)) (imaginary 3))

;; add & sub
(test (parse '(+ 1 (2 i))) (add (real 1) (imaginary 2)))
(test (parse '(- 1 2)) (sub (real 1) (real 2)))
(test (parse '(+ 1 2)) (add (real 1) (real 2)))
(test (parse '(- (8 i) 2)) (sub (imaginary 8) (real 2)))
(test (parse '(- (8 i) (+ 2 2))) (sub (imaginary 8) (add (real 2) (real 2))))

;; id & local definitions
(test (parse 'x) (id 'x))
(test (parse '(with [(z 2)] (- z (2 i)))) (with (list (cons 'z (real 2))) (sub (id 'z) (imaginary 2))))
(test (parse '(with [(x 1) (y 1)] (+ x y))) (with (list (cons 'x (real 1)) (cons 'y (real 1))) (add (id 'x) (id 'y))))

;; no bindings/invalid binding errors
(test/exn (parse '(with [] 1)) "parse: 'with' expects at least one definition")
(test/exn (parse '(with [(x)] 1)) "parse: invalid binding format in 'with'")

;; from-CValue
(test (from-CValue (compV 3 4)) (add (real 3) (imaginary 4)))
(test (from-CValue (compV 5 0)) (add (real 5) (imaginary 0)))
(test (from-CValue (compV 0 7)) (add (real 0) (imaginary 7)))
(test (from-CValue (compV 0 0)) (add (real 0) (imaginary 0)))
(test (from-CValue (compV -3 -7)) (add (real -3) (imaginary -7)))
(test (from-CValue (compV 12 -10)) (add (real 12) (imaginary -10)))

;; cmplx+
(test (cmplx+ (compV 0 0) (compV 3 0)) (compV 3 0))
(test (cmplx+ (compV 7 4) (compV 3 0)) (compV 10 4))
(test (cmplx+ (compV 0 0) (compV 0 0)) (compV 0 0))
(test (cmplx+ (compV -2 -1) (compV 3 1)) (compV 1 0))
(test (cmplx+ (compV -3 2) (compV 3 0)) (compV 0 2))
(test (cmplx+ (compV 1 2) (compV 3 4)) (compV 4 6))

;; cmplx-
(test (cmplx- (compV 0 0) (compV 3 0)) (compV -3 0))
(test (cmplx- (compV 7 4) (compV 3 0)) (compV 4 4))
(test (cmplx- (compV 0 0) (compV 0 0)) (compV 0 0))
(test (cmplx- (compV -2 -1) (compV 3 -1)) (compV -5 0))
(test (cmplx- (compV -3 2) (compV 3 0)) (compV -6 2))
(test (cmplx- (compV 1 2) (compV 3 4)) (compV -2 -2))

;; cmplx0?
(test (cmplx0? (compV 0 6)) #f)
(test (cmplx0? (compV 7 0)) #f)
(test (cmplx0? (cmplx- (compV 9 8) (compV 9 8))) #t)
(test (cmplx0? (cmplx+ (compV 23 1) (compV -23 -1))) #t)
(test (cmplx0? (cmplx- (compV 9 8) (compV 9 8545))) #f)
(test (cmplx0? (cmplx+ (compV 2444 1) (compV -23 -1))) #f)
(test (cmplx0? (compV 0 0)) #t)

;; remove-shadow
;; This is a function I created that removes a binding from a list of bindings, basically to remove shadowed bindings as to avoid their substitution
(test (remove-shadow 'x '((x (real 2)) (y (imaginary 1)) (z (add (id 'x) (id 'y))))) '((y (imaginary 1)) (z (add (id 'x) (id 'y)))))
(test (remove-shadow 'w '((x (real 2)) (y (imaginary 1)) (z (add (id 'x) (id 'y))))) '((x (real 2)) (y (imaginary 1)) (z (add (id 'x) (id 'y)))))
(test (remove-shadow 'x '((x (real 2)) (x (imaginary 3)) (y (sub (id 'x) (real 1)))))'((y (sub (id 'x) (real 1)))))
(test (remove-shadow 'x '()) '())
(test (remove-shadow 'z '((x (real 2)) (y (imaginary 1)) (z (add (real 3) (id 'y))) (z (sub (real 4) (id 'x))))) '((x (real 2)) (y (imaginary 1))))

;; subst
(test (subst (parse '1) 'x (real 2)) (real 1))
(test (subst (parse 'x) 'x (real 2)) (real 2))
(test (subst (parse '(+ 1 x)) 'x (real 2)) (add (real 1) (real 2)))
(test (subst (parse '(+ x x)) 'x (real 2)) (add (real 2) (real 2)))

;; no shadowing
(test (subst (parse '(with [(x 2) (y z)] (+ x z))) 'z (real 1)) (with (list (cons 'x (real 2)) (cons 'y (real 1))) (add (id 'x) (real 1))))

;; shadowing
(test (subst (parse '(with [(x 2) (y x)] (+ x x))) 'x (real 1)) (with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))))

(test (subst (parse '(with [(x 1) (y (+ x x))] (+ y x))) 'x (real 2)) (with (list (cons 'x (real 1)) (cons 'y (add (id 'x) (id 'x)))) (add (id 'y) (id 'x))))
(test/exn (subst (parse '(with [(x)] (+ x 1))) 'x (real 2)) "parse: invalid binding format in 'with'")



