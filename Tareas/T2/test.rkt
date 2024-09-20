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
(test (eval-or (list (tt) (ff))) (ttV))
(test (eval-or (list (ff) (tt) (tt))) (ttV))
(test (eval-or (list (ff) (ff) (ff))) (ffV))
(test (eval-or (list (ff) (ff) (ff) (tt))) (ttV))

;; eval-and
(test (eval-and (list (tt))) (ttV))
(test (eval-and (list (ff))) (ffV))
(test (eval-and (list (tt) (ff))) (ffV)) 
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
