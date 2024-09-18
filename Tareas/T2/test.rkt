#lang play
(require "T2.rkt")

(print-only-errors #t)

; tests for parse-prop
(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))
(test (parse-prop '(not true)) (p-not (tt)))
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))))
(test (parse-prop '(or true false)) (p-or (list (tt) (ff))))
(test (parse-prop '(false where [x true])) (p-where (ff) 'x (tt)))
(test (parse-prop '(x where [x true])) (p-where (p-id 'x) 'x (tt)))
(test (parse-prop '((and true false) where [x true])) (p-where (p-and (list (tt) (ff))) 'x (tt)))
(test/exn (parse-prop '(and true)) "parse-prop: and expects at least two operands")
(test/exn (parse-prop '(or false)) "parse-prop: or expects at least two operands")

; tests for from-Pvalue
(test (from-Pvalue (ttV)) (tt))
(test (from-Pvalue (ffV)) (ff))
(test (from-Pvalue (andV (list (ttV) (ffV) (ttV)))) (p-and (list (tt) (ff) (tt))))
(test (from-Pvalue (orV (list (ttV) (ffV) (ffV) (ffV)))) (p-or (list (tt) (ff) (ff) (ff))))
(test (from-Pvalue (idV 'x)) (p-id 'x))
(test (from-Pvalue (whereV (ttV) 'x (ffV))) (p-where (tt) 'x (ff)))
(test (from-Pvalue (whereV (idV 'y) 'x (ffV))) (p-where (p-id 'y) 'x (ff)))
(test/exn (from-Pvalue (andV (list (ffV)))) "from-Pvalue: andV expects at least two operands")
(test/exn (from-Pvalue (orV (list (ttV)))) "from-Pvalue: orV expects at least two operands")