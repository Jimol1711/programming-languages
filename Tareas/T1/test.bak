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

;; tests for from-to
(test (from-to 0 3) '(0 1 2))
(test (from-to 3 0) '(0 1 2))
(test (from-to 3 7) '(3 4 5 6))
(test (from-to -7 3) '(-7 -6 -5 -4 -3 -2 -1 0 1 2))
(test (from-to 3 -1) '(-1 0 1 2))

;; tests for mysterious-list
(test (mysterious-list 0) '(3.0))
(test (mysterious-list 1)'(3.0 3.166666666666667))
(test (mysterious-list 5) '(3.0 3.166666666666667 3.1333333333333337 3.1452380952380956 3.13968253968254 3.142712842712843))
(test (mysterious-list 15) '(3.0 3.166666666666667 3.1333333333333337 3.1452380952380956 3.13968253968254 3.142712842712843 3.140881340881341 3.142071817071817 3.141254823607765 3.141839618929402 3.141406718496502 3.1417360992606653 3.141479689004255 3.141683189207755 3.141518985595275 3.1416533941974265))
(test (mysterious-list 20) '(3.0 3.166666666666667 3.1333333333333337 3.1452380952380956 3.13968253968254 3.142712842712843 3.140881340881341 3.142071817071817 3.141254823607765 3.141839618929402 3.141406718496502 3.1417360992606653 3.141479689004255 3.141683189207755 3.141518985595275 3.1416533941974265 3.1415419859977822 3.141635356679388 3.1415563302845726 3.1416238066678384 3.1415657346585473))

;; tests for rac-to-cf
(test (rac-to-cf (+ 3 49/200)) (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
(test (rac-to-cf 5.333333333333333) (compound 5 1 (simple 3)))
(test (rac-to-cf 0) (simple 0))
(test (rac-to-cf 4.0) (simple 4))
(test (rac-to-cf 59/13) (compound 4 7 (simple 13)))