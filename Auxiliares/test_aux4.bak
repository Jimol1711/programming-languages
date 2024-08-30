#lang play

(require "aux_4.rkt")

;;tests booleanos
(test (parse #t) (bool #t))
(test (parse #f) (bool #f))
(test (parse '{not #f}) (neg (bool #f)))
(test (parse '{or #t #f}) (lor (bool #t) (bool #f)))
(test (parse '{and #t #f}) (land (bool #t) (bool #f)))
(test (parse '{or {and #t #f} {not #f}}) (lor (land (bool #t) (bool #f)) (neg (bool #f))))

;;tests operaciones aritmeticas
(test (parse '1) (num 1))
(test (parse '2) (num 2))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{- {+ 1 2} 1}) (sub (add (num 1) (num 2)) (num 1)) )
(test (parse '{* 1 2}) (mult (num 1) (num 2)))
(test (parse '{< 1 2}) (less (num 1) (num 2)))
(test (parse '{* 1 {/ 4 2}}) (mult (num 1) (div (num 4) (num 2))))

;;combinacion con el <
(test (parse '{and #t {< 1 2}}) (land (bool #t) (less (num 1) (num 2))))
;; if con pred
(test (parse '{if {and #f #t} 1 2}) (ifp (land (bool #f) (bool #t)) (num 1) (num 2)))

;----typecheck

;;valores basicos
(test (typecheck (parse '1)) (Num))
(test (typecheck (parse '#f)) (Bool))
;;and
(test (typecheck (parse '{and #t #f})) (Bool))
(test/exn (typecheck (parse '{and #t 1})) "Static type error:")
;;or
(test (typecheck (parse '{not #t})) (Bool))
(test/exn (typecheck (parse '{not 1})) "Static type error:")
;;suma
(test (typecheck (parse '(+ 1 2))) (Num))
(test/exn (typecheck (parse '(+ 1 #t))) "Static type error:")
;;resta
(test (typecheck (parse '(- 1 2))) (Num))
(test/exn (typecheck (parse '(- 1 #f))) "Static type error:")
;;mul
(test (typecheck (parse '(* 1 2))) (Num))
(test/exn (typecheck (parse '(* 1 #t))) "Static type error:")
;;div
(test (typecheck (parse '(/ 1 2))) (Num))
(test/exn (typecheck (parse '(/ 1 #f))) "Static type error:")
;;less
(test (typecheck (parse '(< 1 2))) (Bool))
(test/exn (typecheck (parse '(< 1 #f)) ) "Static type error:")
;;if
(test (typecheck (parse '(if (< 1 2) 1 2))) (Num))
(test/exn (typecheck (parse '(if (+ 1 1) 1 2))) "Static type error:")
(test/exn (typecheck (parse '(if (and #t #f) 1 #f))) "Static type error:")



;---run
;;basicos
(test (run '1) 1)
(test (run '#t) #t)
;;aritmeticas
;;suma
(test (run '{+ 1 2}) 3)
(test/exn (run '{+ 1 #t}) "Static type error:")
;;resta
(test (run '{- 1 2}) -1)
(test/exn (run '{- 1 #t}) "Static type error:")
;;mult
(test (run '{* 1 2}) 2)
(test/exn (run '{* 1 #t}) "Static type error:")
;;div
(test (run '{/ 4 2}) 2)
(test/exn (run '{/ 1 #t}) "Static type error:")
(test/exn (run '{/ 1 0}) "Division by zero")
;;less
(test (run '{< 1 2}) #t)
(test (run '{< 2 1}) #f)
(test/exn (run '{< 1 #t}) "Static type error:")
;;if
(test (run '{if #t 1 2}) 1)
(test (run '{if #f #t #f}) #f)
(test (run '{if {< 1 2} {and #t #f} #f}) #f)
;;ramas de diferentes tipos
(test/exn (run '{if {< 1 2} #t 1}) "Static type error:")
(test/exn (run '{if 2 #t #f}) "Static type error:")
;; ----operacioens logicas
;;not
(test (run '{not #t}) #f)
(test (run '{not {and #t #f}}) #t)
(test/exn (run '{not 1}) "Static type error:")
;;and
(test (run '{and #t #f}) #f)
(test (run '{and #t {and #t #f}}) #f)
(test/exn (run '{and 1 {and #t #f}}) "Static type error:")
;;or
(test (run '{or #t #f}) #t)
(test (run '{or #f {and #t #f}}) #f)
(test/exn (run '{or 1 {and #t #f}}) "Static type error:")
