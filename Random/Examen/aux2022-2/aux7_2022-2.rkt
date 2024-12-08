#lang play

; P1.
; a)
; Llamado por la cola: Llamar a la función en el retorno
; Recursión por la cola: Que el llamado de la función sea la misma función

; b)
; No TCO ni TRO
(define (fact n)
	(if (= 0 n)
		1
		(* n (fact (- n 1)))))

; TRO
(define (even n)
	(cond
          ([= n 0] #t)
          ([= n 1] #f)
          (else (even (- n 2))))) ; Se puede aplicar TRO ya que es recursiva por la cola

(define (odd n)
  (void))

; TCO but not TRO directly
(define (even-tco n)
	(if (= n 0)
		#t
		(odd (- n 1)))) ; No TRO ya que no es recursiva por la cola, pero si tiene un llamado por la cola

; P2.
; Haciendo la suma currificadamente con una lambda
;; <expr> ::= <num> 
;;          | (+ <num> <num>)
(deftype Expr
  (num n)
  (add l r))

;; interp :: Expr (Num -> Num) ->  Num
(define (interp expr cont)  
	(match expr    
		[(num n) (cont n)]    
		[(add l r) 
		 (interp l (λ (vl)
                             (interp r (λ (vr)
                                         (cont (+ vl vr))))))]))

(test (interp (add (num 1) (num 2)) identity) 3)
(test (interp (add (add (num 1) (num 2)) (num 2)) identity) 5)

; P3.
; a) Hay que compilarlo (que paja)
; b) usar un accum, restarle 1 a value y sumarle 2 a accum
; c) y d) compilar (no)

; P4.
; Hay una ventaja en el uso de memoria

; P5. y P6. Son evoluciones de programas y de envs y stores