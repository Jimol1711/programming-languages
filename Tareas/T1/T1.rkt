#lang play
(require math/flonum)

#|
Hizo Ud uso de la whiteboard policy:  (Indique SI/NO)
En caso que afirmativo, indique con quién y sobre qué ejercicio:
|#

;; Parte a)

#|
<CFraction> ::= (simple <value>)
             |  (compound <value> <value> <CFraction>)
|#
; Datatype to represent a finite continuous fraction to integer coefficients
(deftype CFraction
  (simple value)
  (compound first-value second-value c-fraction))

;; Parte b)

;; eval :: CFraction -> Rational
;; Returns the rational value resulting of evaluating c-fraction
(define (eval c-fraction)
  (match c-fraction
    [(simple value) value]
    [(compound a b d) (if (= (eval d) 0)
                          (error "zero division")
                          (+ a (/ b (eval d))))]))

;; Parte c)
;; degree ::  CFraction -> Integer
;; Returns the degree of c-fraction
(define (degree c-fraction)
  (match c-fraction
    [(simple value) 0]
    [(compound a b d) (if (= (eval d) 0)
                          (error "zero division")
                          (+ 1 (degree d)))]))

;; Parte d)
;; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
;; Captures the recursive scheme of CFraction
(define (fold-cfraction simp rec)
  (λ (c-fraction)
    (match c-fraction
      [(simple value) (simp value)]
      [(compound a b d) (rec a b ((fold-cfraction simp rec) d))])))

;; Parte e)
;; eval2 :: CFraction -> Rational
(define eval2
  (fold-cfraction
   (lambda (value) value)
   (lambda (a b d) (if (= d 0)
                       (error "zero division")
                       (+ a (/ b d))))))

;; degree2 ::  CFraction -> Integer
(define degree2
  (fold-cfraction
   (lambda (value) 0)
   (lambda (a b d) (if (= d 0)
                       (error "zero division")
                       (+ 1 d)))))


;; Parte f)
;; mysterious-cf :: Integer -> CFraction


;; Parte g)
;; from-to :: Integer -> Integer -> ListOf Integer


;; mysterious-list :: Integer -> listOf Float


;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?


;; Parte h)
;; rac-to-cf :: Rational -> CFraction
