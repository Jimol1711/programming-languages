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
;; Captures the recursive schema of CFraction
(define (fold-cfraction simp rec)
  (λ (c-fraction)
    (match c-fraction
      [(simple value) (simp value)]
      [(compound a b d) (rec a b ((fold-cfraction simp rec) d))])))

;; Parte e)
;; eval2 :: CFraction -> Rational
;; Returns the rational value resulting of evaluating a given c-fraction
(define eval2
  (fold-cfraction
   identity
   (λ (a b d) (if (= d 0)
                  (error "zero division")
                  (+ a (/ b d))))))

;; degree2 ::  CFraction -> Integer
;; Returns the degree of a given c-fraction
(define degree2
  (fold-cfraction
   (λ (value) 0)
   (λ (a b d) (+ 1 d))))

;; Parte f)
;; mysterious-cf :: Integer -> CFraction
;; Generates a mysterious continous fractions
(define (mysterious-cf value)
  (if (> 0 value)
      (error "Error: argumento negativo")
      (match value
        [0 (simple 6)]
        [v (compound 6 (sqr (- (* 2 v) 1))(mysterious-cf (- v 1)))])))

;; (define (mysterious-cf value)
;;   (cond [(< value 0) (error "Error: argumento negativo")]
;;         [(zero? value) (simple 6)]
;;         [else (compound 6 (sqr (- (* 2 (- value 1)) 1))(mysterious-cf (- value 1)))]))

;; (define (mysterious-cf value)
;;   (define (helper current-value)
;;     (if (= current-value value)
;;         (simple 6)
;;         (compound 6 (sqr (+ (* 2 current-value) 1)) (helper (+ current-value 1)))))
;;   (helper 0))

(simple 6)
(compound 6 (sqr 1) (simple 6))
(compound 6 (sqr 1) (compound 6 (sqr 3)(simple 6)))
(compound 6 (sqr 1) (compound 6 (sqr 3) (compound 6 (sqr 5)(simple 6))))


;; Parte g)
;; from-to :: Integer -> Integer -> ListOf Integer


;; mysterious-list :: Integer -> listOf Float


;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?


;; Parte h)
;; rac-to-cf :: Rational -> CFraction
