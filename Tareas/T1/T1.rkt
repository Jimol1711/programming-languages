#lang play
(require math/flonum)

;; Juan Ignacio Molina - Sección 1

#|
Hizo Ud uso de la whiteboard policy:  NO
En caso que afirmativo, indique con quién y sobre qué ejercicio: 
|#

;; Parte a)

#|
<CFraction> ::= (simple <value>)
             |  (compound <value> <value> <CFraction>)
|#
;; Datatype to represent a finite continuous fraction to integer coefficients
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
  (if (< value 0)
      (error "Error: argumento negativo")
      (foldl
       (lambda (v acc) (compound 6 (sqr (- (* 2 v) 1)) acc))
       (simple 6)
       (range value 0 -1)))) ; Esto tuve que hacerlo así porque la recursión me generaba que la fracción se retornaba invertida.

;; Parte g)

;; from-to :: Integer Integer -> ListOf Integer
;; Constructs a list of integers between two given integers
(define (from-to i j)
  (if (> i j)
      (from-to j i)
      (range i j)))

;; mysterious-list :: Integer -> ListOf Float
;; Returns a list such that the ith element is the difference between the evaluation of (mysterious-cf i) and 3
(define (mysterious-list n)
      (map (λ (x) (- (fl (eval (mysterious-cf x))) 3)) (from-to 0 (+ n 1))))

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?

;; Dandole a mysterious-list un n relativamente alto podemos ver que (mysterious-cf k) tiende a pi cuando k tiende a infinito
;; Esto se prueba con varios valores de n en los tests

;; Parte h)
;; rac-to-cf :: Rational -> CFraction
;; Transforms a non-negative rational number into it's CFraction representation
(define (rac-to-cf r)
  (let* ([i (floor r)]
        [f (- r i)])
    (if (zero? f)
        (simple r)
        (let ([recip (/ 1 f)])
             (compound r (rac-to-cf recip))))))
