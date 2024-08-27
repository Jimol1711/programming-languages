#lang play
(print-only-errors #t)

; P1
; a)
(deftype NTree
  (nleaf v)
  (nnode v childs))

; b)
;; fold-ntree :: (Number -> A) (Number ListOf[A]) -> (NTree -> A)
;; ...
(define (fold-ntree f g)
  (λ (nt)
    (match nt
      [(nleaf v) (f v)]
      [(nnode v childs) (g v (map (fold-ntree f g) childs))]
      )))

; c)
;; sum-tree :: NTree -> Number
;; ...
(define (sum-tree nt)
  (fold-ntree identity
              (λ (v childs) (+ v (foldl + 0 childs))))
               nt)

; d)

;; contains-n-tree? :: NTree Number -> Boolean
;; retorna #t valor v se encuentra en algún nodo del árbol n-ario nt y #f si no
(define (contains-n-tree? nt v)
  ((fold-ntree (λ (x) (equal? v x))
               (λ (x childs) (if (equal? x v)
                                 #t
                                 (foldl (λ (x y) (or x y)) #f childs))))
   nt))

(test (contains-n-tree? (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) )) 12) #f)
(test (contains-n-tree? (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) )) 4) #t)
(test (contains-n-tree? (nleaf 2) 12) #f)

; e)

;; select-n-tree :: (Number -> Boolean) NTree -> ListOf[Number]
;; ...
(define (select-n-tree p nt)
  ((fold-ntree (λ (v) (if (p v)
                          (list v)
                          '()))
               (λ (v childs) (if (p v)
                                 (append (list v) (apply append childs))
                                 (apply append childs)))
               ) nt))

(test (select-n-tree even? (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) ))) (list 2 4 6 10))
(test (select-n-tree (λ (x) (< 10 x)) (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) ))) '())

; P2
; a)

#|
<Polinomio> ::= (nullp)
             |  (plus <Number> <Number> <Polinomio>)
|#
(deftype Polynomio
  (nullp)
  (plus coef grado resto))

; b)
;----------;obtener coeficiente-----------

;; get-list-coef :: ListOf[Char] -> ListOf[Char]
;; toma una lista de characters que es el polinomio y retorna la lista
;; de characters hasta encontrar el x, sin incluir el x
(define (get-list-coef pol)
  (if (equal? (car pol) #\x)
      '()
      (append (list (car pol)) (get-list-coef (cdr pol)))
      ))

;; get-coef :: ListOf[Char] -> Number
;; toma un polinomio escrito en characteres y saca su coef
(define (get-coef pol)
  (string->number (list->string (get-list-coef pol))))

(test (get-coef (list #\2 #\1 #\x #\2 #\3)) 21)

;; --------------obtener grado---------


;; get-list-grado :: ListOf[Char] -> ListOf[Char]
;; toma una lista de characeteres que es el polinomio y retorna la lista
;; de characters hasta encontrar el ^
(define (get-list-grado pol)
    (if (equal? (car pol) #\^)
      (cdr pol)
      (get-list-grado (cdr pol))
      ))

;; get-coef :: ListOf[Char] -> Number
;:; toma un polinomio escrito AL REVÉS en characteres y saca su grado
(define (get-grado pol)
  (string->number (list->string (get-list-grado pol))))

(test (get-grado (list #\2 #\1 #\x #\^ #\2 #\3)) 23)

;; parse-p :: ListOf[Symbol] -> Polinomio
(define (parse-p p)
  (match p
    [(list monomio '+ resto ...) (let ([lista-char (string->list (symbol->string monomio))])
                                      (plus (get-coef lista-char) (get-grado lista-char) (parse-p resto)))]
    [(list n) #:when (number? n) (plus n 1 (nullp))]
    [(list monomio) (let ([lista-char (string->list (symbol->string monomio))])
                         (plus (get-coef lista-char) (get-grado lista-char) (nullp)))]
    ))

(test (parse-p '(23x^5 + 2x^2 + 1x^1)) (plus 23 5 (plus 2 2 (plus 1 1 (nullp)))));;monomios completos
(test (parse-p '(23x^5 + 2x^-3 + 1x^12)) (plus 23 5 (plus 2 -3 (plus 1 12 (nullp))))) ;;grado de mas de 1 digito
(test (parse-p '(23x^5 + -2x^2 + 5)) (plus 23 5 (plus -2 2 (plus 5 1 (nullp))))) ;;monomio grado cero y coef negativo

;c)
;; fold-p :: A (Number Number A -> A) -> (Polynomio -> A)
;; abstrae el patrón recursivo en los polinomios
(define (fold-p f g)
  (λ (p)
      (match p
        [(nullp) f]
        [(plus c grado resto) (g c grado ((fold-p f g) resto)) ]
        )))

; d)
(define (eval n p)
  ((fold-p 0
            (λ (coef grado resto) (+ (* coef (expt n grado)) resto))
            ) p))

; e)
;; nf? :: Polynomio -> Boolean
;; un polynomio es normal-form si NO tiene coeficientes 0 (ya que cero por algo es inncesario)
;; y además los grados tienen que estar de mayor a menor estrictamente (sin repeticiones) en dirección ->
(define (nf? p)
  (match p
    [(nullp) #t]
    [(plus coef grado resto) (cond [(nullp? resto) (and (not (zero? coef)) (nf? resto))]
                                   [else (and (> grado (plus-grado (plus-resto p))) (not (zero? coef)) (nf? resto))])]
          ))