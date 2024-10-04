#lang play

(print-only-errors #t)


;; P1 Arboles N-Arios
;; Variaciones de los ejercicios propuestos en clase

;a)
#|
<NTree> ::= (leaf <Number>)
            (in-node <Number> <NTree>*)
|#

(deftype NTree
  (nleaf v)
  (nnode v childs))


;b)
;; fold-ntree :: (Number -> A) (Number ListOf(A) -> A) -> (NTree -> A)
(define (fold-ntree f g)
   (λ (bt)
     (match bt
       [(nleaf v) (f v)]
       [(nnode v childs) (g v (map (fold-ntree f g) childs))]
       ))
  )

; Al igual que bintree tenemos que aplicar recursivamente foldntree a cada hoja,
; para eso usamos map para aplicar la funcion con los argumentos incluidos.

;c)
;(sum-n-tree bt) :: NTree -> Number
;; hace la suma de todos los valores internos y hojas
(define (sum-n-tree bt)
  ( (fold-ntree (λ (v) v)
                (λ (v childs) (+ v (foldl + 0 childs)))) ; Tambien puede ser (apply + leaves)
                bt)
  )

(test (sum-n-tree (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) ))) 26  )
(test (sum-n-tree (nleaf 2)) 2 )

;; contains-n-tree? :: NTree Number -> Boolean
;; retorna #t valor v se encuentra en algún nodo del árbol n-ario nt y #f si no
(define (contains-n-tree? nt v)
  ( (fold-ntree (λ (x) (equal? x v))
          (λ (x childs) (if (equal? x v)
                         #t
                         (foldl (λ (x y) (or x y)) #f childs)))) ; No se puede usar apply o directamente or por que or
                                                                 ;; NO es una función, es una MACRO
    nt))

(test (contains-n-tree? (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) )) 12) #f)
(test (contains-n-tree? (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) )) 4) #t)
(test (contains-n-tree? (nleaf 2) 12) #f)

;; select-n-tree:: (A -> Boolean) NTree -> ListOf(Numbers)
;; lista de valores del árbol n-ario nt que satisfacen el predicado p.
(define (select-n-tree p nt)
  ( (fold-ntree (λ (v) (if (p v)
                             (list v)
                             '()))
                  (λ (v childs) (if (p v)
                                 (append (list v) (apply append childs)) ; Como nuestro caso base devuelve una lista, hay que aplicar append a cada elemento de childs
                                 (apply append childs)))                 ; si no, se arma una lista de listas anidadas.
                  ) nt)
  )

(test (select-n-tree even? (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) ))) (list 2 4 6 10))
(test (select-n-tree (λ (x) (< 10 x)) (nnode 1 (list (nleaf 2) (nleaf 3) (nnode 4 (list (nleaf 6) (nleaf 10)) ) ))) '())



;;p2) a)
;-----------Polinomios

#|
Cree la estructura Polinomio, donde un ejemplo es

'(2x^2 + 4x + 5)

sería

(plus 2 2 (plus 4 1 (plus 5 0 (nullp))))

|#

(deftype Polynomio
  (nullp)
  (plus coef grado resto))


;;b)
;----------;obtener coeficiente-----------

;; ;; get-list-coef :: ListOf[Char] -> ListOf[Char]
;; toma una lista de characeteres que es el polinomio y retorna la lista
;; de characters hasta encontrar el x, sin incluir el x
(define (get-list-coef pol)
  (if (equal? (car pol) #\x)
      '()
      (append (list (car pol)) (get-list-coef (cdr pol)))
      ))

;; get-coef :: ListOf[Char] -> Number
;:; toma un polinomio escrito en characteres y saca su coef
(define (get-coef pol)
  (string->number (list->string (get-list-coef pol))))

(test (get-coef (list #\2 #\1 #\x #\2 #\3)) 21)

;; --------------obtener grado---------


;; ;; get-list-grado :: ListOf[Char] -> ListOf[Char]
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

;;-------Parse--------

(define (parse-p pol)
    (match pol
      ;;forma (2x^1 + ...)
      [ (list polinomio '+ resto ...) (let ([pol-lista (string->list (symbol->string polinomio))])
                                 (plus (get-coef pol-lista) (get-grado pol-lista) (parse-p resto))) ]
      ;;forma (2)
      [ (list n) #:when (number? n) (plus n 1 (nullp)) ]
      ;;forma (2x^2)
      [ (list polinomio) (let ([pol-lista (string->list (symbol->string polinomio))])
                                        (plus (get-coef pol-lista) (get-grado pol-lista) (nullp))) ]
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


;;d)
;;eval :: Number Polynomio -> Number
;; evalua el polinomio
(define (eval n p)
  ((fold-p 0
          (λ (c g resto) (+ (* c (expt n g)) resto))) p)
  )

(test (eval 3 (parse-p '(-23x^5 + 2x^2 + 1x^1))) -5568)
(test (eval 0 (parse-p '(-23x^5 + 2x^2 + 1x^1))) 0)

;;e)
;nf? :: Polynomio -> Boolean
;; un polynomio es normal-form si NO tiene coeficientes 0 (ya que cero por algo es inncesario)
;; y además los grados tienen que estar de mayor a menor estrictamente (sin repeticiones) en dirección ->
(define (nf? p)
  (match p
    [(nullp) #t ]
    [(plus coef grado resto) (cond [(nullp? resto) (and (not (zero? coef)) (nf? resto))]
                                   [ else (and (> grado (plus-grado (plus-resto p))) (not (zero? coef)) (nf? resto))] )]
          ))
  
                                                                                    
(test (nf? (parse-p '(23x^5 + 2x^2 + 1x^1))) #t) ;;orden correcto
(test (nf? (parse-p '(23x^5 + 2x^5 + 1x^1))) #f) ;;repetido 
(test (nf? (parse-p '(23x^3 + 2x^4 + 1x^1))) #f) ;;orden incorrecto
(test (nf? (parse-p '(0x^5 + 2x^2 + 1x^1))) #f) ;;orden correcto y coef zero
(test (nf? (parse-p '(0x^1 + 2x^3 + 1x^1))) #f) ;;orden incorrecto y coef zero


