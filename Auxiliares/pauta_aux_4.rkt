#lang play


(print-only-errors #t)

;;syntaxys concreta

#|
 <expr> ::= (num <Number>)
           |(bool <Bolean>)
           |(neg <expr>)
           |(lor <expr> <expr>)
           |(land <expr> <expr>)
           |(add <expr <expr>)
           |(sub <expr> <expr>)
           |(ifp <expr> <expr> <expr>
           |(mult <expr> <expr>)
           |(div <expr> <expr>)
           |(less <expr> <expr>)
           

|#

;parseado en el AST
(deftype Expr
  (num n)
  (bool v)
  (neg p)
  (lor p q)
  (land p q)
  (add l r)
  (sub l r)
  (ifp c t f)
  (mult l r)
  (div l r)
  (less l r)
  (tuple l r)
  )

#|
<s-expr> ::= <Num>
           | <Boolean>
           | (list 'not <s-expr>)
           | (list 'and <s-expr> <s-expr>)
           | (list 'or <s-expr> <s-expr>)
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if <s-expr> <s-expr> <s-expr>)
           | (list '* <s-expr> <s-expr>)
           | (list '/ <s-expr> <s-expr>)
           | (list '< <s-expr> <s-expr>)

|#


;; parse :: s-expr -> Expr
;; parsea el codigo a un AST
(define (parse s-expr)
  (match s-expr
    [ (? number?) (num s-expr) ]
    [ x #:when (boolean? x) (bool x) ]
    ;;booleanos
    [ (list 'not formula) (neg (parse formula)) ]
    [ (list 'or formula1 formula2 ) (lor (parse formula1) (parse formula2)) ]
    [ (list 'and formula1 formula2 ) (land (parse formula1) (parse formula2)) ]
    ;;aritmeticas
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mult (parse l) (parse r))]
    [(list '/ l r) (div (parse l) (parse r))]
    [(list '< l r) (less (parse l) (parse r))]
    [(list 'if c t f) (ifp (parse c) (parse t) (parse f))]
    ))


#|
<Type> ::= (Num)
           |(Bool)
|#

(deftype Type
  (Num)
  (Bool)
  (Tuple t1 t2))

;; typecheck :: Expr -> Type o error
;; Realiza chequeo de tipos estatico sobre una expresion y devuelve su tipo
(define (typecheck expr)
  (match expr
    [(num n) (Num)]
    [(bool b) (Bool)]
    [(tuple l r) (Tuple (typecheck l) (typecheck l))]
    [(neg b) (if (equal? (typecheck b) (Bool)) ;;(Bool? (typecheck b))
                 (Bool)
                 (error "Static type error: expected Bool found:" (typecheck b)))]
    [(land l r) (if (equal? (typecheck l) (Bool))
                   (if (equal? (typecheck r) (Bool))
                       (Bool)
                       (error "Static type error: expected Bool found:" (typecheck r)))
                   (error "Static type error: expected Bool found:" (typecheck l)))]
    [(lor l r) (if (equal? (typecheck l) (Bool))
                   (if (equal? (typecheck r) (Bool))
                       (Bool)
                       (error "Static type error: expected Bool found:" (typecheck r)))
                   (error "Static type error: expected Bool found:" (typecheck l)))]
    [(add l r) (if (equal? (typecheck l) (Num))
                   (if (equal? (typecheck r) (Num))
                       (Num)
                       (error "Static type error: expected Num found:" (typecheck r)))
                   (error "Static type error: expected Num found:" (typecheck l)))]
    [(sub l r) (if (equal? (typecheck l) (Num))
                   (if (equal? (typecheck r) (Num))
                       (Num)
                       (error "Static type error: expected Num found:" (typecheck r)))
                   (error "Static type error: expected Num found:" (typecheck l)))]
    [(mult l r) (if (equal? (typecheck l) (Num))
                   (if (equal? (typecheck r) (Num))
                       (Num)
                       (error "Static type error: expected Num found:" (typecheck r)))
                   (error "Static type error: expected Num found:" (typecheck l)))]
    [(div l r) (if (equal? (typecheck l) (Num))
                   (if (equal? (typecheck r) (Num))
                       (Num)
                       (error "Static type error: expected Num found:" (typecheck r)))
                   (error "Static type error: expected Num found:" (typecheck l)))]
    [(less l r) (if (equal? (typecheck l) (Num))
                   (if (equal? (typecheck r) (Num))
                       (Bool)
                       (error "Static type error: expected Num found:" (typecheck r)))
                   (error "Static type error: expected Num found:" (typecheck l)))]
    [(ifp c t f) (if (equal? (typecheck c) (Bool))
                     (if (equal? (typecheck t) (typecheck f)) ;; Ambas ramas del if deben tener el mismo tipo.
                         (typecheck t)
                         (error "Static type error: expected:" (typecheck t) "found:" (typecheck f)))
                     (error "Static type error: expected Bool found:" (typecheck c)))]
    
    ))



;; --- Calcualdora------

;; calc :: Expr (AST) -> Number/Boolean
;; Calcula el valor de la expresion
(define (calc expr)
  (match expr
    [(num n) n]
    [(bool x) x]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(mult l r) (* (calc l) (calc r))]
    [(div l r) (if (zero? (calc r))
                   (error "Division by zero")
                   (/ (calc l) (calc r)))]
    [(ifp c t f) (if (calc c)
                    (calc t)
                    (calc f))]
    [(less l r) (< (calc l) (calc r)) ]
    ;;operaciones logicas
    [(neg p) (not (calc p)) ]
    [(land l r) (and (calc l) (calc r)) ]
    [(lor l r) (or (calc l) (calc r)) ]
    ))
    

;; run :: ListOf[Symbol] -> Number/Boolean
;; el main del programa
(define (run prog)
  (def parsed-prog (parse prog))
  (def tipo_prog (typecheck parsed-prog)) ;;dara error si hay alguno
  (calc parsed-prog))




