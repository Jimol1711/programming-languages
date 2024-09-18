#lang play

(print-only-errors #t)

;; P1)
;;sintaxis concreta

#|
 <expr> ::= (num <Number>)
           |(add <expr <expr>)
           |(sub <expr> <expr>)
           |(mul <expr> <expr>)
           |(div <expr> <expr>)
           |(bool <Boolean>)
           |(land <expr> <expr>)
           |(lor <expr> <expr>)
           |(neg <expr>)
           |(less <expr> <expr>)
           |(ifp <expr> <expr> <expr>)
           |(tuple <expr> <expr>)
|#

;parseado en el AST
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (mult l r)
  (div l r)
  (bool b)
  (land l r)
  (lor l r)
  (neg e)
  (less l r)
  (ifp cond if-res else-res)
  (tuple l r)
  )

#|
<s-expr> ::= <Num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list '* <s-expr> <s-expr>)
           | (list '/ <s-expr> <s-expr>)
           | <Boolean>
           | (list 'and <s-expr> <s-expr>)
           | (list 'or <s-expr> <s-expr>)
           | (list 'not <s-expr>)
           | (list '< <s-expr> <s-expr>)
           | (list 'if <s-expr> <s-expr> <s-expr>)
|#

;; parse :: s-expr -> Expr
;; parsea el codigo a un AST
(define (parse s-expr)
  (match s-expr
    [ x #:when (number? x) (num x) ]
    [ b #:when (boolean? b) (bool b)]
    ;;aritmeticas
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mult (parse l) (parse r))]
    [(list '/ l r) (div (parse l) (parse r))]
    [(list 'and l r) (land (parse l) (parse r))]
    [(list 'or l r) (lor (parse l) (parse r))]
    [(list 'not e) (neg (parse e))]
    [(list '< l r) (less (parse l) (parse r))]
    [(list 'if cond if-res else-res) (ifp (parse cond) (parse if-res) (parse else-res))]
    ))

;; --- Calculadora------

;; calc :: Expr (AST) -> Number/Boolean
;; Calcula el valor de la expresion
(define (calc expr)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(mult l r) (* (calc l) (calc r))]
    [(div l r) (if (zero? (calc r))
                   (error "Division by zero")
                   (/ (calc l) (calc r)))]
    [(land l r) (and (calc l) (calc r))]
    [(lor l r) (or (calc l) (calc r))]
    [(neg e) (not (calc e))]
    [(less l r) (< (calc l) (calc r))]
    [(ifp cond if-res else-res) (if (calc cond) (calc if-res) (calc else-res))]
    ))

;; P2)
;; --- Chequeo de tipos ---

#|
<Type> ::= (Num)
         | (Bool)
|#

(deftype Type
  (Num)
  (Bool))

;; typecheck :: Expr -> Type o error
;; Realiza chequeo de tipos estatico sobre una expresion y devuelve su tipo
(define (typecheck expr)
  (match expr
    [(num n) (Num)]
    [(bool b) (Bool)]
    [(neg b) (if (equal? (typecheck b) (Bool))
                 (Bool)
                 (error "Static type error: expected bool found:" (typecheck b)))]
    [(land l r) (if (equal? (typecheck l) (Bool))
                    (if (equal? (typecheck r) (Bool))
                        (Bool)
                        (error "Static type error: expected bool, found:" (typecheck r)))
                    (error "Static type error: expected bool, found:" (typecheck l)))]
    [(lor l r) (if (equal? (typecheck l) (Bool))
                    (if (equal? (typecheck r) (Bool))
                        (Bool)
                        (error "Static type error: expected bool, found:" (typecheck r)))
                    (error "Static type error: expected bool, found:" (typecheck l)))]))

;; run :: ListOf[Symbol] -> Number/Boolean
;; el main del programa
(define (run prog)
  (def parsed-prog (parse prog))
  (def tipo_prog (typecheck parsed-prog))
  (calc parsed-prog))




