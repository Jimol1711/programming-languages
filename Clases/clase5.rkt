#lang play

#|
<expr> := (num <number>)
        | (sum <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)

<number> := ......
|#
;; Inductive datatype to represent arithmetic operations
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c l r)
  (with id named-expr body)
  (id x))

;; calc ;; Expr -> number
;; Reduces an arithmetic operation
(define (calc expr)
  (match expr
      [(num n) n]
      [(add l r) (+ (calc l) (calc r))]
      [(sub l r) (- (calc l) (calc r))]
      [(if0 c l r) (if (zero? (calc c))
                       (calc l) (calc r))]
      [(with x e b) (calc (subst b x (num (calc e))))]
      [(id x) (error 'calc "Open program (free ocurrence of ~a)" x)]))

#|
<s-expr> := <number>
          | (list '+ <s-expr> <s-expr>)
          | (list '- <s-expr> <s-expr>)
          | (list 'if0 <s-expr> <s-expr> <s-expr>)
          | (list 'with (list <sym> <s-expr>) <s-expr>)
          | <sym> 
|#
;; Concrete syntax of our language

;; parse ;; s-expr -> Expr
;; Transforms a given language in it's concrete syntax to it's abstract version (parsing)
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n)]
    [ x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c l r) (if0 (parse c)
                            (parse l)
                            (parse r))]
    [(list 'with (list x e) b) (with x (parse e)
                                       (parse b))]))

;; subst :: Expr symbol Expr -> Expr
;; (subst in what for) substitutes all free occurrences of
;; identifier 'what' in expression 'in' for expression 'for'
(define (subst in what for)
  (match in
    [(num n) (num n)]
    [(add l r) (add (subst l what for)
                    (subst r what for))]
    [(sub l r) (sub (subst l what for)
                    (subst r what for))]
    [(if0 c l r) (if0 (subst c what for)
                      (subst l what for)
                      (subst r what for))]
    [(with x e b) (with x
                        (subst e what for)
                        (if (symbol=? x what)
                            b
                            (subst b what for)))]
    [(id x) (if (symbol=? x what)
                for
                (id x))]))

;; run :: s-expr -> number
;; Reduces a given program into it's concrete syntax
(define (run s-expr)
  (calc (parse s-expr)))

; Pregunta de control: Interprete. Entrada es ast (abstract syntax tree) en resultado.
; Diferencia con compilador es que este último toma ast pero devuelve el programa equivalente en otro lenguaje.