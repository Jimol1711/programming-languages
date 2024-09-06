;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        FUNCTIONS WITH SUBSTITUTION            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <id> <expr> <expr>)
         | (id <id>)
         | (app <id> <expr>)
|#
;; Inductive type for representing arith-
;; metical expressions with conditionals,
;; identifiers and function calls
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with id named-expr body)
  (id x)
  (app f-name f-arg))


;; Definition of a function 
;; <fundef> ::=  <id> <id> <expr> 
(deftype FunDef
  (fundef name arg body))


;; look-up :: symbol listof(FunDef) -> FunDef
;; searches a function definition within a list of definitions
(define (look-up f-name l)
  (match l
    [ (list) (error 'lookup "Function ~a not found" f-name) ]
    [ (cons head tail) (if (symbol=? f-name  (fundef-name head))
                           head
                           (look-up f-name tail))]))


;; subst: Expr symbol Expr -> Expr
;; (subst in what for) substitutes all free occurrences of
;; identifier 'what' in expresion 'in' for expression 'for'
(define (subst in what for)
  (match in
    [(num n) (num n)]
    [(add l r) (add (subst l what for)
                    (subst r what for))]
    [(sub l r) (sub (subst l what for)
                    (subst r what for))]
    [(if0 c t f) (if0 (subst c what for)
                      (subst t what for)
                      (subst f what for))]
    [(with x e b)
     (with x
           (subst e what for)
           (if (symbol=? x what)
               b   ; x does not occur free in b
               (subst b what for)))]
    [(id x) (if (symbol=? x what) for (id x))]
    [(app f e) (app f (subst e what for))]))


;; eval :: Expr listof(FunDef) -> number
;; evaluates an arithmetical expression with function calls
(define (eval expr f-list)
  (match expr
    [(num n) n]
    [(add l r) (+ (eval l f-list) (eval r f-list))]
    [(sub l r) (- (eval l f-list) (eval r f-list))]
    [(if0 c t f) (if  (zero? (eval c f-list))
                      (eval t f-list)
                      (eval f f-list))]
    [(with x e b) (eval (subst b x (num (eval e f-list))) f-list)]
    [(id x) (error 'eval "Open expression (free occurrence of ~a)" x)]
    [(app f e) (def (fundef _ the-arg the-body) (look-up f f-list))
               (eval (subst the-body the-arg (num (eval e f-list))) f-list)]))


;; s-expressions used as concrete 
;; syntax for our programs
#|
<s-expr> ::= <num>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | <sym>
           | (list <sym> <s-expr>) 
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c)
                            (parse t)
                            (parse f))]
    [(list 'with (list x e) b) #:when (symbol? x)
         (with x (parse e) (parse b))]
    [(list f a) #:when (symbol? f) (app f (parse a))]))


;; run :: s-expr listof(FunDef) -> number
(define (run prog f-list)
  (eval (parse prog) f-list))

;; (define my-funcs
;;   (list (fundef 'my-plus1 'x (parse '(+ x 1)))
;;         (fundef 'my-double 'x (parse '(+ x x)))
;;         (fundef 'my-S 'x (match x
;;                              [zero? x 0]
;;                              [parse '(+ x my-S(x-1))])))       

;; (test (run '(my-double (my-plus1 5)) my-funcs) 12) 