#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <sym> <expr> <expr>)
         | (id <sym>)
         | (app <sym> <expr>

<fundef> ::= (fundef <sym> <sym> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x named-expr body)
  (id x)
  (app f-name f-arg)
  )

#|
<s-expr> ::= <num>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | <sym>
           | (list <sym> <s-expr>))
|#
  
;; parse :: <s-expr> -> Expr
;; Parses arithmetical language.
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l-sexpr r-sexpr) (add (parse l-sexpr) (parse r-sexpr))]
    [(list '- l-sexpr r-sexpr) (sub (parse l-sexpr) (parse r-sexpr))]
    [(list 'if0 c-sexpr t-sexpr f-sexpr)
     (if0 (parse c-sexpr) (parse t-sexpr) (parse f-sexpr))]
    [(list 'with (list (? symbol? x) named-expr) body)
     (with x (parse named-expr) (parse body))]
    [(list f-name f-arg) (app f-name (parse f-arg))]))


;; Function definitions
;; <fundef> ::= (fundef <sym> <sym> <expr>)
(deftype FunDef
  (fundef name arg body))

;; lookup :: symbol listof(FunDef) -> FunDef
;; Searches a function definition within a list of definitions.
(define (lookup f-name defs)
  (match defs
    [(list) (error 'lookup "Function ~a not found" f-name)]
    [(cons head tail) (if (symbol=? f-name (fundef-name head))
                          head
                          (lookup f-name tail))]))

;; <env> ::= mtEnv
;;         | (aEnv <sym> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; empty-env :: Env
(define empty-env (mtEnv))

;; extend-env :: Symbol Value Env -> Env
(define extend-env aEnv)

;; env-lookup :: Symbol Env -> Value
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? x id)
                            val
                            (env-lookup x rest))]))

;; interp :: Expr listof(FunDef) Env -> number
;; Evaluates an arithmetic expression.
(define (interp expr f-list env)
  (match expr
    [(num n) n]
    [(add l-expr r-expr) (+ (interp l-expr f-list env) (interp r-expr f-list env))]
    [(sub l-expr r-expr) (- (interp l-expr f-list env) (interp r-expr f-list env))]
    [(if0 c-expr t-expr f-expr) (if (zero? (interp c-expr f-list env))
                                    (interp t-expr f-list env)
                                    (interp f-expr f-list env))]
    
    [(with x named-expr body)
     (def new-env (extend-env x (interp named-expr f-list env) env))
     (interp body f-list new-env)]

    [(id x) (env-lookup x env)]
    [(app fname arg-expr)
     (def (fundef _ farg fbody) (lookup fname f-list))
     (def new-env (extend-env farg (interp arg-expr f-list env) empty-env))
     (interp fbody f-list new-env)]
    ))


;; run :: s-expr listof(FunDef) -> number
(define (run prog f-list)
  (interp (parse prog) f-list empty-env))

; Tests P3.a)
(test (run '{add 1 2 3 4}
           (list (fundef 'add (list 'e1 'e2 'e3 'e4) (parse `(+ e1 (+ e2 (+ e3 e4)))))))
      10)

(test (run '{const5}
           (list (fundef 'const5 (list) (parse `5))))
      5)

; Tests P3.b)
(test (run `{with-fun {define add1 x {+ 1 x}}
              {add1 5}} '())
      6)

(test (run `{with-fun {define foo x {+ 1 x}}
              {foo 5}}
           (list (fundef 'foo (list 'x) (parse `{+ 10 x}))))
      6)

