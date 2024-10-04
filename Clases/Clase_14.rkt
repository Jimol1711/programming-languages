#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (mul <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <sym>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
         | (rec <sym> <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (mul l r)
  (if0 c t f)
  (id x)
  (fun x b)
  (app f arg)
  (rec id named-expr body))

#|
<s-expr> ::= <num>
           | <sym>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list '* <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | (list 'rec (list <sym> <s-expr>) <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)           
           | (list <s-expr> <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'with (list (? symbol? x) named-expr) body)
     (app (fun x (parse body)) (parse named-expr))]
    [(list 'rec (list (? symbol? x) named-expr) body)
     (rec x (parse named-expr) (parse body))]
    [(list 'fun (list (? symbol? x)) body) (fun x (parse body))]
    [(list f-expr arg-expr) (app (parse f-expr) (parse arg-expr))]))     

;; Values of expressions
(deftype Value
  (numV n)
  (closureV id body env))  

(define (numV+ v1 v2)
  (def (numV n1) v1)
  (def (numV n2) v2)
  (numV (+ n1 n2)))

(define (numV- v1 v2)
  (def (numV n1) v1)
  (def (numV n2) v2)
  (numV (- n1 n2)))

(define (numV* v1 v2)
  (def (numV n1) v1)
  (def (numV n2) v2)
  (numV (* n1 n2)))

(define (numV-zero? v)
  (def (numV n) v)
  (zero? n))

;; Environment
(deftype Env
  (mtEnv)
  (aEnv x v env)
  (aBoxEnv id bval env))

(define empty-env mtEnv)

(define extend-env aEnv)

(define box-extend-env aBoxEnv)

(define (env-lookup x env)
  (match env
    [(mtEnv) (error "free identifier ~a" x)]
    [(aEnv y v e)
     (if (symbol=? x y)
         v
         (env-lookup x e))]
    [(aBoxEnv id bval rest)
     (if (symbol=? x id)
         (unbox bval)
         (env-lookup x rest))]))

;; cyclic-env :: Sym Expr Env -> Env
;; Assumption: expr is a function expression.
(define (cyclic-env id fun-expr env)
  (def fun-val-holder (box 'dummy))
  (def new-env (box-extend-env id fun-val-holder env))
  (def fun-val (interp fun-expr new-env))
  (begin
    (set-box! fun-val-holder fun-val)
    new-env))  

;; interp :: Expr Env -> Value
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (numV+ (interp l env) (interp r env))]
    [(sub l r) (numV- (interp l env) (interp r env))]
    [(mul l r) (numV* (interp l env) (interp r env))]
    [(if0 c t f) (if (numV-zero? (interp c env))
                     (interp t env)
                     (interp f env))]
    [(app f e)
     (def (closureV the-arg the-body closed-env) (interp f env))     
     (def new-env (extend-env the-arg (interp e env) closed-env))
     (interp the-body new-env)]
    
    [(rec x named-expr body)
     (def new-env (cyclic-env x named-expr env))
     (interp body new-env)]))

;; run
(define (run s-expr)
  (interp (parse s-expr) (empty-env)))

(test (run '{* 3 2}) (numV 6))

(define fact0 '{rec {fact {fun {n} {if0 n 1 {* n {fact {- n 1}}}}}} {fact 0}})

(define fact5 '{rec {fact {fun {n} {if0 n 1 {* n {fact {- n 1}}}}}} {fact 5}})

;(test (run fact0) (numV 1))
;(test (run fact5) (numV 120))


