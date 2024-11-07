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
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (mul l r)
  (if0 c t f)
  (id x)
  (fun x b)
  (app f arg))

#|
<s-expr> ::= <num>
           | <sym>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list '* <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
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
    [(list 'fun (list (? symbol? x)) body) (fun x (parse body))]
    [(list f-expr arg-expr) (app (parse f-expr) (parse arg-expr))]))

;; Values of expressions
(deftype Value
  (numV n)
  ;(closureV id body env)
  (closureV f))

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
  (aEnv x v env))

(define empty-env mtEnv)

(define extend-env aEnv)

(define (env-lookup x env)
  (match env
    [(mtEnv) (error "free identifier ~a" x)]
    [(aEnv y v e)
     (if (symbol=? x y)
         v
         (env-lookup x e))]))

;; interp :: Expr Env -> Value
(define (interp expr env)
  (match expr
    [(num n) (numV n)]    
    [(id x) (env-lookup x env)]
    [(add l r) (numV+ (interp l env) (interp r env))]
    [(sub l r) (numV- (interp l env) (interp r env))]
    [(mul l r) (numV* (interp l env) (interp r env))]
    [(if0 c t f) (if (numV-zero? (interp c env))
                     (interp t env)
                     (interp f env))]
    
    ;[(fun id body) (closureV id body env)]
    [(fun id body)
     (def fun-val (λ (arg-val) (interp body (extend-env id arg-val env))))
     (closureV fun-val)]
    
    ;[(app f e)
    ;(def (closureV the-arg the-body closed-env) (interp f env))     
    ; (def new-env (extend-env the-arg (interp e env) closed-env))
    ; (interp the-body new-env)]))
    [(app f e)
     (def (closureV fun-val) (interp f env))
     (def arg-val (interp e env))
     (fun-val arg-val)]
    ))

;; run
(define (run s-expr)
  (interp (parse s-expr) (empty-env)))

(test (run '{* 3 2}) (numV 6))

;; interp-nowrapper :: Expr Env -> number/procedure
(define (interp-nowrapper expr env)
  (match expr
    [(num n) n]
    [(fun id body) (λ (arg-val) (interp body (extend-env id arg-val env)))]
    [(app f e) ((interp f env) (interp e env))]    
    [(id x) (env-lookup x env)]   
    [(add l r) (+ (interp l env) (interp r env))]
    [(sub l r) (- (interp l env) (interp r env))]
    [(mul l r) (* (interp l env) (interp r env))]
    [(if0 c t f) (if (zero? (interp c env))
                     (interp t env)
                     (interp f env))]))

(define empty-env-f
  (λ (id) (error "free identifier: ~a" id)))

(define (env-lookup-f id env)
  (env id))

(define (extend-env-f new-id value env)
  (λ (id)
    (if (symbol=? id new-id)
        value
        (env id))))

(define e1
  (extend-env-f 'x 4
              (extend-env-f 'y 5
                            empty-env-f)))

(define e11
  (extend-env-f 'x 4
                (extend-env-f 'y 5
                              (λ (id) (error "free identifier: ~a" id)))))

(define e12
  (extend-env-f 'x 4
                (λ (id) (if (symbol=? id 'y)
                              5
                              ((λ (id) (error "free identifier: ~a" id)) id)))))

(define e13
  (λ (id)
    (if (symbol=? id 'x)
        4
        ((λ (id) (if (symbol=? id 'y)
                              5
                              ((λ (id) (error "free identifier: ~a" id)) id))) id))))
           

              

