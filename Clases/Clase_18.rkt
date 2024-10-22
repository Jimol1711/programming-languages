#lang play

(print-only-errors #t)

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <sym>)
         | (fun <sym> <expr>)
         | (refun <sym> <expr>)
         | (app <expr> <expr>)
         | (set <sym> <expr>)
         | (seqn <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun x b)
  (refun x b)
  (app f arg)
  (set x v)
  (seqn e1 e2))

#|
<s-expr> ::= <num>
           | <sym>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list 'set <sym> <s-expr>)
           | (list 'seqn <s-expr> <s-expr>)
           | (list <s-expr> <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'with (list (? symbol? x) named-expr) body)
     (app (fun x (parse body)) (parse named-expr))]
    [(list 'fun (list (? symbol? x)) body) (fun x (parse body))]
    [(list 'refun (list (? symbol? x)) body) (refun x (parse body))]
    [(list 'set (? symbol? x) value-expr) (set x (parse value-expr))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list f-expr arg-expr) (app (parse f-expr) (parse arg-expr))]))

;; Values of expressions
(deftype Value
  (numV n)
  (closureV id body env)
  (refclosureV id body env))  

(define (numV+ v1 v2)
  (def (numV n1) v1)
  (def (numV n2) v2)
  (numV (+ n1 n2)))

(define (numV- v1 v2)
  (def (numV n1) v1)
  (def (numV n2) v2)
  (numV (- n1 n2)))

(define (numV-zero? v)
  (def (numV n) v)
  (zero? n))

;; Environment
;; asocia identificadores con locaciones de memoria
(deftype Env
  (mtEnv)
  (aEnv x loc env))

(define empty-env mtEnv)

(define extend-env aEnv)

(define (env-lookup x env)
  (match env
    [(mtEnv) (error "free identifier ~a" x)]
    [(aEnv y loc e)
     (if (symbol=? x y)
         loc
         (env-lookup x e))]))

;; Store
;; asocia locaciones de memoria con valores
(deftype Store
  (mtSto)
  (aSto loc val sto))

(define empty-sto mtSto)

(define extend-sto aSto)

(define (lookup-sto l sto)
  (match sto
    [(mtSto) (error 'lookup-sto "No value at location: ~a" l)]
    [(aSto loc val rest)
     (if (equal? l loc)
         val
         (lookup-sto l rest))]))

;; next-location :: Store -> Loc
;; returns next free location of a store
(define (next-location sto)
  (match sto
    [(mtSto) 0]
    [(aSto _ _ rest) (+ 1 (next-location rest))]))

(deftype Value*Store
  (v*s val sto))

;; interp :: Expr Env Store -> Value*Store
(define (interp expr env sto)
  (match expr
    [(num n) (v*s (numV n) sto)]

    [(fun id body) (v*s (closureV id body env) sto)]
    [(refun id body) (v*s (refclosureV id body env) sto)]
    
    [(id x) (v*s (lookup-sto (env-lookup x env) sto) sto)]

    [(if0 c t f)
     (def (v*s c-val c-sto) (interp c env sto))
     (if (numV-zero? c-val)
         (interp t env c-sto)
         (interp f env c-sto))]
    
    [(add l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (numV+ l-val r-val) r-sto)]
    
    [(sub l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (numV- l-val r-val) r-sto)]    

    [(seqn expr1 expr2)
     (def (v*s _ sto1) (interp expr1 env sto))
     (interp expr2 env sto1)]

    [(set id val-expr)
     (def (v*s val-val val-sto) (interp val-expr env sto))
     ;; modificar el valor de una variable: asociar un nuevo valor, a la misma locación de memoria.
     ;; cual locacion debo usar?
     (def loc (env-lookup id env))
     (v*s val-val (extend-sto loc val-val val-sto))]

    [(app fun-expr arg-expr)
     (def (v*s fun-val fun-sto) (interp fun-expr env sto))
     (match fun-val
       [(closureV id body fenv)
        (def (v*s arg-val arg-sto) (interp arg-expr env fun-sto))
        (def new-loc (next-location arg-sto))
        (interp body
                (extend-env id new-loc fenv)
                (extend-sto new-loc arg-val arg-sto))]

       ;; Se asume que arg-expr es un nodo (id x)
       [(refclosureV idf body fenv)
        ;; obtener el símbolo id pasado como argumento        
        (def (id x) arg-expr)
        ;; obtener la locacion de memoria de la variable que se usa como argumento
        (def loc (env-lookup x env))
        ;; interpretar el cuerpo de la función, usando la locacion existente
        ;; para el parametro actual
        (interp body (extend-env idf loc fenv) fun-sto)])]))

;; run
(define (run s-expr)
  (def (v*s result _) (interp (parse s-expr) (empty-env) (empty-sto)))
  result)

(test (run '1) (numV 1))
(test (run '{+ 1 1}) (numV 2))
(test (run '{{fun {x} x} 3}) (numV 3))
(test (run '{+ {{fun {x} x} 3} 4}) (numV 7))
(test (run '{{fun {x} {+ x x}} 3}) (numV 6))
(test (run '{{fun {x} x} {fun {y} y}}) (closureV 'y (id 'y) (mtEnv)))

(test (run '{with {b 0} {seqn {set b {+ 1 b}} b}}) (numV 1))
(test (run '{with {b 0} {if0 {seqn {set b 5} b} 1 b}}) (numV 5))

(test (run '{with {v 0}
            {with {f {fun {y} {set y 5}}}
                  {seqn {f v} v}}}) (numV 0))

(test (run '{with {v 0}
            {with {f {refun {y} {set y 5}}}
                  {seqn {f v} v}}}) (numV 5))


