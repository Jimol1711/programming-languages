#lang play


(print-only-errors #t)

;;syntaxys concreta


;Environments
#|
  Env ::= (mtEnv)
          |(aEnv <symbol> <Value> aEnv)

|#

(deftype Env
  (mtEnv)
  (aEnv id loc env)
  )

;;syntax-sugar
;;empty-env :: Env
(define empty-env mtEnv)

;;extend-env :: Symbol Value Env -> Env
(define extend-env aEnv)

;;env-lookpu :: Symbol Env -> Loc
;;find the loc of the symbol in the environment, if is not founded, return an error
(define (env-lookup id env)
  (match env
    [ (mtEnv) (error 'env-lookup "not found symbol ~a" id) ]
    [ (aEnv x loc next-env) (if (equal? id x)
                                  loc
                                  (env-lookup id next-env)) ]
    ))


;; ----store----

(deftype Sto
  (mtSto)
  (aSto loc value sto)
  )

(define empty-sto mtSto)

(define extend-sto aSto)

;;sto-lookpu :: Loc Sto -> Value
;;find the value of the symbol in the environment, if is not founded, return an error
(define (sto-lookup loc sto)
  (match sto
    [ (mtSto) (error 'env-lookup "not found loc ~a" loc)]
    [ (aSto nextloc value next-sto) (if (equal? loc nextloc)
                                  value
                                  (sto-lookup loc next-sto)) ]
    ))

;;next-locatiion ;; Sto -> Loc (INT)
;; returns the next location by the store
(define (next-location sto)
  (match sto
    [ (mtSto) 0 ]
    [ (aSto loc value next-sto) (+ 1 (next-location next-sto)) ]
    ))


;;tipo especial para el retorno, que sera par value*store
(deftype Value*Store
  (val*sto val store))

#|
 <expr> ::= (num <num>)
           ;; (add <expr <expr>)
           ;; (if0 <expr> <expr> <expr>
           ;; (newbox <expr>)
           ;; (openbox <expr>)
           ;; (setbox <expr> <expr>)
           ;; (seqn <expr> <expr>)
           | (id <id>)
           | (set <sym> <expr>)
           | (app <expr> <expr>) --> ahora (app (fun ('x) (id 'x)) (num 3))
           | (fun <symbol> <expr>)
           | (refun <symbol> <expr>)

|#

;parseado en el AST
(deftype Expr
  (num n)
  (id x)
  (add l r)
  (if0 c t f)
  (app f arg)
  (newbox expr)
  (openbox box)
  (setbox box expr)
  (set id expr)
  (seqn expr1 expr2)
  (fun parameter body)
  (pair expr1 expr2)
  (fst pair-expr)
  (snd pair-expr)
  )

#| 
<Value> ::= (numV <number>)
           |(closureV <sym> <expr> <env>)
|#

(deftype Value
  (numV v)
  (closureV parameter body env)
  (pairV v1 v2)
  (boxV v) ;;caja que contiene un valor que será una location
  )

#|
<s-expr> ::= <num>
           | <symbol>
           | (list '+ <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'fun <symbol> <s-expr>)
           | (list 'with { <symbol> <s-expr> } <s-expr>) <- syntactic sugar
           | (list <symbol> <s-expr>)

|#


;; parse :: s-expr -> Expr
;; parsea el codigo a un AST
(define (parse s-expr)
  (match s-expr
    [ x #:when (number? x) (num x) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'newbox arg) (newbox (parse arg)) ]
    [(list 'openbox box) (openbox (parse box)) ]
    [(list 'setbox box value) (setbox (parse box) (parse value)) ]
    [(list 'seqn expr1 expr2) (seqn (parse expr1) (parse expr2)) ]
    [(list 'set id expr) (set id (parse expr)) ]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) body) (fun x (parse body)) ]
    [(list 'pair e1 e2) (pair (parse e1) (parse e2)) ]
    [(list 'first par) (fst (parse par)) ]
    [(list 'second par) (snd (parse par)) ]
    [(list 'with (list x e) b) (app (fun x (parse b)) (parse e)) ]
    [(list function arg-expr) (app (parse function) (parse arg-expr)) ]
    ))



;;dual-value-op :: NumV NumV (Int Int -> Int) -> NumV
;; hace el calculo de los numV con la op
(define (dual-value-op l r op)
  (numV (op (numV-v l) (numV-v r))))
                         
;;interp:: Expr Env Sto-> Value*Store
;;interpreta la expresion
(define (interp expr env sto)
  (match expr
    [ (num x) (val*sto (numV x) sto) ]
    [ (id x) (val*sto (sto-lookup (env-lookup x env) sto) sto) ]
    [(add l r) ;;se debe evaluar de izquierda a derecha
     (def (val*sto l-val l-sto) (interp l env sto))
     (def (val*sto r-val r-sto) (interp r env l-sto))
     (val*sto (dual-value-op l-val r-val +) r-sto) ]
    [(if0 c t f)
     (def (val*sto c-val c-sto) (interp c env sto))
     (if (equal? (numV-v c-val) 0)
                     (interp t env c-sto)
                     (interp f env c-sto)) ]
    [(fun x body) (val*sto (closureV x body env) sto) ]
    [(set id val-expr) ;; (set 'x (num 3))
     (def location_id (env-lookup id env))
     (def (val*sto val-val val-sto) (interp val-expr env sto))
     (val*sto val-val (extend-sto location_id val-val val-sto))
     ]
    ;;aca las funciones son como (closure 'x (id 'x)) env) 
    [(app f arg)
      (def (val*sto fun-expr f-sto) (interp f env sto))
      (def (closureV paremeter body fenv) fun-expr)
          (def (val*sto arg-value arg-sto) (interp arg env f-sto))
          (def location (next-location arg-sto)) ;;generamos la nueva locacion
          (interp body
                  (extend-env paremeter location fenv)
                  (extend-sto location arg-value arg-sto))]
    
    [ (seqn expr1 expr2)
     (def (val*sto _ e1-sto) (interp expr1 env sto))
     (interp expr2 env e1-sto)]

    [ (newbox expr)
      (def (val*sto val val-sto) (interp expr env sto))
      (def new-location (next-location val-sto))
      (val*sto (boxV new-location)
               (extend-sto new-location val val-sto)) ]

    [ (openbox box)
      ;;se interpretea la caja pq puede ser un id que traiga consigo todo}
      ;; como (id 'cajita) donde en store (cajita-> dir_cajita) y (dir_cajita -> (boxV loc))
      (def (val*sto (boxV loc) box-sto) (interp box env sto) )
      (val*sto (sto-lookup loc box-sto) box-sto) ]
    ;;set
    [ (setbox box expr)
      (def (val*sto (boxV loc) box-sto) (interp box env sto) )
      (def (val*sto val expr-sto) (interp expr env box-sto))
      ;; ahora loc apunta hacia el nuevo valor
      (val*sto val (extend-sto loc val expr-sto)) ]
    ;;pares
    [ (pair expr1 expr2)
      (def (val*sto val1 sto1) (interp expr1 env sto))
      (def (val*sto val2 sto2) (interp expr2 env sto1))
      (val*sto (pairV val1 val2) sto2) ]
    ; ; retorna la primera componente de un par
    [ (fst pair-expr)
      (def (val*sto (pairV val1 val2) pair-sto) (interp pair-expr env sto))
      (val*sto val1 sto)]
    ;; retorna la segunda componente de un par
    [ (snd pair-expr)
      (def (val*sto (pairV val1 val2) pair-sto) (interp pair-expr env sto))
      (val*sto val2 sto)]
    ))

(define (run prog)
  (def (val*sto val store) (interp (parse prog) (empty-env) (empty-sto)))
  val)
    
;; ------------- MUTACIONES -----------

;;a)
(define p1 '(pair (openbox a) (with (a (newbox 0)) 1)))
(test/exn (run p1) "env-lookup: not found symbol a")

;;b)
(define p2 ' (pair (with (a (newbox 0)) 1) (openbox a) ))
(test/exn (run p2) "env-lookup: not found symbol a")



;;c)
(run ' (with (a (newbox 0))
             (+ ( first
                  (seqn
                   (setbox a 1)
                   (pair (openbox a) 2)))
                (openbox a))))

;;debería dar 2

;;d) correcion es
#|
    [ (fst pair-expr)
      (def (val*sto (pairV val1 val2) pair-sto) (interp pair-expr env sto))
      (val*sto val1 pair-sto)] ----> usar pair-sto en vez de sto   
|#

               
