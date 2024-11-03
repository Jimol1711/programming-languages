#lang play

;; Juan Ignacio Molina - Sección 1

#|

Hizo Ud uso de la whiteboard policy: NO
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------------;;
;; P1.a, P1.e, P2.a ;;
;;------------------;;

#|
Abstract syntax of expressions:

<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (nil)
         | (id <symbol>)
         | (conz <expr> <expr>)
         | (fun <pattern> <expr>)
         | (app <expr> <expr>)
         | (pmatch <expr> <match-clause> ...)

<match-clause> ::= [<pattern> <expr>]
|#
(deftype Expr
  (num n)
  (add l r)
  (nil)
  (id x)
  (conz l r)
  (fun p body)
  (app f arg)
  (pmatch expr clauses))

;;------------------;;
;; P1.b, P1.e, P2.b ;;
;;------------------;;


;; parse : s-expr -> Expr
;; parses a syntax expr into and expression
(define (parse s-expr)
  (match s-expr
    [(? number? n) (num n)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'nil) (nil)]
    [(list 'cons l r) (conz (parse l) (parse r))]
    [(list 'list elems ...)
     (foldr (lambda (e acc) (conz (parse e) acc))
            (nil)
            elems)]
    [(list 'fun pattern body) (fun (parse-pattern pattern) (parse body))]
    [(list 'match expr clauses ...)
     (if (null? clauses)
         (error "SyntaxError: match expression must have at least one case")
         (pmatch (parse expr)
                 (map (lambda (clause)
                        (match clause
                          [(list pat body)
                           (cons (parse-pattern pat) (parse body))]
                          [_ (error "SyntaxError: invalid match clause format")]))
                      clauses)))]
    [(list f-expr arg-expr) (app (parse f-expr) (parse arg-expr))]))

;;----- ;;
;; P1.c ;;
;;----- ;;

#|
<pattern> ::= (numP <num>)
            | (nilP)
            | (varP <symbol>)
            | (conzP <pattern> <pattern>)
|#
;; Datatype to represent a pattern on the language
(deftype Pattern
  (numP n)      
  (varP v)      
  (nilP)        
  (conzP p1 p2))

;;----- ;;
;; P1.d ;;
;;----- ;;

;; parse-pattern : s-expr -> Pattern
;; parses a syntax expr into a pattern
(define (parse-pattern s-expr)
  (match s-expr
    [(? number? n) (numP n)]                    
    [(list 'nil) (nilP)]                         
    [(? symbol? v) (varP v)]                     
    [(list 'cons e1 e2)                         
     (conzP (parse-pattern e1) (parse-pattern e2))]
    [(list 'list elems ...)                      
     (foldr (lambda (e acc) (conzP (parse-pattern e) acc))
            (nilP)
            elems)])) 

;;----- ;;
;; P1.f ;;
;;----- ;;

#|
<value> ::= (numV <num>)
          | (nilV)
          | (consV <value> <value>
          | (closureV <symbol> <value> <value>)
|#
;; Datatype to represent a value from the language
(deftype Value
  (numV n)
  (nilV)
  (consV l r)
  (closureV id body env))

#|
BEGIN utility definitions
|#

#|
<env> ::= (mtEnv)
       | (extEnv <sym> <value> <env>)
|#
(deftype Env
  (mtEnv)
  (extEnv x v env))

;; extend-env : Symbol Value Env -> Env
(define (extend-env x v env)
  (extEnv x v env))

;; empty-env : Env
(define empty-env (mtEnv))

;; extend-env* : (Listof (Symbol * Value)) Env -> Env
(define (extend-env* bindings env)
  (foldr
   (lambda (binding env) (match binding [(cons x v) (extend-env x v env)]))
   env
   bindings))

;; lookup-env : Symbol Env -> Value
(define (lookup-env x env)
  (match env
    [(mtEnv) (error "LookupError: variable ~a not found" x)]
    [(extEnv y v env) (if (equal? x y) v (lookup-env x env))]))

;; num+ : Value Value -> Value
(define (num+ v1 v2)
  (match v1
    [(numV n) (match v2
                [(numV m) (numV (+ n m))]
                [_ (error "TypeError: expected a number")])]
    [_ (error "TypeError: expected a number")]))

#|
END utility definitions
|#

;;----- ;;
;; P1.g ;;
;;----- ;;

#|
<result> e v ::= (failure e)
               | (success v)
|#
(deftype Result
  (failure e)
  (success v))

;; generate-substs : Pattern Value -> (Result String (Listof (Symbol * Value)))
;; subtitutes bindings on a pattern matching clause
(define (generate-substs p v)
  (match (list p v)
    [(list (numP n) (numV m))
     (if (= n m)
         (success '())
         (failure "MatchError: given number does not match pattern"))]
    [(list (varP x) val)
     (success (list (cons x val)))]
    [(list (nilP) (nilV))
     (success '())]
    [(list (conzP p1 p2) (consV v1 v2))
     (match (generate-substs p1 v1)
       [(failure e) (failure e)]
       [(success substs1)
        (match (generate-substs p2 v2)
          [(failure e) (failure e)]
          [(success substs2)
           (success (append substs1 substs2))])])]
    [(list (numP _) _) (failure "MatchError: expected a number")]
    [(list (nilP) _) (failure "MatchError: expected nil")]
    [(list (conzP _ _) _) (failure "MatchError: expected a cons constructor")]))

;;------------;;
;; P1.h, P2.c ;;
;;------------;;

;; interp : Expr Env -> Value
;; reduces parsed expression to a value from the language
(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(add l r) 
     (num+ (interp l env) (interp r env))]
    [(nil) (nilV)]
    [(id x) (lookup-env x env)]
    [(conz l r) 
     (consV (interp l env) (interp r env))]
    [(fun p body) 
     (closureV p body env)]
    [(app f-expr arg-expr)
     (let* ([f-val (interp f-expr env)]
            [arg-val (interp arg-expr env)])
       (match f-val
         [(closureV p body closure-env)
          (match (generate-substs p arg-val)
            [(failure e) (error e)]
            [(success bindings)
             (interp body (extend-env* bindings closure-env))])]))]
    [(pmatch expr clauses)
     (let ([val (interp expr env)])
       (define (match-clause clause)
         (match clause
           [(cons p body)
            (match p
              [(numP n)
               (if (and (numV? val) (equal? (numV-n val) n))
                   (interp body env)
                   #f)]
              [(nilP)
               (if (nilV? val)
                   (interp body env)
                   #f)]
              [(varP v)
               (interp body (extend-env v val env))]
              [(conzP p1 p2)
               (if (consV? val)
                   (match (generate-substs p1 (consV-l val))
                     [(failure e) #f]
                     [(success bindings1)
                      (match (generate-substs p2 (consV-r val))
                        [(failure e) #f]
                        [(success bindings2)
                         (interp body (extend-env* (append bindings1 bindings2) env))])])
                   #f)]
              )]))
       (define (find-match clauses)
         (if (null? clauses)
             (error "MatchError: expression does not match any pattern")
             (let ([result (match-clause (car clauses))])
               (if result
                   result
                   (find-match (cdr clauses))))))
       (find-match clauses))]))


;;----- ;;
;; P2.d ;;
;;----- ;;

#|
En función de lo implementado en la pregunta anterior, argumente porqué es útil que la función
generate-subst no lance un error (cuando el valor no calza con el patrón) y, en cambio, retorne
un mensaje.

R: Porque permite al programador saber el patrón específico que no se está cumpliendo, permitiendo
manejar los errores "clausula por clausula". Esto es particularmente útil por ejemplo en el caso de
cons, ya que este requiere matcheos anidados, y tener un mensaje específico a cada patrón permite saber
si el error es en el constructor cons o es en alguno de sus elementos que buscan matchear con otro
patrón.

Además, el mensaje que retornaría generate-substs se le daría al error que arroja interp y así se
puede generalizar esto sin hacer un matching con muchos casos de errores en interp. Luego si se quiere
agregar un nuevo error simplemente se extiende generate-substs.
|#
