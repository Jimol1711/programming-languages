#lang play

;; Juan Ignacio Molina - Sección 1

#|

Hizo Ud uso de la whiteboard policy: NO
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;


;;----- ;;
;; P1.a ;;
;;----- ;;


#|
Abstract syntax of propositions:

<prop> ::= (tt)
         | (ff)
         | (p-not <prop>)
         | (p-and (listof <prop>))
         | (p-or (listof <prop>))
         | (p-id <sym>)
         | (p-where <prop> <sym> <prop>)
|#
(deftype Prop
  (tt)
  (ff)
  (p-not Prop)
  (p-and (props))
  (p-or (props))
  (p-id id)
  (p-where res name value))


;;----- ;;
;; P1.b ;;
;;----- ;;

#|
Concrete syntax of propositions:

<s-prop> ::= true
          | false
          | (list 'not <s-prop>)
          | (list 'and <s-prop>+)                    
          | (list 'or <s-prop>+)                     
          | id                                       
          | (list <s-prop> 'where (list id <s-prop>))
|#

;; parse-prop : <s-prop> -> Prop
;; Parser of concrete syntax of a proposition
(define (parse-prop s-expr)
  (match s-expr
    ['true (tt)]
    ['false (ff)]
    [(list 'not p) (p-not (parse-prop p))]
    [(list 'and elems ...)
     (if (>= (length elems) 2)
         (p-and (map parse-prop elems))
         (error "parse-prop: and expects at least two operands"))]
    [(list 'or elems ...)
     (if (>= (length elems) 2)
         (p-or (map parse-prop elems))
         (error "parse-prop: or expects at least two operands"))]
    [(? symbol? id) (p-id id)]
    [(list expr 'where (list id expr2)) 
     (p-where (parse-prop expr) id (parse-prop expr2))]))


;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<value> ::= (ttV)
          | (ffV)
          | (andV (listof <value>))
          | (orV (listof <value>))
          | (idV <sym>)
          | (whereV <value> <sym> <value>)
|#
;; Datatype to represent the values captured by the language
(deftype PValue
  (ttV)
  (ffV)
  (andV (pvalues))
  (orV (pvalues))
  (idV sym)
  (whereV resV nameV valueV))

;; from-Pvalue : PValue -> Prop
;; Function to transform a PValue to a Prop type
(define (from-Pvalue p-value)
  (match p-value
    [(ttV) (tt)]
    [(ffV) (ff)]
    [(andV values)
     (let ([props (map from-Pvalue values)])
       (if (< (length props) 2)
           (error "from-Pvalue: andV expects at least two operands")
           (p-and props)))]
    [(orV values)
     (let ([props (map from-Pvalue values)])
       (if (< (length props) 2)
           (error "from-Pvalue: orV expects at least two operands")
           (p-or props)))]
    [(idV id) (p-id id)]
    [(whereV resV nameV valueV)
     (p-where (from-Pvalue resV) nameV (from-Pvalue valueV))]))


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
;; Substitutes a proposition for it's identifier
(define (p-subst target name substitution)
  (match target
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-id id) (if (symbol=? id name)
               substitution
               (p-id id))]
    [(p-not p)
     (p-not (p-subst p name substitution))]
    [(p-and props)
     (p-and (map (λ (p) (p-subst p name substitution)) props))]
    [(p-or props)
     (p-or (map (λ (p) (p-subst p name substitution)) props))]
    [(p-where res id value)
     (if (symbol=? id name)
         (p-where res id value)
         (p-where
          (p-subst res name substitution)
          id
          (p-subst value name substitution)))]))


;;----- ;;
;; P1.e ;;
;;----- ;;


;; eval-or : (Listof Prop) -> PValue
;; Implements short circuit behaviour for a list of props on an or proposition
(define (eval-or ps)
  (match ps
    ['() (ffV)]
    [(list p elems ...)
     (match (p-eval p)
       [(ttV) (ttV)]
       [(ffV) (eval-or elems)])])) 

;; eval-and : (Listof Prop) -> PValue
;; Implements short circuit behaviour for a list of props on an and proposition
(define (eval-and ps)
  (match ps
    ['() (ttV)]
    [(list p elems ...)
     (match (p-eval p)
       [(ffV) (ffV)]
       [(ttV) (eval-and elems)])]))

;; p-eval : Prop -> PValue
;; Reduces a proposition to a PValue
(define (p-eval p)
  (match p
    [(tt) (ttV)]
    [(ff) (ffV)]
    [(p-id id) (error 'p-eval (format "unbound identifier: ~a" id))]
    [(p-not prop)
     (match (p-eval prop)
       [(ttV) (ffV)]
       [(ffV) (ttV)])]
    [(p-and props)
     (eval-and props)]
    [(p-or props)
     (eval-or props)]
    [(p-where res id value)
     (p-eval (p-subst res id value))]))

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;

#|
Abstract syntax of expressions:

<expr> ::= (real <num>)
         | (imaginary <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with [<def>*] <expr>)
         | (id <sym>)

<def> ::= (<sym> <expr>)
|#
(deftype Expr
  (real r)
  (imaginary i)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with bindings body)
  (id x))

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<defs> ::= <def> | <def> <defs>

<def> ::= (list <sym> <s-expr>)

<s-expr> ::= <num>
           | (<num> i)
           | (+ <s-expr> <s-expr>)
           | (- <s-expr> <s-expr>)
           | (if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with <defs> <s-expr>)
           | <sym>
|#

;; parse : <s-expr> -> Expr
;; Parser of concrete syntax of an expression
(define (parse s-expr)
  (match s-expr
    [(? number? n) (real n)]
    [(list n 'i) (imaginary n)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f)
     (if0 (parse c) (parse t) (parse f))]
    [(list 'with bindings body)
     (if (empty? bindings)
         (error "parse: 'with' expects at least one definition")
         (with (map (λ (binding)
                      (match binding
                        [(list (? symbol? x) expr) (cons x (parse expr))]
                        [_ (error "parse: invalid binding format in 'with'")]))
                    bindings)
               (parse body)))]))

;;----- ;;
;; P2.c ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
;; Performs substitution of an identifier for an expression
(define (subst in what for)
  (match in
    [(real n) (real n)]
    [(imaginary n) (imaginary n)]
    [(add l r) (add (subst l what for) (subst r what for))]
    [(sub l r) (sub (subst l what for) (subst r what for))]
    [(if0 c t f) (if0 (subst c what for)
                      (subst t what for)
                      (subst f what for))]
    [(id x) (if (symbol=? x what)
                for
                (id x))]
    [(with bindings body)
     (define (subst-bindings bindings defined-vars)
       (match bindings
         [(cons (cons x expr) rest)
          (let ([new-expr (if (member what defined-vars)
                              expr
                              (subst expr what for))])
            (cons (cons x new-expr)
                  (subst-bindings rest (cons x defined-vars))))]
         ['() '()]))
     (with (subst-bindings bindings '())
           (if (member what (map car bindings))
               body
               (subst body what for)))])) 

;;----- ;;
;; P2.d ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#
;; Datatype to represent complex numbers by their two components
(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
;; Transforms a CValue into an expression from the language
(define (from-CValue v)
  (match v
    [(compV r i)
     (cond
       [(and (not (= r 0)) (not (= i 0)))
        (add (real r) (imaginary i))]
       [(not (= r 0)) (add (real r) (imaginary 0))]
       [(not (= i 0)) (add (real 0) (imaginary i))]
       [else (add (real 0) (imaginary 0))])]))

;; cmplx+ :: CValue CValue -> CValue
;; Adds two CValues with complex numbers rules, returning the corresponding CValue
(define (cmplx+ v1 v2)
  (match (list (from-CValue v1) (from-CValue v2))
    [(list (add (real r1) (imaginary i1))
           (add (real r2) (imaginary i2)))
     (compV (+ r1 r2) (+ i1 i2))]))

;; cmplx- :: CValue CValue -> CValue
;; Subs two CValues with complex numbers rules, returning the corresponding CValue
(define (cmplx- v1 v2)
  (match (list (from-CValue v1) (from-CValue v2))
    [(list (add (real r1) (imaginary i1))
           (add (real r2) (imaginary i2)))
     (compV (- r1 r2) (- i1 i2))]))

;; cmplx0? :: CValue -> Boolean
;; Returns true if the complex represent by the given CValue is 0, false otherwise
(define (cmplx0? v)
  (match v
    [(compV r i) (and (= r 0) (= i 0))]
    [_ #f]))

;;----- ;;
;; P2.e ;;
;;----- ;;

;; Helper function to find the binding in the environment
(define (find-binding x env)
  (match env
    ['() #f] ;; Not found
    [(cons (cons y value) rest)
     (if (equal? x y)
         value
         (find-binding x rest))]))

;; Helper function to perform substitution over a list of definitions
(define (subst-bindings body bindings)
  (define (subst-one body binding)
    (match binding
      [(cons x expr) ;; Ensure binding is a pair
       (subst body x (from-CValue (interp expr)))] ;; Substitute the binding value
      [else (error 'subst-one "Invalid binding format: ~a" binding)])) ;; Error if binding is not a pair
  
  (foldl subst-one body bindings))

;; interp : Expr -> CValue
;; Reduces an expression Expr to a value of the language CValue
(define (interp expr)
  (match expr
    [(real r) (compV r 0)]
    [(imaginary i) (compV 0 i)]
    [(add l r)
     (cmplx+ (interp l) (interp r))]
    [(sub l r)
     (cmplx- (interp l) (interp r))]
    [(if0 c t f)
     (if (cmplx0? (interp c))
         (interp t)
         (interp f))]
    [(with bindings body)
     ;; Check the structure of bindings
     (printf "Bindings: ~a\n" bindings) ;; Debug output
     ;; Substitute the bindings into the body
     (let ([new-body (subst-bindings body bindings)])
       (interp new-body))]
    [(id x) (error 'interp "unbound identifier ~a" x)]))
