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
<expr> ::= ...
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | ...
|#
(deftype Expr
  ; ...
  (add l r)
  (sub l r)
  (if0 c t f)
  ; ...
  )

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= ...
        | (+ <s-expr> <s-expr>)
        | (- <s-expr> <s-expr>)
        | (if0 <s-expr> <s-expr> <s-expr>)
        | ...
|#

;; parse : <s-expr> -> Expr

(define (parse s-expr) '???)

;;----- ;;
;; P2.c ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
(define (subst in what for) '???)

;;----- ;;
;; P2.d ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v) '???)

;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2) '???)

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2) '???)

;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v) '???)


;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
(define (interp expr) '???)
