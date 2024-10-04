#lang play

(print-only-errors #t)

;;syntaxys concreta


#|
 <expr> ::= (num <num>)
           ;; (add <expr <expr>)
           ;; (sub <expr> <expr>)
           ;; (if0 <expr> <expr> <expr>
           ;; (mult <expr> <expr>)
           ;; (div <expr> <expr>)
           | (with <id> <expr> <expr>)
           | (id <id>)

|#

;parseado en el AST
(deftype Expr
  (num n)
  (id x)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x e body)
  )

#|
<s-expr> ::= <num>
           | <symbol>
           | (list '+ <s-expr> <s-expr>)
           | (list '- <s-expr> <s-expr>)
           | (list 'if0 <s-expr> <s-expr> <s-expr>)
           | (list 'with { <symbol> <s-expr> } <s-expr>)

|#


;; parse :: s-expr -> Expr
;; parsea el codigo a un AST
(define (parse s-expr)
  (match s-expr
    [ x #:when (number? x) (num x) ]
    [ x #:when (symbol? x) (id x) ]
    ;; [ x (? number?) (num x)] esto es equivalente a lo de arriba
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f)) ]
    [(list 'with (list x e) b) (with x (parse e) (parse b)) ]
    ))


(test (parse '1) (num 1))
(test (parse '2) (num 2))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{- {+ 1 2} 1}) (sub (add (num 1) (num 2)) (num 1)) )
(test (parse '{if0 {- 2 1} 1 2}) (if0 (sub (num 2) (num 1)) (num 1) (num 2)) )


;;subst:: Expr Symbol Expr -> Expr
;;recibe una expresion como (with ( ('x (num 3)) (add (id 'x) (num 1))
;; y devuelve la expresion con x substituido  (with ( ('x (num 3)) (add (num 3) (num 1))
;; pero ojo si tenemos dos with juntos y se repite el simbolo, por ejemplo
;;{with {'x 3} {with {'x {+1 x}} {+ x x }}
;; en ese caso, el binding el segundo 'x debe tomar el valor 4 = 1+3
;; pero para el cuerpo final, + x x, debe ser 8, y no tomar el valor de x=3
(define (subst in what for)
  (match in
    [ (num x) x ]
    [ (id x) (if (equal? x what)
                 for
                 (id x)) ]
    [(add l r) (add (subst l what for) (subst r what for)) ]
    [(sub l r) (sub (subst l what for) (subst r what for)) ]
    [(if0 c t f) (if0 (subst c what for)
                     (subst t what for)
                     (subst f what for)
                     )]
    [(with x e body) (if (equal? x what)
                         (with x (subst e what for) body)
                         (with x (subst e what for) (subst body what for))
                         )]
    ))

                         

;;interp:: Expr -> Number
;;interpreta la expresion
(define (interp expr)
  (match expr
    [ (num x) x ]
    [ (id x) (error 'expr "free variable ~a" x) ]
    [(add l r) (+ (parse l) (parse r)) ]
    [(sub l r) (- (parse l) (parse r))]
    [(if0 c t f) (if (equal? (parse c) 0)
                     (parse t)
                     (parse f))]
    [ (with x e b)
      (interp (subst b x (num (interp e))))
      ]
    ))

