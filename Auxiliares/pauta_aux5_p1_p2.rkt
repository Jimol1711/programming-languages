#lang play


(print-only-errors #t)

(require "base_aux5.rkt")

;; -----------AUX5---------



;;P1) free-vars

;;free-aux :: Expr List -> ListOf[Symbol]
;; recibe la lista de bindings hasta el momento y si hay una variable libre la guarda
(define (free-aux expr lista)
  (match expr
    [(num n) '()]
    [(id x) (if (member x lista)
                '()
                (list x))]
    [(add l r) (append (free-aux l lista) (free-aux r lista))]
    [(sub l r) (append (free-aux l lista) (free-aux r lista))]
    [(if0 c t f) (append (free-aux c lista) (free-aux t lista) (free-aux f lista))]
    [(with x e b) (append (free-aux e lista) (free-aux b (append lista (list x)))) ]
    ))
    
;;free-vars :: Expr -> ListOf[Symbol]
;;retorna la lista de variables libres en una expresión
(define (free-vars expr)
  (free-aux expr '()))
  

(test (free-vars (parse '{with {x 3} {+ x x}})) '())
(test (free-vars (parse '{with {x y} {+ x x}})) '(y)) ;;free var in binding
(test (free-vars (parse '{with {x {with {a b} {+ a 3}}} {+ x x}})) '(b)) ;;free var in with inside a binding
(test (free-vars (parse '{with {x y} {+ x y}})) '(y y)) ;;free var in binding with repeated
(test (free-vars (parse '{with {x 3} {+ x y}})) '(y)) ;;free var in body
(test (free-vars (parse '{with {x y} {+ x z}})) '(y z)) ;;free var in body y binding
(test (free-vars (parse '{with {x 3} {with {x y} {+ x x}}})) '(y)) ;;free var in binding in second level
(test (free-vars (parse '{with {x 3} {with {x 2} {+ x z}}})) '(z)) ;;free var in body in second level
(test (free-vars (parse '{with {x 3} {with {x y} {+ x z}}})) '(y z));;free var in binding y body in second level



;-------------
;;P2
#|
Tenemos syntactic sugar con with* que lo escribe el programador y queremos pasarlo
a withs concatenados, un ejemplo sería

{with* {{x 3} {y 2}}
      {+ x y}}

que debería pasar a ser
{with {x 3}
    {with {y 2}
          {+ x y}}}

debemos programar la funcion unrolling_with*()
que recibe la expresion en sintaxis concreta
|#

;; unrolling_with* :: s-expr -> s-expr
;desenrolla el syntactic sugar de with* a withs concatenados
(define (unrolling-with* s-expr)
  (match s-expr
    [(list 'with* '() body) (unrolling-with* body)]
    [(list 'with* (list primer_binding resto_bindings ...) body) ;;se hace un unrolling_with* de primer_binding por si es un with*, si no seria primer_binding simplemente
                                                                (list 'with (list (car primer_binding) (unrolling-with* (car (cdr primer_binding))))
                                                                       (unrolling-with* (list 'with* resto_bindings body)))]
    [_ s-expr]
    ))


;;2 bindings
(test (unrolling-with* '{with* {{x 3} {y 2}} {+ x y}} ) '{with {x 3}
                                                               {with {y 2}
                                                                     {+ x y}}})
;;2 bindings and a with in the body
(test (unrolling-with* '{with* {{x 3} {y 2}} {with {z 3} {+ x y z}}}) '{with {x 3}
                                                                             {with {y 2}
                                                                                   {with {z 3} {+ x y z}}}})


;;2 bindings and a with in the binding
(test (unrolling-with* '{with* {{x {with {a 3} a}} {y 2}} {with {z 3} {+ x y z}}}) '{with {x {with {a 3} a}}
                                                                                          {with {y 2}
                                                                                                {with {z 3} {+ x y z}}}})
;;2 bindings and a with* in the binding
(test (unrolling-with* '{with* {{x {with* {{a 3} {b 1}} {+ a b}}} {y 2}} {with {z 3} {+ x y z}}}) '{with {x {with {a 3} {with {b 1} {+ a b}}}}
                                                                                                         {with {y 2}
                                                                                                               {with {z 3} {+ x y z}}}})

;;2 bindings and a with* in the body
(test (unrolling-with* '{with* {{x 3} {y 2}} {with* {{z 3} {a 4}} {+ x y z a}}}) '{with {x 3}
                                                                                      {with {y 2}
                                                                                            {with {z 3}
                                                                                                  {with {a 4}
                                                                                                    {+ x y z a}}}}})
;; 3 bindings 
(test (unrolling-with* '{with* {{x 3} {y 2} {z 3}} {+ x y z}} ) '{with {x 3}
                                                                     {with {y 2}
                                                                           {with {z 3}
                                                                                 {+ x y z}}}})






