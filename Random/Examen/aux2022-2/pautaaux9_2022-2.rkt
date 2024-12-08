#lang play

; P1
#;(define (interp expr env sto)
  (match expr
    ;...
    [(app fun-expr arg-expr)
     (def (v*s (closureV param body fenv)fun-sto) (interp fun-expr env sto))
     (def (v*s arg-val arg-sto) (interp arg-expr env fun-sto))
     (def arg-loc (next-location arg-sto))
     (interp body
             (extend-env param arg-loc fenv)
             (extend-sto arg-loc arg-val arg-sto))]))

; Call by calue porque el arg-loc se pasa como una nueva ubicación en la memoria en la linea
; (def arg-loc (next-location arg-sto))

; para cambiarla a la otra semantica hay que hacer lookup en vez de next location
; (lookup-env (id-x arg-expr) env)

;P2
;(a) ¿Puede implementar esta función en nuestro intérprete? ¿Qué hace falta?


;(b) Impleméntela en el intérprete

(define expr1 '(with {a 0}
                      {with {b 1}
                            {with {swap
                                   {fun {a}
                                        {fun {b}
                                             {with {tmp a}
                                                   {seqn
                                                    {set a b}
                                                         {set b tmp}}}}}}
                                  {seqn {{swap a} b} a}}}))
; responde 0

(define expr2 '(with {a 0}
                      {with {b 1}
                            {with {swap
                                   {fun {a}
                                        {fun {b}
                                             {with {tmp a}
                                                   {seqn
                                                    {set a b}
                                                         {set b tmp}}}}}}
                                  {seqn
                                   {with {tmp a}
                                         {seqn
                                          {set a b}
                                          {set b tmp}}}
                                   a}}}))
; responde 1 (copiamos el body donde estaba swap)

(define expr3 '(with {a 0}
                      {with {b 1}
                            {with {swap
                                   {refun {a}
                                        {refun {b}
                                             {with {tmp a}
                                                   {seqn
                                                    {set a b}
                                                         {set b tmp}}}}}}
                                  {seqn {{swap a} b} a}}}))
; responde 1 con call by ref, las mutaciones en el cuerpo de una funcion afectan el resultado 

;(c) ¿Puede implemetarla en Racket?

(define (swap a b)
  (let ([tmp a])
    (begin
      (set! a b)
      (set! b tmp))))

(let ([a 0] [b 1])
  (begin
    (swap a b)
    a))

(let ([a 0] [b 1])
  (begin
     (let ([tmp a])
    (begin
      (set! a b)
      (set! b tmp)))
    a))

; P3
; los 4 implementan call by value

; P4
; Se puede implementar, pero no se puede añadir corto-circuito. Esto rompería con la semántica de call by value