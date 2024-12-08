#lang play

; P1.
; El problema es que los methods se están definiendo antes que los fields, por lo que los methods no conocen los fields. Se deberían definir primero los fields y luego los métodos.
(defmac (OBJECT ([field fname init] ...)
                ([method mname args body] ...))
  #:keywords field method
  (let ([methods (list (cons 'mname (λ args body)) ...)])
     (let [(fname init) ...]
        (λ (msg . vals)
          (let ([found (assoc msg methods)])
             (if found
                  (apply (cdr found) vals)
                  (error "Method not found")))))))

(defmac (-> o m arg ... )
  (o 'm arg ... ))

; Objeto donde no se manifiesta el problema, ya que el método f no usa el field x
;; (define no-problem-object
;;   (OBJECT ([field x 1])
;;           ([method f () 1])))

; Objeto donde se manifiesta el problema, ya que el método f usa el field x
;; (define problem-object
;;   (OBJECT ([field x 1])
;;           ([method f () x])))

; Para arreglarlo solo se invierten las líneas 4 y 5

; P2.
; En el caso 1. da error y en el segundo da true
; En el caso 1. da error ya que el self no está parametrizado en los métodos. Esto genera que el objeto envíe un mensaje al comparer, más este con self se refiere a si mismo y da
; el compare de el Comparer
; En el caso 2. da true ya que el self si está parametrizado por lo que al recibir el mensaje < también recibe el objeto que debe enviar el mensaje compare, y como este objeto era un
; (make-string "much") entiende el mensaje como el método de un objeto de este tipo.

; P3.
; 1.
(define bool (λ (b) (OBJECT () ([method interp () b]))))
(define num (λ (n) (OBJECT () ([method interp () n]))))
(define add (λ (l r) (OBJECT () ([method interp () (+ (-> l interp)
                                                (-> r interp))]))))
(define ifc (λ (c t f) (OBJECT () ([method interp ()
                                           (if (-> c interp)
                                               (-> t interp)
                                               (-> f interp))]))))
