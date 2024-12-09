#lang play

; -----THINGS-----
; Program to understand difference between call by value (copy on the argument) and call by reference
; Difference is that one looks for the already existing location (reference)
;
;{with {v 0}
;      {with {f {fun {y} {set y 5}}}
;            {seqn {f v}
;                  v}}}
;
; call by value is 0 and reference is 5

; -----CODES-----
; See the plai interpreters on the courses page

; On folder pautaaux3_2022-1:
; first-order.rkt and first-class.rkt are languages with first order and first class functions respectively and both static and dynamic scope evals

; Types of macros for objects
; Object without forwarding or error handling when message not understood
(defmac (OBJECT-SIMP ([field fname init] ...)
                     ([method mname args body] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname init] ...)
              (let ([methods (list (cons 'mname (λ args body)) ...)])
                (λ (msg . vals)
                  (apply (cdr (assoc msg methods)) vals))))])
    self))

; Object without forwarding
; Object that if it doesn't understand a message, simply returns an error
(defmac (OBJECT ([field fname init] ...)
                ([method mname args body] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname init] ...)
              (let ([methods (list (cons 'mname (λ args body)) ...)])
                (λ (msg . vals)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) vals)
                        (error "message not understood:" msg))))))])
    self))  

; Object with forwarding
; Object that receives another object to which is forwards messages it doesn't understand
(defmac (OBJECT-FWD target
                    ([field fname fval] ...)
                    ([method mname mparams mbody ...] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname fval] ...)
              (let ([methods (list (cons 'mname (λ mparams mbody ...)) ...)])
                (λ (msg . args)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) args)
                        (apply target msg args))))))])
    self))

(define root
  (λ (msg . args)
    (error "message not understood" msg)))

(defmac (--> o m arg ...)
  (o 'm arg ...))

; Object with delegation
; Object that receives a parent object to which it forwards messages to and parameterizes each method with itself as to allow for a dynamic self
(defmac (OBJECT-DEL parent
                    ([field fname fval] ...)
                    ([method mname (mparam ...) mbody ...] ...))
  #:keywords field method
  #:captures self
  ; difference: doesn't use letrec for self
  (let ([fname fval] ...)
    (let ([methods
           (list (cons 'mname (λ (self mparam ...) mbody ...)) ...)]) ; difference: parameterizes with self instead of letrec
      (λ (current) ; difference: first gives the current object and then return the λ
        (λ (msg . args)
          (let ([found (assoc msg methods)])
            (if found
                (apply (cdr found) (cons current args))
                (apply (parent current) msg args)))))))) ; difference: if message is not understood, it must send the current receiver along with it's parent

(defmac (-> o m arg ...)
  (let ([obj o]) ; OJO aquí se declara obj para forzar la evaluación de o. Sería incorrecto (o o) porque se evaluaría dos veces
    ((obj obj) 'm arg ...)))

; CLASS Macro defined on class 24

; Final CLASS macro on Clase 24 and slides with ? and ! defined inside
