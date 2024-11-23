#lang play

; Aux12
; p2.
(defmac (switch input
                [case pred -> result]
                ...
                [default -> default-result])
  #:keywords case default ->
  (cond
    [(pred input) result] ...
    [else default-result]))

; p3.
; VER PAUTA
; el ? es un getter

; p4.
(defmac ND-FSA
  (init : initial-state)
  (final : final-states ...)
  (transitions : [state : ]))