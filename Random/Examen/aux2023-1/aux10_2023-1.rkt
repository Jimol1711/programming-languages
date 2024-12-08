#lang play

; P2.
; a)
;
;{with {v 0}
;      {with {f {fun {y} {set y 5}}}
;            {seqn {f v}
;                  v}}}
;
; call by value da 0 y reference da 5

; b) value, ya que usa una new-loc

; c) habría que buscar la locación existente

; P3.
(defmac (DOUBLE a)
  (set-box! a (* (unbox a) 2)))
; a) da 10

(defmac (DOUBLE-ERROR a)
  #:captures a
  (set-box! a (* (unbox a) 2)))
; b) da error

; P4.
(defmac (unless guard do body)
  #:keywords do
  (letrec ([f (lambda () (when (not guard)
                              (begin
                                body
                                (f))))])
    (f)))

; P5. YA HECHA
