#lang play

; P1.
; a) Sí ya que si no se tendrían que usar algún patrón de auto aplicación o combinador de punto fijo
; b) ? 
; c) Lo de siempre

; P2.
; a) Cualquier función sin llamado por la cola
; b) Cualquier función recursiva por la cola
; c) Cualquier función con llamado por la cola no recursivo

; P3.
; Continuation passing style

; P4.
; a) Se ve
; b)
(define (concat l1 l2)
  (concatTR (reverse l1) l2))
                
(define (concatTR l1 l2)
  (match l1
    [(list) l2]
    [(cons x xs) (concatTR xs (cons x l2))]))