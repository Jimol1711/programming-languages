#lang play

; P1
#;(define (interp expr env sto)
  (match expr
    ...
    [ (add l r)
      (def (v*s vl l-sto ) (interp l env sto))
      (def (v*s vr r-sto ) (interp r env l-sto ))
      (v*s (numV+ vl vr) r-sto)]))


#;'(with (b (newbox 0))
     (+ (seqn (setbox b 5) (openbox b)) (openbox b)))


#;(define (interp expr env sto)
  (match expr
    ...
    [ (add l r)
      (def (v*s vl r-sto ) (interp r env sto)) ;; esta linea cambia
      (def (v*s vr l-sto ) (interp l env r-sto )) ;; esta linea cambia
      (v*s (numV+ vl vr) l-sto)])) ;; esta linea cambia

