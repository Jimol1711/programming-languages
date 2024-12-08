#lang play

; P1.
; a)
;;  (with (b (box 5))
;;        (+ (with (n (openbox b))
;;                 (seqn
;;                  (setbox b (+ n n)) n))
;;           (openbox b)))
; setea la caja en 10 y retorna 5, luego le suma lo de la caja que era 10, luego retorna 15

; b)
;; (with (acum (box 0))
;;       (with (sum (fun (n)
;;                       (seqn
;;                        (setbox acum (+ n (openbox acum)))
;;                        (openbox acum))))
;;             (+ (sum 5) (sum 10))))
; devuelve 20 porque primero da 5 y despues a 5 le suma 10, sumando 5 + 15 = 20

; c) y d) más de lo mismo