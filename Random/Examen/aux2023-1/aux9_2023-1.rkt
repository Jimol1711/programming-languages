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

; P2.
; a)
#|
(sum 4)
(+ 4 (sum 3))
(+ 4 (+ 3 (sum 2)))
(+ 4 (+ 3 (+ 2 (sum 1))))
(+ 4 (+ 3 (+ 2 (+ 1 (sum 0)))))
(+ 4 (+ 3 (+ 2 (+ 1 0))))
(+ 4 (+ 3 (+ 2 1)))
(+ 4 (+ 3 3))
(+ 4 6)
10

En cada llamado a sum se crea un nuevo frame
|#

; b)
; No lo es
; Version TR
(define (sumTR n)
  (define (sum-aux n acc)
    (if (equal? n 0)
        acc
        (sum-aux (- n 1) (+ acc n))))
  (sum-aux n 0))

(define (evenTR n)
  (define (even-aux n acc)
    (cond
      ((= n 0) acc)
      ((= n 1) (not acc))
      (else (even-aux (- n 1) (not acc)))))
  (even-aux n #t))

; TR fibonacci
(define (fibsTR n)
  (define (fibs count curr next)
    (if (<= count 1)
        curr
        (fibs (- count 1) next (+ curr next))))

  (fibs n 0 1))

; P3.
; 1. Como retorna un v*s, dara un error el aplicar num-zero? a eso. Se debe extraer el v y aplicarselo a eso
; 2. Para arreglarlo hacer un def que extraiga el value al hacer interp de la condición y usar ese value con el numzero?
; Hacer programa que falla si no se actualiza el store en las ramas
; (with (b (newbox 5))
;       (if0 (with (_ (setbox b 10)) 0)
;            (openbox b)
;            (+ 1 (openbox b)))
; 3. OJO también hay que preocuparse de darle a las ramas verdadera y falsa el store actualizado obtenido

; P4.
