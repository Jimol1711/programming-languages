#lang play
#|

       SUPER PAUTA - Auxiliar #5

================================================
P2
================================================

-- 2.a
(define (kim-possible)
  (let ([x (begin (print "Call Me ") 1)]
        [y (begin (print "Beep Me ") 2)]
        [z (begin (print "If you wanna reach me ") 3)])
    (+ z y x (+ z y))))

Temprana:     Call Me Beep Me If you wanna reach me 11
Call-by-name: If you wanna reach me Beep Me Call Me If you wanna reach me Beep Me 11
Call-by-need: If you wanna reach me Beep Me Call Me 11



-- 2.b
(define (correcaminos)
  (let ([x (begin (print "bip ") 1)])
    (+ x x x x)))

Temprana:     bip 4
Call-by-name: bip bip bip bip 4
Call-by-need: bip 4



-- 2.c
(define (oh-no?)
  (let ([x (begin (error "Oh no") 1)]
        [y (begin (print "Yeah") 2)])
    (+ y y)))

Temprana:     Error: Oh no
Call-by-name: Yeah Yeah 4
Call-by-need: Yeah 4


|#