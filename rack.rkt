#lang play

(define x (box 10))  ; x is a variable in the environment that points to a box.
                      ; The box's initial content in the store is 10.

(define (update-box new-val)
  (set-box! x new-val)) ; Changes the content of the box, not the variable x itself.

(define (display-box)
  (displayln (unbox x))) ; Accesses the current content of the box.

; Static scope preserved:
(display-box)            ; Outputs: 10

(update-box 20)          ; Dynamically changes the box's content in the store.

(display-box)            ; Outputs: 20
