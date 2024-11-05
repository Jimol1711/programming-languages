#lang play

;; Aux 9 done by me

; P1
; a) 15
(with (b (newbox 5))
      (+ (with (n (openbox b))
               (seqn
                (setbox b (+ n n))
                 n))
         (openbox b)))

; b) 20
(with (acum (newbox 0))
      (with (sum (fun (n)
                      (seqn
                       (setbox acum (+ n (openbox acum)))
                       (openbox acum))))
            (+ (sum 5) (sum 10))))

; c) 19
(with (b (box 1))
      (with (f (fun (x)
                    (with (e (openbox b))
                          (seqn (setbox x 18) e))))
            (+ (f b) (f b))))

; d) 1
(with (switch (newbox 0))
      (with (toggle (fun (dummy)
                         (if0 (openbox switch)
                               (seqn (setbox switch 1) 1)
                               (seqn (setbox switch 0) 0))))
            (+ (toggle 5) (toggle 6))))


