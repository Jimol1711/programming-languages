#lang play

; a)
; No porque una función eager puede tener muchos argumentos que requieren evaluación, y en un lazy no se evalúan porque no se ocupan.

; b)
; No, porque se calculan todos los argumentos

; c)
; No, ya que es una Macro. No es función por el cortocircuito.Lo mismo con el or. Esto se hace por eficiencia. 

; d)
; Eager

(define (f n)
  1)

(f (begin (println "hola soy eager")
          1)) ; Como si printea, es eager

; e)
; Lazy, ejemplo de la clase:
;; f :: Int -> Int
;; f n = 1
;;
;; main = do
;;        (print (f (head [])))
          ; head de lista vacía da error, pero este programa no lo da => es Lazy

; f)
; En eager da error de que no existe y (unbound y)
; En lazy da 47 (Ojo, lazy es más eficiente en tiempo, pero no necesariamente en memoria)

; g)
; En eager no se puede, por lo mismo que el if, and y or.
; Debe ser lazy. Debe ser by name también por que si no la condición se calcula una pura vez.

; h)
; closures or promises. Clausura es la expr, el ambiente y en las by need es el cache (box)
; exprV e env (box ...), el env se usa para guardar lo evaluado en los argumentos

; P2

; a)
; Call me 
; Beep me
; If you wanna reach me
; 11

; ...

; b)
; ...

; P3

; en strict
; debe hacer strict al resultado interpretado
; (let ([val (strict (interp expr env))]))

; en interp
; falta strict
; [(if0 c t f) (if (numV-zero? (strict (interp c env)))
; también, el mtEnv en (app f e) debería ser new-env
; (def new-env (extend-env the-arg (exprV e env (box #f)) closed-env))


; en run
; falta strict (strict (interp (parse s-expr) (empty-env))))

; P4

; en Haskell buu
; Haskell es lazy, curryfied, static.
