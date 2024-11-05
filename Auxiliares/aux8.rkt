#lang play

(print-only-errors #t)

; (require "base_aux8.rkt")

; P1.
; a)
;; cyclic-list :: ListOf[A] -> ListOf[A/Box(L)]
;; returns list as a cyclic version of it
(define (cyclic-list the-list)
  (define placeholder (box #f))
  (define the-cyclic-list (append the-list (list placeholder)))
  (set-box! placeholder the-cyclic-list)
  the-cyclic-list)

; b)
;; take-list-n :: ListOf[A/L] Int -> ListOf[A]
(define (take-list-n l n)
  (match n
    [0 '()]
    [_ (if (box? (car l))
           (append (list (car (unbox (car l)))) (take-list-n (cdr (unbox (car l))) (- n 1)))
           (append (list (car l)) (take-list-n (cdr l) (- n 1))))])) ; TERMINAR

(test (take-list-n (cyclic-list (list 1 2 3)) 5) (list 1 2 3 1 2))
(test (take-list-n (cyclic-list (list 1 2 3)) 7) (list 1 2 3 1 2 3 1))

; c)
;; cyclic-list-to-mcons :: ListOf[A] -> Mcons(A ...)
(define (cyclic-list-to-mcons lst)
  (if (null? lst)
      '() ; Return an empty list if the input is empty
      (let* ([head (mcons (car lst) '())] ; Create the head of the list
             [current head]) ; Start building from the head
        ;; Loop through the list, creating mcons pairs
        (for-each (lambda (x)
                    (let ([new-node (mcons x '())])
                      (set-mcdr! current new-node)
                      (set! current new-node)))
                  (cdr lst))
        ;; Create the cycle by pointing the last node's cdr to the head
        (set-mcdr! current head)
        head)))

; d)
;; take-mcons-n :: Mcons(A...) Int -> ListOf[A]
(define (take-mcons-n mut-pair n)
  (if (zero? n)
      '()
      (append (list (mcar mut-pair)) (take-mcons-n (mcdr mut-pair) (- n 1)))))

(test (take-mcons-n (mcons 1 (mcons 2 '())) 1) (list 1))
(test (take-mcons-n (mcons 1 (mcons 2 (mcons 3 '()))) 2) (list 1 2))
(test (take-mcons-n (mcons 5 (mcons 2 (mcons 3 (mcons 4 '())))) 3) (list 5 2 3))

(define (interp a b) (void)) ; interprete
(define box-extend-env (void)) ; debería ir aEnv

; P2.
(define (cyclic-env2 id1 fun-expr1 id2 fun-expr2 env)
  (define p1 (box #f))
  (define p2 (box #f))
  (define new-env (box-extend-env id1 p1
                                  (box-extend-env id2 p2 env)))
  (define fun-val1 (interp fun-expr1 new-env))
  (define fun-val2 (interp fun-expr2 new-env))
  (set-box! p1 fun-val1)
  (set-box! p2 fun-val2)
  new-env)

; P3.
; a)
; recursión por la cola es un caso particular de llamado por la cola, donde la función llamada en el retorno de la función es la misma función (recursión)

; b)
; no TCO ni TRO
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

; TRO
(define (factTRO n)
  (define (fact-auxTRO n acc)
    (if (zero? n)
        acc
        (fact-auxTRO (- n 1) (* n acc))))
  (fact-auxTRO n 1))

; TCO and not directly TRO
; el even y el odd del ejemplo

; P4.
(define (par? n)
  (define (parTRO n acc)
  (cond
    [(= n 0) #t]
    [else (parTRO (- n 1) (not acc))]))
  (parTRO n #t))

; transformación

(deftype Expr
  (num n)
  (add l r))

;; <expr> ::= <num>
;;          | (+ <num> <num>)
(define (eval expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (eval l) (eval r))]))

(test (eval (add (num 3) (num 5))) 8)

;; TRO
(define (eval-tr expr)
  (define (eval-tail expr acc)
    (match expr
      [(num n) (+ acc n)]  ; Add the number to the accumulated result
      [(add l r) (eval-tail l (eval-tail r acc))])) ; Recur on both sides
  (eval-tail expr 0))

(test (eval-tr (add (num 8) (num 5))) 13)
