#lang play

; p1.
; (a)
(define (moveToEnd l)
  (if (empty? l)
      (void)
      (set! l (append (cdr l) (list (car l))))))

(let ([l '(1 2 3 4)])
  (begin
    (moveToEnd l)
    l))

; Racket es call-by-value, osea, crea una copia en moveToEnd, luego no se modifica

; (b) Si se modifica, ya que no está dentro de una función el l

; (c) Meter el l en un box
(define (moveToEndBox l)
  (if (empty? (unbox l))
      (void)
      (set-box! l (append (cdr (unbox l)) (list (car (unbox l)))))))

; p2.
; (a)
(defmac (unless guard do body)
  #:keywords do
  (letrec ([iter
            (lambda () (if guard
                           (void)
                           (begin
                             body
                             (iter))))])
    (iter)))

; (b) No se puede pq Racket es eager, es decir, los argumentos se evaluan inmediatamente.

; p3.
; (a) 
(define (filter f l)
  (match l
    [(list) (list)]
    [(cons a b) (if (f a)
                    (cons a (filter f b))
                    (filter f b))]
    ))

(define (filterTR f l)
  (define (filter-aux f l acc)
    (match l
      [(list) acc] ; empty list, return acc
      [(cons a b) (if (f a) ; non-empty, keep filtering
                      (filter-aux f (cdr l) (append acc (list a)))
                      (filter-aux f (cdr l) acc))])
    )
  (filter-aux f l '()))

; (b) 
(require "base_p3.rkt")

(define factorialTRO
  (parse-fundef '{fundef factorialTRO {n acc} {if0 n
                                                   {+ 0 acc} ;; es {+ 0 acc} para probar que funcione con expresiones no simples (no solo (num) e (id))
                                                   {factorialTRO { {- n 1} {* n acc}}}
                                          }}
                ))


(define factorial
  (parse-fundef '{fundef factorial {n} {if0 n
                                          1
                                          {* n {factorial {{- n 1}}}}
                                          }}
                ))

(define ListaFunciones (list factorialTRO factorial))

(define (isTRO?)
  (match (fundef-body fun)
    [(num n) #f]))




















