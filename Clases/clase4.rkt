#lang play
(print-only-errors #t)

; clase4

;; AB -> C [--curry-->] A -> (B -> C)
;; curry :: (A B -> C) -> (A -> (B -> C))
(define (curry f)
  (λ (a) (λ (b) (f a b))))

#|
<BinTree> ::= (leaf <value>)
           |  (in-node <value> <BinTree> <BinTree>)
|#
;; Inductive type to represent a Binary Tree
(deftype BinTree
  (leaf value)
  (in-node value left right))

; Constructor example top-down
(define bt
  (in-node 5
           (in-node 4
                    (leaf 1)
                    (leaf 8))
           (leaf 3)))

; Destructor or Accesor
(in-node-value bt)
(in-node-right bt)

; Predicado de tipos
(BinTree? "abc")

;; height ;; BinTree -> Integer
;; Devuelve la altura de un árbol
(define (height bt)
  (match bt
    [(leaf v) 0]
    [(in-node v l r) (+ 1 (max (height l) (height r)))]))

(test (height bt) 2)

(define (contains? t n)
  (match t
    [(leaf v) (= v n)]
    [(in-node v l r) (or (= v n)
                         (contains? l n)
                         (contains? r n))]))

(test (contains? bt 8) #t)

; Para funciones se debe escribir siguiendo la metodología (véase PrePlai capítulo 4)
; 1. Escribir firma
; 2. Escribir descripción
; 3. Hacer tests
; 4. Programar función

; Para datatypes escribir gramática (véase BinTree)