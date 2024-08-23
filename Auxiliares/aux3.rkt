#lang play

; P1
; a)
(deftype NTree
  (nleaf v)
  (nnode v childs))

;; fold-ntree :: (Number -> A) (Number ListOf[A]) -> (NTree -> A)
;; ...

; b)
(define (fold-ntree f g)
  (λ (nt)
    (match nt
      [(nleaf v) (f v)]
      [(nnode v childs) (g v (map (fold-ntree f g) childs))]
      )))

; c)
;; sum-tree :: NTree -> Number
;; ...
(define (sum-tree nt)
  (fold-ntree identity
              (λ (v childs) (+ v childs))) nt)

; d)
;; (define (contains-n-tree? nt v)
;;   ((fold-ntree (λ (x) (if (equal? v x)
;;                           #t
;;                           #f))
;;                (λ (x childs) (if (equals? x v)
;;                                  #t
;;                                  (foldl (λ (x y) (or x y)) #f childs))))))

; e)

;; select-n-tree :: (Number -> Boolean) NTree -> ListOf[Number]
;; ...
(define (select-n-tree p nt)
  ((fold-ntree (λ (v) (if (p v)
                          (list v)
                          '()))
               (λ (v childs) (if (p v)
                                 (append (list v) (apply append childs))
                                 (apply append childs)))
               ) nt))

; P2
; a)

#|
<Polinomio> ::= (nullp)
             |  (plus <Number> <Number> <Polinomio>)
|#
(deftype Polynomio
  (nullp)
  (plus coef grado resto))

; b)
;; parse-p :: ListOf[Symbol] -> Polinomio
;; (define (parse-p p)
;;   (match p
;;     [(list monomio '+ resto ...) (let ([lista-char (string->list (symbol->string monomio))])
;;                                       (plus (get-coef list-char) (get-grado lista-char) (parse-p resto)))]
;;     [(list n) #:when (number? n) (plus n 0 (nullp))]
;;     [(list monomio) (let ([lista-char (string->list (symbol->string monomio))])
;;                          (plus (get-coef lista-char) (get-grado lista-char) (nullp)))]
;;     ))

; d)
;; (define (eval n p)
;;   ((fold-p 0
;;             (λ (coef grado resto) (+ (* coef (expt n grado)) resto))
;;             )p))

; e)
