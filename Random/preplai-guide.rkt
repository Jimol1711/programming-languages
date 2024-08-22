#lang play
(print-only-errors #t)

; declaring to variables locally and returning the maximum one
(let ([x 4]
      [y 5])
  (max x y))

; sums a and b
(define (sum a b)
  (+ a b))

; chooses random element between x and y
(define (pick-random x y)
  (let ([rand (random)])
     (if (< rand 1/2)
      x y)))

; sums 1 to each element on pair p
(define (pair-add1 p)
  (let* ([x (+ (car p) 1)]
         [y (+ (cdr p) 1)])
    (cons x y)))

; returns random element from l
(define (list-pick-random l)
  (let* ([rand (random (length l))]
         [index (list-ref l rand)])
    (index)))

; switches random value on v with given x
(define (vector-set-random! v x)
  (let ([rand (random (vector-length v))])
  (vector-set! v rand x)))

; different lists to try iterations, filtering and finding
(define iter-list (list 1 2 3 4))
(define filter-list (list 1 2 3 4))
(define find-list (list 1 2 3 4))

; map
(map (lambda (number)
         (+ 1 number))
       iter-list)

; andmap and ormap
(andmap negative? iter-list)
(ormap positive? iter-list)

; for-each
(for-each (lambda (arg)
              (printf "~a\n" (+ arg 1))
              99999)
            iter-list)

; foldl y foldr
(foldl cons '() iter-list)
(foldr cons '() iter-list)

; filter
(filter negative? filter-list)

; remove and remove* (there's versions where the procedure is a defined one like eq?)
(remove 3 filter-list <)
(remove* '(2) filter-list <)

; returns list of elements of lst that do not meet pred
(define (reject lst pred)
  (let* ([lst-inverted (foldl (lambda (x acc)
           (if (pred x)
           acc
           (cons x acc)))
         '() lst)]
         [lst-not-inverted (foldl (lambda (x empty-list)
                                    (cons x empty-list))
         '() lst-inverted)])
    lst-not-inverted))

; Example usage:
(define lst '(1 2 3 4 5 6))
(define pred1 even?)
(define pred2 odd?)

(test (reject lst pred1) '(1 3 5))
(test (reject lst pred2) '(2 4 6))

; my-max
(define (my-max a b)
  (cond [(> a b) a]
        [(= a b) (printf "son iguales ~a " a) a]
        [else b]))

(test (my-max 1 2) 2)
(test (my-max 2 1) 2)
(test (my-max 1 1) 1)

; pick-random-in-interval
(define (pick-random-in-interval a b)
  (let ([rango (- b a)])
    (+ (* (random) rango) a)))

(test (<= 5 (pick-random-in-interval 5 10)) #t)
(test (< (pick-random-in-interval 5 10) 10) #t)

; 1.
(define (negate p)
  (lambda (x) (not (p x))))

; 2.
; defined before

; 3.
(define (apply-twice f)
  (lambda (x) (f (f x))))

(define (apply-twice2 f x)
  (f (f x)))

(define (apply-twice-2 f x y)
  (f (f x y) (f x y )))

; findf return first element that is a multiple of 13 or if there's not, returns #f
(define (findf lst)
  (ormap (lambda (x) (and (zero? (modulo x 13)) x)) lst))

(test (findf '(1 2 3)) #f)
(test (findf '(1 13 3)) 13)
(test (findf '(1 3 26 3 39)) 26)

; reject with negate and filter
(define (reject2 lst pred)
  (filter (negate pred) lst))

(test (reject2 lst pred1) '(1 3 5))
(test (reject2 lst pred2) '(2 4 6))

; curry given a function of type (AB -> C) returns an equivalent of type A->(B->C)
(define (curry f)
  (lambda (a)
    (lambda (b)
      (f a b))))

; memoize given a function of type (A -> B) returns an equivalent function that holds a cache <argument->value> on a hash table
(define (memoize f)
  (define cache (make-hash))
  (lambda (x)
    (let ([cached (hash-ref cache x #f)])
      (if cached
          cached
          (let ([result (f x)])
            (hash-set! cache x result)
            result)))))

; tests with square
(define (square x) (* x x))
(define memoized-square (memoize square))

(test (memoized-square 5) 25)
(test (memoized-square 5) 25)
(test (memoized-square 7) 49)

; length :: List -> Number
; retorna el largo de la lista
(define (length l)
  (match l
    [(list) 0]
    [(cons h t) (+ 1 (length t))]))
 
(test (length (list)) 0)
(test (length '(1 2 3 4)) 4)

; sum-list :: List Number -> Number
; sums the elements of a list
(define (sum-list lst)
  (match lst
    [(list) 0]
    [(cons h t) (+ h (sum-list t))]))

(test (sum-list '(2 0 7)) 9)
(test (sum-list '(1 2 3)) 6)
(test (sum-list '()) 0)

; my-reverse :: List Any -> List Any
; returns the same list reversed
(define (my-reverse lst)
  (define (reverse-helper lst acc)
    (match lst
      [(list) acc]
      [(cons h t) (reverse-helper t (cons h acc))]))
  (reverse-helper lst '())) 

(test (my-reverse '()) '())
(test (my-reverse '(1 2 3 4 5)) '(5 4 3 2 1))
(test (my-reverse '(1 6 2 43 5 90)) '(90 5 43 2 6 1))

; my-map :: Proc ListAny -> List Any
; map proc on lst and return the mapped list
(define (my-map proc lst)
  (match lst
   [(list) '()]
   [(cons h t) (cons (proc h) (map proc t))]))

(test (my-map (lambda (number) (+ 1 number)) '(1 2 3 4)) '(2 3 4 5))
(test (my-map even? '(1 2 3 4)) '(#f #t #f #t))

; my-foldl :: Proc Init ListAny -> Any
; input lists is traversed from left to right and proc is applied using the return value of the last operation as argument
(define (my-foldl f init lst)
  (match lst
    [(list) init]
    [(cons h t) (my-foldl f (f init h) t)]))

(test (my-foldl cons '() '(1 2 3 4)) '((((() . 1) . 2) . 3) . 4))
(test (my-foldl + 0 '(1 2 3 4)) 10)

; sum-bt :: BinTree -> Number
; sums the elements of the binary tree bt
(define (sum-bt bt)
  (match bt
   [(list val left-bt right-bt) (+ val (+ (sum-bt left-bt) (sum-bt right-bt)))]
   [(list val side-bt) (+ val (sum-bt side-bt))]
   [val val]))

(test (sum-bt '(1 (2 3 4) (5 (6 7 8) 9))) 45)
(test (sum-bt '(3 4)) 7)
(test (sum-bt 4) 4)

; max-bt :: BinTree -> Number
; returns the maximum value on bt
(define (max-bt bt)
  (match bt
   [(list val left-bt right-bt) (max (max-bt left-bt) (max-bt right-bt))]
   [(list val side-bt) (max val (max-bt side-bt))]
   [val val]))

(test (max-bt '(1 (2 3 4) (5 (6 7 8) 9))) 9)
(test (max-bt 50) 50)
(test (max-bt '(3 4)) 4)

; map-bt :: BinTree Proc -> Any
; maps bt by performing proc on it's elements
(define (map-bt bt proc)
  (match bt
    [(list val left-bt right-bt) (list (proc val) (map-bt left-bt proc) (map-bt right-bt proc))]
    [(list val side-bt) (list (proc val) (map-bt side-bt proc))]
    [val (proc val)]))

(test (map-bt '(1 (2 3 4) (5 (6 7 8) 9)) (λ (number) (+ 1 number))) '(2 (3 4 5) (6 (7 8 9) 10)))
(test (map-bt '(1 2) even?) '(#f #t))

; foldlf-bt :: Proc Any Bintree -> Any
; performs proc with foldl procedure over bt TERMINAR
;; (define (foldl proc init bt)
;;   (match bt
;;     [(list val left-bt right-bt) (foldl proc init (cons (foldl-bt proc init left-bt) (foldl-bt proc init right-bt)))]))