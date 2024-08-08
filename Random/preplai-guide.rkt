#lang slideshow

; declaring to variables locally and returning the maximum one
(let ([x 4]
      [y 5])
  (max x y))

; sums a and b
(define (sum a b)
  (+ a b))

; chooses random element between x and y
(define (pick-random x y)
  (define rand (random))
  (if (< rand 1/2)
      x y))

; sums 1 to each element on pair p
(define (pair-add1 p)
  (define x (+ (car p) 1))
  (define y (+ (cdr p) 1))
  (cons x y))

; returns random element from l
(define (list-pick-random l)
  (define rand (random (length l)))
  (list-ref l rand))

; switches random value on v with given x
(define (vector-set-random! v x)
  (define rand (random (vector-length v)))
  (vector-set! v rand x))

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

; remove and remove* (there's versions where the procedure is a defined one)
(remove 3 filter-list <)
(remove* '(2) filter-list <)

; returns list of elements of lst that do not meet pred
(define (reject lst pred)
  (foldl pred
         '()
         lst)) ; NO TERMINADO

; Example usage:
(define lst '(1 2 3 4 5 6))
(define pred even?)

(reject lst pred) ; => '(1 3 5)