#lang play

; P1
; (a)
(define (my-map proc lst)
  (match lst
   [(list) '()]
   [(cons h t) (cons (proc h) (map proc t))]))

(test (my-map (lambda (number) (+ 1 number)) '(1 2 3 4)) '(2 3 4 5))
(test (my-map even? '(1 2 3 4)) '(#f #t #f #t))

; (b)
(define (my-foldl f init lst)
  (match lst
    [(list) init]
    [(cons h t) (my-foldl f (f init h) t)]))

(test (my-foldl cons '() '(1 2 3 4)) '((((() . 1) . 2) . 3) . 4))
(test (my-foldl + 0 '(1 2 3 4)) 10)

;(c)
(define (my-filter proc lst)
  (match lst
    [(list) lst]
    [(cons h t) (if (proc h)
                (cons h (my-filter proc t))
                (my-filter proc t))]))

(test (my-filter even? '(1 2 3 4)) '(2 4))
(test (my-filter (λ (x) (> x 3)) '(1 2 3 4)) '(4))

; P2
; (a)
(define (prom_edad lst)
  (define clean_list (filter (λ (x) (not (boolean? x))) lst))
  (if (empty? clean_list)
      (error "full of trash")
      (let* ([len (length clean_list)]
             [numbers_list (map string->number clean_list)]
             [sum_of_lst (foldl + 0 numbers_list)])
        (/ sum_of_lst len))
        ))

(test (prom_edad '("10" #f "40" "10" #f)) 20)
(test (prom_edad (list "20")) 20)
(test/exn (prom_edad (list  #f #f))"full of trash")

; (b)
(define (string_or_list element)
  (match element
    [(? string?) (string->number element)]
    [(? list?) (calcular_prom_rec element)]))

(define (calcular_prom_rec lst)
  (define clean_list (filter (λ (x) (not (boolean? x))) lst))
  (if (empty? clean_list)
      (error "full of trash")
      (let ([len (length clean_list)]
            [sum_total (foldl + 0 (map string_or_list clean_list))])
      (/ sum_total len))
  ))

(test (calcular_prom_rec (list "10" #f "40" "10" #f)) 20)
(test (calcular_prom_rec (list "20")) 20)
(test/exn (calcular_prom_rec (list  #f #f))"full of trash")
(test (calcular_prom_rec  (list "10" (list "10" #f "40" "10" #f)  "40" "10" #f)) 20)
(test (calcular_prom_rec (list (list "10" #f "40" "10" #f))) 20)
(test/exn (calcular_prom_rec (list (list #f #f) #f)) "full of trash")

; P3
; (a)
(define (curry-aux-n n f args)
  (if (zero? n)
      (apply f args)
      (λ (x)
        (curry-aux-n (- n 1) f (append args (list x))))))

(define (curry-n n f)
  (curry-aux-n n f '()))

(test (((curry-n 2 +) 1) 2) (+ 1 2))
(test (((curry-n 2 -) 1) 2) (- 1 2))

; (b)
(define (uncurry-2 f)
  (λ (x y) ((f x) y)))

(define curry-sum (λ (a) (λ (b) (+ a b))))
(define curry-res (λ (a) (λ (b) (- a b))))
(test ((uncurry-2 curry-sum) 1 2) ((curry-sum 1) 2))
(test ((uncurry-2 curry-res) 1 2) ((curry-res 1) 2))