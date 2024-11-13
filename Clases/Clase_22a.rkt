#lang play

(defmac (OBJECT ([field fname init] ...)
                ([method mname args body] ...))
  #:keywords field method
  (let ([fname init] ...)
    (let ([methods (list (cons 'mname (λ args body)) ...)])
      (λ (msg . vals)
        (let ([found (assoc msg methods)])
          (if found
              (apply (cdr found) vals)
              (error "message not understood:" msg))))
      )))

(defmac (-> o m arg ...)
  (o 'm arg ...))

(define counter
  (OBJECT ([field count 0])
          ([method inc () (begin (set! count (+ 1 count)) count)]
           [method dec () (begin (set! count (- count 1)) count)])))

(define stack
  (OBJECT ([field vals '()])
          ([method pop ()
                   (if (empty? vals)
                       (error "cannot pop from an empty stack")
                       (let ([val (car vals)])
                         (set! vals (cdr vals))
                         val))]
           [method push (val) (set! vals (cons val vals))])))

(define p1
  (OBJECT ([field x 0]
           [field y 0])
          ([method get-x () x]
           [method set-x (new-x) (set! x new-x)]
           [method get-y () y]
           [method set-y (new-y) (set! y new-y)])))

(define (make-point init-x init-y)
  (OBJECT ([field x init-x]
           [field y init-y])
          ([method x? () x]
           [method x! (new-x) (set! x new-x)]
           [method y? () y]
           [method y! (new-y) (set! y new-y)])))

(define p2 (make-point 0 0))
(define p3 (make-point 1 3))

(define (make-node l r)
  (OBJECT
   ([field left l]
    [field right r])
   ([method sum () (+ (-> left sum) (-> right sum))])))

(define (make-leaf v)
  (OBJECT
   ([field value v])
   ([method sum () value])))

(define tree1
  (make-node
   (make-node
    (make-leaf 3)
    (make-node
     (make-leaf 10)
     (make-leaf 4)))
   (make-leaf 1)))





         







                    
           
                   
