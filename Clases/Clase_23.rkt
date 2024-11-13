#lang play

(defmac (OBJECT ([field fname init] ...)
                ([method mname args body] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname init] ...)
              (let ([methods (list (cons 'mname (λ args body)) ...)])
                (λ (msg . vals)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) vals)
                        (error "message not understood:" msg))))))])
    self))      

(defmac (-> o m arg ...)
  (o 'm arg ...))

(define (make-point init-x init-y)
  (OBJECT
   ([field x init-x]
    [field y init-y])
   ([method x? () x]
    [method y? () y]
    [method x! (new-x) (set! x new-x)]
    [method y! (new-y) (set! y new-y)]
    [method above (other-point)
            (if (> (-> other-point y?) y)
                other-point
                self)])))

(define p1 (make-point 5 5))
(define p2 (make-point 2 2))

;; FORWARDING
(define seller
  (OBJECT ()
          ([method price (prod)
                   (* (case prod
                        [(1) (-> self price1)]
                        [(2) (-> self price2)])
                      (-> self unit))]
           [method price1 () 100]
           [method price2 () 200]
           [method unit () 1])))

(define broker
  (OBJECT
   ([field provider seller])
   ([method price (prod)
            (-> provider price prod)])))


(defmac (OBJECT-FWD target
                    ([field fname init] ...)
                    ([method mname args body] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname init] ...)
              (let ([methods (list (cons 'mname (λ args body)) ...)])
                (λ (msg . vals)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) vals)
                        (apply target msg vals))))))])
    self))

(define broker-fwd
  (OBJECT-FWD seller () ()))

;; DELEGATION ...
