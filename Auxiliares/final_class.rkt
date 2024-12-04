#lang play

(define-struct obj
  (class values))

(defmac (CLASS ([field f init] ...)
               ([method m params body] ...))
  #:keywords field method
  #:captures self
  (let ([methods (list (cons 'm (λ (self) (λ params body))) ...)])
    (letrec
        ([class
             (λ (msg . args)
               (case msg
                 [(create) (make-obj class (make-hash (list (cons 'f init)) ...))]
                 [(read) (dict-ref (obj-values (first args)) (second args))]
                 [(write) (dict-set! (obj-values (first args))
                                     (second args)
                                     (third args))]
                 [(invoke)
                  (let ([found (assoc (second args) methods)])
                    (if found
                        (apply ((cdr found) (first args)) (cddr args))
                        (error "message not understood")))]))])
      class)))

(defmac (-> obj m arg ...)
  (let ([o obj])
    ((obj-class o) 'invoke o 'm arg ...)))

(defmac (? fd)
  #:captures self
  ((obj-class self) 'read self 'fd))

(defmac (! fd v)
  #:captures self
  ((obj-class self) 'write self 'fd v))

(define (new class)
  (class 'create))

(define Point
  (CLASS ([field x 0])
         ([method x? () (? x)]
          [method x! (new-x) (! x new-x)]
          [method move (n) (-> self x! (+ (-> self x?) n))])))

(define p1 (new Point))

(define p2 (new Point))










