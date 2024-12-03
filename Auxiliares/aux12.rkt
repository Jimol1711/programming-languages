#lang play

; Aux12

; p1.
; (a)
(defmac (OBJECT ([field fname init] ...)
                ([method mname args body] ...))
  #:keywords field method
  (let ([methods (list (cons 'mname (lambda args body)) ...)])
     (let ([fname init] ... )
        (lambda (msg . vals)
          (apply (cdr (assoc msg methods)) vals)))))

(defmac (--> o m arg ... )
  (o 'm arg ... ))

; Problema: Los métodos no saben quienes son los campos
;; Corrección: Hacer que referencie a self para que se referencie a si mismo y por extensión a sus campos

;; Programa donde no se manifiesta
(define (punto-2d x-init y-init)
  (OBJECT ([field x x-init]
           [field y y-init])
          ([method hola x 1])))

; Programa donde se manifiesta
;; (define (punto-2d x-init y-init)
;;   (OBJECT ([field x x-init] [field y y-init])
;;           ([method x? () x] [method y? () y])))

;; Corrección
(defmac (OBJECT-CORR ([field fname init] ...)
                     ([method mname args body] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname init] ...) ;iniciamos las variables con sus valores iniciales
              (let ([methods (list (cons 'mname (λ args body)) ...)])
                (λ (msg . vals)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) vals)
                        (error 'error "message not understand ~a" msg))))))])
    self))

; Ya no se ve el unbound identifier error
(define (punto-2d-corr x-init y-init)
  (OBJECT-CORR ([field x x-init] [field y y-init])
               ([method x? () x] [method y? () y])))

; (b)
(define (in-node v l-init r-init)
  (OBJECT-CORR ([field l l-init]
                [field r r-init])
               ([method height () (+ 1 (max (--> l height) (--> r height)))])))

(define (leaf v)
  (OBJECT ([field value v])
          ([method height () 0])))

; p2.
(defmac (switch input
                [case pred -> result]
                ...
                [default -> default-result])
  #:keywords case default ->
  (cond
    [(pred input) result]
     ...
    [else default-result]))

(define-struct obj
  (class values))

; -- CLASSSTART --
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

; -- CLASSEND --

; p3.
; el ? es un getter
(define (square l-init w-init)
  (CLASS ([field length l-init]
                [field width w-init])
               ([method area () (* (? length) (? width))]
                [method bigger-than (s) (> (-> s area) (-> self area))])))

; p4.
(defmac (ND-FSA (init : initial-state)
                (final : accepting-states ...)
                (transitions : [state : (action → target ...) ...] ...))
  #:keywords init final transitions : →
  (letrec
      ([state
        (if (member (quote state) '(accepting-states ...))
            (lambda (stream)
              (or (null? stream)
                  (case (car stream)
                    [(action) (or (target (cdr stream)) ...)]
                    ...
                    [else #f])))
            (lambda (stream)
              (and (pair? stream)
                   (case (car stream)
                     [(action) (or (target (cdr stream)) ...)]
                     ...
                     [else #f]))))]
       ...)
    initial-state))
