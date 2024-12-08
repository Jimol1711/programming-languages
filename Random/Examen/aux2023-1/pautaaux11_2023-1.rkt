#lang play

;============= P1 ================
#|
(defmac (OBJECT ((field fname init) ...)
                ((method mname args body) ...))
  #:keywords field method
   (let ((fname init) ...)
     (let ((methods (list (cons 'mname (lambda args body)) ...)))
      (lambda (msg . vals)
        (apply (cdr (assoc msg methods)) vals)))))

(defmac (-> o m arg ...)
  (o m arg ...))
|#

;; El problema es que methods se está definiendo antes que fields
;; por lo tanto si un método intenta utilizar un campo, este no estará deifnido.

;; Respuesta: Funciona en los que no usan sus campos
#|
(define origin-point
  (OBJECT ()
          ((method x? () 0)
           (method y? () 0))))

(define point
  (OBJECT ((field x 1))
          ((method x? (x) x)
           (method y? () 0))))
|#


;============= P2 ================

(define (greeter-factory name)
  (let ([methods (list
                  (cons 'greet (λ ()
                                 (printf "Hola ~a!" name))))])
    (λ (msg . vals)
      (apply (cdr (assoc msg methods)) vals))))

(define greeter-Tomas (greeter-factory "Tomas"))
(greeter-Tomas 'greet)

;============= P3 ================

(defmac (OBJECT ([field fname fval] ...)
                ([method mname mparams mbody ...] ...))
  #:keywords field method
  (letrec
      ([self
        (let ([fname fval] ...)
          (let ([methods (list (cons 'mname (λ mparams mbody ...)) ...)])
            (λ (msg . args)
              (let ([found (assoc msg methods)])
                (if found
                    (apply (cdr found) args)
                    (error "message not understood:" msg))))))])
    self))
 
(defmac (-> o m arg ...)
  (o 'm arg ...))

(define bool (λ (b) (OBJECT () ([method interp () b]))))
(define num (λ (n) (OBJECT () ([method interp () n]))))
(define add (λ (l r) (OBJECT () ([method interp () (+ (-> l interp)
                                                      (-> r interp))]))))
(define ifc (λ (c t f) (OBJECT () ([method interp ()
                                           (if (-> c interp)
                                               (-> t interp)
                                               (-> f interp))]))))
(test (-> (add (num 1)
                    (add (num 2) (num 3))) interp) 6) 


;============= P4 ================
(struct obj (class values))
(defmac (CLASS ([field fname fval] ...)
               ([method mname (mparam ...) mbody ...] ...))
     #:keywords field method
     #:captures self
     (let ([methods
            (list (cons 'mname (λ (self mparam ...) mbody ...)) ...)])
       (letrec
           ([class
                (λ (msg . args)
                  (match msg
                    ['-create
                     (let ([o (obj class (make-hash (list (cons 'fname fval) ...)))])
                       (when (not (empty? args))
                         (let ([found (assoc 'initialize methods)])
                           (if found
                               (apply (cdr found) (cons o args))
                               (error "initialize not implemented in:" class))))
                       o)]
                    ['-lookup
                     (let ([found (assoc (first args) methods)])
                       (if found
                           (cdr found)
                           (error "message not understood:" (first args))))]

                    ))])
         class)))


#;(define (prev-new c)
  (c '-create))
(define (new class . init-vals)
    (apply class (cons '-create init-vals)))

(defmac (ABSTRACT-CLASS ([field f init] ...)
                        ([method m (param ...) body ...] ...))
  #:keywords field method
  #:captures self ? ! -->
  (let ([c (CLASS ([field f init] ...)
                  ([method m (param ...) body ...] ...))])
    (λ (msg . vals)
      (match msg
        ['-create (error "cannot instantiate abstract class")]
        [else (apply c (cons msg vals))]))))

(define A (ABSTRACT-CLASS ([field x 1])
                           ([method m (x) (+ x 3)])))
(test/exn (new A) "abstract") ; -> fail, cannot instantiate..

;============= P5 ================

(defmac (->> o m arg ...)
  (let ([obj o])
    (((obj-class obj) '-lookup 'm) obj arg ...)))

(defmac (? f) #:captures self
  (dict-ref (obj-values self) 'f))
 
(defmac (! f v) #:captures self
  (dict-set! (obj-values self) 'f v))

(define Square 
  (CLASS ([field length 0])
         ([method initialize ([nl 0]) (! length nl)]
          [method length? () (? length)]
          [method length! (nl) (! length nl)]
          [method area () (expt (? length) 2)]
          [method is_bigger_than (c2) (> (? length) (->> c2 length?) )])))

(define c1 (new Square 5))
(define c2 (new Square 3))
(->> c1 is_bigger_than c2)