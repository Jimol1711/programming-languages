#lang play

;P1

#;(defmac (OBJECT ((field fname init) ...)
                ((method mname args body) ...))
  #:keywords fields method
  (let ((methods (list (cons 'mname (λ args body)) ...)))
    (let ((fname init) ...)
      (λ (msg . vals)
        (let ([found  (assoc msg methods)])
          (if found
              (apply (cdr found) vals)
              (error "Method not found")))))))

;; El problema es que methods se está definiendo antes que fields
;; por lo tanto si un método intenta utilizar un campo, este no estará deifnido.
;; Respuesta: Funciona en los que no usan sus campos

#;(define origin-point
  (OBJECT ()
          ((method x? () 0)
           (method y? () 0))))

#;(define point
  (OBJECT ((field x 5) (field y 4))
          ((method x? () x)
           (method y? () y))))

; Arreglado
(defmac (OBJETO ((field fname init) ...)
                ((method mname args body) ...))
  #:keywords fields method
  (let ((fname init) ...)
    (let ((methods (list (cons 'mname (λ args body)) ...)))
      (λ (msg . vals)
        (let ([found  (assoc msg methods)])
          (if found
              (apply (cdr found) vals)
              (error "Method not found")))))))


; P2
(define root
  (λ (msg . args)
    (error "not understood" msg)))


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

(defmac (OBJECT-DEL parent
                    ([field fname init] ...)
                    ([method mname args body] ...))
  #:keywords field method
  #:captures self
  (let ([fname init] ...)
    (let ([methods
           (list (cons 'mname
                       (λ (self) (λ args body))) ...)]) 
      (λ (current)          
        (λ (msg . vals)
          (let ([found (assoc msg methods)])
            (if found
                (apply ((cdr found) current) vals)
                (apply (parent current) msg vals))))))))

; Renombrado a ->> para que no choque con la definición más abajo
(defmac (->> o msg vals ...)
  (o 'msg vals ...))

(define Comparer
    (OBJECT-FWD root
            ()
            ([method compare (that) (error "Unimplemented method compare")]
             [method < (that) (< (->> self compare that) 0)])))

(define (make-string s)
    (OBJECT-FWD Comparer
                ([field value s])
                ([method size () (string-length s)]
                 [method compare (that) (- (->> self size) (->> that size))])))

; Descomentar para ver resultado
; (->> (make-string "such") < (make-string "message"))


(defmac (->>> o msg vals ...)
  ((o o) 'msg vals ...))

(define Comparer-DEL
    (OBJECT-DEL root
            ()
            ([method compare (that) (error "Unimplemented method compare")]
             [method < (that) (< (->>> self compare that) 0)])))

(define (make-string-del s)
    (OBJECT-DEL Comparer-DEL
                ([field value s])
                ([method size () (string-length s)]
                 [method compare (that) (- (->>> self size) (->>> that size))])))

;descomentar para ver resultado
;(->>> (make-string-del "much") < (make-string-del "comparing"))


;; P3
(defmac (OBJECT ([field fname init] ...)
                ([method mname args body] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname init] ...)
              (let ([methods (list (cons 'mname (λ args body)) ...)])
                (λ (msg . vals)
                  (apply (cdr (assoc msg methods)) vals))))])
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
