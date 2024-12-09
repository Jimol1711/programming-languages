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

; ----- EXTRA GAMBLER CLASS -----

(define Gambler
  (CLASS ([field savings 0])
         ([method savings? () (? savings)]
          [method savings! (n) (! savings n)]
          [method init (init-sav) (-> self savings! init-sav)]
          [method inc-money (amount) (-> self savings! (+ (-> self savings?) amount))]
          [method dec-money (amount) (-> self savings! (- (-> self savings?) amount))]
          [method gamble (bet cas) (-> cas gamble bet self)])))

(define Casino
  (CLASS ([field jackpot 0]
          [field odds 0])
         (
          ; getters, setters and init
          [method jackpot! (j) (! jackpot j)]
          [method odds! (o) (! odds o)]
          [method jackpot? () (? jackpot)]
          [method odds? () (? odds)]
          [method inc-jackpot (amount) (-> self jackpot! (+ (-> self jackpot?) amount))]
          [method dec-jackpot (amount) (-> self jackpot! (- (-> self jackpot?) amount))]
          [method init (jack odds) (begin (-> self jackpot! jack) (-> self odds! odds))]

          ; casino methods
          [method spin () (equal? (random (-> self odds?)) (- (-> self odds?) 1))]
          [method payout (bet gam) (begin
                                         (println (format "You won ~a gazillion dollars!!!" (* bet (-> self odds?))))
                                         (-> gam inc-money (* bet (-> self odds?)))
                                         (-> self dec-jackpot (* bet (-> self odds?)))
                                         )]
          [method charge (bet gam) (begin
                                         (println (format "You lost ~a dollars T_T" bet))
                                         (-> gam dec-money bet)
                                         (-> self inc-jackpot bet)
                                         )]
          [method gamble (bet gam) (if (-> self spin)
                                           (-> self payout bet gam)
                                           (-> self charge bet gam))]
          )))







