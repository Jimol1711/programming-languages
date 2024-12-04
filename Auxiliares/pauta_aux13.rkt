#lang play

(print-only-errors #t)

;;a)
;;moveToEnd :: ListOf[Int] -> (Void)
;;mueve el primer el elemento al final de la lista
(define (moveToEnd l)
  (if (empty? l)
      (void)
      (set! l (append (cdr l) (list (car l))))))

;;no funciona pq las funciones de Racket son Call-by-value por default
;; es decir, crea una nueva dirección de memoria dada en el argumento y eso es lo que modifican (set!)
;; por lo que al usar la variable original (el espacio de memoria o direccion original) este no puede ver esos cambios
(let ([l '(1 2 3 4)])
  (begin
    (moveToEnd l)
    (test l '(1 2 3 4))))


;;b)
;;aca si se puede ver el cambio por que el seteo no fue através de una función, por lo tanto no se crea una
;;nueva direccion y si se cambia la dirección dada
(let ([l '(1 2 3 4)])
  (begin
    (if (empty? l)
        (void)
        (set! l (append (cdr l) (list (car l)))))
    (test l '(2 3 4 1))))



 ;;c)
;; moveToEndBox :: Box(ListOf[Int]) -> Void
;; recibe la lista envuelta en una caja
(define (moveToEndBox l)
  (if (empty? l)
      (void)
      (set-box! l (append (cdr (unbox l)) (list (car (unbox l)))))))


;;las cajas al ser estructuras mutables que guardan la dirección, al hacerles set!, se obtiene esa dirección original
;;(es decir, no se crea una nueva) y se cambia, por lo tanto, los cambios si son perceptibles desde afuera
(let ([l (box '(1 2 3 4))])
  (begin
    (moveToEndBox l)
    (test (unbox l) '(2 3 4 1))))



;;----------------P2--------------------

;;a)
(defmac (unless cond do body)
  #:keywords do
  (letrec ([iter
            (λ ()
              (if (not cond) ;;si es falsa la condicion entonces seguimos
                  (begin body
                         (iter))
                  (void)))])
    (iter)))
                  


(define a 10)
(unless (< a 5) do
  (begin
    ( println a)
    ( set! a (- a 1))))


;;b)
#|
(define (unless-fun cond body)
  (letrec ([iter
            (λ ()
              (if (not cond)
                  (begin body
                         (iter))
                  (void)))])
  (iter)))

(define b 10)
(unless-fun (< b 5) (begin
    ( println b)
    ( set! b (- b 1))))

Imprime 10 y se queda pegado, porque cond y body se evaluan altiro.
cond devuelve #f y body devuelve 9, esto porque (set! b (- b 1)) cambia el valor
de b y devuelve el valor actualizado (retorna 9). Entonces el iter definido en el letrec queda:

iter = λ () (if (not #f) (begin 9 (iter)) (void))
que se repite infinito.
|#





;;----------------P3--------------

;;a)
;;filterTRO :: (A -> Boolean) ListOf[A] -> ListOf[A]
;;filtra la lista por el predicado dado y es TRO
(define (filterTRO f l)
  (define (filter-aux f* l* acc)
    (match l*
      ['() acc]
      [(list x rest ...) (if (f x)
                             (filter-aux f rest (append acc (list x)))
                             (filter-aux f rest acc))]))
  (filter-aux f l '()))


(define l1 '(1 2 3 4 5 6))
(test (filterTRO even? l1) '(2 4 6))
(test (filterTRO odd? l1) '(1 3 5))




;;b)

(require "base_p3.rkt")


(define factorialTRO
  (parse-fundef '{fundef factorialTRO {n acc} {if0 n
                                                   {+ 0 acc} ;; es {+ 0 acc} para probar que funcione con expresiones no simples (no solo (num) e (id))
                                                   {factorialTRO { {- n 1} {* n acc}}}
                                          }}
                ))


(define factorial
  (parse-fundef '{fundef factorial {n} {if0 n
                                          1
                                          {* n {factorial {{- n 1}}}}
                                          }}
                ))

(define ListaFunciones (list factorialTRO factorial))


;;Recursive? :: Expr Symbol -> Boolean
;;si la expr es de tipo {factorialTRO { {- n 1} {* n acc}} entonces #t, si no cualquier otra forma no es recursiva
;; o no sirve para la optimización
(define (Recursive? expr fname)
  (match expr
    [(app f args) (if (equal? f fname)
                      #t
                      (isTRO? (look-up f ListaFunciones)))]
    [_ #f])
  )


;; notRecursive :: Expr Symbol -> Boolean
;; retorna #t si la expresión NO es recursiva, si no llama al fname entregado en NINGUNA instancia
(define (notRecursive expr fname)
  (match expr
    [(num n) #t]
    [(id x) #t]
    [(add l r) (and (notRecursive l fname) (notRecursive r fname)) ]
    [(sub l r) (and (notRecursive l fname) (notRecursive r fname)) ]
    [(mult l r) (and (notRecursive l fname) (notRecursive r fname)) ]
    [(if0 c t f) (and (notRecursive c fname) (notRecursive t fname) (notRecursive f fname)) ]
    [(app f args) (if (equal? f fname)
                      #f
                      (not (isTRO? (look-up f ListaFunciones))))] ;;si es TRO no nos sirve, pq no queremos ser recursivos
    ))
                      
                      
;; isTRO? :: Fundef -> Boolean
;; #t si es TRO y #f si no
(define (isTRO? fun)
  (match (fundef-body fun)
    [(if0 c t f) (and
                  (notRecursive c (fundef-name fun))
                  ;;queremos que una de las ramas sea recursiva y la otra NO
                  (or
                   (and (Recursive? t (fundef-name fun)) (notRecursive f (fundef-name fun)))
                   (and (Recursive? f (fundef-name fun)) (notRecursive t (fundef-name fun)))
                   ))]
    [(app f args) (if (equal? f (fundef-name fun))
                      #f ;;seria FUNCION bucle
                      (isTRO? (look-up (f ListaFunciones))))]
    [_ #f]))
                   

(test (isTRO? factorialTRO) #t)
(test (isTRO? factorial) #f)

;;----------------P4--------------

(require "final_class.rkt")

(define CLASS-Casino
  (CLASS ([field jackpot 0]
          [field odds 0])
         ([method jackpot? () (? jackpot)]
          [method odds? () (? odds)]
          [method jackpot! (new-v) (! jackpot new-v)]
          [method odds! (new-v) (! odds new-v)]
          [method increase-jackpot (inc) (-> self jackpot! (+ (-> self jackpot?) inc))]
          [method decrease-jackpot (dec) (-> self jackpot! (- (-> self jackpot?) dec))]
          [method payout (bet gambler) (begin
                                         (println (format "You won ~a gazillion dollars!!!" (* bet (-> self odds?))))
                                         (-> gambler increase-money (* bet (-> self odds?)))
                                         (-> self decrease-jackpot (* bet (-> self odds?)))
                                         )]
          [method charge (bet gambler) (begin
                                         (println (format"You lost ~a dollars T_T" bet))
                                         (-> gambler decrease-money bet)
                                         (-> self increase-jackpot bet)
                                         )]
          [method initialize (nx ny) (begin (-> self jackpot! nx) (-> self odds! ny))]
          [method spin () (equal? (random (-> self odds?)) (- (-> self odds?) 1))]
          [method gamble (bet gambler) (if (-> self spin)
                                           (-> self payout bet gambler)
                                           (-> self charge bet gambler))]))
  )

(define CLASS-Gambler
  (CLASS ([field savings 0])
         ([method savings? () (? savings)]
          [method savings! (new-v) (! savings new-v)]
          [method initialize (n) (-> self savings! n)]
          [method increase-money (inc) (-> self savings! (+ (-> self savings?) inc))]
          [method decrease-money (dec) (-> self savings! (- (-> self savings?) dec))]
          [method gamble (bet casino) (-> casino gamble bet self)])
         ))


(define gambler (new CLASS-Gambler 5000))
(define casino (new CLASS-Casino 100000 5))


;;P5

#|
a) Debemos calcular (+ 6 (f 12))
y en scope estatico, para evaluar el cuerpo de la funcion se ocupa el ambiente que se tenía hasta el momento de la DEFINICIÓN de f
y ese env era ['x -> 5], asi que (f 12) sería (+ 12 5) = 17 y el resultado total sería 6 + 17 = 23


b) En dinamico para evaluar el cuerpo ocupa el ultimo ambiente y este es ['x -> 6, 'x -> 5], así que toma que x vale 6 y da como resultado 24

c) Las clausuras nacen para poder mantener el scope lexico en un lenguaje con sustitución diferida, ya que la directa es muy ineficiente O(N^2)

d) Lenguaje con scope lexico y con sustitución diferida

e) 1) Poder hacer cosas infinitas, como listas infinitas (haskell)
   2) Más eficiencia en tiempo, ya que las cosas se evaluan solo si es necesaria, y si es lazy-by-need es aún más eficiente en tiempo, ya que tiene
    un cache que guarda los valores

|#



;;P6)
;;a) falla que el self está despues de los campos y de los metodos y además NO se captura


;;b)
#|
(define (Punto1D x-init)
  (OBJECT ([field x x-init])
          ([method x? () x]
           [method bigger-than (otherPoint) (if (> x (-> otherPoint x?))
                                                self
                                                otherPoint)])
  ))


unbound identifier con self pq no sabe quien es

|#

;;c)
(defmac (OBJECT ([field fname init] ... )
                ( [method mname args body] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname init ] ... )
               (let ([methods (list (cons 'mname (lambda args body)) ...)])
                  (λ (msg . vals)
                    (let ([found (assoc msg methods)])
                       (if found
                            (apply (cdr found) vals)
                            ( error "message not understood:" msg))))))])
                  self)
  )
                    