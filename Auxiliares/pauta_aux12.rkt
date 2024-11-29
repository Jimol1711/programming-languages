#lang play

(print-only-errors #t)


;;P1
(defmac (OBJECT ([field fname init] ...)
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


(defmac (--> obj m args ...)
  (obj 'm args ...))

;;antes de corregir el error lo que pasa es que los metodos no saben quienes son los campos, por lo tanto
;;si un metodo intenta llamar a un campo, le daría un unbound identifier

;;un ejemplo para ver que funciona seria una funcion que cree un punto en el plano
(define (punto-2d x-init y-init)
  (OBJECT ([field x x-init] [field y y-init])
          ([method x? () x] [method y? () y])))

(def p1 (punto-2d 1 1))
(test (--> p1 x?) 1)
(test (--> p1 y?) 1)
                

;;b)
;; in-node :: Int Tree Tree -> Tree
;;crea un arbol a partir de un valor y 2 hijos (lado izq y der)
(define (in-node v l-init r-init)
  (OBJECT ([field l l-init]
           [field r r-init]
           [field valor v])
          ([method height () (+ 1 (max (--> l height) (--> r height)))]) ;;la altura es el maximo entre ambos
          ))

;; in-node :: Int -> Leaf
;;crea un objeto hoja
(define (leaf v)
  (OBJECT ([field valor v])
          ([method height () 0]) ;;una hoja tiene altura cero
          ))

;;prueba de que funciona
(def arbol (in-node 1 (in-node 2 (leaf 1) (leaf 0)) (leaf 4)))
(test (--> arbol height) 2)



;;--------------------P2---------------------
(defmac (switch input
               [case predicado -> resultado] ...
               [default -> valor_final])
  #:keywords case default ->
  (cond [(predicado input) resultado]
        ...
        [else valor_final]))

;;swich-fun :: Int -> A
;;Funcion que recibe el input y evalua este con la macro switch
(define (switch-fun input)
  (switch input
       [case positive? -> (begin
                            (println "primero")
                            7)]
       [case negative? -> (begin
                            (println "segundo")
                            40)]
       [case number? -> (begin
                            (println "tercero")
                            1729)]
       [default -> (begin
                         (println "ultimo")
                         8)]))

;;solo imprime "Segundo", a pesar de que de que tambien cumple
;;el tercer caso
(test (switch-fun (- 14 17)) 40)


;;-----------------P3-----------------------
(require "final_class.rkt")


(define (Cuadrado largo-init ancho-init)
  (CLASS ([field largo largo-init]
          [field ancho ancho-init])
         ([method area () (* (? largo) (? ancho))]
          [method bigger-than (otroCuadrado) (if (> (-> otroCuadrado area) (-> self area))
                                                 otroCuadrado
                                                 self)])
         ))

(define c1 (new (Cuadrado 1 3)))
(define c2 (new (Cuadrado 2 4)))
(test (-> c1 area) 3)
(test (-> c2 area) 8)
(test (-> (-> c1 bigger-than c2) area) 8)


;;-----------------P4-----------------------


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

(define m (ND-FSA
           (init : q0) ; estado inicial
           (final : acc1 acc2) ; estados de aceptacion
           (transitions : [q0 : (a → q1) ; transiciones
                                (b → acc2)]
                          [q1 : (a → q1)
                                (b → q1 acc1)]
                          [acc1 : ]
                          [acc2 : (a → acc2)
                                  (b → acc2)])))



(test (m '(a b a b)) #t)
(test (m '(a b a b a)) #f)
(test (m '(b a b)) #t)



;;-------------------P5-------------------
(define (last-element lst)
  (if (pair? (cdr lst))
      (last-element (cdr lst))
      (cdr lst)))


(defmac (AFND (init : initial-state)
                (final : accepting-states ...)
                (transitions : [state : (action → target ...) ...] ...))
  #:keywords : ->
  (letrec
      ([state
        (if (member (quote state) '(accepting-states ...))
            (lambda (stream estado)
              (if (empty? stream)
                  (cons estado '())
                  (case (car stream)
                    [(action)  (cons estado
                                 (or (target (cdr stream) 'target) ...))]
                    ...
                    [else #f])))
            ;;estado no final
            (lambda (stream estado)
              (and (pair? stream)
                  (case (car stream)
                     [(action) (cons estado
                                 (or (target (cdr stream) 'target) ...))]
                     ...
                     [else #f]))))]
       ...)
     (λ (input)
       (let ([resultado (initial-state input 'initial-state)])
         (if (last-element resultado)
             resultado
             #f)))))
             


(define m2 (AFND
           (init : q0) ; estado inicial
           (final : acc1 acc2) ; estados de aceptacion
           (transitions : [q0 : (a → q1) ; transiciones
                                (b → acc2)]
                          [q1 : (a → q1)
                                (b → q1 acc1)]
                          [acc1 : ]
                          [acc2 : (a → acc2)
                                  (b → acc2)])))



(test (m2 '(a b a b)) '(q0 q1 q1 q1 acc1))
(test (m2 '(a b a b a)) #f)
(test (m2 '(b a b)) '(q0 acc2 acc2 acc2))


  

