#lang play

(defmac (my-while-bad cond body)
  (if cond
      (begin body (my-while cond body))
      (void)))

(defmac (my-while cond body)
  (letrec ([iter (λ ()
                   (if cond
                       (begin body (iter))
                       (void)))])
    (iter)))

(let ([x 0])
    (my-while
     (<= x 5)
     (begin (set! x (+ 1 x)) (printf "~a\n" x))))

;; Define automaton macro
;; Enfoque inicial: codificación manual

;; Función para estado "init" del ejemplo
#|
(λ (stream)
  (and (cons? stream)
       (case (first stream)
         [(c) (more (rest stream))] ;; <-- avanzar de estado es invocar la función del siguiente estado
         [else false])))
|#

(define end
  (λ (stream)
    (empty? stream))) ;; <-- aceptación es cuando no hay más simbolos por procesar

(define more
  (λ (stream)
    (match stream
      [(cons 'a rest) (more rest)]
      [(cons 'd rest) (more rest)]
      [(cons 'r rest) (end rest)]
      [_  #f])))

(define init (λ (stream)
  (match stream
    [(cons 'c rest) (more rest)]
    [_ #f])))       
       
(define input1 '(c a a d r)) ;; #t
(define input2 '(c a d a r)) ;; #t
(define input3 '(c a r a d r)) ;; #f
(define input4 '(c d d a)) ;; #f


;; Enfoque 2: codificación manual con letrec

;; (define m (automaton ...))
(define m
  (letrec ([init
            (λ (stream)
              (match stream
                [(cons 'c rest) (more rest)]
                [_ #f]))]

           [more
            (λ (stream)
              (match stream
                [(cons 'a rest) (more rest)]
                [(cons 'd rest) (more rest)]
                [(cons 'r rest) (end rest)]
                [_ #f]))]

           [end
            (λ (stream)
              (empty? stream))])
    init))

;; Enfoque 3: mecanizar para un automata arbitrario (slide 22)
(defmac (automaton init-state
                   final-state
                   [state : (action -> target) ...] ...)
  #:keywords : ->
  (letrec ([state
            (if (eq? (quote state) (quote final-state))
               ;; if final state
               (λ (stream)
                 (or (empty? stream)
                     (match stream
                       [(cons (quote action) rest) (target rest)] ...
                       [_ #f])))
               ;; if not final state
               (λ (stream)
                 (match stream
                   [(cons (quote action) rest) (target rest)] ...
                   [_ #f])))] ...)
    init-state))
            
            
(define m2 (automaton init
                      end
                      [init : (c -> more)]
                      [more : (a -> more)
                              (d -> more)
                              (r -> end)]
                      [end : ]))

(defmac (switch expr
               [case pred -> result] ...
               [default -> df-val])
  #:keywords case ->

  ;; Macro expansion
  (let ((val expr))  ;; Bind the value of the expression
    (cond
     ;; Process all cases
     ,@(map (lambda (case)
              (match case
                     [(list 'case pred -> result)
                      `(,(list 'if pred) val ,result)]))
            (filter (lambda (x) (match x [(list 'case _ _)] #t) (else #f)) expr))
     
     ;; Handle the default case
     [(default) df-val])))












