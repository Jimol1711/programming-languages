#lang play


; P1
; a) No, existen patrones de autoaplicación, así como combinadores de punto
; fijo que permiten recursión sin aplicar mutación dentro de la implementación del lenguaje.

; b) Si vemos la ejecución paso a paso:
; [ (rec x e b)
; (interp b (cyclic-env 'x (id x) env))]
; -> (def box-val (box 'undefined)) ;; 1
; -> (def new-env (aRecEnv 'x box-val env)) ;; 2
; -> (def clo-val (interp (id x) new-env)) ;; 3
; ;;;;;; -> (def clo-val 'undefined) ;; 3
; -> (set-box! box-val clo-val ) ;; 4
; ;;;;;; -> (set-box! box-val 'undefined) ;; 4
; -> new-env
; En el paso 3, al interpretar la expresión nombrada, se busca el valor de x que de
; momento está indefinido (basta ver cómo se extiende el ambiente en 2). De modo
; que el valor que se le asigna a la clausura es indefinido, y por tanto, también el
; contenido de la caja asociada a x.

; c) Llamado por la cola (o tail call ocurre cuando la última instrucción de una
; función es una llamada a alguna otra función, si el llamado es recursivo entonces
; hablamos de recursión por la cola (o tail recursion). Por ejemplo:
; ; TAIL CALL ;; TAIL REC ;; None
(define (bar x) #f)
(define ... #f)

(define (TC x)
  ...
  (bar ...))

(define (TREC x)
  ...
  (TREC ...))

(define (none x)
  (+ ... (none 11)))

;P2

; a) Puede ser cualquier función que no posea llamada por la cola.
(define (fact n)
  (if (= 0 n)
      1
      (* n ( fact (- n 1)))))
  
; La función factorial debe multiplicar posterior al llamado recursivo, por lo que no es llamada por la cola.

; b) Basta con que sea recursiva por la cola.
(define (even n)
  (cond
    ([= n 0] #t)
    ([= n 1] #f)
    (else (even (- n 2)))))

; En este caso, el llamado recursivo es la última instrucción que ejecuta la función,
; de modo que no es necesario recordar nada del llamado previo.

;c) Basta con que posea un llamado por la cola no recursivo.
(define (even2 n)
  (if (= n 0)
      #t
      (odd (- n 1))))

(define (odd n)
  (if (= n 1)
      #t
      (even2 (- n 1))))

; En este caso, el llamado a odd es la última instrucción que ejecuta even, y
; viceversa.


; P3
; Se utiliza Continuation-Passing Style, el cual consiste en entregar una función
; que indique qué hacer una vez que se llega a una rama terminal (no recursiva) de
; la función. Esta función puede irse componiendo a medida que se realizan llamados
; recursivos.
(deftype Expr
  (num n)
  (add l r))

; ; cont :: Num -> Num
; ; Que hacer una vez calculado algo
(define (interp expr cont)
  (match expr
    [(num n) (cont n)]
    [(add l r)
     (interp l (λ (vl)
                 (interp r (λ (vr)
                             (cont (+ vl vr))))))] ))

; En el caso num, se aplica la función cont directamente. Por otro lado, en la suma
; no se pueden realizar 2 llamados a interp simultáneos y luego sumar, por lo que se
; calcula el valor izquierdo, dejando a cont el trabajo de evaluar la expresión derecha,
; y posterior a ello sumar ambos valores.

;P4
; a) porque en el caso recursivo concat no es llamado en posicion de cola.
; b)
(define (concat l1 l2)
  (concatTR (reverse l1) l2))
                
(define (concatTR l1 l2)
  (match l1
    [(list) l2]
    [(cons x xs) (concatTR xs (cons x l2))]))