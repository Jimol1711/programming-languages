#lang play
(print-only-errors)
;;P1)

;; a)
;; my-map :: Fun (A->B) (ListOf B) -> (ListOf B)
;; Aplica la funcion a todos los elementos de la lista, retornando la lista resultante
(define (my-map f lst)
  (if (empty? lst)
      '()
      (cons (f (car lst)) (my-map f (cdr lst)))
  )
)
(test (my-map add1 '(1 2 3)) '(2 3 4))
(test (my-map even? '(1 2 3)) '(#f #t #f))
(test (my-map identity '(1 2 3)) '(1 2 3))
(test (my-map add1 '()) '())

;; b) 
;; my-foldl :: Fun (A B -> B) B (ListOf A) -> B
;; Aplica sucesivamente la funcion que recibe al resultado
;; de su anterior aplicacion (o el valor inicial) y a un elemento de la lista.
(define (my-foldl f v lst)
  (if (empty? lst)
      v
      (my-foldl f (f (car lst) v) (cdr lst))
  )
)
(test (my-foldl + 0 '(1 2 3)) 6)
(test (my-foldl cons '() '(1 2 3)) '(3 2 1))
(test (my-foldl even? 1 '()) 1)

;; c)
;; my-filter :: Fun (A -> Bool) (ListOf A) -> (ListOf A)
;; Recibe una lista y predicado y devuelve una lista con valores que 
;; cumplan el predicado.
(define (my-filter pred lst)
  (if (empty? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (my-filter pred (cdr lst)))
          (my-filter pred (cdr lst))
      )
  )
)
(test (my-filter even? '(1 2 3 4)) '(2 4))
(test (my-filter integer? '( "a" 4 20 2+3i)) '(4 20))


;;P2) a)
;; calcular_prom :: ListOf[Sting/Boolean] -> Float
;; calcula el promedio de edades de una lista dada

(define (calcular_prom l)
  ;;primero debemos sacar los elementos basura que son los #f
  (define lista_limpia (filter (λ (x) (not (boolean? x))) l) )
  (if (empty? lista_limpia)
      (error "la lista solo contenia basura")
      (let ( [ largo_lista (length lista_limpia) ]
             [ suma_total (foldl + 0 (map string->number lista_limpia))])
      (/ suma_total largo_lista)
      )
  )
)
  
(test (calcular_prom (list "10" #f "40" "10" #f)) 20)
(test (calcular_prom (list "20")) 20)
(test/exn (calcular_prom (list  #f #f))"la lista solo contenia basura")

;;P2 b)
;; lo mismo pero ahora un elemento también puede ser una lista con lo mismo
;; define A = ListOf[Sting/Boolean/A]
;; string_or_recursivo :: String/ListOf[Sting/Boolean/A] -> Number
(define (string_or_recursivo element)
  (match element
    [ (? string?) (string->number element) ]
    [ (? list?) (calcular_prom_rec element) ]
    ))

      
(define (calcular_prom_rec l)
  ;;primero debemos sacar los elementos basura que son los #f
  (define lista_limpia (filter (λ (x) (not (boolean? x))) l) )
  (if (empty? lista_limpia)
      (error "la lista solo contenia basura")
      (let ( [ largo_lista (length lista_limpia) ]
              [ suma_total (foldl + 0 (map string_or_recursivo lista_limpia))])
      (/ suma_total largo_lista)
      )
  )
)

(test (calcular_prom_rec (list "10" #f "40" "10" #f)) 20)
(test (calcular_prom_rec (list "20")) 20)
(test/exn (calcular_prom_rec (list  #f #f))"la lista solo contenia basura")
(test (calcular_prom_rec  (list "10" (list "10" #f "40" "10" #f)  "40" "10" #f)) 20)
(test (calcular_prom_rec (list (list "10" #f "40" "10" #f))) 20)
(test/exn (calcular_prom_rec (list (list #f #f) #f)) "la lista solo contenia basura")


;; P3
;; a) curry-n :: Int Fun (A1 ... An -> B) -> Fun (A1 -> (... -> (An -> B)))
;; Recibe una funcion de n argumentos y devuelve su version currificada
(define (curry-n n f)
  ;; Fun aux para arg extra
  (define (curry-n-aux n f args)
    (if (zero? n)
        (apply f (reverse args))
        (lambda (x) (curry-n-aux (- n 1) f (cons x args)))
    )
  )
  (curry-n-aux n f '())
)
(test (((curry-n 2 +) 1) 2) (+ 1 2))
(test (((curry-n 2 -) 1) 2) (- 1 2))


#|
Partimos de la nocion de que una funcion currificada es del tipo:

(lambda (x y) (+ x y)) -> (lambda (x) (lambda (y) (+ x y)))

La idea es armar el lambda de forma recursiva, acumulando los argumentos en una lista. 

1. El caso base es cuando n es igual a 0,
donde se tiene que todos los elementos que recibe la función f ya están disponibles,
en particular están almacenados en args, con lo que ahora solo se debe aplicar la función
f con estos argumentos.

2. En el caso en que n es mayor a 0, aún faltan argumentos para poder aplicar la
función f, por lo que se crea una función anónima que esperará dicho argumento, y una vez recibido,
verificará si se deben recibir más o si ya se puede aplicar la función f. Para hacer esto último
se hace la recursión (curry-aux (- n 1) f (cons x args)) dentro de dicha función anónima.

Por ultimo  se llama bajo el estado inicial, que es (curry-aux n f '()). 
Aquí se tiene una función f de n argumentos que no tiene ningún argumento
inicial almacenado.

Nota: apply toma una funcion y una lista y aplica los elementos de la lista como argumentos en orden
Ej: (apply + '(1 2 3)) <=> (+ 1 2 3)

Por ejemplo para caso n = 2
(curry-n-aux 2 f '())
-> (lambda (x) (curry-n-aux (- 2 1) f (cons x '())))
-> (lambda (x) (lambda (y) (curry-n-aux (-1 1) f (cons y '(x))))))
-> (lambda (x) (lambda (y) (apply f (reverse '(y x))) ))) // caso base apply
-> (lambda (x) (lambda (y) (apply f '(x y)) ))) // reverse ya que la lista se arma al reves.

De esta forma currificamos f y el body de la funcion seria usar apply con la funcion. Y cada 
vez que la llamamos con un argumento, definimos un elemento de la lista.
|#

;; b) uncurry-2 :: Fun (A -> (B -> C)) -> Fun (A B -> C)
;; Toma una funcion currificada de 2 argumentos y devuelve su version normal.
(define (uncurry-2 f)
  (λ (x y) ((f x) y)))


(define curry-sum (λ (a) (λ (b) (+ a b))))
(define curry-res (λ (a) (λ (b) (- a b))))
(test ((uncurry-2 curry-sum) 1 2) ((curry-sum 1) 2))
(test ((uncurry-2 curry-res) 1 2) ((curry-res 1) 2))