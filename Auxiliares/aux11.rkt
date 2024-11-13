#lang play

(print-only-errors #t)

; P1
; No es TR ya que no retorna la misma función
(define (concatTRO l1 l2)
  (define (concat-acc l1* l2* acc)
    (match l1*
      ['() (append acc l2*)]
      [ (list x xs ...) (concat-acc xs l2* (append acc (list x)))]
      ))
  (concat-acc l1 l2 '()))

; P2
; a)
(λ (f)
  (match f
    [ 'a 1]
    [ 'b 2]
    [ 'c 3]
    [_ (error 'record " invalid key: ~a" f ) ]
    ))

; b)
(defmac (my-record record [field-name : field-value] ...)
  #:keywords record :
  (lambda (field)
    (match field
      [ ‘field-name field-value ] ...
      ; ; Aqui si es necesario el error clause:
      [_ (error 'record " invalid key: ~a" field ) ]
      ))
  )