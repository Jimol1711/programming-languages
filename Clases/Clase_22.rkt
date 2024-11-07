#lang play

(define counter
  (let ([count 0])
    (lambda ()
      (begin
        (set! count (+ 1 count))
        count))))

(define counter2
  (let ([count 0])
    (lambda (cmd)
      (match cmd
        ['inc (begin (set! count (+ 1 count)) count)]
        ['dec (begin (set! count (- count 1)) count)]))))


(define stack
  (let ([vals '()])
    (let ([pop (lambda ()
                 (if (empty? vals)
                     (error "Cannot pop from empty stack")
                     (let ([val (car vals)])
                       (begin
                         (set! vals (cdr vals))
                         val))))]          
          [push (lambda (val) (set! vals (cons val vals)))])
    (lambda (cmd . args)
      (match cmd
        ['pop (pop)]
        ['push (push (car args))]
        [else (error "Invalid message")])))))
    

(define point
  (let ([x 0]
        [y 0])
    (let ([get-x (lambda () x)]
          [get-y (lambda () y)]
          [set-x (lambda (new-x) (begin (set! x new-x) x))]
          [set-y (lambda (new-y) (begin (set! y new-y) y))])
      (lambda (cmd . args)
        (match cmd
          ['get-x (get-x)]
          ['get-y (get-y)]
          ['set-x (set-x (car args))]
          ['set-y (set-y (car args))]
          [else (error "Message unknown")])))))


(defmac (OBJECT ([field fname init] ...)
                ([method mname args body] ...))
  #:keywords field method
  (let ([fname init] ...)
    (let ([methods (list (cons 'mname (lambda args body)) ...)])
      (lambda (msg . vals)
        (apply (cdr (assoc msg methods)) vals)))))

(defmac (-> o m arg ...)
  (o 'm arg ...))

(define stack2
  (OBJECT ([field vals '()])
          ([method pop () (begin
                            (if (empty? vals)
                                (error "Cannot pop from empty stack")
                                (let ([val (car vals)])
                                  (begin
                                    (set! vals (cdr vals))
                                    val))))]
           [method push (val) (set! vals (cons val vals))])))
                   




            


























  