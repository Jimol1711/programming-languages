#lang plai-typed

(print-only-errors #t)

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

;; two versions of good? (one with p-match another with cond)
(define (good?-pm [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
    [caml (humps) (>= humps 2)]
    [yacc (height) (> height 2.1)]))

(define (good?-cond [ma : MisspelledAnimal]) : boolean
  (cond
    [(caml? ma) (>= (caml-humps ma) 2)]
    [(yacc? ma) (> (yacc-height ma) 2.1)]))

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (cond : ExprC) (then : ExprC) (else : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Binding
  [bind (name : symbol) (val : number)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

; adding functions
(define-type FunDefC
  [funC (name : symbol) (arg : symbol) (body : symbol)])

(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; get-fundef : symbol * (listof FunDefC) -> FunDefC
; helper func definition finder
;; (define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
;;   (cond
;;     [(empty? fds) (error 'get-fundef "reference to undefined function")]
;;     [(cons? fds) (cond
;;                   [(equal? n (fdC-name (first fds))) (first fds)]
;;                   [else (get-fundef n (rest fds))])]))

; subst : ExprC * symbol * ExprC -> ExprC
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC (subst what for f) (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]
    [ifC (cond then else-branch) 
      (ifC (subst what for cond)
           (subst what for then)
           (subst what for else-branch))]
    [fdC (name arg body)
      (cond
        [(symbol=? arg for) in]
        [else (fdC name arg (subst what for body))])]))

(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [(symbol=? for (bind-name (first env))) (numV (bind-val (first env)))]
    [else (lookup for (rest env))]))

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))
 
(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a)
      (local ([define funval (interp f env)]
              [define argval (interp a env)])
        (if (funV? funval)
            (interp (funV-body funval)
                    (extend-env (bind (funV-arg funval) argval)
                                env))
            (error 'interp "Expected a function, got something else")))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [ifC (cond then else-branch) (if (not (= (interp cond env) 0))
                                      (interp then env)
                                      (interp else-branch env))]
    [fdC (n a b) expr]))

; tests for interp
(test (interp (numC 3)) 3)
(test (interp (plusC (numC 3) (numC 4))) 7)
(test (interp (multC (numC 3) (numC 4))) 12)

(test (interp (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)

(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)

(test/exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          "name not found")
                        

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [ifS (cond : ArithS) (then : ArithS) (else : ArithS)])

(define (desugar [as : ArithS]) : ExprC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [uminusS (e) (desugar (bminusS (numS 0) e))]
    [ifS (cond then else-branch) (ifC (desugar cond)
                               (desugar then)
                               (desugar else-branch))]))

(test (interp (ifC (numC 0) (numC 1) (numC 2))) 2)
(test (interp (ifC (numC 5) (numC 1) (numC 2))) 1)
(test (interp (ifC (multC (numC 2) (numC 0)) (numC 3) (numC 4))) 4) 
(test (interp (ifC (plusC (numC 0) (numC 0)) (numC 10) (numC 20))) 20)