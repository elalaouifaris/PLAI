#lang plai-typed
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup for env)
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [(cons? env) (cond
                   [(equal? for (bind-name (first env))) (bind-val (first env))]
                   [else (lookup for (rest env))])]))

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [appC (f a) (local [(define fd (interp f env))]
                  (if (funV? fd)
                      (interp (funV-body fd)
                          (extend-env (bind (funV-arg fd)
                                            (interp a env))
                                       mt-env))
                      (error 'appC "The given argument does not evalaute to funV")))]
    [plusC (l r) (numop + (interp l env) (interp r env))]
    [multC (l r) (numop * (interp l env) (interp r env))]
    [fdC (n a b) (funV n a b)]))

(define (numop [op : (number number -> number)] [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'numop "one argument was not a number")]))

;; =========
;;   TESTS
;; =========
(test (interp (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

(test/exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                      (numC 4)))
                    (numC 3))
              mt-env)
          "name not found")
      