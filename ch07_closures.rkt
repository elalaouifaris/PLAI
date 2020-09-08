#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

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
    [appC (f a) (local [(define f-value (interp f env))]
                  (if (closV? f-value)
                      (interp (closV-body f-value)
                          (extend-env (bind (closV-arg f-value)
                                            (interp a env))
                                       (closV-env f-value)))
                      (error 'appC "The given argument does not evalaute to funV")))]
    [plusC (l r) (numop + (interp l env) (interp r env))]
    [multC (l r) (numop * (interp l env) (interp r env))]
    [lamC (a b) (closV a b env)]))

(define (numop [op : (number number -> number)] [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'numop "one argument was not a number")]))

;; =========
;;   TESTS
;; =========

