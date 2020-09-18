#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [objC (ns : (listof symbol)) (es : (listof ExprC))]
  [msgC (o : ExprC) (n : symbol)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [objV (ns : (listof symbol)) (vs : (listof Value))])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

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
    [lamC (a b) (closV a b env)]
    [objC (ns es) (objV ns (map (λ (e)
                                  (interp e env))
                                es))]
    [msgC (o n) (lookup-msg n (interp o env))]))

(define (numop [op : (number number -> number)] [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'numop "one argument was not a number")]))

(define (lookup for env)
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [(cons? env) (cond
                   [(equal? for (bind-name (first env))) (bind-val (first env))]
                   [else (lookup for (rest env))])]))

(define (lookup-msg [n : symbol] [o : Value])
  (type-case Value o
    [objV (ns vs) (lookup-msg-inner n ns vs)]
    [else (error 'lookup-msg "the given value is not a object")]))

(define (lookup-msg-inner n [ns : (listof symbol)] [vs : (listof Value)])
          (cond
            [(empty? vs) (error 'lookup-msg "Method not found.")]
            [(cons? vs) (cond
                          [(equal? n (first ns)) (first vs)]
                          [else (lookup-msg-inner n (rest ns) (rest vs))])]))
