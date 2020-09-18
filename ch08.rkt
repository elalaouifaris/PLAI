#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (l : Location)])

(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  [v*s (v : Value) (s : Store)])

(define (interp [expr : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [idC (n) (v*s (fetch (lookup n env) sto) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                  [v*s (v-f s-f)
                       (type-case Result (interp a env s-f)
                         [v*s (v-a s-a)
                              (let [(where (new-loc))]
                                (interp (closV-body v-f)
                                        (extend-env (bind (closV-arg v-f)
                                                          where)
                                                    (closV-env v-f))
                                        (override-store (cell where  v-a)
                                                        s-a)))])])]
    [plusC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (numop + v-l v-r) s-r)])])]
    [multC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (numop * v-l v-r) s-r)])])]
    [lamC (a b) (v*s (closV a b env) sto)]
    [boxC (a) (type-case Result (interp a env sto)
                [v*s (v-a s-a)
                     (let [(where (new-loc))]
                       (v*s (boxV where)
                            (override-store (cell where v-a)
                                            s-a)))])]
    [unboxC (b) (type-case Result (interp b env sto)
                  [v*s (v-b s-b)
                       (v*s (fetch (boxV-l v-b) s-b) s-b)])]
    [setboxC (b v) (type-case Result (interp b env sto)
                     [v*s (v-b s-b)
                          (type-case Result (interp v env s-b)
                            [v*s (v-v s-v)
                                 (v*s v-v
                                      (update-store s-v
                                                    (boxV-l v-b)
                                                    v-v))])])]
                                   
    [seqC (b1 b2) (type-case Result  (interp b1 env sto)
                    [v*s (v-b1 s-b1)
                         (interp b2 env s-b1)])]))


(define (lookup [for : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error 'lookup "not found")]
    [(cons? env) (cond
                   [(equal? for (bind-name (first env))) (bind-val (first env))]
                   [else (lookup for (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "not found")]
    [(cons? sto) (cond
                   [(equal? loc (cell-location (first sto))) (cell-val (first sto))]
                   [else (fetch loc (rest sto))])]))
(define (update-store [sto : Store] [l : Location] [v : Value])
  (cons (cell l v)
        (filter (λ (c)
                  (not (equal? l
                               (cell-location c))))
                sto)))

(define (numop [op : (number number -> number)] [l : Value] [r : Value])
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else (error 'numop "one of given values is not of numV type")]))
    
(define new-loc
  (let [(n (box 0))]
    (λ ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))


  
    
;; ========================
;;         EXAMPLES
;; ========================

(define new-loc-broken
  (λ ()
    (let [(n (box 0))]
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))


;; ========================
;;         TESTS
;; ========================
(define (evaluate [exp : ExprC]) : Value
  (type-case Result (interp exp mt-env mt-store)
    [v*s (v s) v]))

(test (evaluate (plusC (numC 1) (numC 1)))
      (numV 2))

(test (evaluate (multC (numC 0) (numC 1)))
      (numV 0))

(test (evaluate (appC (lamC 'double (appC (idC 'double) (numC 10)))
                      (lamC 'x (plusC (idC 'x) (idC 'x)))))
      (numV 20))

(test (evaluate (appC (lamC 'b (plusC (plusC (seqC (setboxC (idC 'b) (numC 1))
                                                   (unboxC (idC 'b)))
                                             (seqC (setboxC (idC 'b) (numC 2))
                                                   (unboxC (idC 'b))))
                                      (unboxC (idC 'b))))
                      (boxC (numC 0))))
      (numV 5))