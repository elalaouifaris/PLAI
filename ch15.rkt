#lang plai-typed

(define-type Type
  [numT]
  [boolT]
  [funT (arg : Type) (ret : Type)])

(define-type TyExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [boolC (b : boolean)]
  [condC (tst : TyExprC) (thn : TyExprC) (els : TyExprC)]
  [appC (fun : TyExprC) (arg : TyExprC)]
  [plusC (l : TyExprC) (r : TyExprC)]
  [multC (l : TyExprC) (r : TyExprC)]
  [lamC (arg : symbol) (argT : Type) (retT : Type) (body : TyExprC)])

(define-type Binding
  [bind (name : symbol) (type : Type)])

(define-type-alias TyEnv (listof Binding))
(define mt-ty-env empty)
(define extend-ty-env cons)

(define (lookup [n : symbol] [tenv : TyEnv]) : Type
  (cond
    [(empty? tenv) (error 'lookup "name not found")]
    [(cons? tenv) (cond
                    [(equal? n (bind-name (first tenv))) (bind-type (first tenv))]
                    [else (lookup n (rest tenv))])]))

(define (tc [expr : TyExprC] [tenv : TyEnv]) : Type
  (type-case TyExprC expr
    [numC (n) (numT)]
    [idC (n) (lookup n tenv)]
    [boolC (b) (boolT)]
    [condC (tst thn els) (let ([tst-t (tc tst tenv)]
                               [thn-t (tc thn tenv)]
                               [els-t (tc els tenv)])
                          (cond
                            [(not (boolT? tst-t))
                             (error 'tc "test clause not boolean")]
                            [(not (equal? thn-t els-t))
                             (error 'tc "cond branches have different types")]
                            [else thn-t]))]
    [plusC (l r) (let ([lt (tc l tenv)]
                       [rt (tc r tenv)])
                   (if (and (numT? lt)
                            (numT? rt))
                       (numT)
                       (error 'tc "+ not both numbers")))]
    [multC (l r) (let ([lt (tc l tenv)]
                       [rt (tc r tenv)])
                   (if (and (numT? lt)
                            (numT? rt))
                       (numT)
                       (error 'tc "* not both numbers")))]
    [appC (f a) (let ([ft (tc f tenv)]
                      [at (tc a tenv)])
                  (cond
                    [(not (funT? ft))
                     (error 'tc "not a function")]
                    [(not (equal? (funT-arg ft) at))
                     (error 'tc "app arg mismatch")]
                    [else (funT-ret ft)]))]
    [lamC (a argT retT b)
          (if (equal? (tc b (extend-ty-env (bind a argT) tenv))
                      retT)
              (funT argT retT)
              (error 'tc "lam type mismatch"))]))

;; TESTS
(test (tc (numC 2) mt-ty-env)
      (numT))

(test (tc (multC (numC 1) (numC 2)) mt-ty-env)
      (numT))

(test (tc (lamC 'x (numT) (numT) (plusC (idC 'x) (numC 1))) mt-ty-env)
      (funT (numT) (numT)))

(test (tc (appC (lamC 'x (numT) (numT) (plusC (idC 'x) (numC 1)))
                (numC 1))
          mt-ty-env)
      (numT))

(test (tc (condC (boolC true) (numC 1) (numC 2)) mt-ty-env)
      (numT))

(test/exn (tc (appC (lamC 'x (funT (numT) (numT)) (numT) (plusC (idC 'x) (numC 1)))
                (numC 1))
          mt-ty-env)
      "not both numbers")