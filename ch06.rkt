#lang plai-typed
;; =============
;; ENVIRONEMENT
;; =============

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup n env)
  (cond
    [(empty? env) (error 'lookup "Undefined identifier")]
    [(cons? env) (cond
                   [(equal? n (bind-name (first env))) (bind-val (first env))]
                   [else (lookup n (rest env))])]))

;; ====================
;; FUNCTION DEFINITIONS
;; ====================
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (get-fundef f fds)
  (cond
    [(empty? fds) (error 'get-fundef "function not found")]
    [(cons? fds) (cond
                   [(equal? f (fdC-name (first fds))) (first fds)]
                   [else (get-fundef f (rest fds))])]))

;; ===================
;;     INTERPRETER
;; ===================

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  (multC (l : ExprC) (r : ExprC)))

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local [(define fd (get-fundef f fds))]
                  (interp (fdC-body fd)
                          (extend-env (bind (fdC-arg fd)
                                            (interp a env fds))
                                      ;; Lexical scoping with 'mt-env'.
                                      ;; using 'env' instead leads to dynamic scoping
                                      mt-env)
                          fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))

;; =========
;;   TESTS
;; =========

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)

(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)

(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)

(test/exn (interp (appC 'f1 (numC 3))
                  mt-env
                  (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                        (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))
          "Undefined identifier")