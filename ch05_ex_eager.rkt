#lang plai-typed

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(symbol=? s (fdC-name (first fds)))
     (first fds)]
    [else (get-fundef s (rest fds))]))
  
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local [(define fd (get-fundef f fds))]
                  (interp (subst (interp a fds)
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interp l fds)
                    (interp r fds))]
    [multC (l r) (* (interp l fds)
                    (interp r fds))]))

(define (subst [what : number] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) (numC what)]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))
    
;; =========
;;   TESTS
;; =========

;; Basic Operations
(test
 (interp (numC 1)
         (list))
 1)

(test
 (interp (plusC (numC 1) (numC 1))
         (list))
 2)

(test
 (interp (multC (numC 2) (numC 2))
         (list))
 4)

;; Finding function definitions
(local [(define f1 (fdC 'f1 'x (idC 'x)))
        (define f2 (fdC 'f2 'y (idC 'y)))
        (define fds (list f1 f2))]
  (begin
    (test (get-fundef 'f1 fds) f1)
    (test (get-fundef 'f2 fds) f2)))

;; Function application
(local [(define double
          (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
        (define quadruple
          (fdC 'quadruple 'x (idC 'x))) ;; TO DO
        (define const5
          (fdC 'const5 '_ (numC 5)))
        ;; Function list definition:
        (define fds (list double quadruple const5))]
  (begin
    (test (interp (appC 'double (numC 1)) fds)
          2)
    (test (interp (appC 'double (appC 'double (numC 1))) fds)
          4)))