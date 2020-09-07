#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

;; ===========
;;    TESTS
;; ===========

(test
 (interp (numC 1))
 1)

(test
 (interp (plusC (numC 1) (numC 1)))
 2)

(test
 (interp (multC (numC 2) (numC 2)))
 4)