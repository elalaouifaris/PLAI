#lang plai-typed

;; Exercise Add conditionals to ArithC
;; Continued from ch03.rkt

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [condC (p : ArithC) (t : ArithC) (e : ArithC)])

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [condC (p t e) (if (zero? (interp p))
                       (interp e)
                       (interp t))]))

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

(test
 (interp (condC (multC (numC 0) (numC 1))
                (numC 1)
                (numC 2)))
 2)

(test
 (interp (condC (plusC (numC 0) (numC 1))
                (numC 1)
                (numC 2)))
 1)