#lang plai-typed

;; Data type for the surface language
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1)
                                 (desugar r)))]
    [uminusS (e) (desugar (bminusS (numS 0) e ))]))

;; ===========
;;    TESTS
;; ===========

(test
 (desugar (numS 1))
 (numC 1))

(test
 (desugar (plusS (numS 1) (numS 1)))
 (plusC (numC 1) (numC 1)))

(test
 (desugar (multS (numS 1) (numS 1)))
 (multC (numC 1) (numC 1)))

(test
 (desugar (bminusS (numS 1) (numS 3)))
 (plusC (numC 1)
        (multC (numC -1) (numC  3))))

(test
 (desugar (uminusS (numS 2)))
 (plusC (numC 0)
        (multC (numC -1)
               (numC 2))))

(test
 (desugar (uminusS (uminusS (numS 2))))
 (plusC (numC 0)
        (multC (numC -1)
               (plusC (numC 0)
                      (multC (numC -1)
                             (numC 2))))))