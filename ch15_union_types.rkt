#lang plai-typed

(define-type Animal
  [armadillo (alive? : boolean)]
  [boa (length : number)])

(define-type Plant
  [tree (heigth : number)])

(define-type LivingThings
  [animal (a : Animal)]
  [plant (p : Plant)])
;; Above are tagged unions