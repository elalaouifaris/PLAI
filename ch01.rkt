#lang plai-typed
(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(define ma1 : MisspelledAnimal (caml 2))
(define ma2 : MisspelledAnimal (yacc 1.9))

;; Also Works with type infference
;; (define ma1 (caml 2))
;; (define ma2 (yacc 1.9))

(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
    [caml (humps)  (>= humps 2)]
    [yacc (height) (> height 2.1)]))

(test (good? ma1) #t)
(test (good? ma2) #f)