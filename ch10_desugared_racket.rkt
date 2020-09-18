#lang plai

;; Object Definition
(define o-1
  (λ (m)
    (case m
      [(add1) (λ (x) (+ x 1))]
      [(sub1) (λ (x) (- x 1))])))

(test ((o-1 'add1) 5)
      6)

;; Method Invocation
(define (msg o m . a)
  (apply (o m) a))

(test (msg o-1 'add1 5)
      6)

(define o-2
  (λ (m)
    (case m
      [(add) (λ (x y) (+ x y))]
      [(sub1) (λ (x) (- x 1))])))

(test (msg o-2 'add 2 3)
      5)

;; Constructors
(define (o-constr-1 x)
  (λ (m)
    (case m
      [(addX) (λ (y) (+ x y))])))

(test (msg (o-constr-1 5) 'addX 3)
      8)
(test (msg (o-constr-1 2) 'addX 3)
      5)

;; State
(define (o-state-1 count)
  (λ (m)
    (case m
      [(inc) (λ () (set! count (+ count 1)))]
      [(dec) (λ () (set! count (- count 1)))]
      [(get) (λ () count)])))

(test (let [(o (o-state-1 5))]
        (begin (msg o 'inc)
               (msg o 'dec)
               (msg o 'get)))
      5)

(test (let [(o1 (o-state-1 3))
            (o2 (o-state-1 3))]
        (begin (msg o1 'inc)
               (msg o1 'inc)
               (+ (msg o1 'get)
                  (msg o2 'get))))
      (+ 5 3))

;; Private Members -- Count is private to the object below
(define (o-state-2 init)
  (let [(count init)]
    (λ (m)
      (case m
        [(inc) (λ () (set! count (+ count 1)))]
        [(dec) (λ () (set! count (- count 1)))]
        [(get) (λ () count)]))))

;; Static Members
(define o-static-1
  (let [(counter 0)]
    (λ (amount)
      (begin
        (set! counter (+ 1 counter))
        (λ (m)
          (case m
            [(inc) (λ (n) (set! amount (+ amount n)))]
            [(dec) (λ (n) (set! amount (- amount n)))]
            [(get) (λ () amount)]
            [(count) (λ () counter)]))))))

(test (let [(o (o-static-1 1000))]
        (msg o 'count))
      1)
(test (let [(o (o-static-1 0))]
        (msg o 'count))
      2)

;; Self-Reference
;;Using Mutation
(define o-self!
  (let [(self 'dummy)]
    (begin
      (set! self
            (λ (m)
              (case m
                [(first) (λ (x) (msg self 'second (+ x 1)))]
                [(second) (λ (x) (+ x 1))])))
      self)))

(test (msg o-self! 'first 5)
      7)

;; Without Mutation
(define o-self-no!
  (λ (m)
    (case m
      [(first) (λ (self x) (msg/self self 'second (+ x 1)))]
      [(second) (λ (self x) (+ x 1))])))

(define (msg/self o m . a)
  (apply (o m) o a))

;; Dynamic Dispatch

(define (mt)
  (let [(self 'dummy)]
    (begin
      (set! self
            (λ (m)
              (case m
                [(add) (λ () 0)])))
      self)))

(define (node v l r)
  (let [(self 'dummy)]
    (begin
      (set! self
            (λ (m)
              (case m
                [(add) (λ () (+ v
                                (msg l 'add)
                                (msg r 'add)))])))
      self)))

(define a-tree
  (node 10
        (node 5 (mt) (mt))
        (node 15 (node 6 (mt) (mt)) (mt))))

(test (msg a-tree 'add)
      (+ 10 5 15 6))
              
;; Classes
(define (node/size parent-maker v l r)
  (let [(parent-object (parent-maker v l r))
        (self 'dummy)]
    (begin
      (set! self
            (λ (m)
              (case m
                [(size) (λ () (+ 1
                                 (msg l 'size)
                                 (msg r 'size)))]
                [else (parent-object m)])))
      self)))

(define (mt/size parent-maker)
  (let [(parent-object (parent-maker))
        (self 'dummy)]
    (begin
      (set! self
            (λ (m)
              (case m
                [(size) (λ () 0)]
                [else (parent-object m)])))
      self)))

(define a-tree/size
  (node/size node
             10
             (node/size node 5 (mt/size mt) (mt/size mt))
             (node/size node 15
                        (node/size node 6 (mt/size mt) (mt/size mt))
                        (mt/size mt))))

(test (msg a-tree/size 'add)
      (+ 10 5 15 6))
(test (msg a-tree/size 'size)
      4)