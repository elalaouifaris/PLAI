#lang plai

(define-syntax ليكن
  (syntax-rules ()
    [(my-let-1 (var val) body)
     ((λ (var) body) val)]))

(test (ليكن (x 10) (+ x x))
      20)

(define-syntax my-let-2
  (syntax-rules ()
    [(my-let-2 ([var val] ...) body)
     ((λ (var ...) body) val ...)]))

(test (my-let-2 [(x 10)
                 (y 20)]
                (+ x y))
      30)

(define-syntax (my-let-3 x)
  (syntax-case x ()
    [(my-let-3 (var val) body)
     (identifier? #'var)
     #'((λ (var) body) val)]))

(test (my-let-3 (x 10) (+ x x))
      20)

(define-syntax (my-or x)
  (syntax-case x ()
    [(my-or) #'#f]
    [(my-or e0 e1 ...)
     #'(if e0
           e0
           (my-or e1 ...))]))

(test (my-or #f #f #t)
      #t)

(let ([init #f])
  (my-or (begin (set! init (not init))
                init)
         #f))

(define-syntax (my-or-4 x)
  (syntax-case x ()
    [(my-or-4)
     #'#f]
    [(my-or-4 e)
     #'e]
    [(my-or-4 e0 e1 ...)
     #'(let ([v e0])
         (if v
             v
             (my-or-4 e1 ...)))]))

(let ([init #f])
  (my-or-4 (begin (set! init (not init))
                  init)
           #f))

(define-syntax (object/self-1 x)
  (syntax-case x ()
    [(object [mtd-name (var) val] ...)
     (with-syntax ([self (datum->syntax x 'self)])
       #'(let ([self (λ (msg-name)
                     (λ (v) (error 'object "nothing here")))])
         (begin
           (set! self
                 (λ (msg-name)
                   (case msg-name
                     [(mtd-name) (λ (var) val)]
                     ...)))
           self)))]))

(define os-1
  (object/self-1
   [first (x) (msg self 'second (+ x 1))]
   [second (x) (+ x 1)]))