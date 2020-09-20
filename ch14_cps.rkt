#lang plai

(define-syntax (cps e)
  (syntax-case e (with rec lam cnd seq set quote display read-number)
    [(_ (with (v e) b))
     #'(cps ((lam (v) b) e))]
    [(_ (rec (v f) b))
     #'(cps (with (v (lam (arg) (error 'dummy "nothing")))
                  (seq
                   (set v f)
                   b)))]
    [(_ (lam (a1 a2) b))
     (and (identifier? #'a1) (identifier? #'a2))
     #'(λ (k)
         (k (λ (a1 a2 dyn-k)
              ((cps b) dyn-k))))]
    [(_ (lam (a) b))
     (identifier? #'a)
     #'(λ (k)
         (k (λ (a dyn-k)
              ((cps b) dyn-k))))]
    [(_ (lam () b))
     #'(λ (k)
         (k (λ (dyn-k)
              ((cps b) dyn-k))))]
    [(_ (cnd tst thn els))
     #'(λ (k)
         ((cps tst) (λ (tstv)
                      (if tstv
                          ((cps thn) k)
                          ((cps els) k)))))]
    [(_ (display output))
     #'(λ (k)
         ((cps output) (λ (ov)
                         (k (display ov)))))]
    [(_ (read-number prompt))
     #'(λ (k)
         ((cps prompt) (λ (pv)
                         (read-number/suspend pv k))))]
    [(_ (seq e1 e2))
        #'(λ (k)
            ((cps e1) (λ (_)
                        ((cps e2) k))))]
    [(_ (set v e))
     #'(λ (k)
         ((cps e) (λ (ev)
                    (k (set! v ev)))))]
    [(_ 'e)
     #'(λ (k) ( 'e))]
    [(_ (f))
     #'(λ (k)
         ((cps f) (λ (fv)
                    (fv k))))]
    [(_ (f a))
     #'(λ (k)
         ((cps f) (λ (fv)
                    ((cps a) (λ (av)
                               (fv av k))))))]
    [(_ (f a b))
     #'(λ (k)
         ((cps a) (λ (av)
                    ((cps b) (λ (bv)
                               (k (f av bv)))))))]
    [(_ atomic)
     #'(λ (k)
         (k atomic))]))

(define (run c) (c identity))

;; Testing
(test (run (cps 3))
      3)
(test (run (cps ((lam () 5))))
      5)
(test (run (cps ((lam (x)   (* x x)) 5)))
      25)
(test (run (cps (+ 5 ((lam (x) (* x x)) 5))))
      30)