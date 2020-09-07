#lang lazy
;; Pair Structure:
(define (pair A B)
  (λ (sel)
    (sel A B)))

(define (fst A B) A)
(define (snd A B) B)

;; Conditionals:
(define (cond C T E)
  (C T E))
(define (truth A B) A)
(define (falsity A B) B)

;; Factorial How to make a lambda version
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (sub1 n)))))

;; Numbers:
(define z (λ (f) (λ (x) x)))
(define (succ N)
  (λ (f)
    (λ (x)
      (f ((N f) x)))))

(define (n->i N) ((N add1) 0))
(define (i->n i) (if (zero? i) z (succ (i->n (sub1 i)))))

;; M + N = 1 + 1 + ... + 1 + N
(define ((add M) N)
  ((M succ) N))

;; M * N = M + ... + M + 0
(define ((mult M) N)
  ((N (add M)) z))

;; Check zero?
(define (z? N)
  ((N (λ (_) falsity)) truth))

;; < 0, 0 >
;; < 0, 1 >
;; < 1, 2 >
;; < 2, 3 >
(define (pred N)
  (((N (λ (p)
         (pair
          (p snd)
          (succ (p snd)))))
    (pair z z))
   fst))

;; How to express unbounded recursion with lambda's
(define BOMB (error 'not "defined"))

(define mk-fact
  (λ (f)
    (λ (n)
      (if (zero? n)
          1
          (* n ((f f) (sub1 n)))))))

(define fact_b (mk-fact mk-fact))

;; The Y Combinator
;; (Y F) = (F (Y F))
(define Y
  (λ (p)
     ((λ (f)
        (f f))
      (λ (f)
        (p (f f))))))

(define fact_p
  (Y
   (λ (fct)
     (λ (n)
       (if (zero? n)
           1
           (* n (fct (sub1 n))))))))


