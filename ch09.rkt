#lang plai

(let [(b (box 'dummy))]
  (begin
    (set-box! b b)
    b))

;; ========================
;;    Function Recusion:
;; ========================

;; This fails with unbond error:
;; (let ([fact (λ (n)
;;               (if (equal? 0 n)
;;                   1
;;                   (* n (fact (- n 1)))))])
;;   (fact 10))

;;Using a mutable box
;;-------------------
(let [(fact (box 'dummy))]
  (let [(fact-fun
         (λ (n)
           (if (zero? n)
               1
               (* n ((unbox fact) (- n 1))))))]
    (begin
      (set-box! fact fact-fun)
      ((unbox fact) 10))))

(let [(fact (box 'dummy))]
  (begin
    (set-box! fact
              (λ (n)
                (if (zero? n)
                    1
                    (* n ((unbox fact) (- n 1))))))
    ((unbox fact) 10)))

;; Using a variable:
;;-----------------
(let [(fact 'dummy)]
  (begin
    (set! fact
          (λ (n)
            (if (zero? n)
                1
                (* n (fact (- n 1))))))
    (fact 10)))