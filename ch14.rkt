#lang plai-typed

(define (read-number [prompt : string]) : number
  (begin
    (display prompt)
    (let [(v (read))]
      (if (s-exp-number? v)
          (s-exp->number v)
          (read-number prompt)))))

;; (+ (read-number "First number")
;;    (read-number "Second number"))


;; Program Decomposition into now and Later

(λ (v1)
  (display
   (+ v1
      (read-number "Second number"))))

(define-type-alias label number)

(define new-label
  (let [(label 0)]
    (λ ()
      (begin
        (set! label (add1 label))
        label))))

(define table (make-hash empty))

(define (read-number/suspend [prompt : string] rest)
  (let [(g (new-label))]
    (begin
      (hash-set! table g rest)
      (display prompt)
      (display " To enter it, use the action field label ")
      (display g))))

(read-number/suspend "First number"
                     (λ (v1)
                       (read-number/suspend "Second number"
                                            (λ (v2)
                                              (display
                                               (+ v1 v2))))))


(define (resume [g : label] [n : number])
  ((some-v (hash-ref table g)) n))

;; CPS by desugaring
