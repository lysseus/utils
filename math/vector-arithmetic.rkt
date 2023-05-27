#lang racket

;;;
;;; VECTOR ARITMETIC
;;;

(provide +/mod
         +/vector)

(module+ test
  (require rackunit
           (submod "..")))

;;
;; Add vals, applying mod n to the result.
(define/contract (+/mod n . vals)
  (->* (and/c natural? (>/c 0)) () #:rest (listof real?) any)
  (modulo (apply + vals) n))

(module+ test
  (test-case "+/mod"
             (check-equal? (+/mod 2 1 1) 0)
             (check-equal? (+/mod 2 1 1 1 1) 0)))

;; Add vectors, mod n is applied to each element of the resulting vector.
(define/contract (+/vector #:mod (mod 1) . vs)
  (->* () (#:mod (and/c natural? (>/c 0))) #:rest (or/c empty? (listof (vectorof real?))) any)
  (vector-map (λ (v) (modulo v 2))
              (if (empty? vs)
                  #()
                  (for/fold ([result (car vs)])
                        ([next (rest vs)])
                (for/vector ([v1 result]
                             [v2 next])
                  (+ v1 v2))))))

(module+ test
  (test-case "+/vector tests"
             (check-equal? (+/vector #(1 1 1) #(1 0 1)) #(0 1 0))
             (check-equal? (+/vector #(1 1 0)) #(1 1 0))
             (check-equal? (+/vector) #())))
