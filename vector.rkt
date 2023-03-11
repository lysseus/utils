#lang racket


;;;
;;; VECTOR
;;;

(provide vector-findf)

(module+ test (require rackunit
                       (submod "..")))

;; Returns the index and value of the first element in the vector
;; for wich pred returns true. If no element is found then returns
;; false for the index and value.
(define (vector-findf vec pred)
  (for/fold ([idx #f]
             [val #f]) 
          ([v vec]
           [n (in-naturals)])  
 #:break (not (false? idx))
  (if (pred v)
      (values n v)
      (values idx val))))

(module+ test
  (test-case "vector-findf negative test"
             (define-values (idx val) (vector-findf #(0 0 0) (λ (v) (> v 0))))
             (check-equal? idx #f)
             (check-equal? val #f))
  (test-case "vector-findf positive test"
             (define-values (idx val) (vector-findf #(0 0 0 0 5 0 0 10) (λ (v) (> v 0))))
             (check-equal? idx 4)
             (check-equal? val 5)))
