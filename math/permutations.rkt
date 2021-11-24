#lang racket

(provide factorial
         k-permutations
         combinations
         permutations/distinct
         number-of-permutations)

;; Returns the number of k-permuations that give the same k-comination when the order is ignored.
(define (factorial n)
  (for/product ([v (in-range 1 (add1 n))]) v))

;; Returns the number of k-permuations of n.
;; The number of seuuences of k distinct elements of S.
(define (k-permutations n k)
  (for/product ([v (range (add1 (- n k)) (add1 n))]) v))

(define (combinations n k)
  ;; The numerator gives the number of k-permutations of n, i.e., of sequences of k
  ;; distinct elements of S, while the denominator gives the number of such k-permutations
  ;; that give the same k-combination when the order is ignored.
  (/ (k-permutations n k)
     (factorial k)))

(define/contract (permutations/distinct l (n (length l)))
  (->i ([lst list?]) ([n (lst) (integer-in 1 (length lst))]) any)
  (define ans (remove-duplicates (map (λ (v) (take v n)) (permutations l))))
  (values ans (length ans)))

(define/contract (number-of-permutations l) (-> list? any)
  (define ns (cons (length l)
                   (for/list ([v (remove-duplicates l)]
                              [n (in-naturals)])
                     (count (λ (x) (equal? x v)) (drop l n)))))
  (/ (apply * (range 1 (add1 (car ns))))
     (for/product ([n (cdr ns)])
       (apply * (range 1 (add1 n))))))
