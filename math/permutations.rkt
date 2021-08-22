#lang racket

(provide permutations/distinct
         number-of-permutations)

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
