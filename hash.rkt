#lang racket

(provide hash-stats
         hash-fold)

;; hash-stats: test-thunk num-of-tests -> list?
;; Executs test-thunk num-of-test times, accumulating the
;; occurrences of the result of test-thunk into a hash table
;; which is then sorted into descending frequency.
(define (hash-stats test-thunk num-of-tests)
  (let ([hsh (make-hash)])
    (for ([n (range num-of-tests)])
      (define key (test-thunk))
      (hash-set! hsh key (if (hash-has-key? hsh key)
                             (add1 (hash-ref hsh key))
                             1)))
    (sort (map (λ (v) (cons (car v) (/ (cdr v) num-of-tests 1.0))) (hash->list hsh))
          (λ (a b) (> (cdr a) (cdr b))))))

;; hash-fold hsh (accessor) -> hash?
;; Iterates over a hash that behaves like suspect/alibi key/values
;; and produces a hash of suspect/collected alibis. For instance a hash
;; whose list looks like '((1 2) (2 3) (3 4) (4))' produces a hash whose
;; list would look like '((1 2 3 4) (2 1 3 4) (3 1 2 4) (4 1 2 3))
(define/contract (hash-fold hsh (accessor identity))
  (->* (hash?) (procedure?) hash?)
  (define (fold lst)
    (define ans
      (remove-duplicates
       (for/list ([x lst])
         (remove-duplicates
          (flatten
           (filter (λ (v) (not (set-empty? (set-intersect (list->set x)
                                                          (list->set v)))))
                   lst))))))
    (cond
      [(= (length lst) (length ans)) ans]
      [else (fold ans)]))
  (define lst
    (for/list ([(k v) hsh])
      (cons k (accessor v))))  
  (for*/hash ([v (fold lst)]
           [s# v])
  (values s# (remove s# (set->list v)))))