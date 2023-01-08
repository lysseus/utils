#lang racket

;;; 
;;; PROBABILITY
;;; 

(provide ! P C
         sample-space
         conditional-probability
         k-permutations
         k-combinations)

(module+ test (require rackunit
                       (submod "..")))

(define (! n)
  (for/product ([v (range 1 (add1 n))]) v))

;; Permutaiton formula:
;; How many ways of taking k items from n items without replacement?
(define (P n k) (/ (! n) (! (- n k))))

;; Combinations formula:
;; How many ways of taking k items from n items without replacement
;; where order doesn't matter?
;; P = k!C
(define (C n k) (/ (! n) (* (! (- n k)) (! k))))

;; Produces a list sample space where each element consists of permutations
;; of the values of the event grouped by the number of occurrences of the event.
(define/contract (sample-space vals-of-event (occurrences-of-event 1))
  (->* ((listof any/c)) (nonnegative-integer?) list?)
  (define v (length vals-of-event))
  (for/list ([r (range (expt v occurrences-of-event))])
    (for/list ([c (reverse (range occurrences-of-event))])
      (list-ref vals-of-event (modulo (quotient r (expt v c)) v)))))

;; P(A|B) = P(A ∩ B) / P(B)
(define/contract (conditional-probability ς α (β identity))
  (->* ((listof any/c) any/c) (any/c) number?)
  (define (β? v) (cond
                   [(procedure? β) (β v)]
                   [else (member β v)]))
  (define (α? v) (cond
                   [(procedure? α) (α v)]
                   [else (member α v)]))
  (/ (length (filter α? (filter β? ς)))
     (length (filter β? ς))))

(module+ test
  (test-case "conditional-probability tests"
             (check-equal? (conditional-probability (sample-space '(m f) 2) 'f 'm)
                           2/3)
             (check-equal? (conditional-probability (sample-space (range 1 7) 2)
                                                    (λ (vs) (= (+ (first vs) (second vs)) 7)))
                           1/6)))

(define (k-permutations #:distinct? (distinct? #t) vals (k (length vals)))
  (define len (length vals))
  (reverse (for/fold ([acc empty])
                     ([r (range (expt len k))])
             (define ans (for/list ([c (reverse (range k))])
                           (modulo (quotient r (expt len c)) len)))
             (cond
               [(false? distinct?)
                (cons (map (λ (idx) (list-ref vals idx)) ans) acc)]
               [(= k (length (remove-duplicates ans)))
                (cons (map (λ (idx) (list-ref vals idx)) ans) acc)]
               [else acc]))))

(define (k-combinations #:distinct? (distinct? #t) vals (k (length vals)))
  (define ans (k-permutations vals k #:distinct? distinct?))
  (define-values (vs ss)
    (for/fold ([vs empty]
               [acc empty])
              ([v ans])
      (define s (list->set v))
      (if (member s acc)
          (values vs acc)
          (values (cons v vs) (cons s acc)))))
  (reverse vs))

(module+ test
  (test-case "k-permutations tests"
             (check-equal? (k-permutations '(a b c) 2)
                           '((a b) (a c) (b a) (b c) (c a) (c b)))
             (check-equal? (k-permutations '(a b c) 2 #:distinct? #f)
                           '((a a) (a b) (a c) (b a) (b b) (b c) (c a) (c b) (c c)))
             (check-equal? (k-permutations '(a b b) 2 #:distinct? #f)
                           '((a a) (a b) (a b) (b a) (b b) (b b) (b a) (b b) (b b)))
             (check-equal? (k-permutations '(a b b) 2 #:distinct? #t)
                           '((a b) (a b) (b a) (b b) (b a) (b b))))
  (test-case "k-combinations tests"
             (check-equal? (k-combinations '(a b c) 2)
                           '((a b) (a c) (b c)))
             (check-equal? (k-combinations '(a b c) 2 #:distinct? #f)
                           '((a a) (a b) (a c) (b b) (b c) (c c)))
             (check-equal? (k-combinations '(a b b) 2 #:distinct? #f)
                           '((a a) (a b) (b b)))
             (check-equal? (k-combinations '(a b b) 2 #:distinct? #t)
                           '((a b) (b b)))))
