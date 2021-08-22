#lang racket

;; '(a (b0 b1) c) ;=> ((a b0 c) (a b1 c))
#;(define (foo lst)
  (define (br lst)
    (apply * (map (λ (v) (cond
                  [(list? v) (length v)]
                  [else 1])) lst)))
  (make-list (br lst) '()))

(define (alts lst)
  (define result (make-vector (apply * (map (λ (v) (cond
                                                     [(list? v) (length v)]
                                                     [else 1])) lst))
                              '()))
  (for* ([n (vector-length result)]
         [v lst])
    (define r
      (cond
        [(list? v) (list-ref v (modulo n (length v)))]
        [else v]))
    (vector-set! result n (cons r (vector-ref result n))))
  (vector-map reverse result))


  

(define f (alts '(a (b0 b1 b2) (c0 c1 c2 c3) d (e0 e1))))
#;(alts (list 'bob (list '|,| 'and '|.|) 'take 'the 'blue 'ball '|.|))














