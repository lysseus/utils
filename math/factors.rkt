#lang racket

;;;
;;; FACTORS
;;; Provides positive-integer factors for positive-integers.
;;;

(provide factor factors)

(require "../list.rkt")

;; Returns a list of factors for n.
;; Example: (factor 12) => '(1 2 3 4 6 12)
(define/contract (factor n #:pairs? (pairs? #f))
  (->* (natural?) (#:pairs? boolean?) any)
  (define m (sqrt n))
  (define-values (fs qs) (for/fold ([fs empty]
                                    [qs empty])
                                   ([f (range 1 m)])
                           (define-values (q r) (quotient/remainder n f))
               (if (zero? r)
                   (values (cons f fs) (cons q qs))
                   (values fs qs))))  
  (if pairs?      
      (append (reverse (for/list ([f fs]
                         [q qs])
                (list f q)))
              (if (integer? m) (list m m) '()))
      (append (reverse fs) (if (integer? m) (list m) '()) qs)))

;; Creates a hash table of factors for n, and recursively for its factors.
;; This is used as a helper fulnction by factors.
(define (factor-table n)
  (define h (make-hash))
  (define (load n)
    (define fs (factor n))
    (hash-set! h n fs)
    (for ([f fs])
      (unless (hash-has-key? h f)
        (load f)))
    h)
  (load n))

;; Creates a list of unique factor pairs for n.
;; Example: (factor-pairs 12) => '((1 12) (2 6) (3 4))
(define (factor-pairs n (tbl (factor-table n)))
  (remove-duplicates (for/list ([f (hash-ref tbl n)])
                       (sort (list f (quotient n f)) <))))

;; Produces 2 lists of factor pairs combinations.
;; If ((p0 q0) (p1 q1) (p2 q2) ...) reprent the list of factor pairs of n,
;; then factor-combs products the following:
;; 1st: ((factor-pairs p0) (factor-pairs p1) (factor-pairs p2) ...)
;; 2nd: ((factor-pairs q0) (factor-pairs q1) (factor-pairs q2) ...)
(define (factor-combinations n (tbl (factor-table n)))
  (define pairs (factor-pairs n tbl))
  (define-values (1st 2nd)
    (for/fold ([1st empty]
               [2nd empty])
              ([pair pairs])
      (values (cons (factor-pairs (first pair) tbl) 1st)
              (cons (factor-pairs (second pair) tbl) 2nd))))
  (values (reverse 1st) (reverse 2nd)))

;; Creates a list of unique factors for n.
;; Example: (factors 12) => '((1 12) (2 6) (3 4) (2 2 3)).
(define/contract (factors n) (-> positive-integer? any)
  ;; Create a factor-table for n.
  (define tbl (factor-table n))
  ;; Create the 2 factor-combinations for n.
  (define-values (1st 2nd) (factor-combinations n tbl))
  ;; Zip and strip the factor-combinations lists.
  (remove-duplicates
   (map (compose      
         (λ (l)
           (define v (remove* '(1) l))
           (case (length v)
             [(0) '(1 1)]
             [(1) (cons 1 v)]
             [else v]))      
         (λ (v) (sort v <)) flatten)
        (apply append
               (for/list ([p 1st]
                          [q 2nd])
                 (zip p q))))))
