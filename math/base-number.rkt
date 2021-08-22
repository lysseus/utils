#lang racket

;;;
;;; BASE NUMBER
;;; A library for converting between natural numbers and base numbers.
;;; A base number is a list of natural numbers. The first number in the list
;;; is the radix, with each subsequent number representing:
;;;    n1*r^i n2*r^i-1 ...nv*r^0
;;; For instance (2 1 0 0) represents the natural number 4.
;;;

(provide base-number?
         natural->base-number
         base-number->natural
         base-number->base-number
         base-number->base-number-symbol
         base-number-symbol->base-number
         natural->base-number-symbol)

(module+ test (require rackunit))

;; A base-number is a list of natural numbers. 
(define (base-number? v)
  (and (and (pair? v))
       (andmap natural? v)
       (> (car v) 1)
       (andmap (λ (n) (> (car v) n)) (cdr v))))

(module+ test
  (test-case "base-number tests"
             (check-true (base-number? '(2 1 0 1)))
             (check-false (base-number? '(2 1 0 2)))
             (check-false (base-number? 42))))

(define/contract (base-number->natural bnum) (-> base-number? natural?)
  (for/sum ([r (reverse (cdr bnum))]
            [n (in-naturals)])
    (* r (expt (car bnum) n))))

(module+ test
  (test-case "base-number->natural"
             (check-equal? (base-number->natural '(6 4 3)) 27)
             (check-equal? (base-number->natural '(2 0 0 1 1 0 1 1)) 27)))

;; Converts a natural number into a base-number.
(define/contract (natural->base-number n base (size #f) (acc empty))
  (->* (natural? natural?) ((or/c #f natural?) list?) base-number?)
  (cond
    [(or (and (number? size) (zero? size)) (and (false? size) (zero? n)))
     (cons base acc)]    
    [else
     (define-values (q r) (quotient/remainder n base))
     (natural->base-number q base (if (false? size) #f (sub1 size)) (cons (modulo r base) acc))]))

(module+ test
  (test-case "naturral->base-number tests"
             (check-equal? (natural->base-number 27 6)
                           '(6 4 3))
             (check-equal? (natural->base-number 42 2)
                           '(2 1 0 1 0 1 0))))

;; Converts a base-number to a different base.
;; Size, when provided represents the length of the cdr of the list. 
(define/contract (base-number->base-number bnum base (size #f))
  (->* (base-number? (and/c natural? (λ (n) (> n 1)))) (natural?) base-number?)    
  (natural->base-number (base-number->natural bnum) base size))

(module+ test
  (test-case "base-number->base->number tests"
             (check-equal? (base-number->base-number '(10 1 0) 2)
                           '(2 1 0 1 0))
             (check-equal? (base-number->base-number '(10 1 2 9 5) 6 4)
                           '(6 5 5 5 5))))

(define (base-number->base-number-symbol bnum)
  (define base (car bnum))
  (define len (length (cdr (natural->base-number (sub1 base) 10))))
  (define slst (map number->string bnum))
  (string->symbol
   (string-append "%"
                  (cond
                    #;[#f #f]
                    [(= len 1)
                     (string-append
                      (car slst)
                      (~a #\u00B7)      
                      (apply string-append (cdr slst)))]
                    [else
                     (string-append
                      (apply string-append
                             (map (λ (v) (string-append v (~a #\u00B7)))
                                  (take slst (sub1 (length slst)))))
                      (last slst))]))))

(module+ test
  (test-case "base-number->base-number-symbol tests" 
             (check-equal? (base-number->base-number-symbol '(6 4 3))
                           '%6·43)
             (check-equal? (base-number->base-number-symbol '(16 2 10))
                           '%16·2·10)))

(define (base-number-symbol->base-number bsym)
  (define bstr (symbol->string bsym))
  (define blst (string-split bstr (~a #\u00B7)))
  (define base (string->number (string-trim (car blst) "%")))
  (define len (length (cdr (natural->base-number (sub1 base) 10))))
  (cons base
        (map string->number
             (cond
               [(= len 1)
                (remove* '("") (string-split (cadr blst) ""))]
               [else
                (cdr blst)]))))

(module+ test
  (test-case "base-number-symbol->base-number tests" 
             (check-equal? (base-number-symbol->base-number '%6·43)
                           '(6 4 3))
             (check-equal? (base-number-symbol->base-number '%16·2·10)
                           '(16 2 10))))

(define (natural->base-number-symbol n b (s #f))
  (base-number->base-number-symbol (natural->base-number n b s)))

(module+ test
  (test-case "natural->base-number-symbol tests" 
             (check-equal? (natural->base-number-symbol 42 6)
                            '%6·110)
             (check-equal? (natural->base-number-symbol 42 16)
                            '%16·2·10)))

(define (base-number-symbol->natural bsym)
  (base-number->natural (base-number-symbol->base-number bsym)))

(module+ test
  (test-case "base-number-symbol->natural tests" 
             (check-equal? (base-number-symbol->natural '%6·110)
                            42)
             (check-equal? (base-number-symbol->natural '%16·2·10)
                            42)))
