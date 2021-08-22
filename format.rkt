#lang racket

;;;
;;; FORMAT
;;;
;;; The racket/format library provides functions for converting
;;; Racket values to strings. In addition to features like padding
;;; and numeric formatting, the functions have the virtue of being
;;; shorter than format (with format string), number->string, or string-append.
;;;

(provide ~z)

(require "./math/precision.rkt")

(module+ test (require rackunit

                       (submod "..")))

(define (string-format str
                       #:sep (sep " ")
                       #:n-sep (n-sep 3)
                       #:from-end? (from-end? #t))
  (define (loop lst n acc)
    (cond
      [(empty? lst) ((if from-end? identity reverse) acc)]
      [(and (not (empty? (cdr lst)))  (not (zero? n-sep)) (zero? (modulo n n-sep)))
       (loop (cdr lst) (add1 n) (cons sep (cons (car lst) acc)))]
      [else (loop (cdr lst) (add1 n) (cons (car lst) acc))]))
  (apply string-append
         (map ~a (loop ((if from-end? reverse identity) (string->list str)) 1 '()))))

(define (~z r
            #:sign (sign #f)
            #:base (base 10)
            #:precision (precision 6)
            #:mode (mode 'fix)
            #:format-exponent (format-exponent #f)
            #:digits (digits 10)
            #:sep? (sep? #f))
  (define (->notation n)
    (cond
      [(eq? mode 'fix)
       (if (< n (expt 10 digits)) 'positional 'exponential)]
      [else 'exponential]))
  (define notation (->notation r))
  (define order (order-of-magnitude (truncate r)))
  (define Δprecision (cond
                       [(eq? notation 'positional)                        
                        (max 0 (min (- digits (add1 order)) precision))]
                       [else (min (- digits 3) precision)]))
  (define fmt-exp
    (cond
      [(false? format-exponent)
       (if (= base 10)
           "e"
           (format "*~a^" base))]
      [else format-exponent]))
  (define str
    (~r r
        #:sign sign
        #:base base
        #:notation notation
        #:format-exponent fmt-exp
        #:precision (list '= Δprecision)))  
  (define (eng num (p precision))    
    (define-values (s e) (apply values (string-split str fmt-exp)))
    (define n (modulo (string->number e) 3))
    (cond
      [(zero? n) str]
      [else
       (define (shift s n)
         (define-values (w d) (apply values (take (append (string-split s ".") (list #f)) 2)))
         (string-append w (substring d 0 n) "." (substring d n)))
       (string-append (shift s n) fmt-exp (~r (- (string->number e) n) #:sign '++
                                          #:min-width 2
                                          #:pad-string "0"))]))
  (define (sep str)    
    (define-values (w f) (apply values (take (append (string-split str ".") (list "")) 2)))
    (cond
      [(string->number (substring w 0 1))
       (string-append (string-format w  #:sep ",") "." f)]
     [else
      (string-append (substring w 0 1) (string-format (substring w 1)  #:sep ",") "." f)]))
  (cond
    [(eq? mode 'eng) (eng r)]
    [(eq? mode 'sci) str]
    [sep? (sep str)]
    [else str]))

(module+ test
  (test-case "fix tests"
             (check-equal? (~z 1)
                           "1.000000")
             (check-equal? (~z pi)
                           "3.141593")
             (check-equal? (~z (* pi 100000))
                           "314159.2654")
             (check-equal? (~z (/ pi 100000))
                           "0.000031"))
  (test-case "sci tests"
             (check-equal? (~z 1 #:mode 'sci)
                           "1.000000e+00")
             (check-equal? (~z pi #:mode 'sci)
                           "3.141593e+00")
             (check-equal? (~z (* pi 100000) #:mode 'sci)
                           "3.141593e+05")
             (check-equal? (~z (/ pi 100000) #:mode 'sci)
                           "3.141593e-05"))
  (test-case "eng tests"
             (check-equal? (~z 1 #:mode 'eng)
                           "1.000000e+00")
             (check-equal? (~z pi #:mode 'eng)
                           "3.141593e+00")
             (check-equal? (~z (* pi 100000) #:mode 'eng)
                           "314.1593e+03")
             (check-equal? (~z (/ pi 100000) #:mode 'eng)
                           "31.41593e-06")))





