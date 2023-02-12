#lang racket

;;; 
;;; Precision. A module for creating numbers of varying precision.
;;;

(provide order-of-magnitude lim-bounds lim-define)

(require (for-syntax syntax/parse)
         (for-syntax racket/syntax))

;; Limits an expression to a value between lower and upper boundary value inclusive.
(define/contract (lim-bounds val lower upper)
  (-> number? number? number? number?)
  (cond
    [(< lower val upper) val]
    [(< lower val) upper]
    [else lower]))

;; Defines var to a val limited by the lower and upper boundary value.
(define-syntax (lim-define stx)
    (syntax-parse stx
      [(_ var:id val:expr lower:number upper:number)
       (with-syntax ([var (format-id #'var "~a" #'var)])
         #'(define var (lim-bounds val lower upper)))]))

;; Returns the d-1 where d is the number of digits comprising the integer n.
(define/contract (order-of-magnitude n)
  (-> (and/c integer? (between/c (- (expt 10 10000)) (expt 10 10000))) any)
  (inexact->exact
   (if (zero? n)
       0
       (floor (+ (/ (log (abs n)) (log 10))             
             3.637978807091713e-12)))))
