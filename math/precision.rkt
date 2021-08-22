#lang racket

;;; 
;;; Precision. A module for creating numbers of varying precision.
;;;

(provide order-of-magnitude)

;; Returns the d-1 where d is the number of digits comprising the integer n.
(define/contract (order-of-magnitude n)
  (-> (and/c integer? (between/c (- (expt 10 10000)) (expt 10 10000))) any)
  (inexact->exact
   (if (zero? n)
       0
       (floor (+ (/ (log (abs n)) (log 10))             
             3.637978807091713e-12)))))
