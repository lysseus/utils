#lang racket

;;;
;;; STRING
;;;

(provide string->expr string->exprs)

(module+ test (require rackunit
                       (submod "..")))

;; Converts a string to a Racket expression. If the string does not
;; covnert to an expression, an error results. If the string contains
;; consecutive expressions the first is converted and the rest are lost.
(define/contract (string->expr str) (-> string? any)
  (read (open-input-string str)))

(module+ test
  (test-case "string->expr tests"
             (check-equal? (string->expr "(a . b) (c . d)")
                           '(a . b))))

;; Converts a string into a list of Racket expressions. If the string does not
;; convert to an expression, an error results. Consumes the whole string until
;; eof. 
(define/contract (string->exprs str (idx #f)) (->* (string?) ((or/c #f natural?)) any)
  (define in (open-input-string str))
  (define-values (exprs stop)
    (for/fold ([acc empty] [stop? #f])
              ([n (in-naturals)]
               #:break (or stop? (and (natural? idx) (> n idx))))
      (define expr (read in))
      (if (eof-object? expr)
          (values acc #t)
          (values (cons expr acc) #f))))
  (if (false? idx)
      (reverse exprs)
      (list-ref (reverse exprs) idx)))

(module+ test
  (test-case "string->exprs tests"
             (check-equal? (string->exprs "(a . b) (c . d)")
                           '((a . b)(c . d)))
             (check-equal? (string->exprs "(a . b) (c . d)" 0)
                           '(a . b))))
