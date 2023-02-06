#lang racket

;;;
;;; SYMBOL
;;;

(require "string.rkt")

(module+ test (require rackunit
                       (submod "..")))

(define/contract (symbol-split #:trim? (trim? #t)
                               #:repeat? (repeat? #f)
                               #:exprs? (exprs? #t)
                               sym (sep #f))
  (->* (symbol?)
       ((or/c string? regexp?)
        #:trim? any/c
        #:repeat? any/c
        #:exprs? boolean?)
       any)
  (define spl (cond
                [(false? sep) (string-split (~a sym) #:trim? trim? #:repeat? repeat?)]
                [else (string-split (~a sym) sep #:trim? trim? #:repeat? repeat?)]))
  (if (false? exprs?)
      (map string->symbol spl)
      (for/list ([str spl])
        (string->expr str))))

(module+ test
  (test-case "symbol->split tests"
             (check-equal? (symbol-split '|relate! #f|)
                           '(relate! #f))
             (check-equal? (symbol-split '|relate! #f| #:exprs? #f)
                           '(relate! |#f|))))


