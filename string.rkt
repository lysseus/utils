#lang racket

;;;
;;; STRING
;;;

(provide string->expr)

;; Converts a string to a Racket expression. If the string does not
;; covnert to an expression, an error results.
(define/contract (string->expr str) (-> string? any)
  (read (open-input-string str)))

