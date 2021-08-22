#lang racket

#;(define-syntax whenlet
  (syntax-rules ()
    [(whenlet ([var val]) body ...)
     (when var body ...)]
    [(whenlet ([var0 val0] [var val] ...) body ...)
     (let ([var 0 val0]))]))

(define-syntax whenlet
  (syntax-rules ()
    [(whenlet test ([var val] ...) body ...)
     (let* ([var val] ...)
       (when test body ...))]))

(whenlet #t () 'foo)
(whenlet id ([id (random 12)]) id)
(whenlet (or x y) ([x #f] [y (not x)]) (list x y))

(define-syntax cascade
  (syntax-rules ()
    [(cascade (test body ...) ...)
     (unless test body ...) ...]))

(cascade (#t ( )))