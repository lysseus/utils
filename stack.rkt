#lang racket

;;;
;;; STACK
;;;

(provide (struct-out Fifo)
         (struct-out Lifo)
         gen:Stack
         push!
         stack-empty?
         peek
         pop!
         size)

(require racket/generic
         racket/undefined)

(module+ test (require rackunit
                       (submod "..")))

(define-generics Stack
  [push! Stack item]
  [stack-empty? Stack]
  [peek Stack]
  [pop! Stack]                
  [size Stack])


(struct Fifo  ([data #:auto])
  #:auto-value '()
  #:mutable #:transparent
  #:methods gen:Stack
  [(define (push! Stack item)
     (match Stack
       [(Fifo data) (set-Fifo-data! Stack (cons item data))]))
   (define (stack-empty? Stack)
     (match Stack
       [(Fifo data) (empty? data)]))
   (define (peek Stack)
     (match Stack
       [(Fifo data) (if (empty? data) undefined (car data))]))
   (define (pop! Stack)
     (match Stack
       [(Fifo data)
        (define item (peek Stack))
        (unless (empty? data) (set-Fifo-data! Stack (cdr data)))
        item]))
   (define (size Stack)
     (match Stack
       [(Fifo data) (length data)]))])

(struct Lifo  ([data #:auto])
  #:auto-value '()
  #:mutable #:transparent
  #:methods gen:Stack
  [(define (push! Stack item)
     (match Stack
       [(Lifo data) (set-Lifo-data! Stack (append data (list item)))]))
   (define (stack-empty? Stack)
     (match Stack
       [(Lifo data) (empty? data)]))
   (define (peek Stack)
     (match Stack
       [(Lifo data) (if (empty? data) undefined (car data))]))
   (define (pop! Stack)
     (match Stack
       [(Lifo data)
        (define item (peek Stack))
        (unless (empty? data) (set-Lifo-data! Stack (cdr data)))
        item]))
   (define (size Stack)
     (match Stack
       [(Lifo data) (length data)]))])




(module+ test
  (test-case "Fifo tests"
             (define fifo (Fifo))
             (push! fifo 'a)
             (push! fifo 'b)
             (push! fifo 'c)
             (check-equal? (size fifo) 3)
             (check-equal? (peek fifo) 'c)
             (check-equal? (pop! fifo) 'c)
             (check-equal? (pop! fifo) 'b)
             (check-equal? (pop! fifo) 'a)
             (check-equal? (size fifo) 0))
  (test-case "Lifo tests"
             (define lifo (Lifo))
             (push! lifo 'a)
             (push! lifo 'b)
             (push! lifo 'c)
             (check-equal? (size lifo) 3)
             (check-equal? (peek lifo) 'a)
             (check-equal? (pop! lifo) 'a)
             (check-equal? (pop! lifo) 'b)
             (check-equal? (pop! lifo) 'c)))
