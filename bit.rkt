#lang racket

;;;
;;; BIT
;;;
;;; A library for manipulating bits.
;;;

(provide define-bit-indices bit-indices-set? bit-indices->natural
         natural->bit-indices bit-indices-toggle
         define-bit-flags bit-flags-set? bit-flags->natural
         natural->bit-flags bit-flags-toggle)

(require (for-syntax syntax/parse
                     racket/syntax))

(module+ test (require rackunit
                       (submod "..")))

;; Binds the bit index names to their positional index values (0, 1, 2 ...). 
(define-syntax (define-bit-indices stx)
  (syntax-parse stx
    [(_ name:id ...)
     #`(begin
         #,@(for/list ([x (syntax->list #'(name ...))]
                       [y (in-naturals)])
              (with-syntax ([name x]
                            [index y]                              )
                #`(define name index))))]))

;; Returns the natural number represented by the specified bit indices.
(define/contract (bit-indices-set? n . bit-indices)
  (->* (natural?) () #:rest (listof natural?) boolean?)
  (for/and ([bit-index bit-indices])
    (bitwise-bit-set? n bit-index)))

(define/contract (bit-indices->natural . bit-indices)
  (->* () () #:rest (listof natural?) natural?)
  (for/sum ([bit-index bit-indices]) (expt 2 bit-index)))

;; Returns the list of bit indices for the natural number.
(define/contract (natural->bit-indices n)
  (-> natural? (or/c empty? (listof natural?)))
  (for/fold ([acc empty])
            ([b (range  (string-length (~r #:base 2 n)))]
             [idx (in-naturals)])
    (if (bitwise-bit-set? n idx)
        (cons idx acc)
        acc)))

;; Toggles the specified bits of a natural number.
(define/contract (bit-indices-toggle n . bit-indices)
  (->* (natural?) () #:rest (listof natural?) natural?)
  (for/fold ([n n])
            ([bit-index bit-indices])
    (bitwise-xor n (expt 2 bit-index))))

(module+ test
  (test-case "define-bit-indices tests"
             (define-bit-indices x y z)
             (check-equal? x 0)
             (check-equal? y 1)
             (check-equal? z 2))
  (test-case "bit-indices->natural tests"
             (define-bit-indices x y z)
             (check-equal? (bit-indices->natural x z) 5))
  (test-case "natural->bit-indices tests"
             (check-equal? (natural->bit-indices 5) '(2 0)))
  (test-case "bit-blags-set? tests"
             (define-bit-indices x y z)
             (define v (bit-indices->natural x z))
             (check-true (bit-indices-set? v x))
             (check-false (bit-indices-set? v y))
             (check-true (bit-indices-set? v z)))
  (test-case "flags-toggle tests"
             (define-bit-indices x y z)
             (check-equal? (bit-indices-toggle 0 x z) 5)
             (check-equal? (bit-indices-toggle 5 x z) 0)))

(define-syntax (define-bit-flags stx)
  (syntax-parse stx
    [(_ flag ...)
     #`(begin
         #,@(for/list ([x (syntax->list #'(flag ...))]
                       [y (in-naturals)])
              (with-syntax ([flg x]
                            [n y]                              )
                #`(define flg n))))]))

(define (bit-flags-set? n . bit-flags)
  (for/and ([bit-flag bit-flags])
    (bitwise-bit-set? n bit-flag)))

(define (bit-flags->natural . bit-flgs)
  (for/sum ([bit-flg bit-flgs]) (expt 2 bit-flg)))

(define (natural->bit-flags n)
  (for/fold ([acc empty])
            ([b (range  (string-length (~r #:base 2 n)))]
             [idx (in-naturals)])
    (if (bitwise-bit-set? n idx)
        (cons idx acc)
        acc)))

(define (bit-flags-toggle n . bit-flgs)
  (for/fold ([n n])
            ([bit-flg bit-flgs])
    (bitwise-xor n (expt 2 bit-flg))))

(module+ test
  (test-case "define-bit-flags tests"
             (define-bit-flags x y z)
             (check-equal? x 0)
             (check-equal? y 1)
             (check-equal? z 2))
  (test-case "bit-flags->natural tests"
             (define-bit-flags x y z)
             (check-equal? (bit-flags->natural x z) 5))
  (test-case "natural->bit-flags tests"
             (check-equal? (natural->bit-flags 5) '(2 0)))
  (test-case "bit-blags-set? tests"
             (define-bit-flags x y z)
             (define v (bit-flags->natural x z))
             (check-true (bit-flags-set? v x))
             (check-false (bit-flags-set? v y))
             (check-true (bit-flags-set? v z)))
  (test-case "flags-toggle tests"
             (define-bit-flags x y z)
             (check-equal? (bit-flags-toggle 0 x z) 5)
             (check-equal? (bit-flags-toggle 5 x z) 0)))