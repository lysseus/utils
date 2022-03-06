#lang racket

;;;
;;; ZIL
;;; A library implementing ZIL-style macros and functions.
;;;

(provide aux
         routines
         routine
         return
         rfalse
         rtrue
         symbol->routine
         repeat
         SET!
         SETV)

(require (for-syntax syntax/parse)
         racket/stxparam)

(module+ test (require rackunit
                       (submod "..")))

;; Returns the value of the variable being set.
(define-syntax-rule (SET! var val) (begin (set! var val) val))

;;; aux: a macro for defining "aux" variables
;;; where unassigned variables are assigned #f.
(define-syntax (aux stx)
 (syntax-case stx ()
   [(_) #'(begin)]
   [(_ (var val) . rst)
    (identifier? #'var)
    #'(begin
        (define var val)
        (aux . rst))]
   [(_ var . rst)
    (identifier? #'var)
    #'(begin
        (define var #f)
        (aux . rst))]))

(module+ test
  (test-case "aux tests"
             (aux a (b pi))
             (check-false a)
             (check-equal? b pi)))

;; Defining ZIL-style return, rfalse, and rtrue for continuations.
(define-syntax-parameter return (syntax-rules ()))
(define-syntax-parameter rfalse (syntax-rules ()))
(define-syntax-parameter rtrue (syntax-rules ()))

;; A macro for defining ZIL-style routines.
;; Stores routines in routines hash by symbol key.
(define-syntax (routine stx)
  (syntax-case stx ()
    [(_ name argspec body0 body ...)
     #'(begin
         (define tmp (let ([tmp 'name]
               [name (λ argspec
                       (let/cc fn-exit
                         (syntax-parameterize ([return (syntax-rules () [(_ x) (fn-exit x)])])
                           (syntax-parameterize ([rfalse (syntax-rules () [(_) (return #f)])])
                             (syntax-parameterize ([rtrue (syntax-rules () [(_) (return #t)])])
                               body0 body ...)))))])
           (hash-set! routines tmp name)
           name))
         (define name tmp))]))

(define/contract current-routines
  (parameter/c (and/c hash-eq? (not/c immutable?)))
  (make-parameter (make-hasheq)))
(define-syntax (routines stx)
  (syntax-parse stx
    [routines #'(current-routines)]))

(define (symbol->routine sym) (hash-ref routines sym))

(module+ test
  (test-case "routine tests"
             (parameterize ([current-routines (make-hasheq)])
               (routine foo (a b)
                      (cond
                        [(< a b) (rtrue)]
                        [(> a b) (rfalse)]
                        [else (return 'equal)])
                      (printf "boo!"))
             (check-true (foo 1 2))
             (check-false (foo 2 1))
             (check-equal? (foo 3 3) 'equal)
             (routine bar (#:default (default #f) a b (c #f) . rst)
                      (list a b c rst default))
             (check-equal? (bar 1 2 3 4 5 6 #:default 7)
                           '(1 2 3 (4 5 6) 7)))))

;; Loops until a return, rfalse, or rtrue is evaluated.
;; The argspec is optional, but should be modified using set! in the body.
(define-syntax (repeat stx)
  (syntax-case stx ()
    [(_ (argspec ...) body0 body ...)
     #'(let/cc loop-exit
         (aux argspec ...)
         (for ([(gensym) (in-naturals)])
           (syntax-parameterize ([return (syntax-rules () [(_ x) (loop-exit x)])])
             (syntax-parameterize ([rfalse (syntax-rules () [(_) (return #f)])])
               (syntax-parameterize ([rtrue (syntax-rules () [(_) (return #t)])])
                 body0 body ...)))))]))

(module+ test
  (test-case "repeat tests"
             (check-true (repeat () (rtrue)))
             (check-true (repeat ((cnt 5))
                                 (cond [(zero? cnt) (rtrue)]
                                       [else (set! cnt (sub1 cnt))])))))

;; Checks whenther tahe id is bound, and if so then evaluates iftrue,
;; otherwise evaluates iffalse.
(define-syntax (if-defined stx)
  (syntax-case stx ()
    [(_ id iftrue iffalse)
     (let ([where (identifier-binding #'id)])
       (if where #'iftrue #'iffalse))]))

;; If id is bound, sets id to val; otherwise defines id as val.
(define-syntax (SETV stx)
  (syntax-case stx ()
    [(_ id val)
     (let ([where (identifier-binding #'id)])
       (if where #'(set! id val) #'(define id val)))]))
