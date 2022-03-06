#lang racket

;;; Author: Philip McGrath
;;; License: Apache-2
;; A library for Passing through" keyword arguments in Racket

(provide kw-pass-through-lambda
         local-keyword-apply
         local-kw-lst
         local-kw-val-lst
         (contract-out
          [keyword-apply/filter
           (-> procedure? (listof keyword?) list? list?
               any)]))

(module+ test
  (require rackunit)
  (define (h #:c c . x)
    (list c x))
  (define g
    (kw-pass-through-lambda (#:c [c 0] . args)
      (list c (local-keyword-apply h args))))
  (define f
    (kw-pass-through-lambda (n p #:a [a 0] #:b [b 0] . ns)
      (list local-kw-lst local-kw-val-lst
            a b n p ns
            (local-keyword-apply g ns))))
  (check-equal? (f 2 3 4 5 #:a 42 #:c 52)
                '((#:a #:c) (42 52) 42 0 2 3 (4 5) (52 (52 (4 5)))))
  ;; My implementation of "filtering" keywords has a different result,
  ;; but maybe I don't understand what you were trying to do.
  ;; Your version did this:
  ;;   (check-exn #rx"procedure: h\n  given keyword: #:z"
  ;;              (λ () (f 2 3 4 5 #:z 42 #:c 52)))
  ;; Mine does this instead:
  (check-equal? (f 2 3 4 5 #:z 42 #:c 52)
                '((#:c #:z) (52 42) 0 0 2 3 (4 5) (52 (52 (4 5)))))  
  (check-equal? (f 2 3 4 5 #:z 42 #:c 52)
                '((#:c #:z) (52 42) 0 0 2 3 (4 5) (52 (52 (4 5)))))
  ;; Added test for passing #f value in keyword. (klf)
  (check-equal? (f 2 3 4 5 #:z 42 #:c #f)
                '((#:c #:z) (#f 42) 0 0 2 3 (4 5) (#f (#f (4 5))))))

;; potential further extensions:
;;  - make keyword-apply/filter and local-keyword-apply
;;    accept extra keyword and by-position args like keyword-apply
;;  - implement a define version of kw-pass-through-lambda
;;  - various performance optimizations

(require syntax/parse/define
         racket/stxparam
         (for-syntax syntax/parse/lib/function-header
                     racket/list
                     racket/match
                     racket/sequence
                     syntax/transformer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runtime support

(define (keyword-apply/filter proc kw-lst kw-val-lst by-pos-args)
  ;; like keyword-apply, but skips keywords that aren't allowed
  (define-values [required-kws allowed-kws]
    (procedure-keywords proc))
  (match allowed-kws
    [#f ;; accepts all keywords
     (keyword-apply proc kw-lst kw-val-lst by-pos-args)]
    ['() ;; accepts no keywords
     (apply proc by-pos-args)]
    [_
     (for/lists [kw-lst*
                 kw-val-lst*
                 #:result (keyword-apply proc
                                         kw-lst*
                                         kw-val-lst*
                                         by-pos-args)]
                ([kw (in-list kw-lst)]
                 [val (in-list kw-val-lst)]
                 #:when (memq kw allowed-kws))
       (values kw val))]))

(define (kw-arg-ref kw kw-lst kw-val-lst
                    [fail-thunk
                     ;; we'll use procedure-reduce-keyword-arity
                     ;; to avoid getting here when required kws are missing
                     (λ () (error 'kw-arg-ref "shouldn't get here"))])
  ;; This code modified to handle the passing of #f in the keyword. (klf).
  ((or (for/first ([this-kw (in-list kw-lst)]
                  [val (in-list kw-val-lst)]
                  #:break (keyword<? kw this-kw)
                  #:when (eq? kw this-kw))
        (thunk val))
       fail-thunk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax layer

(define-for-syntax (stxparam-uninitialized stx)
  (raise-syntax-error #f "only allowed inside kw-pass-through-lambda" stx))

(define-syntax-parameter local-kw-lst stxparam-uninitialized)
(define-syntax-parameter local-kw-val-lst stxparam-uninitialized)
(define-syntax-parameter local-keyword-apply stxparam-uninitialized)

(define-simple-macro (lambda/name kw-formals #:name name:id body ...+)
  ;; a simple helper (w/ minimal cheking)
  ;; to give a function a good inferred name
  (let ([name (λ kw-formals body ...)]) name))

(define-for-syntax (check-required-not-after-optional names kws defaults)
  ;; required by-position arguments must come before
  ;; optional by-position arguments:
  ;; if any don't, return the first offending identifier
  (let*-values
      ([{names defaults}
        ;; ignore kw args
        (for/lists [names defaults]
                   ([n (in-list names)]
                    [d (in-list defaults)]
                    [kw (in-list kws)]
                    #:unless kw)
          (values n d))]
       [{names defaults}
        ;; drop leading required args
        (let loop ([names names]
                   [defaults defaults])
          (match defaults
            [(cons #f defaults)
             (loop (cdr names) defaults)]
            [_
             (values names defaults)]))])
    (for/first ([n (in-list names)]
                [d (in-list defaults)]
                #:unless d)
      n)))

(define-syntax-parser kw-pass-through-lambda
  [(_ (arg:formal ... . (~or* rest-arg-name:id ()))
      body ...+)
   #:fail-when (check-duplicate-identifier
                (syntax->list #'(arg.name ... (~? rest-arg-name))))
   "duplicate argument name"
   #:fail-when (check-duplicates (syntax->list #'((~? arg.kw) ...))
                                 #:key syntax-e
                                 eq?)
   "duplicate keyword for argument"
   #:fail-when (check-required-not-after-optional (attribute arg.name)
                                                  (attribute arg.kw)
                                                  (attribute arg.default))
   "default-value expression missing" ;; the error message λ gives
   ;; sort formals
   #:with ([by-pos-name:id (~optional by-pos-default:expr)] ...)
   (for/list ([stx (in-syntax #'([arg.name (~? arg.default)] ...))]
              [kw? (in-list (attribute arg.kw))]
              #:unless kw?)
     stx)
   #:with ((~alt [opt-kw:keyword opt-kw-name:id opt-kw-default:expr]
                 [reqired-kw:keyword reqired-kw-name:id])
           ...)
   #'((~? [arg.kw arg.name (~? arg.default)]) ...)
   #:with (by-pos-formal ...)
   #'((~? [by-pos-name by-pos-default] by-pos-name) ...)
   #:with inferred-name:id (or (syntax-local-name) #'kw-pass-through-procedure)
   #:with (core-arg-name:id ...) #'(by-pos-name ...
                                    reqired-kw-name ...
                                    opt-kw-name ...
                                    (~? rest-arg-name))
   #'(let*
         ([core
           ;; w/ only required args
           (lambda/name (kw-lst kw-val-lst core-arg-name ...)
             #:name inferred-name
             (define (the-local-keyword-apply proc by-pos-args)
               (keyword-apply/filter proc kw-lst kw-val-lst by-pos-args))
             (syntax-parameterize
                 ([local-kw-lst
                   (make-variable-like-transformer #'kw-lst)]
                  [local-kw-val-lst
                   (make-variable-like-transformer #'kw-val-lst)]
                  [local-keyword-apply
                   (make-variable-like-transformer #'the-local-keyword-apply)])
               body ...))]
          [explicit-kws-proc
           ;; version that handles finding kw arg values and calls core
           ;; all by-pos args must be present
           (lambda/name (kw-lst kw-val-lst by-pos-name ... (~? rest-arg-name))
             #:name inferred-name
             (let ([reqired-kw-name
                    (kw-arg-ref 'reqired-kw kw-lst kw-val-lst)]
                   ...)
               (let* ([opt-kw-name
                       (kw-arg-ref 'opt-kw kw-lst kw-val-lst
                                   (λ () opt-kw-default))]
                      ...)
                 (core kw-lst kw-val-lst core-arg-name ...))))]
          [implicit-kw-proc
           ;; let λ handle optional by-position arguments and arity
           (make-keyword-procedure
            (lambda/name (kw-lst kw-val-lst by-pos-formal ...
                                 . (~? rest-arg-name ()))
              #:name inferred-name
              (explicit-kws-proc kw-lst kw-val-lst
                                 by-pos-name ...
                                 (~? rest-arg-name)))
            (lambda/name (by-pos-formal ... . (~? rest-arg-name ()))
              #:name inferred-name
              (explicit-kws-proc '() '()
                                 by-pos-name ...
                                 (~? rest-arg-name))))])
       (procedure-reduce-keyword-arity-mask
        implicit-kw-proc
        ;; optimization: compute arity mask statically
        (procedure-arity-mask implicit-kw-proc)
        '(reqired-kw ...)
        ;; accept all keywords
        #f))])
