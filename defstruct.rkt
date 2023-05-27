#lang racket

;;;
;;; DEFSTRUCT
;;;
;;; A macro for defining mutable, transparent structs that can be instantited
;;; without having to provide all field values. The macro also creates parameter
;;; variables providing mutable default values for each struct field, enforced by
;;; contract.
;;;

(provide defstruct)

(require (for-syntax syntax/parse
                     racket/syntax))

(module+ test (require rackunit
                       (submod "..")))

;; A helper macro producing the getters/setters hash for id.
(define-syntax (make-id-getters/setters stx)
  (syntax-parse stx
    [(_ id:id field:id ...)
     (with-syntax ([id-getters/setters (format-id #'id "~a-getters/setters" #'id)])
       #`(begin
           (define id-getters/setters (make-hash))
           #,@(for/list ([x (syntax->list #'(field ...))])
                (with-syntax ([fld x]
                              [getter (format-id #'id "~a-~a" #'id x)]
                              [setter (format-id #'id "set-~a-~a!" #'id x)]                              )
                  #`(hash-set! id-getters/setters (quote fld) (list getter setter))))))]))

;; The defstruct macro:
;; a) Defines a simple mutable, transparent struct
;; b) Defines parameter variables for each struct field
;;   initialized with a default value and enforced by contract
;; c) Defines a hash for the id of its gield getters and setters
;; d) Defines a make-id macro that will construct an instance of id,
;;    assign default values to fields, and set the fields of the struct
;;    fields to the values specified by a list of field/val pairs
(define-syntax (defstruct stx)
  (syntax-parse stx
    [(_ id:id ((field default cntrct) ...))
     (with-syntax ([id-getters/setters (format-id #'id "~a-getters/setters" #'id)]
                   [make-id (format-id #'id "make-~a" #'id)]
                   [oo #'(... ...)])
       #`(begin
           ;; Define the struct as mutable, transparent.
           (struct id (field ...) #:mutable #:transparent)
           ;; Define paramaeters for each field with default value.
           #,@(for/list ([x (syntax->list #'(field ...))]
                         [y (syntax->list #'(default ...))]
                         [z (syntax->list #'(cntrct ...))])
                (with-syntax ([prm-id
                               (format-id #'id "current-~a-~a" #'id x)]                              
                              [dft-val y]
                              [cntrct z])
                  #`(define/contract prm-id (parameter/c cntrct) (make-parameter dft-val))))
           
           ;; Define id setters.
           (make-id-getters/setters id field ...)
           
           ;; Define "make" for id struct instantiation.           
           (define-syntax (make-id stx)
             (syntax-parse stx
               [(_ (fld:id val:expr) oo)
                #'(begin
                    (let ([s (id #,@(for/list ([x (syntax->list #'(field ...))])
                                      (with-syntax ([prm-id
                                                     (format-id #'id "current-~a-~a"
                                                                #'id x)]                              )
                                        #`(prm-id))))])
                      (for ([f (list (quote fld) oo)]
                            [v (list val oo)])
                        (define getter/setter (hash-ref id-getters/setters f #f))
                        (when (false? getter/setter)
                          (raise-user-error (format "Struct does not define field: ~a" f)))
                        (define setter (second getter/setter))
                        (setter s v))
                      s))]))))]))

;; TESTING≥
(module+ test
  (test-case "defstruct tests"
             (defstruct foo ((a #f boolean?) (b 0 number?) (c "" string?) (d 'foo symbol?)))
             (check-equal? (hash-ref foo-getters/setters 'c)
                           (list foo-c set-foo-c!))
             (check-equal? (make-foo (a #t) (c "goodbye") (d 'bar))
                           (foo #t 0 "goodbye" 'bar))
             (check-exn exn:fail? (thunk (make-foo (e 25))))))
