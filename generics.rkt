#lang racket

;; This module implements a set of generics utilities.

(provide helper-bind-gen)

;; ---------------------------------
;; import and implementation section

(require (for-syntax racket/syntax))

(module+ test
  (require rackunit
           racket/generic))

;; helper-bind-gen: id ->||
;; Creates bindings for helper functions used in generics. Should
;; be the first expression in a #:methods statement after the generics id.
;; Wrap calls to helper functions with (bind-this id ...) to provide the 
;; generics binding to the helper function. 
;; If helper function needs reference to the generics instance, use this:id.
;;
;; Example:
;;
;; #:methods gen:w
;;    [(helper-bind-gen w)
;;     (define (helper v) (set-foo-x! this:w v))
;;     (define (set-x! w v) (bind-this w (helper v)))]
(define-syntax (helper-bind-gen stx)
  (syntax-case stx ()
    [(_ g)
     (with-syntax ([gen:name (format-id stx "~a" #'g)]
                   [parm:name (format-id stx "parm:~a" #'g)]
                   [bind:name (format-id stx "this:~a" #'g)]
                   [with:name (format-id stx "~a" 'bind-this)])
       #'(begin
           (define parm:name (make-parameter #f))
           (define-syntax bind:name
             (syntax-id-rules ()
               [(set! _ e) (set! parm:name (make-parameter e))]
               [(_ args (... ...)) ((parm:name) args (... ...))]
               [_ (parm:name)]))
           (define-syntax (with:name stx)
             (syntax-case stx ()
               [(_ g (f args (... ...)))
                (with-syntax ([parm:name (format-id stx "parm:~a" #'g)])
                  #'(parameterize ([parm:name g]) (f args (... ...))))]))))]))


; ===========================================================================                                          
;                                          
;                                          
;                                          
;   ;;;;;;;                                
;   ;  ;  ;                  ;             
;      ;     ;;;;    ;;;;;  ;;;;;    ;;;;; 
;      ;    ;    ;  ;    ;   ;      ;    ; 
;      ;    ;;;;;;   ;;;;    ;       ;;;;  
;      ;    ;            ;   ;           ; 
;      ;    ;       ;    ;   ;   ;  ;    ; 
;     ;;;    ;;;;;  ;;;;;     ;;;   ;;;;;  
;                                          
;                                          
;                                          
;   

(module+ test
  (require (submod ".."))
  
  (define-generics w
    [set-x! w v])
  
  (struct foo (x)
    #:mutable
    #:methods gen:w
    [(helper-bind-gen w)
     (define (helper v) (set-foo-x! this:w v))
     (define (set-x! w v) (bind-this w (helper v)))])
  
  (test-case "helper-bind-gen tests"
             (define f1 (foo 3))
             (set-x! f1 10)
             
             (check-equal? (foo-x f1) 10)))
