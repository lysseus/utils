#lang racket

;;;
;;; PARAMETRICS
;;; Defines a group of functions of one or more independent variables, called parameters. 
;;; Commonly to express the coordinates of the points that make up a geometric object. 
;;;

(provide Ρ
         Ρ-2d-line
         Ρ-unit-circle
         Ρ-ellipse)

(require (for-syntax syntax/parse))

(module+ test (require rackunit
                       (submod "..")))

;; (Ρ (parm ...) ((var exp) ...) (dep val) ...)
;; parm: parameter
;; var: coordinate variable
;; exp: coordinate expression
;; dep: dependent variable
;; val: dependent variable value
;; Returns a coordinate vector of the application of the parameters p
;; to the parametric expressions e, given dependent variables d with values v.
(define-syntax (Ρ stx)
  (syntax-parse stx
    [(_ (parm ...) ((var exp) ...) ((dep val) ...))
     #'(let ([dep val] ...)
         (λ (parm ...) (list ((λ (parm ...) (list (quote var) exp)) parm ...) ...)))]))

(struct posn (x y) #:transparent)

(define (Ρ-2d-line x0 y0 θ)
  (Ρ (r)
     ((x (+ x0 (* r (cos (degrees->radians θ)))))
      (y (+ y0 (* r (sin (degrees->radians θ))))))
     ((θ θ))))

(define (Ρ-unit-circle r)
  (Ρ (θ)
     ((x (* r (cos (degrees->radians θ))))
      (y (* r (sin (degrees->radians θ)))))
     ((r r))))

(define (Ρ-ellipse a b)
  (Ρ (θ)
     ((x (* a (cos (degrees->radians θ))))
      (y (* b (sin (degrees->radians θ)))))
     ((a a) (b b))))

(module+ test
  (test-case "Ρ line tests"
             (check-equal? ((Ρ (r)
                               ((x (+ (first point0) (* r (cos (degrees->radians θ)))))
                                (y (+ (second point0) (* r (sin (degrees->radians θ))))))
                               ((point0 '(4 3))
                                (θ 60))) 2)
                           (list (list 'x 5.0) (list 'y 4.732050807568877)))
             (check-equal? ((Ρ (r)
                               ((x (+ (first point0) (* r (cos (degrees->radians θ)))))
                                (y (+ (second point0) (* r (sin (degrees->radians θ))))))
                               ((point0 '(4 3))
                                (θ 60))) 2)
                           ((Ρ-2d-line 4 3 60) 2)))
  (test-case "Ρ circle tests"
             (check-equal? ((Ρ (θ)
                               ((x (* r (cos (degrees->radians θ))))
                                (y (* r (sin (degrees->radians θ)))))
                               ((r 1))) 90)
                           '((x 6.123233995736766e-17) (y 1.0)))
             (check-equal? ((Ρ (θ)
                               ((x (* r (cos (degrees->radians θ))))
                                (y (* r (sin (degrees->radians θ)))))
                               ((r 1))) 90)
                           ((Ρ-unit-circle 1) 90)))
  (test-case "Ρ ellipse tests"
             (check-equal? ((Ρ (θ)
                               ((x (* a (cos (degrees->radians θ))))
                                (y (* b (sin (degrees->radians θ)))))
                               ((a 30) (b 15))) 90)
                            '((x 1.83697019872103e-15) (y 15.0)))
             (check-equal? ((Ρ (θ)
                               ((x (* a (cos (degrees->radians θ))))
                                (y (* b (sin (degrees->radians θ)))))
                               ((a 30) (b 15))) 90)
                           ((Ρ-ellipse 30 15) 90))))
