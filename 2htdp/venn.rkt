#lang racket

(require 2htdp/image
         "text-wrap.rkt˘")

(define MT (empty-scene 600 500 'blue))

(define (circle/text radius text-string text-size text-color img-width)
  (define-values (text-image text-width)
    (text-wrap  text-string text-size text-color img-width))
  (define text-radius (round (sqrt (+ (sqr (image-height text-image)) (sqr text-width)))))
  (overlay text-image
           (circle (if (> radius text-radius) radius text-radius) 'outline 'black)))

(define c1 (circle/text 173
                        "Donald Trump doesn't want to take responsibility"
                        18 'red 100))
(define c2 (circle/text 0
                        "Federal government takes backseat during national on-going crisis"
                        18 'red 100))

(define-values (txt len) (text-wrap "AUSTERITY" 18 'red 100))

(define img
  (overlay (rotate 90 txt)
           (overlay/xy c1
                       200
                       0
                       c2)
           (overlay/xy (circle (quotient (image-width c1) 2) 'solid 'white)
                       200
                       0
                       (circle (quotient (image-width c2) 2) 'solid 'white))))
(place-image img
             (quotient (image-width MT) 2)
             (quotient (image-height MT) 2)
             MT)