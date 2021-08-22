#lang racket

(require 2htdp/image
         2htdp/universe)

(struct canvas (width height color))

(struct tick-event)
(struct key-event (key))

(struct mouse-event (x y event))

(struct world (canvas key-event mouse-event)
  #:mutable #:transparent)


(define (mouse-handler ws x y event)
  (world (world-canvas ws)
         (world-key-event ws)
         (mouse-event x y event)))

(define (key-handler ws ke)
  (world (world-canvas ws)
         (key-event ke)
         (world-mouse-event ws)))

(define MT-WIDTH 800)
(define MT-HEIGHT 600)
(define MT (empty-scene MT-WIDTH MT-HEIGHT))
(define FONT-SIZE 24)
(define FONT-COLOR 'black)

#;(define (on-canvas? ws))

(define (draw-mouse-event ws)
  (define me (world-mouse-event ws))
  (beside (text (~a (mouse-event-x me)) FONT-SIZE FONT-COLOR)
          (rectangle 20 1 'outline 'transparent)
          (text (~a (mouse-event-y me)) FONT-SIZE FONT-COLOR)
          (rectangle 20 1 'outline 'transparent)
          (text (~a (mouse-event-event me)) FONT-SIZE FONT-COLOR)))

(define (draw-world ws)
  (draw-mouse-event ws))

(define (render ws)
  (define c (world-canvas ws))
  (place-image (draw-world ws)
               (quotient (canvas-width c) 2)
               (quotient (canvas-height c) 2)
               (empty-scene (canvas-width c) (canvas-height c) (canvas-color c))))

(define (new-canvas width height (color 'white))
  (canvas width height color))

(define (new-world canvas)
  (world canvas
         (key-event "")
         (mouse-event #f #f #f)))

(big-bang (new-world (new-canvas 800 600))
  (to-draw render)
  (on-mouse mouse-handler))