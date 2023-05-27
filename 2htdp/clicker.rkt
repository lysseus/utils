#lang racket

;;;
;;; CLICKER
;;;
;;; A basic library providing label, button and container widgets for 2htdp/image.
;;;

(provide (contract-out
          [struct label ((name (or/c number? symbol? string? char? image?))
                         (border? boolean?)
                         (font-size (or/c #f  (and/c integer? (between/c 1 255))))
                         (font-color image-color?)
                         (bg-color (or/c pen? image-color?))
                         (bo-color (or/c #f pen? image-color?))
                         (padding (or/c #f natural?)))]
          [struct button ((name (or/c natural? symbol? string? char?))
                          (active? boolean?)
                          (label (or/c #f label?))
                          (up-action procedure?))]
          [struct container ((name (or/c natural? symbol? string? char?))
                             (active? boolean?)
                             (x-offset integer?)
                             (y-offset integer?)
                             (bg-color (or/c pen? image-color?))
                             (border? boolean?)
                             (label (or/c #f label?))
                             (label-height (or/c #f positive-integer?))
                             (buttons-vertical? boolean?)
                             (buttons-border? boolean?)
                             (buttons-x-padding nonnegative-integer?)
                             (buttons-y-padding nonnegative-integer?)
                             (button-width positive-integer?)
                             (button-height positive-integer?)
                             (button-label-defaults (or/c #f label?))
                             (activate (or/c #f procedure?))
                             (deactivate (or/c #f procedure?))
                             (buttons (non-empty-listof button?)))])
         select-container/button
         process-containers
         place-containers
         find-container
         activate-container
         deactivate-container
         find-container/button
         find-button
         activate-button
         deactivate-button
         undefined
         make-label
         current-label-name
         current-label-border?
         current-label-font-size
         current-label-font-color
         current-label-bg-color
         current-label-bo-color
         current-label-padding
         make-button
         current-button-name
         current-button-active?
         current-button-label
         current-button-up-action
         make-container
         current-container-name
         current-container-active?
         current-container-x-offset
         current-container-y-offset
         current-container-bg-color
         current-container-border?
         current-container-label
         current-container-label-height
         current-container-buttons-vertical?
         current-container-buttons-border?
         current-container-buttons-x-padding
         current-container-buttons-y-padding
         current-container-button-width
         current-container-button-height
         current-container-button-label-defaults
         current-container-activate
         current-container-deactivate
         current-container-buttons)

(require 2htdp/image
         anaphoric
         racket/undefined
         utils/defstruct)


(define/contract (pen-or-color? v)
  (-> any/c boolean?)
  (or (pen? v) (image-color? v)))

(define (undefined? v)
  (eq? v undefined))

(define (true? v)
  (cond
    [(false? v) #f]
    [(undefined? v) #f]
    [else #t]))

;; active? o -> boolean?
;; Returns true if the container or button is active;
;; otherwise returns false. 
(define (active? o)
  (cond
    [(and (container? o) (container-active? o)) #t]
    [(and (button? o) (button-active? o)) #t]
    [else #f]))

;; factor: img1 img2 (pad) -> real?
;; Returns a real number that can be used to scale
;; img1 so that it exactly fits within the area bound
;; by img2, reduced by pad. 
(define/contract (factor img1 img2 (pad 0))
  (->* (image? image?) (integer?) real?)
  (define w1 (image-width img1))
  (define h1 (image-height img1))
  (define w2 (- (image-width img2) (* 2 pad)))
  (define h2 (- (image-height img2) (* 2 pad)))
  (define w2/w1 (/ w2 w1))
  (define h2/h1 (/ h2 h1))
  (define m (min w2/w1 h2/h1))
  (if (zero? m) 1 m))

;; draw-border: image -> image?
;; Draws a 2-pixel width frame the same dimensions of
;; image and overlays it. 
(define/contract (draw-border image clr) (-> image? image-color? image?)
  (define w (image-width image))
  (define h (image-height image))
  (overlay (rectangle (sub1 w) (sub1 h) 'outline clr)
           (rectangle w h 'outline clr)
           image))

;;; labels specify:
;;; - name or image
;;; - border?
;;; - font-size (optional)
;;; - font-color (optional)
;;; - bg-color (optional)
;;; - label-padding (optional)()
(defstruct label ((name "" (or/c number? symbol? string? char? image?))
                  (border? #f boolean?)
                  (font-size #f (or/c #f  (and/c integer? (between/c 1 255))))
                  (font-color 'transparent image-color?)
                  (bg-color 'transparent (or/c pen? image-color?))
                  (bo-color 'transparent (or/c #f pen? image-color?))
                  (padding #f (or/c #f natural?))))

;; Buttons specify:
;; - label (optional)
;; - active?
;; - up-action
(defstruct button ((name "" (or/c natural? symbol? string? char?))
                   (active? #t boolean?)
                   (label #f (or/c #f label?))
                   (up-action (λ args (void)) procedure?)))

;;; Continers specify:
;;; - label (optional)
;;; - active?
;;; - x-offset and y-offset
;;; - vertical?
;;; - border?
;;; - bg-color
;;; - button width and height
;;; - container x and y padding (optional) <= around buttons
;;; - button-label-defaults (optional, must have label-name #f)
;;; - button-list
(defstruct container ((name "" (or/c natural? symbol? string? char?))
                      (active? #t boolean?)
                      (x-offset 0 integer?)
                      (y-offset 0 integer?)
                      (bg-color 'transparent (or/c pen? image-color?))
                      (border? #f boolean?)
                      (label #f (or/c #f label?))
                      (label-height #f (or/c #f positive-integer?))
                      (buttons-vertical? #f boolean?)
                      (buttons-border? #f boolean?)
                      (buttons-x-padding 0 nonnegative-integer?)
                      (buttons-y-padding 0 nonnegative-integer?)
                      (button-width 0 positive-integer?)
                      (button-height 0 positive-integer?)
                      (button-label-defaults #f (or/c #f label?))
                      (activate #f (or/c #f procedure?))
                      (deactivate #f (or/c #f procedure?))
                      (buttons (list (make-button)) (non-empty-listof button?))))

(define/contract (draw-label l) (-> label? image?)
  (cond
    ;; The name is an image. When we have a font-size this becomes its scaling factor.
    ;; Otherwise we return it asis to the caller.
    [(image? (label-name l))
     (cond
       ;; Return the label img without factoring
       [(false? (label-font-size l)) (label-name l)]
       [else
        ;; factor to fit into a square of size font-size.
        (define f (factor (label-name l)
                          (square (label-font-size l) 'solid 'gold)
                          (if (false? (label-padding l))
                              0
                              (label-padding l))))
        (scale f (label-name l))])]

    ;; The name is a string with a valid font-size and font-color.
    ;; We convert it to text.
    [(and (or (string? (label-name l)) (symbol? (label-name l))
              (number? (label-name l)) (char? (label-name l)))
          (positive-integer? (label-font-size l))
          (pen-or-color? (label-font-color l)))
     (text (~a (label-name l)) (label-font-size l) (label-font-color l))]
    
    ;; The name is a string with valid font-color. We draw it with a default
    ;; font-size which will be scaled to fit the bg later. 
    [(and (or (string? (label-name l)) (symbol? (label-name l))
              (number? (label-name l)) (char? (label-name l)))
          (false? (label-font-size l))
          (pen-or-color? (label-font-color l)))
     (text (~a (label-name l)) 10 (label-font-color l))]
    
    ;; Return an empty image
    [else empty-image]))

(define/contract (draw-label/bg width height lbl)
  (-> positive-integer? positive-integer? label? image?)
  (define bg
    (rectangle width height 'solid (label-bg-color lbl)))
  (define lbl-img
    (cond
      [(image? (label-name lbl)) (label-name lbl)]
      [else (draw-label lbl)]))
  ;; The label-img must always fit into the bounding box of its bg
  (define lbl/bg-img
    (overlay
     (cond
       [(false? (label-font-size lbl))
        (scale (factor lbl-img bg (label-padding lbl)) lbl-img)]
       [(or (> (+ (* 2 (label-padding lbl)) (image-width lbl-img)) (image-width bg))
            (> (+ (* 2 (label-padding lbl)) (image-height lbl-img)) (image-height bg)))
        (scale (factor lbl-img bg (label-padding lbl)) lbl-img)]
       [else lbl-img])
     bg))
  (if (label-border? lbl) (draw-border lbl/bg-img (label-bo-color lbl)) lbl/bg-img))

;; get-container-label-height: c -> integer?
;; Returns the height of the container-label. When unspecified this is
;; the height of a button. 
(define/contract (get-container-label-height ctn)
  (-> container? integer?)
  (cond
    [(false? (container-label-height ctn)) (container-button-height ctn)]
    [else (container-label-height ctn)]))

(define/contract (draw-container-label ctn) (-> container? image?)
  (define lbl (container-label ctn))
  (cond
    [(false? lbl) empty-image]
    [(false? (label-name lbl)) empty-image]
    [else
     (draw-label/bg (container-button-width ctn)
                    (get-container-label-height ctn)
                    lbl)]))


(define/contract (resolve-button-label blbl clbl)
  (-> (or/c #f label?) (or/c #f label?) label?)
  (define name
    (cond
      [(and blbl (true? (label-name blbl))) (label-name blbl)]
      [(and clbl (true? (label-name clbl))) (label-name clbl)]
      [else #f]))
  (define border?
    (cond
      [(and blbl (or (true? (label-border? blbl)) (false? (label-border? blbl))))
       (label-border? blbl)]
      [(and clbl (true? (label-border? clbl))) (label-border? clbl)]
      [else #f]))
  (define font-size
    (cond
      [(and blbl (true? (label-font-size blbl)))
       (label-font-size blbl)]
      [(and clbl (true? (label-font-size clbl))) (label-font-size clbl)]
      [else #f]))
  (define font-color
    (cond
      [(and blbl (true? (label-font-color blbl)))
       (label-font-color blbl)]
      [(and clbl (true? (label-font-color clbl)))
       (label-font-color clbl)]
      [else 'black]))
  (define bg-color
    (cond
      [(and blbl (true? (label-bg-color blbl)))
       (label-bg-color blbl)]
      [(and clbl (true? (label-bg-color clbl)))
       (label-bg-color clbl)]
      [else 'transparent]))
  (define bo-color
    (cond
      [(and blbl (true? (label-bo-color blbl)))
       (label-bo-color blbl)]
      [(and clbl (true? (label-bo-color clbl)))
       (label-bo-color clbl)]
      [else 'transparent]))
  (define padding
    (cond
      [(and blbl (true? (label-padding blbl)))
       (label-padding blbl)]
      [(and clbl (true? (label-padding clbl)))
       (label-padding clbl)]
      [else 0]))
  (label name
         border?
         font-size
         font-color
         bg-color
         bo-color
         padding))

(define/contract (draw-button ctn btn)
  (-> container? button? image?)
  (define btn-img (draw-label/bg (container-button-width ctn)
                                 (container-button-height ctn)
                                 (resolve-button-label (button-label btn)
                                                       (container-button-label-defaults
                                                        ctn))))
  (define bg (rectangle (+ (container-button-width ctn)
                           (* 2 (container-buttons-x-padding ctn)))
                        (+ (container-button-height ctn)
                           (* 2 (container-buttons-y-padding ctn)))
                        'solid
                        'transparent))
  (overlay btn-img bg))

                                           
;; draw-container: ctn -> image?
;; Draws the buttons of a container, either horizontally
;; or vertically as specified by the container. If the
;; container has a name it is drawn as well. 
(define/contract (draw-container ctn)
  (-> container? image?)
  (define clbl (container-label ctn))
  (define clbl-img (draw-container-label ctn))
  (define images (map (λ (btn) (draw-button ctn btn))
                      (filter active? (container-buttons ctn))))
  (cond
    [(empty? images) empty-image]
    [else
     (define ctn-img
       (above
        clbl-img
        ((if (container-buttons-border? ctn) draw-border (λ (img clr) img))
         (if (> (length images) 1)
             (apply (if (container-buttons-vertical? ctn) above beside) images)
             (first images))
         (if (container-buttons-border? ctn) (label-bo-color clbl) 'transparent))))
     (define ctn-img/bg
       (overlay
        ctn-img
        (rectangle (image-width ctn-img)
                   (image-height ctn-img)
                   'solid
                   (container-bg-color ctn))))
     (if (container-border? ctn)
         (draw-border ctn-img/bg (label-bo-color clbl))
         ctn-img/bg)]))

;; place-container: ctn img -> image?
;; Draws then places the container on the image
;; based on the container's x and y offsets relative
;; to the image's (0 0) coordinate at the top left of
;; the image. 
(define/contract (place-container ctn img)
  (-> container? image? image?)
  (define ctn-img (draw-container ctn))
  (define x (container-x-offset ctn))
  (define y (container-y-offset ctn))
  (place-image ctn-img
               (+ (quotient (image-width ctn-img) 2) x)
               (+ (quotient (image-height ctn-img) 2) y)
               img))

;; place-containers: containers img -> image?
;; Places the active containers on the given image.
(define/contract (place-containers containers img)
  (-> (listof container?) image? image?)
  (define (loop cs img)
    (cond
      [(empty? cs) img]
      [else
       (loop (cdr cs)
             (place-container (first cs) img))]))
  (loop (filter active? containers) img))

(define (button-range v? n xo xp w yo yp h)
  (define (lo n o p v)
    (+ o (* n v) (* (add1 (* 2 n)) p)))
  (define (range n o p v)
    (let ([r (lo n o p v)])
      (values r (+ r v))))
  (define-values (lo-x hi-x) (range (if v? 0 n) xo xp w))
  (define-values (lo-y hi-y) (range (if v? n 0) yo yp h))
  (values lo-x hi-x lo-y hi-y))

(define (in-range? x y v? n xo xp w yo yp h)
  (define-values (lo-x hi-x lo-y hi-y)
    (button-range v? n xo xp w yo yp h))
  (and (<= lo-x x hi-x) (<= lo-y y hi-y)))


;; select-container/button: containers ws x y ->|
;; Selects the active container and button for the given mouse event. 
(define/contract (select-container/button containers ws x y evt)
  (-> (listof container?)
      any/c integer? integer? 
      (or/c "button-down" "button-up" "drag" "move" "enter" "leave")      
      any)
  (define result (make-parameter #f))
  (when (string=? evt "button-up")
    (for ([m (in-naturals)]
          [c (filter active? containers)] #:break (list? (result)))
      (define v? (container-buttons-vertical? c))
      (define xo (container-x-offset c))
      (define yo (+ (container-y-offset c)
                    (if (false? (container-label c))
                        0
                        (get-container-label-height c))))
      (define xp (container-buttons-x-padding c))
      (define yp (container-buttons-y-padding c))
      (define w (container-button-width c))
      (define h (container-button-height c))
      (define buttons (filter active? (container-buttons c)))
      (result
       (for/first ([n (range (length buttons))]
                   [b buttons]
                   #:when (in-range? x y v? n xo xp w yo yp h))
         (list c b m n)))))
  (define info (result))
  (cond
    [(false? info) #f]
    [else
     (define ctn (first info))
     (define btn (second info))
     (list ctn btn)]))

;; process-containers: containers ws x y evt ->|
;; Processes active container actions. 
(define/contract (process-containers containers ws x y evt)
  (-> (listof container?)
      any/c integer? integer? 
      (or/c "button-down" "button-up" "drag" "move" "enter" "leave")      
      any)
  (define info (select-container/button containers ws x y evt))
  (cond
    [(false? info) #f]
    [else
     (define ctn (first info))
     (define btn (second info))
     (define result ((button-up-action btn) ctn btn ws x y))
     (list ctn btn result)]))


;;;======================================================================================
;;; (De)Activating containers and buttons
;;;======================================================================================

;; find-struct: find-accessor find-val structs -> (or/c #f struct?)
;; Finds the structure from the list of structurs that matches the find-val
;; when the find-accessor is applied to it. 
(define (find-struct find-accessor find-val structs)
  (for/first ([o structs] #:when (equal? (find-accessor o) find-val))
    o))

;; find-container: ctn-name containers -> (or/c #f container?)
;; Returns the container matching cnt-name. 
(define/contract (find-container ctn-name containers)
  (-> (or/c symbol? integer?) (listof container?) (or/c #f container?))
  (find-struct container-name ctn-name containers))

;; find-container/button: ctn-name btn-name containers
;; Returns a list containering the container and button specified, or false
;; when no match was found. 
(define/contract (find-container/button ctn-name btn-name containers)
  (-> (or/c symbol? integer?)
      (or/c symbol? integer?)
      (listof container?)
      (or/c #f (list/c container? button?)))
  (and-let [ctn (find-container ctn-name containers)]
           [btn (find-struct button-name btn-name (container-buttons ctn))]
           (list ctn btn)))

;; find-button: ctn-name btn-name containers -> (or/c #f button?)
;; Returns the button only. 
(define/contract (find-button ctn-name btn-name containers)
  (-> (or/c symbol? integer?)
      (or/c symbol? integer?)
      (listof container?)
      (or/c #f button?))
  (and-let [result (find-container/button ctn-name btn-name containers)]
           (second result)))

;; set-accessor! find-accessor find-val set-accessor set-val structs -> (or/c #f struct?)
;; Sets the structure slot set-accessor with set-val for the structure matching
;; the find-val when find-accessor is applied. If no structure matches find-val
;; then #f is treturned. When a match is found the slot is set and the matching
;; structure is returned. 
(define (set-accessor! find-accessor find-val set-accessor set-val structs)
  (define s (find-struct find-accessor find-val structs))
  (when s (set-accessor s set-val))
  s)

;; activate-container: ctn-name ws containers -> (or/c #f container?)
;; Activates the matching container. If the container has an activate function
;; this is passed the world-state and its result returned.
(define/contract (activate-container ctn-name ws containers)
  (-> (or/c symbol? integer?) any/c (listof container?) any)
  (and-let [ctn (set-accessor! container-name
                               ctn-name set-container-active?! #t containers)]
           [activate (container-activate ctn)]
           (activate ctn ws)))

;; deactivate-container: ctn-name ws containers -> any
;; Deactivates the matching container. If the container has a deactivate function
;; this is passed the world-state and its result returned. 
(define/contract (deactivate-container ctn-name ws containers)
  (-> (or/c symbol? integer?) any/c (listof container?) any)
  (and-let [ctn (set-accessor! container-name
                               ctn-name set-container-active?! #f containers)]
           [deactivate (container-deactivate ctn)]
           (deactivate ctn ws)))

;; activate-button: ctn-name btn-name containers -> (or/c #f (list/c container? button?))
;; Activates the specified button for the specified container.
(define/contract (activate-button ctn-name btn-name containers)
  (-> (or/c symbol? integer?)
      (or/c symbol? integer?)
      (listof container?)
      (or/c #f (list/c container? button?)))
  (and-let [ctn (find-container ctn-name containers)]
           [btn (find-struct button-name btn-name (container-buttons ctn))]
           (begin
             (set-button-active?! btn #t)
             (list ctn btn))))

;; deactivate-button ctn-name btn-name containers -> (or/c #f (list/c container? button?))
;; Deactivates the specified button within the specified container.
;; Note that this function does not deactivate the container when all its buttons
;; are deactivated. This is left to the program, but should be unneccessary as
;; containers are not drawn when their buttons are all deactivated, nor clickable.
(define/contract (deactivate-button ctn-name btn-name containers)
  (-> (or/c symbol? integer?)
      (or/c symbol? integer?)
      (listof container?)
      (or/c #f (list/c container? button?)))
  (and-let [ctn (find-container ctn-name containers)]
           [btn (find-struct button-name btn-name (container-buttons ctn))]
           (begin
             (set-button-active?! btn #f)
             (list ctn btn))))
