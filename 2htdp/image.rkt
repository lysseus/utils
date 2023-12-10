#lang racket

;;;
;;; IMAGE
;;; Implements a set of image-related functions for 2htdp/image.
;;;

(provide
 (struct-out posn)
 (struct-out place)
 (struct-out image/loc)
 place-images/loc
 rounded-rectangle
 frame-image
 color-frame/pixels
 color-frame/group
 image-pad
 images-max-dims
 place-image/scale
 place-image/fit
 overlay/fit
 (contract-out
  ; returns the center of the image
  [image-center
   (->i ([img image?])
        ()
        (result (list/c exact-nonnegative-integer?
                        exact-nonnegative-integer?)))]
  ; returns the x-coordinate of the image center  
  [image-center-x
   (->i ([img image?])
        ()
        (result exact-nonnegative-integer?))]
  ; returns the y-coordinate of the image center
  [image-center-y
   (->i ([img image?])
        ()
        (result exact-nonnegative-integer?))]
  ; returns the number of pixels in the image.
  [image-pixels
   (->i ([img image?])
        ()
        (results exact-nonnegative-integer?))]
  ; returns a relative magnitude value of the size of the image.
  [image-magnitude 
   (->i ([img image?])
        ()
        (result exact-nonnegative-integer?))]
  ; converts a matrix row column position to an image xy position
  [board-pos->image-pos
   (->i ([row integer?]
         [col integer?]
         [width exact-nonnegative-integer?]
         [height exact-nonnegative-integer?])
        ()
        (result (list/c integer? integer?)))]
  ; conterts an image xy postion to a matrix row column position
  [image-pos->board-pos
   (->i ([x integer?]
         [y integer?]
         [width exact-nonnegative-integer?]
         [height exact-nonnegative-integer?])
        ()
        (result (list/c integer? integer?)))]
  ; converts an image xy position to the center image xy position
  [xy->center-xy
   (->i ([x integer?]
         [y integer?]
         [width exact-nonnegative-integer?]
         [height exact-nonnegative-integer?])
        ()
        (result (list/c integer? integer?)))]
  ; indicates whether the image xy position is within the matrix
  [on-board
   (->i ([x integer?]
         [y integer?]
         [width exact-nonnegative-integer?]
         [height exact-nonnegative-integer?]
         [mtx (and/c pair? list?)])
        ()
        (values (r0 boolean?)
                (r1 integer?)
                (r2 integer?)))]
  [scale-image
   (->* (natural? natural? image?)
        (#:expand? boolean?)
        image?)]
  [transparent?
   (->i ([lst (non-empty-listof color?)])
        ()
        (result boolean?))]
  ; returns the image's significant pixel (non-transparent) line index 
  [image-sigpixln
   (->i ([img image?])
        ()
        (result exact-nonnegative-integer?))]
  [image-frame/stats
   (->i ([color (or/c symbol? pen?)]
         [img image?])
        ()
        (result image?))]
  [image-stats
   (->i ([img image?])
        ()
        (result none/c))]
  [crop-image
   (->i ([img image?])
        (#;()
         #:pad-top [pad-top integer?]
         #:pad-bottom [pad-bottom integer?]
         #:pad-left [pad-left integer?]
         #:pad-right [pad-right integer?])
        (result image?))]
  [center-crop
   (->i ([width exact-nonnegative-integer?]
         [height exact-nonnegative-integer?]
         [img image?])
        ()
        (result image?))]
  [scale/center-crop
   (->i ([width exact-nonnegative-integer?]
         [height exact-nonnegative-integer?]
         [img image?])
        ()
        (result image?))]
  ; crops image into a matrix of cropped images
  [crop/matrix
   (->i ([rows exact-nonnegative-integer?]
         [cols exact-nonnegative-integer?]
         [width exact-nonnegative-integer?]
         [height exact-nonnegative-integer?]
         [img image?])
        ()
        (result (non-empty-listof (non-empty-listof image?))))]
  ; reconstitutes a matrix of cropped images into a single image
  [draw-board
   (->i ([board (non-empty-listof (non-empty-listof any/c))])
        ([draw-board-square procedure?])
        (result image?))]
  ; frames image in variable thickness
  [hilite
   (->i ([color (or/c symbol? pen?)]
         [n integer?]
         [img image?])
        ()
        (result image?))]
  ; load bitmap images from file-list to a list
  [load-images 
   (->i ([dir string?])
        (#;()
         #:width [width exact-nonnegative-integer?]
         #:height [height exact-nonnegative-integer?]
         #:error-log [error-log string?])
        (result (non-empty-listof image?)))]
  ; load bitmap image file from randomly selected file in file-list
  [load-random-image
   (->i ([dir string?])
        (#;()
         #:width [width exact-nonnegative-integer?]
         #:height [height exact-nonnegative-integer?]
         #:error-log [error-log string?])
        (result image?))]
  ; horizontally stacks planetcute images
  [hstack
   (->i ()
        (#:base [base exact-nonnegative-integer?])
        #:rest [imgs (non-empty-listof image?)]       
        (result image?))]
  ; vertically stacks planetcute images
  [vstack
   (->i ()
        (#:base [base exact-nonnegative-integer?])
        #:rest [imgs (non-empty-listof image?)]       
        (result image?))]));

;;--------------------------------------
;; import and implementation section

(require racket/function
         2htdp/image
         (only-in utils/matrix
                  matrix
                  matrix->list
                  matrix-rows
                  matrix-cols
                  matrix-set
                  matrix-rotate
                  matrix-accessor-find
                  matrix-map)
         (only-in (submod utils/list alt)
                  list->values
                  values->list
                  values-ref)
         (only-in utils/list
                  sublist)
         (only-in utils/file
                  make-dir-path-list))

(module+ test
  (require rackunit
           (only-in (submod utils/list alt)
                    values->list)))


;; image-center: img -> pair
;; retrieves the image center.
(define (image-center img)
  (list (quotient (image-width img) 2) 
        (quotient (image-height img) 2)))

;; image-center-x: img -> integer
;; Returns the x-coordinate of the image's center
(define (image-center-x img)
  (first (image-center img)))

;; image-center-y: img -> integer
;; Returns the y-coordinate of the image's center
(define (image-center-y img)
  (second (image-center img)))

;; image-pixels: image -> integer?
;; Returns the number of pixels in the image.
(define (image-pixels img) (* (image-width img) (image-height img)))

;; image-magnitude: image -> integer?
; Returns a relative magnitude value of the size of the image using 
;; Log base 10.
(define (image-magnitude img)
  (inexact->exact (round (/ (log (image-pixels img)) (log 10)))))

;; board-pos->image-pos: row col width height -> list?
;; Converts board row column position to image x/y pixel position.
(define (board-pos->image-pos row col width height)
  (define X (+ (* col width) (quotient width 2)))
  (define Y (+ (* row height) (quotient height 2)))
  (list X Y))

;; image-pos->board-pos: x y width height -> list?
;; Converts image x/y pixel position to board row column position.
(define (image-pos->board-pos x y width height)
  (define ROW (quotient y height))
  (define COL (quotient x width))
  (list ROW COL))

;; xy->center-xy: x y width height -> list?
;; converts marble xy coordinates to board square-centered xy coordinates.
(define (xy->center-xy x y width height)
  (define-values (ROW COL) (list->values (image-pos->board-pos x y width height)))
  (board-pos->image-pos ROW COL width height))

;; on-board x y unit-image matrix -> boolean? row column
;; Returns a boolean indicating whether the pixel position x y maps
;; to a row and column within the range of the matrix. 
(define (on-board x y width height mtx)
  (define ROWS (matrix-rows mtx))
  (define COLS (matrix-cols mtx))
  (define-values (ROW COL) (list->values (image-pos->board-pos x y width height)))
  (define RESULT (cond
                   [(or (< x 0) (< y 0)) #f]
                   [(and (< ROW ROWS) (< COL COLS)) #t]
                   [else #f]))
  (values RESULT ROW COL))

;; scale-image: width height image -> image
;; scales the image to fit the framing rectangle's width and height.
;; if expand? is false and img already fits within the bounding frame,
;; no scaling is peformed.
(define (scale-image #:expand? (expand? #t) w h img)  
  (define img-w (image-width img))
  (define img-h (image-height img))
  (cond
    [(or (zero? img-w) (zero? img-h)) img]
    [else
     (define Δw (- img-w w))
     (define Δh (- img-h h))  
     (define fw (/ w img-w))
     (define fh (/ h img-h))
     (define f (cond
                 ;; Already in frame, no scale change.
                 [(and (not expand?)
                       (not (positive? Δw))
                       (not (positive? Δh)))               
                  1]
                 ;; fit to frame
                 [else (min fw fh)]))  
     (scale f img)]))

;; transparent?: lst -> boolean
;; predicate reutrns true if color-list is completely transparent.
(define (transparent? color-list)
  (cond
    [(null? color-list) #t]
    [(> (color-alpha (car color-list)) 0) #f]
    [else (transparent? (cdr color-list))]))

;; image-sigpixln: image -> number
;; returns the y-posn indicating the first significant pixel line,
;; a line with at least one non-transparent pixel, of the image 
;; starting from y-posn 0.
(define (image-sigpixln img)
  (let* ([width (image-width img)]
         [height (image-height img)]
         [colors (image->color-list img)]
         [mtx (matrix colors #:cols width #:rows height)])
    (values-ref 0 (thunk (matrix-accessor-find (negate transparent?) mtx)))))

;; image-frame/stats: color img
;; Image stats, center-pinhole and frame. 
(define (image-frame/stats color img)
  (image-stats img)
  (color-frame color (center-pinhole img)))

;; image-stats: image ->|
;; prints some useful image information.
(define (image-stats img)
  (let ([colors (image->color-list img)])
    (printf "~%Dimensions: ~ax~a
Pixels: ~a
Magnitude: ~a
Center: ~s
Baseline=~a
Significant Pixel Line Offsets: left=~a right=~a top=~a bottom=~a
~%" 
            (image-width img)
            (image-height img)
            (image-pixels img)
            (image-magnitude img)
            (image-center img)
            (image-baseline img)
            (image-sigpixln (rotate -90 img))
            (image-sigpixln (rotate 90 img))
            (image-sigpixln img)
            (image-sigpixln (rotate 180 img)))))

;; crop-image: image number number number number -> image
;; Crops the image relative to the respective significant pixel line. 
;; If the pad is #f the image is unchanged in that direction. A positive
;; value pads the image beyond the significant pixel line for the specified 
;; direction. A negative value crops the image starting from the significant 
;; pixel line. Zero crops the image at the significant pixel line.
(define (crop-image img
                    #:pad-top (pad-top 0)
                    #:pad-bottom (pad-bottom 0)
                    #:pad-left (pad-left 0)
                    #:pad-right (pad-right 0))
  (letrec ([image->color-matrix 
            (λ (img)
              (let ([cols (image-width img)]
                    [rows (image-height img)])
                (matrix (image->color-list img)
                        #:cols cols
                        #:rows rows)))]
           [color-matrix->image
            (λ (mtx)
              (let ([color-list (matrix->list mtx)]
                    [width (matrix-cols mtx)]
                    [height (matrix-rows mtx)])
                (color-list->bitmap color-list width height)))]
           [transparent?
            (λ (lst)
              (cond
                [(null? lst) #t]
                [(> (color-alpha (car lst)) 0) #f]
                [else (transparent? (cdr lst))]))]
           [sigpixln 
            (λ (mtx)
              (values-ref 0 
                          (matrix-accessor-find (negate transparent?) 
                                                mtx)))]
           [crop-matrix
            (λ (mtx pad)
              (let* ([elems (make-list (matrix-cols mtx)
                                       (color 255 255 255 0))]
                     [padding (make-list (if (positive? pad) pad 0) elems)]
                     [rems (if (negative? pad) 
                               (+ (abs pad) (sigpixln mtx)) 
                               (sigpixln mtx))])
                (append padding
                        (sublist mtx rems))))]
           [recurse
            (λ (mtx top bottom left right)
              (cond
                [top
                 (recurse (crop-matrix mtx top)
                          #f
                          bottom
                          left
                          right)]
                [bottom
                 (recurse (matrix-rotate 
                           (crop-matrix (matrix-rotate mtx 2) bottom) 
                           -2)
                          top
                          #f
                          left
                          right)]
                [left
                 (recurse (matrix-rotate
                           (crop-matrix (matrix-rotate mtx 1) left)
                           -1)
                          top
                          bottom
                          #f
                          right)]
                [right
                 (recurse (matrix-rotate
                           (crop-matrix (matrix-rotate mtx -1) right)
                           1)
                          top
                          bottom
                          left
                          #f)]
                [else mtx]))]
           [crop-image
            (λ (img top bottom left right)
              (color-matrix->image
               (recurse (image->color-matrix img)
                        top
                        bottom
                        left
                        right)))])
    (crop-image img pad-top pad-bottom pad-left pad-right)))

;; center-crop: width height image -> image?
;; crops image in a rectangle of width x height whose center is image center.
(define (center-crop width height img)
  (crop (- (quotient (image-width img) 2) (quotient width 2))
        (- (quotient (image-height img) 2) (quotient height 2))
        width
        height
        img))


(define (scale/center-crop width height img)
  (define W (/ width (image-width img)))
  (define H (/ height (image-height img)))
  (define S (if (> W H) W H))
  (define SCALE-IMG (scale S img))
  (define CROP-IMG (crop/align "middle" "middle" width height  SCALE-IMG))
  CROP-IMG)

(define (crop/matrix rows cols width height img)
  (define CROPPED-IMG (scale/center-crop
                       (* cols width)
                       (* rows height)
                       img))
  (define IMG-WIDTH (image-width CROPPED-IMG))
  (define IMG-HEIGHT (image-height CROPPED-IMG))
  (define IMG-MATRIX (matrix #:cols cols
                             #:rows rows
                             #:fill-value #f))
  
  (define (crop-it row col val) 
    (define CENTER-POSN (board-pos->image-pos row col width height))
    (define CENTER-X (first CENTER-POSN))
    (define CENTER-Y (second CENTER-POSN))
    (crop (- CENTER-X (quotient width 2))
          (- CENTER-Y (quotient height 2))
          width
          height
          CROPPED-IMG))
  
  (matrix-map crop-it IMG-MATRIX))

;; draw-board: board -> image?
;; Draws the board based on the board matrix.
(define (draw-board board (draw-board-square (λ (s) (color-frame 'white s))))
  (let loop ([board board] [acc empty])
    (cond
      [(null? board) (apply above (reverse acc))]
      [else (loop (cdr board) (cons (draw-board-row (car board) draw-board-square) 
                                    acc))])))

;; draw-board-row: row -> image?
;; Draws a row of the board.
(define (draw-board-row row (draw-board-square identity))
  (let loop ([row row] [acc empty])
    (cond
      [(null? row) (apply beside (reverse acc))]
      [else (loop (cdr row) (cons (draw-board-square (car row)) 
                                  acc))])))

;; hilite: color n img0 -> image?
;; frames image in a line of n+1 pixels, drawn inward.
(define (hilite color n img)
  (define WIDTH (image-width img))
  (define HEIGHT (image-height img))
  (define (draw-frame color n)
    (color-frame color
                 (rectangle (- WIDTH n)
                            (- HEIGHT n)
                            'outline
                            'transparent)))
  (let loop ([n n] [img img])
    (cond
      [(zero? n) (overlay (draw-frame color n) img)]
      [else (loop (sub1 n) (overlay (draw-frame color n) img))])))

;; load-images: rel-dir [#:width] [#:height] -> list?
;; Loads images from file-list and returns the images
;; in a list.
(define (load-images dir
                     #:width (width #f)
                     #:height (height #f)
                     #:error-log (error-log "error-log.txt"))
  (define FILE-LIST (make-dir-path-list dir #:error-log error-log))
  (define IMGS 
    (map bitmap/file FILE-LIST))
  (define (scale-it img)
    (cond
      [(and width height) (scale-image width height img)]
      [width (scale-image width (image-height img) img)]
      [height (scale-image (image-height img) height img)]
      [else img]))
  (map scale-it 
       IMGS))

;; load-random-image: dir -> image
;; Randomly selects an image file from file-list and 
;; converts the file into a bitmap image.
(define (load-random-image dir
                           #:width (width #f)
                           #:height (height #f)
                           #:error-log (error-log "error-log.txt"))
  (define FILE-LIST (make-dir-path-list dir #:error-log error-log))
  (define IMG 
    (apply bitmap/file (list (list-ref FILE-LIST (random (length FILE-LIST))))))
  
  (cond
    [(and width height) (scale-image width height IMG)]
    [width (scale-image width (image-height IMG) IMG)]
    [height (scale-image (image-width IMG) height IMG)]
    [else IMG]))

;; hstack: [#:base] images ... -> image?
;; Horizontally stacks planetcute images.
;; Base default of 40 is for an base image pixel-height of 171.
(define (hstack #:base (base 40) . imgs)
  (let loop ([imgs imgs])
    (cond
      [(empty? (rest imgs)) (first imgs)]
      [else (overlay/xy (first imgs)
                        0 base
                        (loop (rest imgs)))])))

;; vstack: [#:base] images ... -> image?
;; Vertically stacks planetcute images.
;; Base default of 80 is for an base image pixel-height of 171.
(define (vstack #:base (base 80) . imgs)
  (let loop ([imgs imgs] [img (first imgs)])
    (cond
      [(empty? (rest imgs)) img]
      [else
       (define H1 (image-height img))
       (define H2 (image-height (second imgs)))
       (define Y (+ #;80 base (- H1 H2)))
       
       (loop (rest imgs) (underlay/xy img
                                      0 Y
                                      (second imgs)))])))


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
  
  (test-case "centering tests"
             ;; sample image
             (define RECTANGLE (rectangle 80 120 'solid 'gold))
             (define WIDTH (image-width RECTANGLE))
             (define HEIGHT (image-height RECTANGLE))
             
             (test-case "image center tests"
                        
                        (check-equal? (image-center RECTANGLE)
                                      (list (quotient WIDTH 2)
                                            (quotient HEIGHT 2))))
             
             (test-case "center x/y tests"
                        
                        (check-equal? (image-center-x RECTANGLE)
                                      (quotient WIDTH 2))
                        
                        (check-equal? (image-center-y RECTANGLE)
                                      (quotient HEIGHT 2)))
             
             (test-case "board-pos->image-pos tests"
                        (define ROW 10)
                        (define COL 5)
                        (define CENTER-X 440)
                        (define CENTER-Y 1260)
                        
                        ; row 0 col 0 should be the center of the rectangle
                        (check-equal? (board-pos->image-pos
                                       0
                                       0 
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      (list (image-center-x RECTANGLE) 
                                            (image-center-y RECTANGLE)))
                        
                        ; row 1 col 1 should produce the pixel location of the
                        ; center of row 1 col 1 (1.5 image-units).
                        (check-equal? (board-pos->image-pos
                                       1
                                       1 
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      (list (+ (image-width RECTANGLE)
                                               (image-center-x RECTANGLE)) 
                                            (+ (image-height RECTANGLE)
                                               (image-center-y RECTANGLE))))
                        
                        ; row 2 col 2 should produce the pixel location of the
                        ; center of row 2 col 2 (2.5 image-units).
                        (check-equal? (board-pos->image-pos
                                       2
                                       2 
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      (list (+ (* (image-width RECTANGLE) 2)
                                               (image-center-x RECTANGLE)) 
                                            (+ (* (image-height RECTANGLE) 2)
                                               (image-center-y RECTANGLE)))))
             
             (test-case "image-pos->board-pos tests"
                        (define ROW 10)
                        (define COL 5)
                        (define CENTER-X 440)
                        (define CENTER-Y 1260)
                        
                        ; Pixel origin (0,0) should produce board row 0 col 0.
                        (check-equal? (image-pos->board-pos
                                       0
                                       0
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      '(0 0))
                        
                        ; 1 pixel less than the width of the image and 
                        ; 1 pixel less than the height of the image should 
                        ; produce board row 0 col 0.
                        (check-equal? (image-pos->board-pos
                                       (sub1 (image-width RECTANGLE))
                                       (sub1 (image-height RECTANGLE))
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      '(0 0))
                        
                        ; The width of the image and 
                        ; the height of the image should 
                        ; produce board row 0 col 0.                        
                        (check-equal? (image-pos->board-pos
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE)
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      '(1 1))
                        
                        ; 1 pixel less than 2x the width  of the image 
                        ; and 1 pixel less than 2x the height of the image 
                        ; should produce board row 1 col 1.
                        (check-equal? (image-pos->board-pos
                                       (sub1 (* 2 (image-width RECTANGLE)))
                                       (sub1 (* 2 (image-height RECTANGLE)))
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      '(1 1))
                        
                        ; pixel CENTER-X CENTER-Y should produce board 
                        ; position ROW COL.
                        (check-equal? (image-pos->board-pos
                                       CENTER-X 
                                       CENTER-Y 
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      (list ROW COL)))
             
             (test-case "xy->center-xy tests"
                        
                        ; Pixel origin (0,0) should produce the center
                        ; of the unit-image
                        (check-equal? (xy->center-xy 0
                                                     0 
                                                     (image-width RECTANGLE)
                                                     (image-height RECTANGLE))
                                      (image-center RECTANGLE))
                        
                        ; 1 pixel less than the image width and
                        ; 1 pixel less than the image height should
                        ; produce the image center.
                        (check-equal? (xy->center-xy
                                       (sub1 (image-width RECTANGLE))
                                       (sub1 (image-height RECTANGLE))
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      (image-center RECTANGLE))
                        
                        ; The image width and image center should 
                        ; produce 1.5x the width and 1.5x the height.
                        (check-equal? (xy->center-xy
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE)
                                       (image-width RECTANGLE)
                                       (image-height RECTANGLE))
                                      (list 
                                       (+ (image-width RECTANGLE)
                                          (image-center-x RECTANGLE))
                                       (+ (image-height RECTANGLE)
                                          (image-center-y RECTANGLE)))))
             
             (test-case "on-board tests"
                        (define SQUARE (square 20 'solid 'gold))
                        (define M (matrix '(a b c d e f)
                                          #:rows 2
                                          #:cols 3))
                        
                        ; The pixel origin is "on the board" and at
                        ; row 0 col 0.
                        (check-equal? (values->list (on-board 0 
                                                              0 
                                                              (image-width SQUARE)
                                                              (image-height SQUARE)
                                                              M))
                                      '(#t 0 0))
                        
                        ; The pixel at (-1 0) is not on the board. 
                        (check-equal? (values->list (on-board -1 
                                                              0 
                                                              (image-width SQUARE)
                                                              (image-height SQUARE)
                                                              M))
                                      '(#f 0 0))
                        
                        ; The pixel at (20 20) is on the board at row 1 col 1.
                        (check-equal? (values->list (on-board 20 
                                                              20 
                                                              (image-width SQUARE)
                                                              (image-height SQUARE)
                                                              M))
                                      '(#t 1 1))
                        
                        ; The pixel at [60 40) is not on the board.
                        (check-equal? (values->list (on-board 60 
                                                              40 
                                                              (image-width SQUARE)
                                                              (image-height SQUARE)
                                                              M))
                                      '(#f 2 3)))))

;; Place/aligns img1 on 1mg2 after scaling img to fit the x or y
;; dimension (whichever is smaller) of img2. If img1 already fits
;; into img2 it can be placed without expansion through use of the
;; expand? keyword. 
(define (place-image/scale #:expand? (expand? #f)
                           #:x (x 0)
                           #:y (y 0)
                           #:x-place (x-place 'left)
                           #:y-place (y-place 'top)            
                           img1 img2)
  (define img1-w (image-width img1))
  (define img1-h (image-height img1))
  (define img2-w (image-width img2))
  (define img2-h (image-height img2))
  (define img
    (cond
      [(and (<= img1-w img2-w)
            (<= img1-h img2-h)
            (false? expand?)) img1]
      [else (scale-image img2-w
                         img2-h
                         img1)]))
  (place-image/align img
                     x y x-place y-place
                     img2))

;; Overlays img with img1 after scaling it to fit.
;; If pad is supplied then img1 is scaled to fit the
;; frame padding.
(define (overlay/fit #:w-pad (w-pad 0)
                     #:expand? (expand? #t)
                     #:left-pad (left-pad w-pad)
                     #:right-pad (right-pad w-pad)
                     #:h-pad (h-pad 0)
                     #:top-pad (top-pad h-pad)
                     #:bottom-pad (bottom-pad h-pad)
                     img1 img2)
  (define (pad img left-pad right-pad top-pad botom-pad)
    (define left-frame (rectangle left-pad 1 'solid 'transparent))
    (define right-frame (rectangle right-pad 1 'solid 'transparent))
    (define top-frame (rectangle 1 top-pad 'solid 'transparent))
    (define bottom-frame (rectangle 1 bottom-pad 'solid 'transparent))
    (above
     top-frame
     (beside left-frame img right-frame)
     bottom-frame))
  (overlay (pad (scale-image #:expand? expand?
                             (- (image-width img2) left-pad right-pad)
                             (- (image-height img2) top-pad bottom-pad)
                             img1)
                left-pad right-pad top-pad bottom-pad)
           img2))

;; Places img1 on img2 after scaling image to fit the dimensions of img2.
;; Scaling is uniform over both x and y dimensions, so img1 is scaled to
;; fit the least of x or y for img2. 
(define (place-image/fit img1 img2)
  (place-image/scale img1 img2
                     #:expand? #t))


;; Returns the maximum width and hieght for the images.
;; Images can be in a list, vector, or hash-values. 
(define/contract (images-max-dims #:width-min (width-min 0)
                                  #:height-min (height-min 0)
                                  images)
  (->* ((or/c (listof image?) (vectorof image?) hash?))
       (#:width-min natural? #:height-min natural?) any)
  (define images-list
    (cond
      [(or (list? images) (vector? images)) images]
      [else (hash-values images)]))
  (for/fold ([wacc width-min] [hacc height-min])
            ([img images-list])
    (define w (image-width img))
    (define h (image-height img))
    (values (if (> w wacc) w wacc)
            (if (> h hacc) h hacc))))


;; Wrap the image in a rectangular transparent padding
;; of w x h pixels.
(define/contract (image-pad img (w 0) (h w) (clr 'transparent))
  (->* (image?) (natural? natural? image-color?) any)
  (overlay img
           (rectangle (+ (* 2 w) (image-width img))
                      (+ (* 2 h) (image-height img))
                      'solid clr)))

;; Frames an image with orignal image scaled to fit inside the
;; dimensions of the original image, framed with rectangle of
;; specified color and thickness.
(define/contract frame-image
  (->i ((img image?))
       (#:width (w natural?)
        #:height (h natural?)
        #:frame-color (c image-color?)
        #:frame-thickness (t (w h) (between/c 0 (sub1 (quotient (min w h) w)))))
       (result image?))
  (let ([cache (make-hash)])
    (λ (img
        #:width (width (image-width img))
        #:height (height (image-height img))
        #:frame-color (frame-color 'white)
        #:frame-thickness (frame-thickness 1))
  (define key (list img width height frame-color frame-thickness))
  (cond
    [(hash-has-key? cache key)
     (hash-ref cache key)]
    [else     
     (define img-width (image-width img))
     (define img-height (image-height img))     
     (define Δimg-width (- img-width (* 2 frame-thickness)))
     (define Δimg-height (- img-height (* 2 frame-thickness)))     
     (define Δx-f (cond
                    [(positive? img-width) (/ Δimg-width width)]
                    [else #f]))
     (define Δy-f (cond
                    [(positive? Δimg-height) (/ Δimg-height height)]
                    [else #f]))     
     (define scaled-img (cond
                          [(zero? (min img-width img-height)) img]
                          [(or (false? Δx-f)
                               (false? Δy-f))
                           (error "image can't be reduced to frame.")]
                          [else (scale/xy Δx-f Δy-f img)]))     
     (define (draw-frame w h frame-color)       
       [rectangle w h 'outline frame-color])
     (define frame
       (cond
         [(zero? frame-thickness) empty-image]
         [(= frame-thickness 1)       
          (draw-frame width height frame-color)]
         [else (apply overlay (for/list ([n (range frame-thickness )])
                                (define  w (- width (* 2 n)))
                                (define h (- height (* 2 n)))
                                (draw-frame w h frame-color)))]))
     (define framed-img (overlay scaled-img frame))
     (hash-set! cache key framed-img)
     framed-img]))))

;; Wraps the image in a rectangular frame of n pixel thickness.
(define/contract (color-frame/pixels clr img (n 1))
  (->* ((or/c pen? image-color?) image?) (natural?) any)
  (cond
    [(zero? n) img]
    [else (color-frame/pixels clr
                              (overlay img
                                       (rectangle (add1 (image-width img))
                                                  (add1 (image-height img))
                                                  'outline clr))
                              (sub1 n))]))

;; ;; Groups a set of images either above or beside with specified padding, slignment, and framing.
(define/contract (color-frame/group #:above? (above? #t)
                                    #:pad-w (pad-w 4)
                                    #:pad-h (pad-h pad-w)
                                    #:frame-color (frame-color 'white)
                                    #:frame-pixels (frame-pixels 1)
                                    #:x-place (x-place 'left)
                                    images)
  (->* ((listof image?))
       (#:above? boolean?
        #:pad-w natural?
        #:pad-h natural?
        #:frame-color image-color? #:frame-pixels natural?
        #:x-place x-place?)
       any)
  (define padded-images (cond
                          [(and (zero? pad-w) (zero? pad-h)) images]
                          [else (for/list ([image images]) (image-pad image pad-w pad-h))]))
  (define-values (w h) (images-max-dims padded-images))
  (define tframe (rectangle w h 'solid 'transparent))  
  (define frames (for/list ([image padded-images])
                   (color-frame/pixels frame-color
                                       (overlay/align x-place 'top image tframe)
                                       frame-pixels)))
  (apply (if above? above beside) frames))

;; Draws a rounded-rectangle of width w and height h. The rounding r%
;; rerpresents a percentage between 0 and 1 of a readial length 1/2 the
;; minimum of the width and height of the rectangle. An r% of 0 produces
;; a normal rectangle of dimension width and length. An r% of 1 produces
;; a "stadium" in wich each end ot the rectangle is a semicircle of readius
;; width or height. When width = height produces a square for r% = 0 and a
;; circle for r% = 1.
(define/contract (rounded-rectangle w h r% m c)
  (-> (and/c real? (not/c negative?))
      (and/c real? (not/c negative?))
      (and/c real? (between/c 0 1))
      mode? image-color? any)
  (define r (round (* r% (quotient (min w h) 2))))  
  (define corner (crop 0 0 r r (circle r m c)))
  (define wl (- w (* 2 r)))
  (define wline (cond
                  [(zero? wl) empty-image]
                  [(eq? m 'outline) (line (sub1 wl) 0 c)]
                  [else (rectangle wl r 'solid c)]))
  (define top (beside/align "top" corner wline (flip-horizontal corner)))  
  (define bottom (flip-vertical top))
  (define mh (cond
               [(zero? (- h (* 2 r))) 0]
               [else (- h (* 2 r)
                        (if (eq? m 'outline) 2 0))]))  
  (define mline (cond
                  [(zero? mh) empty-image]
                  [else (line 0 (sub1 mh) c)]))
  (define middle (cond
                   [(zero? mh)empty-image]
                   [else (beside mline
                                 (rectangle (- w 2) mh 'solid
                                            (if (eq? m 'outline)
                                                'transparent
                                                c))
                                 mline)]))  
  (define tmp (above top middle bottom))
  (define img
    (cond
      [(eq? m 'outline)
       (define img (overlay (overlay tmp (flip-horizontal tmp)) (flip-vertical tmp)))
       (scale/xy (/ w (image-width img)) (/ h (image-height img))
                 img)]
      [else tmp]))  
  img)

;; Places images on scene as determined by the posn and place
;; values specifed in the image/loc struct. 
(struct posn (x y) #:transparent)
(struct place (x y) #:transparent)
(struct image/loc (image posn place) #:transparent)

(define/contract (place-images/loc #:place-default (place-default (place 'left 'top))
                          images/loc scene)
  (->* ((listof image/loc?) image?) (#:place-default place?) image?)
  (cond
    [(empty? images/loc) scene]
    [else
     (define loc (first images/loc))
     (define image (image/loc-image loc))     
     (define xy-posn (image/loc-posn loc))
     (define x-offset (posn-x xy-posn))
     (define y-offset (posn-y xy-posn))
     (define xy-place (if (false? (image/loc-place loc))                          
                          place-default
                          (image/loc-place loc)))
     (define x-place (place-x xy-place))
     (define y-place (place-y xy-place))
     
     (place-images/loc #:place-default place-default
                       (cdr images/loc)
                       (place-image/align image
                                          x-offset y-offset
                                          x-place y-place
                                          scene))]))
