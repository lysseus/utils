#lang racket

;; Implements a set of image-related functions.

(provide 
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
   (->i ([width real?]
         [height real?]
         [img image?])
        ()
        (result image?))]
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
        (result image?))]))

;;--------------------------------------
;; import and implementation section

(require racket/function
         2htdp/image
         (only-in utils/matrix/matrix
                  matrix
                  matrix->list
                  matrix-rows
                  matrix-cols
                  matrix-set
                  matrix-rotate
                  matrix-accessor-find
                  matrix-map)
         (only-in utils/list
                  list->values
                  values-ref
                  sublist)
         (only-in utils/file
                  make-dir-path-list))

(module+ test
  (require rackunit
           (only-in utils/list
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
(define (scale-image width
                     height
                     image)
  (overlay (scale (min (/ width (image-width image))
                       (/ height (image-height image)))
                  image)
           (rectangle width height 'outline 'transparent)))

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
    (values-ref 0 (matrix-accessor-find (negate transparent?) mtx))))

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
