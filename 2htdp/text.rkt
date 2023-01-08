#lang racket

;;;
;;; TEXT
;;;

(require 2htdp/image
         utils/list)

(provide text-wrap text-tok-wrap)


;; Splits the string in two with the first value being the last character string
;; and the second value as the beginning of the string up to that character.
(define/contract (split-at/last-char str) (-> string? (values string? string?))
  (define chars (string->list str))
  (if (empty? chars)
      (values "" "")
      (values (~a (last chars))
              (apply ~a (take chars (sub1 (length chars)))))))

;; Splits the string into two with the first value being the last word string
;; and the second value as the beginning of the string up to that word.
(define/contract (split-at/last-word txt (acc empty))
  (->* (string?) (empty?) (values string? string?))
  (define-values (ch rst) (split-at/last-char txt))
  (cond
    [(string=? ch "") (values (apply ~a acc) rst)]
    [(string=? ch "\n") (values (apply ~a acc) rst)]
    [(string=? ch " ") (values (apply ~a acc) rst)]
    [else (split-at/last-word rst (cons ch acc))]))

;; Splits the string into a list of image liens and a carry string, which indicates
;; the string yet to be processed, which is longer than the desired maximum width.
(define/contract (split-at/line txt size clr max-width (carry "") (lines empty))
  (->* (string?
        (and/c integer? (between/c 1 255))
        image-color?
        (and/c integer? (between/c 1 255)))
       (string? list?)
       (values (listof image?) string?))
  (cond
    [(zero? (string-length txt)) (values (reverse lines) carry)]
    [(<= (image-width (text txt size clr)) max-width)
     (split-at/line carry size clr max-width "" (cons (text txt size clr) lines))]
    [else
     (define-values (word rst) (split-at/last-word txt))
     (split-at/line rst size clr max-width (cond
                                             [(zero? (string-length carry)) word]
                                             [else (string-append word " " carry)])
                    lines)]))

;; Splits the string into an image with a width less than the maximum
;; desired width (unless the longest word exceeds the desired maximum width, at
;; which point it increments the desired width until it can create an image whoes
;; width is as close to the desired max width as possible.) and the actual image
;; width.
(define/contract (text-wrap txt size clr max-width (alignment 'left))
  (->* (string?
        (and/c integer? (between/c 1 255))
        image-color?
        (and/c integer? (between/c 1 255)))
       (x-place?)
       (values image? (and/c integer? (between/c 1 255))))
  (define-values (lines carry) (split-at/line txt size clr max-width))
  (cond
    [(and (zero? (string-length carry))
          (= (length lines) 1))
     (values (first lines) (image-width (first lines)))]
    [(zero? (string-length carry))
     (define img (apply above/align alignment lines))
     (values img (image-width img))]
    [else (text-wrap txt size clr (add1 max-width) alignment)]))

;; Wraps the 
(define/contract (text-tok-wrap #:max-height (max-height #f)
                                txt font-size font-color max-width (align 'left))
  (->* (string?
        (and/c integer? (between/c 1 255))
        image-color?
        (and/c integer? (between/c 1 1600)))
       (#:max-height (and/c integer? (between/c 1 800))
        x-place?)
       (values image? cons?))
  (define toks (string-split txt))
  (define spaces (make-list (sub1 (length toks)) " "))
  (define strings (cons (car toks)
                        (flatten (for/list ([space spaces]
                                            [tok (rest toks)])
                                   (list space tok)))))

  (define images
    (map (λ (v) (text v font-size font-color)) strings))
  (define widths (map image-width images))
  (define mx (apply max widths))
  (when (> mx max-width)
    (error (format "text-wrap token width ~a exceeds ~a" mx max-width)))

  (define data
    (for/list ([string strings]
               [width widths]
               [n (in-naturals)])
      (list string width n)))

  (define (loop data (w 0) (tmp empty) (acc empty))  
    (cond
      [(empty? data) (reverse (if (empty? tmp)
                                  acc
                                  (cons (reverse tmp) acc)))]
      [else
       (define d (car data))
       (define width (second d))
       (define tot (+ w width))       
       (cond
         [(<= (+ w (second (car data))) max-width)          
          (loop (rest data)
                (+ w (second (car data)))
                (cons (car data) tmp)
                acc)]
         [else          
          (loop (rest data) (second (car data)) (list d)
                (cons (reverse tmp) acc))])]))

  (define ds (loop data))
  (define rows (for/list ([lst ds])               
                 (define cols (for/list ([d lst])
                                (list-ref images (third d))))                 
                 (if (= (length cols) 1)
                     (first cols)
                     (apply beside cols))))  
  (define ans (if (= (length rows) 1)
                  (first rows)
                  (apply above/align align rows)))
  (define f (if (false? max-height) 1 (/ max-height (image-height ans))))
  (define scaled-img (scale f ans))
  (values scaled-img (cons (image-width scaled-img) (image-height scaled-img))))
