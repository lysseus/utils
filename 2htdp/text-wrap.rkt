#lang racket

(provide text-wrap)

(require 2htdp/image)

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
