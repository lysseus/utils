#lang racket

;;;
;;; TIME-FORMATER
;;;

(provide ~t
         ticks->seconds
         seconds->hhmmss)

(module+ test
  (require rackunit
           (submod "..")))

;; Converts ticks to hhmmss time format.
;; ticks/second: ratio of ticks per second
;; sep: separator between hh mm and ss
;; trim-zero-groups?: if true, trims leading "00<sep>" from the time format
;; trim-leading-zero?: if true, trims the leading zero from the leading group
;;                     of the time format.
(define/contract (~t ticks
                     #:ticks/second (ticks/second 28)
                     #:sep (sep ":")
                     #:trim-zero-groups? (trim-zero-groups? #t)
                     #:trim-leading-zero? (trim-leading-zero? #t))
  (->* (natural?) (#:ticks/second (and/c real? positive?)
                   #:sep any/c
                   #:trim-zero-groups? boolean?
                   #:trim-leading-zero? boolean?)
       string?)
  (seconds->hhmmss (ticks->seconds ticks ticks/second)
                   #:sep sep
                   #:trim-zero-groups? trim-zero-groups?
                   #:trim-leading-zero? trim-leading-zero?))

(module+ test
  (test-case "~t ests"
             (check-equal? (~t 720) "25")
             (check-equal? (~t 720 #:ticks/second 1) "12:00")
             (check-equal? (~t (+ (* 23 60 60) (* 59 60) 59) #:ticks/second 1) "23:59:59")))

;; Converrts ticks and ricks/second to seconds.
(define/contract (ticks->seconds ticks (ticks/second 28))
  (->* (natural?) ((and/c real? positive?)) any)
  (quotient ticks ticks/second))

;; Conerts seconds to a string in hhmmss format. 
(define (seconds->hhmmss seconds
                         #:sep (sep ":")
                         #:trim-zero-groups? (trim-zero-groups? #t)
                         #:trim-leading-zero? (trim-leading-zero? #t))
  (define hhmmss
    (string-trim (apply string-append (map (λ (v) (string-append sep v))
                                           (reverse (for/list ([n (range 3)])
                                                      (~r (modulo (quotient seconds (expt 60 n)) 60)
                                                          #:min-width 2
                                                          #:pad-string "0")))))
                 ":"))
  hhmmss
  (define ans
    (if trim-zero-groups?
        (string-trim hhmmss (string-append "00" sep) #:right? #f #:repeat? #t)
        hhmmss))
  ans
  (if trim-leading-zero?
      (string-trim ans "0" #:right? #f)
      ans))

(module+ test
  (test-case "seconds->hhmmss tests"
             (define tick-rate 1)
             (define hours 0)
             (define minutes 23)
             (define seconds 17)
             (define ticks (* (+ (* hours 60 60) (* minutes 60) seconds) tick-rate))
             (check-equal? (seconds->hhmmss (ticks->seconds ticks tick-rate) #:sep ":")
                           "23:17")))