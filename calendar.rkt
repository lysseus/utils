#lang racket


;;;
;;; CALENDAR
;;;

(provide gregorian->jdn
         jdn->gregorian
         jdn->day-of-week
         p-holidays
         set-holidays!
         weekend?
         cal-day?
         bus-day?
         add-cal-days
         add-bus-days
         last-day-of-month
         cal-days-between
         proc-days-between
         num-proc-days-in-month
         gregorian->day-of-year
         day-of-year->gregorian
         nth-proc-day-of-month)

(module+ test (require rackunit
                       (submod "..")))

;;  Return the Julian Day Number for this Gregorian date.
;; The Julian day is the continuous count of days since the beginning of the Julian period.
;; The Julian day number (JDN) is the integer assigned to a whole solar day in the Julian day
;; count starting from noon Universal time, with Julian day number 0 assigned to the day starting
;; at noon on Monday, January 1, 4713 BC, proleptic Julian calendar (November 24, 4714 BC, in the
;; proleptic Gregorian calendar) -Wikipedia: Julian Day Number
(define (gregorian->jdn year month day)
  (let ([Y year]
        [M month]
        [D day])
    ;; JDN = (1461 × (Y + 4800 + (M − 14)/12))/4
    ;;       + (367 × (M − 2 − 12 × ((M − 14)/12)))/12 − (3 × ((Y + 4900 + (M - 14)/12)/100))/4 + D − 32075
    (+ (quotient (* 1461 (+ Y 4800 (quotient (- M 14) 12))) 4)
   (quotient (* 367 (+ M -2 (* -12 (quotient (- M 14) 12)))) 12)
   (- (quotient (* 3 (quotient (+ Y 4900 (quotient (- M 14) 12)) 100)) 4))
   D
   -32075)))

(module+ test
  (test-case "gregorian->jdn tests"
             
             (check-equal? (gregorian->jdn 2021 2 28) 2459274) 
             (check-equal? (gregorian->jdn 2021 3 1) 2459275)))

;; Returns the day of the week as an number
;; value between 0 and 6 (Sunday=0, Monday=1, Tuesday=2, Wednesday=3, Thurday=4, Friday=5, Saturday=6)
(define/contract (jdn->gregorian jdn) (-> (>=/c 0) any)
  (let ([y 4716]
        [j 1401]
        [m 2]
        [n 12]
        [r 4]
        [p 1461]
        [v 3]
        [u 5]
        [s 153]
        [w 2]
        [B 274277]
        [C -38]
        [J jdn])
    ;; f = J + j + (((4 × J + B) div 146097) × 3) div 4 + C
    (define f (+ J j (quotient (* (quotient (+ (* 4 J) B) 146097) 3) 4) C))
    ;; e = r × f + v
    (define e (+ (* r f) v))
    ;; g = mod(e, p) div r
    (define g (quotient (modulo e p) r))
    ;; h = u × g + w
    (define h (+ (* u g) w))
    ;; D = (mod(h, s)) div u + 1
    (define D (add1 (quotient (modulo h s) u)))
    ;; M = mod(h div s + m, n) + 1
    (define M (add1 (modulo (+ (quotient h s) m) n)))
    ;; Y = (e div p) - y + (n + m - M) div n
    (define Y (+ (quotient e p) (- y) (quotient (+ n m (- M)) n)))
    (map inexact->exact (list Y M D))))

(module+ test
  (test-case "jdn->gregorian tests"
             (check-equal? (jdn->gregorian 2459274) '(2021 2 28)) 
             (check-equal? (jdn->gregorian 2459275) '(2021 3 1))
             (check-equal? (jdn->gregorian 2459519) '(2021 10 31))
             (check-equal? (jdn->gregorian 2459520) '(2021 11 1))))

;; Sun=0, Mon=1, ...
(define (jdn->day-of-week jdn) (modulo (add1 jdn) 7))

(module+ test
  (test-case "jdn->day-of-week tests"
             (check-equal? (jdn->day-of-week (gregorian->jdn 2021 2 28)) 0) 
             (check-equal? (jdn->day-of-week (gregorian->jdn 2021 10 23)) 6)))

(define p-holidays (make-parameter '()))

(define (set-holidays! lst) (p-holidays (map (λ (v) (apply gregorian->jdn v)) lst)))

(define (weekend? jdn)
  (zero? (modulo (jdn->day-of-week jdn) 6)))

(module+ test
  (test-case "weekend? tests"
             (check-true (weekend? (gregorian->jdn 2021 2 28))) 
             (check-false (weekend? (gregorian->jdn 2021 10 22)))))

(define (cal-day? year month day)
  (equal? (list year month day)
          (jdn->gregorian (gregorian->jdn year month day))))

(module+ test
  (test-case "cal-day? tests"
             (check-true (cal-day? 2021 2 28))
             (check-false (cal-day? 2021 2 29))))

(define (bus-day? jdn)
  (not (or (weekend? jdn)
           (member jdn (p-holidays)))))

(module+ test
  (parameterize ([p-holidays (list (gregorian->jdn 2021 1 1)
                                   (gregorian->jdn 2021 11 25)
                                   (gregorian->jdn 2021 12 24))])
    (test-case "bus-day? tests"
               (check-false (bus-day? (gregorian->jdn 2021 2 28)))
               (check-true (bus-day? (gregorian->jdn 2021 3 1))))))

(define (add-cal-days jdn n) (+ jdn n))

(module+ test
  (test-case "add-cal-days tests"
             (check-equal? (jdn->gregorian (add-cal-days (gregorian->jdn 2021 2 28) 10))
                           '(2021 3 10))
             (check-equal? (jdn->gregorian (add-cal-days (gregorian->jdn 2021 12 31) 10))
                           '(2022 1 10))
             (check-equal? (jdn->gregorian (add-cal-days (gregorian->jdn 2022 1 10) -10))
                           '(2021 12 31))))

(define (add-bus-days jdn n (c (abs n)))
  (define op (if (negative? n) sub1 add1))
  (cond
    [(zero? c) jdn]
    [(not (bus-day? (op jdn)))
     (add-bus-days (op jdn) n c)]
    [else (add-bus-days (op jdn) n (sub1 c))]))

(module+ test
  (parameterize ([p-holidays (list (gregorian->jdn 2021 1 1)
                                   (gregorian->jdn 2021 11 25)
                                   (gregorian->jdn 2021 12 24))])
    (test-case "add-bus-days tests"
               (check-equal? (jdn->gregorian (add-bus-days (gregorian->jdn 2021 10 23) 1))
                             '(2021 10 25))
               (check-equal? (jdn->gregorian (add-bus-days (gregorian->jdn 2021 10 23) 7))
                             '(2021 11 2))
               (check-equal? (jdn->gregorian (add-bus-days (gregorian->jdn 2021 11 23) 7))
                             '(2021 12 3))
               (check-equal? (jdn->gregorian (add-bus-days (gregorian->jdn 2021 12 3) -7))
                             '(2021 11 23)))))

(define (last-day-of-month year month) (jdn->gregorian (sub1 (gregorian->jdn year (add1 month) 1))))

(module+ test
  (test-case "last-day-of-month tests"
             (check-equal? (last-day-of-month 2021 2)
                           '(2021 2 28))
             (check-equal? (last-day-of-month 2021 12)
                           '(2021 12 31))
             (check-equal? (last-day-of-month 2021 4)
                           '(2021 4 30))
             (check-equal? (last-day-of-month 2020 2)
                           '(2020 2 29))
             (check-equal? (last-day-of-month 2000 2)
                           '(2000 2 29))))

(define (cal-days-between date1 date2)
  (inexact->exact (- (apply gregorian->jdn date2)
                     (apply gregorian->jdn date1))))

(module+ test
  (test-case "cal-days-betwen tests"
             (check-equal? (cal-days-between '(2021 10 1) '(2021 10 7)) 6)))

(define (proc-days-between date1 date2)
  (define jdn1 (apply gregorian->jdn date1))
  (define jdn2 (apply gregorian->jdn date2))
  (length (filter bus-day? (range jdn1 (add1 jdn2)))))

(module+ test
  (test-case "proc-days-betwen tests"
             (check-equal? (proc-days-between '(2021 10 1) '(2021 10 7)) 5)))

(define (num-proc-days-in-month year month)
  (define date1 (list year month 1))
  (define date2 (last-day-of-month year month))
  (proc-days-between date1 date2))

(module+ test
  (parameterize ([p-holidays (list (gregorian->jdn 2021 1 1)
                                   (gregorian->jdn 2021 11 25)
                                   (gregorian->jdn 2021 12 24))])
    (test-case "num-proc-days-in-month tests"
               (check-equal? (num-proc-days-in-month 2021 10) 21)
               (check-equal? (num-proc-days-in-month 2021 11) 21))))

(define (gregorian->day-of-year year month day)
  (define jdn1 (sub1 (gregorian->jdn year 1 1)))
  (define jdn2 (gregorian->jdn year month day))
  (- jdn2 jdn1))

(module+ test
  (test-case "gregorian->day-of-year tests"
             (check-equal? (gregorian->day-of-year 2021 1 1) 1)
             (check-equal? (gregorian->day-of-year 2021 10 24) 297)))

(define (day-of-year->gregorian year n)
  (define jdn (sub1 (gregorian->jdn year 1 1)))
  (jdn->gregorian (+ jdn n)))

(module+ test
  (test-case "day-of-year->gregorian tests"
             (check-equal? (day-of-year->gregorian 2021 1) '(2021 1 1))
             (check-equal? (day-of-year->gregorian 2021 297) '(2021 10 24))))

(define (nth-proc-day-of-month n year month (day 1))
  (define jdn1 (gregorian->jdn year month day))
  (define jdn2 (apply gregorian->jdn (last-day-of-month year month)))
  (define ans
    (for/fold ([acc empty])
                        ([jdn (range jdn1 (add1 jdn2))]
                         #:break (= (length acc) n))      
                (if (bus-day? jdn)
                    (cons jdn acc)
                    acc)))
  (if (< (length ans) n)
      #f
      (jdn->gregorian (first ans))))

(module+ test
  (test-case "nth-proc-day-of-month tests"
             (check-equal? (nth-proc-day-of-month 7 2021 10) '(2021 10 11))
             (check-equal? (nth-proc-day-of-month 7 2021 10 2) '(2021 10 12))
             (check-equal? (nth-proc-day-of-month 7 2021 10 4) '(2021 10 12))
             (check-false (nth-proc-day-of-month 7 2021 10 25))))