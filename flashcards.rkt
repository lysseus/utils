#lang racket

;; Implements a "flashcard" infrastructure.

(provide start)

(require 2htdp/universe
         2htdp/image
         (only-in "../utils/list.rkt"
                  random-from-list))


;;---------------------------------------------------------------
;; Global variables. 
;; These values are concerned with the game initialization, 
;; timing, and world state.
;;---------------------------------------------------------------


; The game name displayed in the window
(define GAME-NAME "Unknown")

; The top range for creating flashcards with values from 0 to m.
(define MAX 0)

; The arithmetic operator, set by start.
(define OP #f)

; The string representation of the arithmetic operator, set by start.
(define OP-STR "?")

; The proc for making a list of flashcards.
(define make-cards (λ (n) empty))

; There are approximately 24 clock ticks to a second.
(define TIME (* 24 10))

; The game world state. 
(struct world (solving?      ; are we waiting for a response?
               ans-correct?  ; was the answer correct?
               card          ; the current card (a numeric pair)
               ans           ; the player's answer so far
               time          ; the time remaining to solve or report results
               correct-cnt   ; the number of correct answers
               missed-cnt    ; the number of incorrect answers
               cards)        ; the list of cards remaining to solve
  #:transparent)             ; makes the world state visible for debugging


;;---------------------------------------------------------------
;; Graphic variables. 
;; These values are concerned with the game image.
;;---------------------------------------------------------------


(define WIDTH 600)
(define HEIGHT 400)
(define MT (empty-scene WIDTH HEIGHT 'gray))

; This font size is used for the flashcard body.
(define TEXT-SIZE 40)

; This font size is used for the flashcard header.
(define SMALL-TEXT 20)


;;---------------------------------------------------------------
;; Helper functions.
;; These functions are common to the handlers used by big-bang.
;;---------------------------------------------------------------


;; new-prolem: worldstate -> world-state
;; Returns a new world state that is an initial state when no previous
;; world state is passed as an argument, or one in which a new card
;; is popped from the cards list to be presented as the next flashcard.
(define (new-problem (ws #f))
  (define CARDS (if ws
                    (world-cards ws)
                    (make-cards MAX)))
  (define CARD (random-from-list CARDS))
  
  (world #t         ; solve-mode?
         #f         ; answer correct?
         CARD       ; the current card
         "?"        ; answer
         TIME       ; solution time
         (if ws     ; correct count 
             (world-correct-cnt ws)
             0) 
         (if ws     ; missed count
             (world-missed-cnt ws)
             0)
         (remove CARD CARDS)))

;; cards-empty?: world-state -> boolean?
;; Answers the question, are there any more cards left?
(define (cards-empty? ws)
  (empty? (world-cards ws)))


;;---------------------------------------------------------------
;; on-tick functions.
;; These functions are concerned with clock-ticks and
;; setting the world state to the appropriate value.
;;---------------------------------------------------------------


;; timer: world-state -> world-state?
;; Counts down the timer while waiting for player responses, or when 
;; giving feedback on an answer. 
(define (timer ws)
  (cond
    ; we're in solve-mode and time has run out
    [(and (world-solving? ws)
          (zero? (world-time ws))) 
     (world #f 
            #f
            (world-card ws)
            (world-ans ws) 
            (quotient TIME 2) 
            (world-correct-cnt ws) 
            (add1 (world-missed-cnt ws))
            (cons (world-card ws) (world-cards ws)))]
    ; We're in solve move, count down.
    [(world-solving? ws)
     (world #t
            #f
            (world-card ws)
            (world-ans ws) 
            (sub1 (world-time ws)) 
            (world-correct-cnt ws) 
            (world-missed-cnt ws)
            (world-cards ws))]
    ; We're announcing success/failure and time has run out
    [(zero? (world-time ws))
     (new-problem ws)]
    ; We're announcing success/failure
    [else (world (world-solving? ws)
                 (world-ans-correct? ws)
                 (world-card ws)
                 (world-ans ws) 
                 (sub1 (world-time ws)) 
                 (world-correct-cnt ws) 
                 (world-missed-cnt ws)
                 (world-cards ws))]))


;;---------------------------------------------------------------
;; on-key functions.
;; These functions are concerned with capturing key-strokes and 
;; setting the world state to the appropriate value.
;;---------------------------------------------------------------


;; chck-ans: world-state key-event -> world-state?
;; Captures number keys (not keypad keys) and determines if the card
;; has been correctly answered. Allows player to erase answers with the
;; backspace key.
(define (check-ans ws ke)
  ; Either a number or #f
  (define KEY-NUMERIC (string->number ke))
  ; Either a number or #f
  (define ANS-NUMERIC (cond
                        [(string? (world-ans ws)) #f]
                        [else (world-ans ws)]))
  (define ANS (cond
                [(and ANS-NUMERIC KEY-NUMERIC)
                 (string->number (string-append (number->string ANS-NUMERIC)
                                                (number->string KEY-NUMERIC)))]
                [KEY-NUMERIC KEY-NUMERIC]
                [(and (key=? ke "\b")
                      ANS-NUMERIC)
                 (define VAL (quotient ANS-NUMERIC 10))
                 (if (zero? VAL) "?" VAL)]
                [else (world-ans ws)]))
  (define CORRECT? (and (number? ANS)
                        (= (OP (first (world-card ws))
                               (second (world-card ws)))
                           ANS)))
  
  (if (world-solving? ws)
      (world (if CORRECT? #f #t)
             CORRECT?
             (world-card ws)
             ANS
             (if CORRECT? 
                 (quotient TIME 4)
                 (world-time ws)) 
             (if CORRECT? 
                 (add1 (world-correct-cnt ws))
                 (world-correct-cnt ws))
             (world-missed-cnt ws)
             (world-cards ws))
      ws))


;;---------------------------------------------------------------
;; to-draw functions.
;; These functions are concerned with drawing the game world. 
;;---------------------------------------------------------------


;; render: world-state -> image?
;; Converts the world state into a flashcard image.
(define (render ws)
  (define OP-IMG (text OP-STR TEXT-SIZE 'black))
  (define BLANK (rectangle (image-width OP-IMG)
                           (image-height OP-IMG)
                           'outline
                           'transparent))
  (define BLANK-ROW (rectangle WIDTH (image-height OP-IMG) 'outline 'transparent))
  (define EQ (text "=" TEXT-SIZE 'black))
  (define M (text (number->string (first (world-card ws))) TEXT-SIZE 'darkgreen))
  (define N (text (number->string (second (world-card ws))) TEXT-SIZE 'darkgreen))
  (define ANS (text (cond
                      [(and (world-solving? ws)
                            (number? (world-ans ws)))
                       (number->string (world-ans ws))]
                      [(world-solving? ws)
                       (world-ans ws)]
                      [else (number->string 
                             (OP (first (world-card ws))
                                 (second (world-card ws))))])
                    TEXT-SIZE
                    'darkblue))
  (define HEADER (beside
                  (text (cond [(cards-empty? ws)
                               "COMPLETED!!"]
                              [(world-solving? ws)
                               (string-append "Time: " 
                                              (number->string 
                                               (quotient (world-time ws)
                                                         28)))]
                              [(world-ans-correct? ws)
                               "CORRECT"]
                              [else "INCORRECT"])
                        SMALL-TEXT
                        'black)
                  BLANK
                  (text (string-append "Correct: "
                                       (number->string
                                        (world-correct-cnt ws)))
                        SMALL-TEXT
                        'darkblue)
                  BLANK
                  (text (string-append "Missed: "
                                       (number->string
                                        (world-missed-cnt ws)))
                        SMALL-TEXT
                        'red)
                  BLANK
                  (text (string-append "Remaining: "
                                       (number->string
                                        (length (world-cards ws))))
                        SMALL-TEXT
                        'black)))
  (define IMG (color-frame 'black (above
                                   BLANK-ROW
                                   BLANK-ROW
                                   BLANK-ROW
                                   BLANK-ROW
                                   (beside M BLANK OP-IMG BLANK N BLANK EQ BLANK ANS)
                                   BLANK-ROW
                                   BLANK-ROW
                                   BLANK-ROW)))
  (define CARD (overlay IMG 
                        (rectangle (image-width IMG)
                                   (image-height IMG)
                                   'solid
                                   'white)))
  (place-image (above HEADER CARD)
               (quotient (image-width MT) 2)
               (quotient (image-height MT) 2)
               MT))


;; star-game game-name op op-str max cards-proc -> world-state?
;; Initializes a new world-state and invokes big-bang.
(define (start #:game-name game-name 
               #:op op 
               #:op-str op-str 
               #:max max
               #:cards-proc cards-proc)
  (set! GAME-NAME game-name)
  (set! OP op)
  (set! OP-STR op-str)
  (set! MAX max)
  (set! make-cards cards-proc)
  
  (big-bang (new-problem)
            (on-tick timer)
            (on-key check-ans)
            (to-draw render)
            (stop-when cards-empty?)
            (name GAME-NAME))
  (void))
