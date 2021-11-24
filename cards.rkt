#lang racket

;;;
;;; CARDS
;;; A libarary for producing card structs and card images for a 52 card standard playing deck.
;;;

(provide (struct-out card) SPADE HEART DIAMOND CLUB cards card-images card->image cards->images
         2-of-kind 3-of-kind 4-of-kind full-house)

(require 2htdp/image)

(struct card (rank suit sym uc) #:transparent)

(define SPADE-UC "\U2660")
(define HEART-UC "\U2665")
(define DIAMOND-UC "\U2666")
(define CLUB-UC "\U2667")

(define SPADE (string->symbol SPADE-UC))
(define HEART (string->symbol HEART-UC))
(define DIAMOND (string->symbol DIAMOND-UC))
(define CLUB (string->symbol CLUB-UC))

(define RANKS '(A 2 3 4 5 6 7 8 9 10 J Q K))
(define OTHER-CARDS '(B F))
(define SUITS '(S H D C))

(define (suit? v) (member v SUITS))
(define (rank? v) (member v RANKS))
(define (other-card? v) (member v OTHER-CARDS))

(struct image-info (rank suit uc fcolor fsize x y w h) #:transparent)

(define IMAGE-INFO
  (list
   (image-info 'B #f "\U1F0A0" 'blue  255 12 0 155 206)
   (image-info 'F #f "\U1F0CF" 'black 180 12 0 155 206)
   
   (image-info 'A 'S "\U1F0A1" 'black 255 12 0 155 206)
   (image-info  2 'S "\U1F0A2" 'black 255 12 0 155 206)
   (image-info  3 'S "\U1F0A3" 'black 255 12 0 155 206)
   (image-info  4 'S "\U1F0A4" 'black 255 12 0 155 206)
   (image-info  5 'S "\U1F0A5" 'black 255 12 0 155 206)
   (image-info  6 'S "\U1F0A6" 'black 255 12 0 155 206)
   (image-info  7 'S "\U1F0A7" 'black 255 12 0 155 206)
   (image-info  8 'S "\U1F0A8" 'black 255 12 0 155 206)
   (image-info  9 'S "\U1F0A9" 'black 255 12 0 155 206)
   (image-info 10 'S "\U1F0AA" 'black 255 12 0 155 206)
   (image-info 'J 'S "\U1F0AB" 'black 255 12 0 155 206)
   (image-info 'Q 'S "\U1F0AD" 'black 255 12 0 155 206)
   (image-info 'K 'S "\U1F0AE" 'black 255 12 0 155 206)

   (image-info 'A 'H "\U1F0B1" 'red   255 12 0 155 206)
   (image-info  2 'H "\U1F0B2" 'red   255 12 0 155 206)
   (image-info  3 'H "\U1F0B3" 'red   255 12 0 155 206)
   (image-info  4 'H "\U1F0B4" 'red   255 12 0 155 206)
   (image-info  5 'H "\U1F0B5" 'red   255 12 0 155 206)
   (image-info  6 'H "\U1F0B6" 'red   255 12 0 155 206)
   (image-info  7 'H "\U1F0B7" 'red   255 12 0 155 206)
   (image-info  8 'H "\U1F0B8" 'red   255 12 0 155 206)
   (image-info  9 'H "\U1F0B9" 'red   255 12 0 155 206)
   (image-info 10 'H "\U1F0BA" 'red   255 12 0 155 206)
   (image-info 'J 'H "\U1F0BB" 'red   255 12 0 155 206)
   (image-info 'Q 'H "\U1F0BD" 'red   255 12 0 155 206)
   (image-info 'K 'H "\U1F0BE" 'red   255 12 0 155 206)

   (image-info 'A 'D "\U1F0C1" 'red   255 12 0 155 206)
   (image-info  2 'D "\U1F0C2" 'red   255 12 0 155 206)
   (image-info  3 'D "\U1F0C3" 'red   255 12 0 155 206)
   (image-info  4 'D "\U1F0C4" 'red   255 12 0 155 206)
   (image-info  5 'D "\U1F0C5" 'red   255 12 0 155 206)
   (image-info  6 'D "\U1F0C6" 'red   255 12 0 155 206)
   (image-info  7 'D "\U1F0C7" 'red   255 12 0 155 206)
   (image-info  8 'D "\U1F0C8" 'red   255 12 0 155 206)
   (image-info  9 'D "\U1F0C9" 'red   255 12 0 155 206)
   (image-info 10 'D "\U1F0CA" 'red   255 12 0 155 206)
   (image-info 'J 'D "\U1F0CB" 'red   255 12 0 155 206)
   (image-info 'Q 'D "\U1F0CD" 'red   255 12 0 155 206)
   (image-info 'K 'D "\U1F0CE" 'red   255 12 0 155 206)

   (image-info 'A 'C "\U1F0D1" 'black 255 12 0 155 206)
   (image-info  2 'C "\U1F0D2" 'black 255 12 0 155 206)
   (image-info  3 'C "\U1F0D3" 'black 255 12 0 155 206)
   (image-info  4 'C "\U1F0D4" 'black 255 12 0 155 206)
   (image-info  5 'C "\U1F0D5" 'black 255 12 0 155 206)
   (image-info  6 'C "\U1F0D6" 'black 255 12 0 155 206)
   (image-info  7 'C "\U1F0D7" 'black 255 12 0 155 206)
   (image-info  8 'C "\U1F0D8" 'black 255 12 0 155 206)
   (image-info  9 'C "\U1F0D9" 'black 255 12 0 155 206)
   (image-info 10 'C "\U1F0DA" 'black 255 12 0 155 206)
   (image-info 'J 'C "\U1F0DB" 'black 255 12 0 155 206)
   (image-info 'Q 'C "\U1F0DD" 'black 255 12 0 155 206)
   (image-info 'K 'C "\U1F0DE" 'black 255 12 0 155 206)))

(define-values (CARDS CARD-IMAGES)
  (for/fold ([cards (hash)]
             [images (hash)])
          ([info IMAGE-INFO])
  (define rank (image-info-rank info))
  (define suit (image-info-suit info))
  (define uc (image-info-uc info))
  (define txt (text uc (image-info-fsize info) (image-info-fcolor info)))
  (define img (crop (image-info-x info) (image-info-y info) (image-info-w info) (image-info-h info)
                    (overlay txt (rectangle (image-width txt) (image-height txt) 'solid 'white))))
  (define sym (string->symbol (string-append (~a rank)
                                             (case suit
                                               [(S) SPADE-UC]
                                               [(H) HEART-UC]
                                               [(D) DIAMOND-UC]
                                               [(C) CLUB-UC]
                                               [else ""]))))
  (define c (card rank suit sym (string->symbol uc)))
  (values (hash-set cards (cons rank suit) c)
          (hash-set images (cons rank suit) img))))

(define (_hash-ref hsh (suit #f) . ranks)    
  (cond    
    [(and (member suit SUITS) (empty? ranks))
     (for/list ([rank RANKS])
       (hash-ref hsh (cons rank suit)))]
    [(member suit SUITS)
     (for/list ([rank ranks])
       (hash-ref hsh (cons rank suit)))]
    [(and (false? suit) (empty? ranks))
     (append (for/list ([rank OTHER-CARDS])
               (hash-ref hsh (cons rank #f)))
             (for*/list ([suit SUITS]
                         [rank RANKS])
               (hash-ref hsh (cons rank suit))))]
    [(false? suit)
     (for/fold ([acc empty])
               ([rank ranks])
       (if (member rank RANKS)
           (values (append acc (for/list ([suit SUITS])
                                 (hash-ref hsh (cons rank suit)))))
           (values (append acc (list (hash-ref hsh (cons rank #f)))))))
     #;(for*/list ([suit SUITS]
                   [rank ranks])
         (if (member rank RANKS)
             (hash-ref hsh (list rank suit))
             (hash-ref hsh (list rank #f))))]
    [(and suit (empty? ranks))
     (for*/list ([suit SUITS]
                 [rank RANKS])
       (hash-ref hsh (cons rank suit)))]
    [else
     (for*/list ([suit SUITS]
                 [rank ranks])
       (hash-ref hsh (cons rank suit)))]))

(define/contract (cards (suit #t) . ranks)
  (->* ()
       ((or/c boolean? suit?))
       #:rest (or/c empty? (listof (or/c rank? other-card?))) any)
  
  (apply _hash-ref CARDS suit ranks))

(define/contract (card-images #:scale (sval #f)
                              (suit #t) . ranks)
  (->* ()
       ((or/c boolean? suit?) #:scale (or/c #f (<=/c 1)))
       #:rest (or/c empty? (listof (or/c rank? other-card?))) any)  
  (define ans (apply _hash-ref CARD-IMAGES suit ranks))
  (if (false? sval)
      ans
      (map (λ (c) (scale sval c)) ans)))

#;(define/contract (2-cards cards (v #f))
  (->* ((listof card?)) ((or/c boolean? rank?)) any)
  (for*/fold ([acc empty])
             ([c1 cards]
              [c2 (remove c1 cards)])
    (cond
      [(false? v) (cons (list c1 c2) acc)]
      [(and (member v RANKS) (equal? (card-rank c1) v) (equal? (card-rank c2) v))
       (cons (list c1 c2) acc)]
      [(and (eq? v #t) (equal? (card-rank c1) (card-rank c2)))
       (cons (list c1 c2) acc)]
      [else acc])
    #;(values (if (equal? (card-rank c1) (card-rank c2))
                (cons (list c1 c2) acc)
                acc))))
(define/contract (2-of-kind cards)
  (-> (listof card?) any)
  (for*/fold ([acc (set)])
             ([c1 cards]
              [c2 (remove c1 cards)])
    (if (equal? (card-rank c1) (card-rank c2))
        (set-add acc (set c1 c2))
        acc)))
(define/contract (3-of-kind cards)
  (-> (listof card?) any)
  (for*/fold ([acc (set)])
             ([c1 cards]
              [c2 (remove c1 cards)]
              [c3 (remove* (list c1 c2) cards)])
    (if (and (equal? (card-rank c1) (card-rank c2))
             (equal? (card-rank c1) (card-rank c3)))
        (set-add acc (set c1 c2 c3))
        acc)))

(define/contract (4-of-kind cards)
  (-> (listof card?) any)
  (for*/fold ([acc (set)])
             ([c1 cards]
              [c2 (remove c1 cards)]
              [c3 (remove* (list c1 c2) cards)]
              [c4 (remove* (list c1 c2 c3) cards)])
    (if (and (equal? (card-rank c1) (card-rank c2))
             (equal? (card-rank c1) (card-rank c3))
             (equal? (card-rank c1) (card-rank c4)))
        (set-add acc (set c1 c2 c3))
        acc)))

(define/contract (full-house cs)
  (-> (listof card?) any)
  (for*/fold ([fh (set)])
             ([kkk (set->list (3-of-kind cs))]
              [kk (2-of-kind (remove* (cards #t (card-rank (first (set->list kkk)))) cs))])
    (set-add fh (list->set (append (set->list kkk) (set->list kk))))))

(define (card->image c)
  (hash-ref CARD-IMAGES (cons (card-rank c) (card-suit c))))

(define (cards->images #:scale (scl 1) v)
  (cond
    [(empty? v) '()]
    [(list? v) (cons (cards->images #:scale scl (first v)) (cards->images #:scale scl (rest v)))]
    [(card? v) (scale scl (card->image v))]
    [(set? v) (cards->images #:scale scl (set->list v))]))
