#lang racket

;; This module implements a set of list utilities.

(provide
 zip
 ; Conversts the values returned from the expression (f args ...) to a list.
 values->list
 ; Conversts the list returned from the expression (f args ...) to values.
 list->values
 ; Returns the nth value of the expression (f args ...).
 values-ref
 list-set*
 ; Applies successive functions to successive levels of nesting in lst*
 apply/outer*
 (contract-out
  [struct seq-elm ((pos exact-nonnegative-integer?) (ref any/c))]
  ; Returns #t if value is an atom, #f otherwise.
  [atom? 
   (->i ([v any/c])
        ()
        (result boolean?))]
  ; Returns the maximum depth of the list.
  [list-max-depth 
   (->i ([lst list?])
        ()
        (result exact-nonnegative-integer?))]
  ; Returns the list of length len starting from pos elements of lst.
  [sublist 
   (->i ([lst list?]
         [pos (lst) (integer-in 0 (length lst))])
        ([len (lst pos) (integer-in 0 (- (length lst) pos))])
        (result list?))]
  ; Produces a new list grouping lst by n elements. 
  [group
   (->i ([lst list?]
         [n (lst) (and/c (integer-in 1 (length lst))
                         (λ (n) 
                           (zero? (modulo (length lst) n))))])
        ()
        (result list?))]
  ; Inserts val at position pos of list returning a new list.
  [insert-at
   (->i ([lst list?]
         [pos (lst) (integer-in 0 (length lst))]
         [val any/c])
        (#:splice [splice boolean?])
        (result list?))]
  ; Returns first position where ref equals a list element or #f if none exits.
  [list-pos
   (->i ([lst list?]
         [ref any/c])
        (#;()
         #:is-equal? [is-euqal? procedure?]
         #:pos [pos (lst) (integer-in 0 (length lst))]
         #:len (len (lst pos) (integer-in 0 (- (length lst) pos))))
        (result (or/c #f exact-nonnegative-integer?)))]
  ; Returns a list of positions where ref equals the assoicated list element.
  [list-pos*
   (->i ([lst list?]
         [ref any/c])
        (#;()
         #:is-equal? [is-euqal? procedure?]
         #:pos [pos (lst) (integer-in 0 (length lst))]
         #:len (len (lst pos) (integer-in 0 (- (length lst) pos))))
        (result list?))]
  ; Retruns an assoc list of positions where ref equals the associated list element.
  [list/ref->assc/pos
   (->i ([lst list?])
        ([refs (and/c pair? cons?)]
         #:is-equal? [is-euqal? procedure?]
         #:pos [pos (lst) (integer-in 0 (length lst))]
         #:len (len (lst pos) (integer-in 0 (- (length lst) pos))))
        (result list?))]
  ; Returns the first element of lst where proc returns a non-false value.
  [findf/elm
   (->i ([proc procedure?]
         [lst list?])
        (#;()
         #:pos [pos (lst) (integer-in 0 (length lst))]
         #:len (len (lst pos) (integer-in 0 (- (length lst) pos))))
        (result (or/c #f seq-elm?)))]
  ; Returns seq-elms of the list where proc returns a non-false value.
  [findf/elm*
   (->i ([proc procedure?]
         [lst list?])
        (#;()
         #:pos [pos (lst) (integer-in 0 (length lst))]
         #:len (len (lst pos) (integer-in 0 (- (length lst) pos))))
        (result (listof seq-elm?)))]
  ; Returns a random element from lst.
  [random-from-list
   (->i ([lst (and/c pair? list?)])
        ()
        (result any/c))]
  ; Returns a new list constructed by replacing lst pos value with val.
  [list-set
   (->i ([lst (and/c pair? list?)]
         [pos (lst) (integer-in 0 (sub1 (length lst)))]
         [val any/c])
        ()
        (result (and/c pair? list?)))]
  ; Returns a new list replacing one element of lst with val by filtering lst 
  ; with pred and then randomly selecting one of the filtered elements. 
  [set-choice-to-value
   (->i ([lst list?] 
         [pred procedure?]
         [val any/c])
        ()
        (values (r0 list?) (r1 boolean?)))]
  ; Just like set-choice-to-value, but able to iterate multiple times over lst.
  [set-choices-to-value
   (->i ([lst list?]
         [pred procedure?]
         [val any/c])
        (#:iter [iter exact-nonnegative-integer?])
        (values (r0 list?) (r1 boolean?)))]
  ; Builds a list of repeated elements in the order of their occurrence. 
  ; Each element in the new list is repeated n times. 
  [repeat-list-elements
   (->i ([n exact-nonnegative-integer?]
         [lst (and/c cons? list?)])
        ()
        (result list?))]
  ; Returns a list with the indices of lst for which pred produces a true value. 
  [filter/pos
   (->i ([pred procedure?]
         [lst list?])
        ()
        (values [r0 list?] [r1 list?]))]
  ; Returns a list with the indices of lst for which pred produces a true value. 
  [filter/elm
   (->i ([pred procedure?]
         [lst list?])
        ()
        (result (or/c null? (non-empty-listof seq-elm?))))]
  ; Applies successive functions to successive levels of nesting in lst*
  [apply*
   (->i ([fs (non-empty-listof procedure?)]
         [lst* (fs) (λ (l) 
                      (= (add1 (list-max-depth l)) (length fs)))])
        ()
        (r0 (lst*) (λ (r) 
                     (= (list-max-depth r) (list-max-depth lst*)))))]
  ; Randomly choose selection from a weighted list.
  [choose
   (->i ([selects (non-empty-listof any/c)])
        ([weights (non-empty-listof real?)]
         [rnd real?])
        (result any/c))]))

;; ---------------------------------
;; import and implementation section

(require (for-syntax racket/syntax
                     syntax/parse
                     racket/list)
         racket/function)

(module+ test (require rackunit
                       (for-syntax syntax/parse
                                   racket/list)
                       (submod "..")))

(struct seq-elm (pos ref) #:transparent)

;; values->list (f args) -> list?
;; conversts the values returned from the expression (f args ...) to a list.
(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ (f args ...))
     #'(call-with-values (thunk (f args ...)) list)]
    [(_ f args ...)
     #'(cond
         [(procedure? f) (values->list (f args ...))]
         [else (values->list (values f args ...))])]))

(module+ test
  (test-case "values->list tests"
             (check-equal? (values->list values) '())
             (check-equal? (values->list values 1 2 3) '(1 2 3))
             (check-equal? (values->list 1 2 3) '(1 2 3))
             (check-equal? (values->list (values 1 2 3)) '(1 2 3))
             (check-equal? (values->list '(values 1 2 3)) '((values 1 2 3)))
             (check-equal? (values->list '(values 1 2 3) '(+ 4 5 6)) 
                           '((values 1 2 3) (+ 4 5 6)))))

;; list->values (f args) -> values?
;; Conversts the values returned from the expression (f args ...) to a list.
(define-syntax (list->values stx)
  (syntax-case stx (quote)
    [(_ (quote arg) ...)
     #'(values (quote arg) ...)]
    [(_ (f args ...))
     #'(apply values (f args ...))]
    [(_ f args ...)
     #'(cond
         [(procedure? f) (list->values (f args ...))]
         [else (list->values (list f args ...))])]))

(module+ test
  (test-case "list->values tests"
             (check-equal? 
              (values->list (list->values list)) '())
             (check-equal? 
              (values->list (list->values list 1 2 3)) '(1 2 3))
             (check-equal? 
              (values->list (list->values 1 2 3)) '(1 2 3))
             (check-equal? 
              (values->list (list->values (list 1 2 3))) '(1 2 3))
             (check-equal? 
              (values->list (list->values '(list 1 2 3))) '((list 1 2 3)))
             (check-equal? 
              (values->list (list->values '(values 1 2 3) '(+ 4 5 6))) 
              '((values 1 2 3) (+ 4 5 6)))))

;; values-ref: n (f args ...) -> any/c
;;           : n f args ... -> any/c
;; Returns the nth value of the expression (f args ...).
(define-syntax (values-ref stx)
  (syntax-case stx ()
    [(_ n (f args ...)) #'(list-ref (values->list (f args ...)) n)]
    [(_ n f args ...)
     #'(cond
         [(procedure? f) (values-ref n (f args ...))]
         [else (values-ref n (values f args ...))])]))

(module+ test
  (test-case "values-ref tests"
             (check-equal? (values-ref 0 values 1 2 3) 1)
             (check-equal? (values-ref 0 1 2 3) 1)
             (check-equal? (values-ref 0 (values 1 2 3)) 1)
             (check-equal? (values-ref 0 '(values 1 2 3)) '(values 1 2 3))
             (check-equal? (values-ref 0 '(values 1 2 3) '(+ 4 5 6)) 
                           '(values 1 2 3))))

;; atom: value -> boolean?
;; Returns #t if value is an atom, #f otherwise.
(define (atom? v) (and (not (pair? v)) (not (null? v))))

(module+ test
  (test-case "atom? tests"
             ; the empty list is not an atom
             (check-false (atom? '()))
             
             ; a list is not an atom
             (check-false (atom? '(a b c)))
             
             ; a symbol is an atom
             (check-true (atom? 'a))
             
             ; a string is an atom
             (check-true (atom? "hello"))
             
             ; a struct is an atom
             (struct foo (a b c))
             (check-true (atom? (foo 1 2 3)))))

;; list-depth: lst -> exact-nonnegative-integer?
;; Returns the maximum depth of the list.
(define (list-max-depth lst)
  (define max 0)
  (let loop ([lst lst] [cnt 0])
    (cond
      [(empty? lst) (when (> cnt max) (set! max cnt)) (sub1 cnt)]
      [(atom? (first lst)) (loop (rest lst) cnt)]
      [else (loop (rest lst) (loop (first lst) (add1 cnt)))]))
  max)

(module+ test
  (test-case "list-max-depth tests"
             (check-equal? (list-max-depth empty) 0)
             (check-equal? (list-max-depth '(() () ())) 1)
             (check-equal? (list-max-depth '(() (((() (())))))) 5)))

;; sublist: list number number -> list
;; Returns the list of length len starting from pos elements of lst.
(define (sublist lst pos (len (- (length lst) pos))) (take (drop lst pos) len))

(module+ test
  (test-case "sublist tests"
             ;; sample list 
             (define LST '(a b c d e f g))
             
             ;; sublist from beginning
             (check-equal? (sublist LST 0 3) '(a b c))
             
             ;; sublist from middle
             (check-equal? (sublist LST 2 3) '(c d e))
             
             ;; sublist to end
             (check-equal? (sublist LST 7) '())
             
             ;; sublist beyond end should fail
             (check-exn exn:fail? 
                        (λ () (sublist LST 8)))))

;; group: lst integer -> list?
;; Produces a new list grouping lst by n elements. 
(define (group lst n)
  (let loop ([lst lst] [acc empty])
    (cond
      [(empty? lst) (reverse acc)]
      [(loop (drop lst n) (cons (take lst n) acc))])))

(module+ test
  (test-case "group tests"
             ;; sample list
             (define LST '(a b c d e f))
             
             ;; group by 1
             (check-equal? (group LST 1) '((a) (b) (c) (d) (e) (f)))
             
             ;; group by 2
             (check-equal? (group LST 2) '((a b) (c d) (e f)))
             
             ;; group by 3
             (check-equal? (group LST 3) '((a b c) (d e f)))
             
             ;; group by 0 should fail
             (check-exn exn:fail? (λ () (group LST 0)))
             
             ;; group by 4 should fail
             (check-exn exn:fail? (λ () (group LST 4)))))

;; insert-at: lst pos val [#:splice] -> list?
;; Inserts val at position pos of list returning a new list.
;;
;; If splice is true and val is a list then the elements of val
;; are spliced into list at position pos.
(define (insert-at lst pos val #:splice (splice #f))
  (define-values (head tail) (split-at lst pos))
  (append head
          (cond
            [(and splice (cons? val)) val]
            [else (list val)])
          tail))

(module+ test
  (test-case "insert-at tests"
             (check-equal? (insert-at '() 0 'a) '(a))
             (check-equal? (insert-at '(b) 0 'a) '(a b))
             (check-equal? (insert-at '(a b) 2 'c) '(a b c))))

;; list-pos: list ref -> (or/c #f exact-nonnegative-integer?)
;; Returns first position where ref equals a list element or #f if none exits.
(define (list-pos lst
                  ref
                  #:is-equal? (is-equal? equal?)
                  #:pos (pos 0)
                  #:len (len (- (length lst) pos)))
  (let loop ([lst (sublist lst pos len)] [pos pos])
    (cond
      [(empty? lst) #f]
      [(is-equal? (first lst) ref) pos]
      [else (loop (rest lst) (add1 pos))])))

(module+ test
  (test-case "list-pos tests"
             (check-equal? (list-pos '(a b c d c e f) 'z) #f)
             
             (check-equal? (list-pos '(a b c d c e f) 'c) 2)))

;; list-pos*: list ref -> list?
;; Returns a list of positions where ref equals the assoicated list element.
(define (list-pos* lst 
                   ref 
                   #:is-equal? (is-equal? equal?)
                   #:pos (pos 0) 
                   #:len (len (- (length lst) pos)))
  (let loop ([lst (sublist lst pos len)] [pos pos] [acc empty])
    (cond
      [(empty? lst) (reverse acc)]
      [(is-equal? (first lst) ref)
       (loop (rest lst) (add1 pos) (cons pos acc))]
      [else (loop (rest lst) (add1 pos) acc)])))

(module+ test
  (test-case "list-pos* tests"
             (check-equal? (list-pos* '(a b c d c e f) 'z)
                           '())
             
             (check-equal? (list-pos* '(a b c d c e f) 'c)
                           '(2 4))))

;; list/ref->assc/pos: lst refs -> assoc?
;; Retruns an assoc list of positions where ref equals the associated list element.
(define (list/ref->assc/pos lst 
                            (refs (remove-duplicates lst))
                            #:is-equal? (is-equal? equal?)
                            #:pos (pos 0) 
                            #:len (len (- (length lst) pos)))
  (let loop ([refs refs] [acc empty])
    (cond
      [(empty? refs) (reverse acc)]
      [else 
       (define VAL (list-pos* lst 
                              (first refs)
                              #:is-equal? is-equal?
                              #:pos pos
                              #:len len))
       (loop (rest refs) (if (empty? VAL)
                             acc
                             (cons (cons (first refs)
                                         VAL)
                                   acc)))])))

(module+ test
  (test-case "list/ref->assc/pos tests"
             (check-equal? 
              (list/ref->assc/pos '(a b c d c e f) '(x y z))
              '())
             
             (check-equal? (list/ref->assc/pos '(a b c d c e b f b g) 
                                               '(c z b))
                           '((c 2 4) (b 1 6 8)))))

;; findf/elm: lst proc [#:pos] [#:len] -> (or/c #f seq-elm?)
;; Returns the first element of lst where proc returns a non-false value; 
;; otherwise returns #f.
;; proc arguments may be value, or pos+value.
(define (findf/elm proc
                   lst
                   #:pos (pos 0)
                   #:len (len (- (length lst) pos)))
  (let loop ([lst (sublist lst pos len)] [pos pos])
    (define eol? (empty? lst))
    (define ref (if eol? #f (first lst)))
    (define val (if eol? 
                    #f 
                    (match (procedure-arity proc)
                      [1 (proc ref)]
                      [2 (proc pos ref)])))
    (cond
      [eol? ref]
      [val (seq-elm pos ref)]
      [else (loop (rest lst) (add1 pos))])))

(module+ test
  (test-case "findf/elm tests"
             (check-equal? (findf/elm odd? '(0 2 4 6 8 10)) #f)
             
             (check-equal? (findf/elm odd? '(0 2 4 5 6 8 9 10))
                           (seq-elm 3 5))))

;; findf/elm*: lst proc [#:pos] [#:len] -> (listof seq-elm?)
;; Returns seq-elms of the list where proc returns a non-false value; 
;; otherwise returns an empty list.
;; proc arguments may be value, or pos+value.
(define (findf/elm* proc
                    lst
                    #:pos (pos 0)
                    #:len (len (- (length lst) pos)))
  (let loop ([lst (sublist lst pos len)] [pos pos] [acc empty])
    (define eol? (empty? lst))
    (define ref (if eol? #f (first lst)))
    (define val (if eol? 
                    #f 
                    (match (procedure-arity proc)
                      [1 (proc ref)]
                      [2 (proc pos ref)])))
    (cond
      [eol? (reverse acc)]
      [val (loop (rest lst) (add1 pos) (cons (seq-elm pos ref) acc))]
      [else (loop (rest lst) (add1 pos) acc)])))

(module+ test
  (test-case "findf/elm* tests"
             (check-equal? (findf/elm* odd? '(0 2 4 6 8 10)) empty)
             
             (check-equal? (findf/elm* odd? '(0 2 4 5 6 8 9 10))
                           (list (seq-elm 3 5) (seq-elm 6 9)))))

;; filter/pos: pred lst -> list? ...
;; Returns a list with the indices of lst for which pred produces a true value. 
;; The pred prodcedure is applied to each element from first to last. The 
;; second value returned is a list with the values of lst for which pred 
;; produces a true value.
(define (filter/pos pred lst)
  (let loop ([lst lst] [n 0] [pos empty] [acc empty])
    (cond
      [(empty? lst) (values (reverse pos) (reverse acc))]
      [(pred (car lst)) 
       (loop (cdr lst) (add1 n) (cons n pos) (cons (car lst) acc))]
      [else (loop (cdr lst) (add1 n) pos acc)])))

(module+ test
  (test-case "filter/pos tests"
             (check-equal? (values->list 
                            (filter/pos odd? (rest (range 11))))
                           '((0 2 4 6 8)
                             (1 3 5 7 9)))))

;; filter/elm: pred lst -> (or/c null (non-empty-listof seq-elm?))
;; Returns a list with the indices of lst for which pred produces a true value. 
;; The pred prodcedure is applied to each element from first to last. The second 
;; value returned is a list of seq-elms of lst for which pred produces a true 
;; value.
(define (filter/elm pred lst)
  (let loop ([lst lst] [n 0] [acc empty])
    (cond
      [(empty? lst) (reverse acc)]
      [(pred (car lst)) 
       (loop (cdr lst) (add1 n) (cons (seq-elm n (car lst)) acc))]
      [else (loop (cdr lst) (add1 n) acc)])))

(module+ test
  (test-case "filter/elm tests"
             (check-equal? (filter/elm odd? (rest (range 11)))
                           (list (seq-elm 0 1) 
                                 (seq-elm 2 3) 
                                 (seq-elm 4 5)
                                 (seq-elm 6 7) 
                                 (seq-elm 8 9)))))

;; random-from-list: lst -> any/c
;; Returns a random element from lst.
(define (random-from-list lst)
  (list-ref lst (random (length lst))))

(module+ test
  (test-case "random-from-list tests"
             (check-equal? (random-from-list '(a)) 'a)))

;; list-set: lst pos val -> list?
;; Returns a new list constructed by replacing lst pos value with val.
(define (list-set lst pos val)
  (let loop ([lst lst] [pos pos] [acc '()])
    (cond
      [(null? lst) (reverse acc)]
      [(zero? pos) (loop (cdr lst) (sub1 pos) (cons val acc))]
      [else (loop (cdr lst) (sub1 pos) (cons (car lst) acc))])))

(module+ test
  (test-case "list-set"
             (check-equal? (list-set '(1 3 3 4) 1 2)
                           '(1 2 3 4))))

(define-syntax list-set*
  (syntax-rules ()
    [(list-set* lst (pos val)) (list-set lst pos val)]
    [(list-set* lst (p v) (ps vs) ...)
     (list-set* (list-set lst p v) (ps vs) ...)]))

(module+ test
  (test-case "list-set* tests"
             (check-equal? (list-set* '(a b c d e f)
                                      (0 'x) (2 'y) (4 'z))
                           '(x b y d z f))))


;; set-choice-to-value lst pred val -> list? boolean?
;; Returns a new list replacing one element of lst with val by filtering lst 
;; with pred and then randomly selecting one of the filtered elements. 
;; If pred does not return true then the original list is returned. The second 
;; return value indicates whether filtering was successful.
(define (set-choice-to-value lst pred val)
  (let ([choices (values-ref 0 (filter/pos pred lst))])
    (if (null? choices)
        (values lst #f)
        (values (list-set lst
                          (random-from-list choices)
                          val) #t))))

(module+ test
  (test-case "set-choice-to-value tests"
             (check-equal? (values->list 
                            (set-choice-to-value '(2 4 5 8 10)
                                                 odd?
                                                 6))
                           '((2 4 6 8 10) #t))))

;; set-choices-to-value: lst pred val iter ... -> list?
;; Just like set-choice-to-value, but able to iterate multiple times over lst.
(define (set-choices-to-value lst pred val #:iter (iter 1))
  (let loop ([lst lst] [pred pred] [val val] [iter iter] [flg #f])
    (cond
      [(zero? iter) (values lst flg)]
      [else (set!-values (lst flg) (set-choice-to-value lst pred val))
            (loop lst pred val (sub1 iter) flg)])))

(module+ test
  (test-case "set-choices-to-value"
             (check-equal? (values->list (set-choices-to-value '(0 1 2 3 4 5)
                                                               odd?
                                                               2
                                                               #:iter 6))
                           '((0 2 2 2 4 2) #f))
             (check-equal? (values->list (set-choices-to-value '(0 1 2 3 4 5)
                                                               even?
                                                               9
                                                               #:iter 3))
                           '((9 1 9 3 9 5) #t))))

;; repeat-list-elements: n list -> list?
;; Builds a list of repeated elements in the order of their occurrence. 
;; Each element in the new list is repeated n times. 
(define (repeat-list-elements n lst)
  (let loop ([lst lst] [acc empty])
    (cond
      [(null? lst) (reverse acc)]
      [else (loop (cdr lst) (append (make-list n (car lst)) acc))])))

(module+ test
  (test-case "repeat-list-elements tests"
             (define LST '(a b c d))
             (define LST2 '(a a b b c c d d))
             (check-equal? (repeat-list-elements 0 LST) null)
             (check-equal? (repeat-list-elements 1 LST) LST)
             (check-equal? (repeat-list-elements 2 LST) LST2)))

;; apply*: fs lst*
;; Applies successive functions to successive levels of nesting in lst*
(define (apply* fs lst*)
  (cond [(empty? fs) lst*]
        [(not (list? lst*)) lst*]
        [else
         (define f (first fs))
         (define rst (rest fs))
         (apply f
                (for/list ([lst (in-list lst*)])
                  (apply* rst lst)))]))

(module+ test
  (test-case "apply*"
             (define (f1 . v) (cons 'f1 v))
             (define (f2 . v) (cons 'f2 v))
             (define (f3 . v) (cons 'f3 v))
             (define (f4 . v) (cons 'f4 v))
             
             (check-equal? (apply* (list f1 f2 f3)
                                   '((1 2 3 4) (5 (6 7) 8)))
                           '(f1 (f2 1 2 3 4) (f2 5 (f3 6 7) 8)))))


;; apply/outer*: fs xs -> list
;; Applies successive functions to successive levels of nesting in lst*
(define-syntax (apply/outer* stx)
  (define-syntax-class (thing fs)
    #:attributes (norm)
    [pattern x #:when (empty? fs)
             #:with norm #'x]
    [pattern (x ...)
             #:declare x (thing (rest fs))
             #:with f (first fs)
             #:with norm #'(f x.norm ...)]
    [pattern x #:with norm #'x])
  
  (syntax-parse stx
    [(_ [f ...] . x)
     #:declare x (thing (syntax->list #'(f ...)))
     #'x.norm]))

(module+ test
  (test-case "apply/outer* tests"
             (define (f1 . v) (cons 'f1 v))
             (define (f2 . v) (cons 'f2 v))
             (define (f3 . v) (cons 'f3 v))
             (define (f4 . v) (cons 'f4 v))
             
             (check-equal? (apply/outer* (f1 f2 f3 f4)
                                         ((1 2 3 4) (5 (6 7) 8)))
                           '(f1 (f2 (f3 1 2 3 4) (f3 5 (f4 6 7) 8))))))

;; choose: selects weights rnd -> any/c?
;; Randomly chooses a selection based on weights.
(define (choose selects
                (weights (make-list (length selects) (/ 1 (length selects))))
                (rnd (random)))
  (define vec (list->vector
               (reverse
                (for/list ([n (range (length weights))])
                  (for/sum ([m (drop-right weights n)]) m)))))
  (list-ref selects
            (vector-member (for/first ([v vec] #:unless (> rnd v)) v) vec)))

(module+ test
  (test-case "choose tests"
             (check-equal? (choose '(a b c d e) '(1/5 1/5 1/5 1/5 1/5) 0.5) 'c)))

(define/contract (zip #:length (len max) . lsts)
  (->* () (#:length (or/c procedure? natural?)) #:rest (listof list?) (listof list?))
  (define (loop cnt (acc empty))
    (cond
      [(zero? cnt) acc]
      [else
       (define n (sub1 cnt))
       (loop n (cons (map (λ (lst) (list-ref lst (modulo n (length lst)))) lsts) acc))]))
  (loop (cond
          [(procedure? len)
           (apply len (map length lsts))]
            [else len])))