#lang racket

;;;
;;; TABLE
;;;

(provide (struct-out Table)
         ->Table Table-get Table-put! Table-push! Table-pop! Table-member Table-copy
         undefined undefined?)

(require racket/undefined)

(module+ test (require rackunit
                       (submod "..")))

(define (undefined? v) (eq? v undefined))

(struct Table (size               ; maximum size of the table
               type?              ; contract for table element validation
               default            ; table initialization default value
               unique?            ; unique entries?
               start              ; tables start at 0 or 1
               (end #:mutable)    ; ending range of actual table usage
               (values #:mutable) ; 1-dimensional table values
               ) #:transparent)


(define/contract (Table-get table pos)
  (->i ((table Table?) (pos (table) (and natural? (between/c 0 (Table-size table))))) (result any/c))
  (vector-ref (Table-values table) pos))

(module+ test
  (test-case "Table-get tests"
             (check-equal? (Table-get (->Table 10) 9) undefined)))

(define/contract (Table-put! table pos val)
  (->i ((table Table?)
        (pos (table) (between/c (Table-start table) (sub1 (Table-size table))))
        (val (table) (or/c (λ (v) (equal? v (Table-default table))) (Table-type? table))))
       (result any/c))
  (unless (and (Table-unique? table)
               (vector-member val (vector-take (Table-values table)
                                               (Table-end table))))
    (vector-set! (Table-values table) pos val)
    (unless (< pos (Table-end table))
      (set-Table-end! table (add1 pos)))))

(module+ test
  (test-case "Table-put! tests"
             (define table (->Table 10 #:type? number?))
             (Table-put! table 0 10)
             (check-equal? (Table-get table 0) 10)
             (Table-put! table 0 undefined)
             (check-equal? (Table-get table 0) undefined)
             (Table-put! table 1 20)
             (check-equal? (Table-get table 1) 20)
             (check-equal? table (Table 10 number? undefined #f 0 2
                                        (vector undefined 20 undefined undefined undefined
                                                undefined undefined undefined undefined undefined)))))

(define/contract (Table-push! table val)
  (->i ((table Table?)
        (val (table) (or/c (λ (v) (equal? v (Table-default table))) (Table-type? table))))       
       ()
       (result any/c))
  (unless (< (Table-end table) (Table-size table))
    (error "Table is full."))
  (Table-put! table (Table-end table) val))

(module+ test
  (test-case "Table-push! tests"
             (define table (->Table 3))
             (Table-push! table 'a)
             (check-equal? table (Table 3 any/c undefined #f 0 1
                                        (vector 'a undefined undefined)))
             (Table-push! table 'b)
             (check-equal? table (Table 3 any/c undefined #f 0 2
                                        (vector 'a 'b undefined)))
             (Table-push! table 'c)
             (check-equal? table (Table 3 any/c undefined #f 0 3
                                        (vector 'a 'b 'c)))
             (define table2 (->Table 3 #:start 1))
             (Table-push! table2 'a)
             (check-equal? table2 (Table 3 any/c undefined #f 1 2
                                         (vector undefined 'a undefined)))
             (Table-push! table2 'b)
             (check-equal? table2 (Table 3 any/c undefined #f 1 3
                                         (vector undefined 'a 'b)))))

(define/contract (Table-pop! table)
  (->i ((table Table?))       
       ()
       (result any/c))  
  (when (zero? (Table-end table))
    (error "Table is empty."))
  (define end (sub1 (Table-end table)))
  (set-Table-end! table end)
  (Table-get table end))

(module+ test
  (test-case "Table-pop! tests"
             (define table (->Table 3))
             (Table-push! table 'foo)
             (Table-push! table 'bar)
             (Table-push! table 'baz)
             (check-equal? table
                           (Table 3 any/c undefined #f 0 3
                                  (vector 'foo 'bar 'baz)))
             (check-equal? (Table-pop! table) 'baz)
             (check-equal? (Table-pop! table) 'bar)
             (check-equal? (Table-pop! table) 'foo)
             (check-equal? table
                           (Table 3 any/c undefined #f 0 0
                                  (vector 'foo 'bar 'baz)))))

(define/contract (Table-member table val (default (Table-default table)))
  (->i ((table Table?)
        (val (table) (or/c (λ (v) (equal? v (Table-default table)))
                           (Table-type? table))))
       ((default any/c))
       (result any/c))
  (define ans (vector-member val (Table-values table)))
  (if (false? ans) default ans))

(module+ test
  (test-case "Table-member tests"
             (check-equal? (Table-member (->Table 3 #:values '(a b c)) 'c) 2)
             (check-equal? (Table-member (->Table 3 #:values '(a b c)) 'd) undefined)
             (check-equal? (Table-member (->Table 3 #:values '(a b c)) 'd #f) #f)))

(define (Table-copy table
                    #:type? (type? (Table-type? table))
                    #:defaolt (default (Table-default table))
                    #:unique? (unique? (Table-unique? table))
                    #:start (start (Table-start table))
                    #:values (values #f))
  (->Table (Table-size table)
           #:type? type?
           #:default default
           #:unique? unique?
           #:start start
           #:values values))

(module+ test
  (test-case "Table-copy tests"
             (check-equal? (Table-copy (->Table 3 #:values '(a b c)))
                           (Table 3 any/c undefined #f 0 0
                                  (vector undefined undefined undefined)))))

(define/contract (->Table size
                          #:type? (type? any/c)
                          #:default (default undefined)
                          #:unique? (unique? #f)
                          #:start (start 0)
                          #:values (values #f))
  (->i ((size natural?))
       (#:type? (type? contract?)
        #:default (default (type?)  (or/c undefined? type?))
        #:unique? (unique? boolean?)
        #:start (start (or/c zero? (λ (v) (and (number? v) (= v 1)))))
        #:values (values (type?) (or/c #f (λ (lst) (and (list? lst)
                                                        (andmap (if (unsupplied-arg? type?) any/c type?) lst))))))
       (result any/c))
  (define table (Table size type? default unique? start start (make-vector size default)))
  (unless (false? values)    
    (for ([val values]
          [n (in-naturals start)])
      (Table-put! table n val)))
  table)

(module+ test
  (test-case "->Table tests"
             (check-equal? (->Table 10 #:type? number? #:values '(1 2 3))
                           (Table 10 number? undefined #f 0 3
                                  (vector 1 2 3 undefined undefined undefined
                                          undefined undefined undefined undefined)))
             (check-equal? (->Table 10 #:unique? #t #:values '(a a a))
                           (Table 10 any/c undefined #t 0 1
                                  (vector 'a undefined undefined undefined undefined undefined
                                          undefined undefined undefined undefined)))))
