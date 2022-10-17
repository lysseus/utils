#lang racket

;; This module implements a matrix datatype

(provide
 ; mtx-elm structure
 (struct-out mtx-elm)
 (contract-out
  ; create a matrix
  [matrix
   (->i ()
        ([lst list?]
         #:rows (rows (or/c #f exact-nonnegative-integer?))
         #:cols (cols (or/c #f exact-nonnegative-integer?))
         #:order (order 
                  (rows cols) 
                  (or/c #f (and/c exact-nonnegative-integer?
                                  (λ (o)
                                    (not (or (integer? rows)
                                             (integer? cols)))))))
         #:fill-value (fill-value any/c))
        (result list?))]
  ; convert matrix to list  
  [matrix->list
   (->i ([mtx list?])
        ()
        (result list?))]
  ; return number of rows in matrix  
  [matrix-rows
   (->i ([mtx list?])
        ()
        (result exact-nonnegative-integer?))]
  ; return nth row of matrix
  [matrix-row
   (->i ([mtx (and/c pair? list?)]
         [n (mtx) (integer-in 0 (sub1 (matrix-rows mtx)))])
        ()
        (result any/c))]
  ; return number of columns in matrix
  [matrix-cols
   (->i ([mtx list?])
        ()
        (result exact-nonnegative-integer?))]
  ; return nth column of matrix
  [matrix-col
   (->i ([mtx (and/c pair? list?)]
         [n (mtx) (integer-in 0 (sub1 (matrix-cols mtx)))])
        ()
        (result list?))]
  ; return the size of the matrix
  [matrix-size
   (->i ([mtx list?])
        ()
        (result pair?))]
  ; returns the length of matrix based on row/column accessor
  [matrix-length
   (->i ([mtx list?])
        (#:accessor (accessor procedure?))
        (result exact-nonnegative-integer?))]
  ; returns the element associated with the matrix's row and column.
  [matrix-ref
   (->i ([mtx (and/c pair? list?)]
         [row (mtx) (integer-in 0 (sub1 (matrix-rows mtx)))]
         [col (mtx) (integer-in 0 (sub1 (matrix-cols mtx)))])
        ()
        (result any/c))]
  ; convert matrix column/row to list position
  [matrix-pos->list-pos
   (->i ([row exact-nonnegative-integer?]
         [col exact-nonnegative-integer?])
        (#:matrix [mtx (or/c #f (and/c pair? list?))]
         #:cols-per-row [cols-per-row exact-nonnegative-integer?])
        (result exact-nonnegative-integer?))]
  ; convert list position to matrix column/row
  [list-pos->matrix-pos
   (->i ([pos exact-nonnegative-integer?])
        (#:matrix [mtx (or/c #f (and/c pair? list?))]
         #:cols-per-row [cols-per-row exact-nonnegative-integer?])
        (results list?))]
  ; return a new matrix setting position to value
  [matrix-set
   (->i ([mtx (and/c pair? list?)]
         [row (mtx) (integer-in 0 (sub1 (matrix-rows mtx)))]
         [col (mtx) (integer-in 0 (sub1 (matrix-cols mtx)))]
         [val any/c])
        ()
        (result (and/c pair? list?)))]
  ; return a new matrix mapping (proc v) to each matrix element
  [matrix-map
   (->i ([proc procedure?]
         [mtx (and/c pair? list?)])
        (#;()
         #:accessor [accessor procedure?]
         #:inner-traversal? [inner-traversal? boolean?])
        (result (and/c pair? list?)))]
  ; rotate the matrix clockwise / counter-clockwise
  [matrix-rotate
   (->i ([mtx list?])
        ([rotations integer?])
        (result list?))]
  ; transpose the matrix (rows become columns, columns become rows)
  [matrix-transpose
   (->i ([mtx list?])
        ()
        (result list?))]
  ; return a new matrix, mapping mtx1 to mtx2
  [matrix->matrix
   (->i ([mtx1 list?]
         [mtx2 list?])
        ()
        (result list?))]
  ; find an element in the matrix based on row/column accessor
  [matrix-accessor-find
   (->i ([pred procedure?]
         [mtx list?])
        (#:accessor [accessor procedure?]
         #:values [values symbol?])
        any)]  
  ; return a matrix element or filtered list of matrix elements for pred
  [matrix-findf
   (->i ([pred procedure?]
         [mtx list?])
        (#;()
         #:accessor [accessor procedure?]
         #:inner-traversal? [inner-traversal boolean?]
         #:acc* [acc* boolean?])
        (result (or/c #f mtx-elm? (listof mtx-elm?))))]
  ; return a filtered list of matrix elements for pred
  [matrix-findf*
   (->i ([pred procedure?]
         [mtx list?])
        (#;()
         #:accessor [accessor procedure?]
         #:inner-traversal? [inner-traversal boolean?])
        (result (listof mtx-elm?)))]
  ; Returns true if matrices are equal, otherwise false.
  [matrix-equal?
   (->i ([m1 list?]
         [m2 list?])
        ()
        (result boolean?))]
  ; Returns true if matrix is square.
  [matrix-square?
   (->i ([mtx list?])
        ()
        (result boolean?))]
  ; Returns true if matrix is symmetric.
  [matrix-symmetric?
   (->i ([mtx list?])
        ()
        (result boolean?))]
  ;; Returns a submatrix for mtx analogous to sublist. 
  [submatrix
   (->i ([mtx list?])
        (#:row-pos [row-pos (mtx) (integer-in 0 (sub1 (matrix-rows mtx)))]
         #:row-len [row-len (mtx row-pos) (integer-in 0 (- (matrix-rows mtx) row-pos))]
         #:col-pos [col-pos (mtx) (integer-in 0 (sub1 (matrix-cols mtx)))]
         #:col-len [col-len (mtx col-pos)
                            (integer-in 0 (- (matrix-cols mtx) col-pos))])
        (result list?))]))

;; ---------------------------------
;; import and implementation section

(require (only-in (submod utils/list alt) 
                  values->list
                  list->values)
         (only-in utils/list
                  sublist
                  group
                  list-set))

(module+ test (require rackunit
                       (submod "..")))

(struct mtx-elm (pos row col val) #:transparent)

;; matrix: list number number numer any -> matrix?
;; produces a square matrix of specified order or a rectangular
;; matrix of specified rows x cols or if unspecified produces a 
;; matrix of rows and cols as close to a square matrix as possible,
;; constrained by the length of the list, and filling in missing 
;; elements with fill-value.
(define (matrix (lst null)
                #:rows (rows #f)
                #:cols (cols #f)
                #:order (order #f)                
                #:fill-value (fill-value 0))
  (let* ([len (length lst)]
         [rows (cond
                 ; any rows specified?
                 [rows rows]
                 ; do we want a square matrix?
                 [order order]
                 ; empty list - did we specify cols?
                 [(zero? len) (if (false? cols) 0 1)]
                 ; non-empty list, unspecified cols - try to make it square
                 [(false? cols) (inexact->exact (floor (sqrt len)))]
                 ; non-empty list, cols specified - calculate rows
                 [else (if (zero? cols) 0 (inexact->exact (ceiling (/ len cols))))])]
         [cols (cond
                 ; any cols specified?
                 [cols cols]
                 ; do we want a suae matrix?
                 [order order]
                 ; empty list - did we specify rows?
                 [(zero? len) (if (false? rows) 0 1)]
                 ; non-empty list - try to make it square
                 [else (if (zero? rows) 0 (inexact->exact (ceiling (/ len rows))))])]       
         [size (* rows cols)]
         [lst
          (cond
            [(= len size) lst]
            [(> len size) (sublist lst 0 size)]
            [else (append lst (make-list (- size len) fill-value))])])
    (if (zero? size)
        lst
        (group lst cols))))

(module+ test
  (test-case "matrix building tests"
             ;; sample list
             (define LST '(a b c d e f g h i j k l m n o p))
             
             ;; build 3 x 3 empty matrix
             (check-equal? (matrix #:rows 2 #:cols 2)
                           '((0 0) (0 0)))
             
             ;; build 4 x 4 matrix
             (check-equal? (matrix LST) 
                           '((a b c d) 
                             (e f g h) 
                             (i j k l) 
                             (m n o p)))
             
             ;; build 4 x 4 matrix with :order
             (check-equal? (matrix LST #:order 4)
                           '((a b c d) 
                             (e f g h) 
                             (i j k l) 
                             (m n o p)))
             
             ;; build 2 x 8 matrix
             (check-equal? (matrix LST #:rows 2 #:cols 8)
                           '((a b c d e f g h) 
                             (i j k l m n o p)))))

;; matrix->list: matrix -> list?
;; Converts the matrix into a list of elements. We don't use the standard
;; Racket flatten proc because the elements themselves may be lists. So we
;; simply flatten the list one level down.
(define (matrix->list mtx)
  (let loop ([mtx mtx] [acc empty])
    (cond
      [(null? mtx) acc]
      [(loop (cdr mtx) (append acc (car mtx)))])))

(module+ test
  (test-case "matrix->list tests"
             (define LST0 '(a b c d))
             (define LST1 '(a b c d e f g h i))
             (define LST2 
               '((0 0 a) (0 1 b) (1 0 c) (1 1 d) (2 0 e) (2 1 f)))
             
             (define M1x4 (matrix LST0
                                  #:rows 1))
             (define M4x1 (matrix LST0
                                  #:cols 1))
             (define M3x3 (matrix LST1
                                  #:order 3))
             (define M2x3L (matrix LST2
                                   #:rows 2 #:cols 3))
             
             ; flattening a 1 row matrix
             (check-equal? (matrix->list M1x4) LST0)
             
             ; flattening a 1 col matrix
             (check-equal? (matrix->list M4x1) LST0)
             
             ; flattening a square matrix
             (check-equal? (matrix->list M3x3) LST1)
             
             ; flattening a matrix of lists
             (check-equal? (matrix->list M2x3L) LST2)))

;; matrix-rows: matrix -> number
;; returns the number of rows in the matrix
(define (matrix-rows mtx)
  (length mtx))

(module+ test
  (test-case "matrix-rows tests"
             (define M0 (matrix '(a b c d e f) #:rows 3))
             (check-equal? (matrix-rows M0) 3)))

;; matrix-row: matrix number -> list
;; returns the nth row of the matrix
(define (matrix-row mtx n)
  (car (list-tail mtx n)))

(module+ test
  (test-case "matrix-row tests"
             (define M0 (matrix '(a b c d e f) #:rows 3))
             (check-equal? (matrix-row M0 0) '(a b))
             (check-equal? (matrix-row M0 1) '(c d))
             (check-equal? (matrix-row M0 2) '(e f))))

;; matrix-cols: matrix -> number
;; returns the number of cols in the matrix
(define (matrix-cols mtx)
  (define rows (matrix-rows mtx))
  (if (zero? rows) 0 (length (matrix-row mtx 0))))

(module+ test
  (test-case "matrix-cols tests"
             (define M0 (matrix '(a b c d e f) #:rows 3))
             (check-equal? (matrix-cols M0) 2)))

;; matrix-col: matrix number -> list
;; returns the nth column of the matrix
(define (matrix-col mtx n)
  (let loop ([mtx mtx] [acc empty])
    (cond
      [(null? mtx) (reverse acc)]
      [(loop (cdr mtx) (cons (list-ref (car mtx) n) acc))])))

(module+ test
  (test-case "matrix-col tests"
             (define M0 (matrix '(a b c d e f) #:rows 3))
             (check-equal? (matrix-col M0 0) '(a c e))
             (check-equal? (matrix-col M0 1) '(b d f))))

;; matrix-size: matrix -> cons?
;; returns the size of the matrix
(define (matrix-size mtx)
  (cons (matrix-rows mtx) (matrix-cols mtx)))

(module+ test
  (test-case "matrix-size tests"
             (define M0 (matrix '(a b c d e f) #:rows 3))
             (check-equal? (matrix-size M0) '(3 . 2))))

;; matrix-length: matrix accessor -> number
;; returns the length of the matrix for the given accessor method.
(define (matrix-length mtx #:accessor (accessor matrix-row))
  (let loop ([len 0])
    (cond 
      [(null? (with-handlers ([exn:fail:contract?
                               (λ (e) null)])
                (accessor mtx len)))
       len]
      [else (loop (add1 len))])))

(module+ test
  (test-case "matrix-length tests"
             (let* ([LST '(a b c d e f)]
                    [mtx (matrix LST
                                 #:rows 2
                                 #:cols 3)])
               (check-equal? (matrix-length mtx 
                                            #:accessor matrix-row) 
                             2)
               (check-equal? (matrix-length mtx 
                                            #:accessor matrix-col)
                             3))))

;; matrix-ref: matrix row col -> any/c?
;; Repturns the element of matrix at position row and col where the matrix's first
;; element is at row 0 col 0.
(define (matrix-ref mtx row col)
  (define COLS-PER-ROW (matrix-cols mtx))
  (list-ref (matrix->list mtx) 
            (matrix-pos->list-pos row col #:cols-per-row COLS-PER-ROW)))

(module+ test
  (test-case "matrix-ref tests"
             (define M0 (matrix '(a b c d e f)
                                #:rows 2))
             
             ; row 0 col 0 test
             (check-equal? (matrix-ref M0 0 0) 'a)
             
             ; row 0 col 1 test
             (check-equal? (matrix-ref M0 0 1) 'b)
             
             ; row 1 col 0 test
             (check-equal? (matrix-ref M0 1 0) 'd)
             ; row 1 col 2 test
             (check-equal? (matrix-ref M0 1 2) 'f)))

;; matrix-pos->list-pos: row col [#:matrix] [#:cols-per-row] -> integer?
;; Converts matrix row column position to list position.
(define (matrix-pos->list-pos row
                              col 
                              #:matrix (mtx #f)
                              #:cols-per-row (cols-per-row 
                                              (if mtx (matrix-cols mtx) #f)))
  (+ (* row cols-per-row) col))

(module+ test
  (test-case "matrix-pos->list-pos tests"
             (let ([M0 (matrix '(a b c d e f)
                               #:rows 2
                               #:cols 3)])
               (check-equal? (matrix-pos->list-pos 1 1 #:matrix M0) 4))))

;; list-pos->matrix-pos: pos [#:matrix] [#cols-per-row] -> list?
;; Converts list position to matrix row column position.
(define (list-pos->matrix-pos pos
                              #:matrix (mtx #f)
                              #:cols-per-row (cols-per-row 
                                              (if mtx (matrix-cols mtx) #f)))
  (values->list (quotient/remainder pos cols-per-row)))

(module+ test
  (test-case "list-pos->matrix-pos tests"
             (let ([M0 (matrix '(a b c d e f)
                               #:rows 2
                               #:cols 3)])
               (check-equal? (list-pos->matrix-pos 4 #:matrix M0) 
                             (list 1 1)))))

;; matrix-set: matrix row col val -> matrix?
;; Returns a new matrix setting the matrix position to value.
(define (matrix-set mtx row col val)
  (define N (matrix-pos->list-pos row
                                  col 
                                  #:matrix mtx))
  (define LST (list-set (matrix->list mtx) N val))
  (matrix LST #:rows (matrix-rows mtx) #:cols (matrix-cols mtx)))

(module+ test
  (test-case "matrix-set tests"
             (let ([M0 (matrix '(a b c d e f)
                               #:rows 2
                               #:cols 3)]
                   [M1 (matrix '(a b c x e f)
                               #:rows 2
                               #:cols 3)])
               (check-equal? M1 
                             (matrix-set M0 1 0 'x)))))

;; matrix-map: proc 
;;             matrix
;;             [#:accessor]
;;             [#:inner-traversal?]
;; -> matrix? or list?
;; Processes the matrix mapping proc to each element
;; of the matrix and returns a new matrix. The rcv-flag parm 
;; determines whether the helper should pass proc the row column value, 
;; row and column, or simply the matrix value for the element.
(define (matrix-map proc 
                    mtx
                    #:accessor (accessor matrix-row)
                    #:inner-traversal? (inner-traversal? #t))
  
  (define seq (if inner-traversal?
                  (matrix #:rows (matrix-rows mtx) #:cols (matrix-cols mtx))
                  empty))
  (define outer-len (matrix-length mtx #:accessor accessor))
  (define inner-len (length (accessor mtx 0)))
  (define row #f)
  (define col #f)
  
  (do ((outer 0 (add1 outer)))
    ((>= outer outer-len) (if inner-traversal? seq (reverse seq)))
    
    (if inner-traversal?
        (do ((inner 0 (add1 inner)))
          ((>= inner inner-len))
          
          (set! row (if (eq? accessor matrix-row) outer inner))
          (set! col (if (eq? accessor matrix-row) inner outer))
          
          (set! seq 
                (matrix-set seq 
                            row 
                            col 
                            (match (procedure-arity proc)
                              [1 (proc (matrix-ref mtx row col))]
                              [2 (proc row col)]
                              [3 (proc row col (matrix-ref mtx row col))]))))
        (set! seq
              (cons (match (procedure-arity proc)
                      [1 (proc (accessor mtx outer))]
                      [2 (proc (if (eq? accessor matrix-row) outer #f)
                               (if (eq? accessor matrix-row) #f outer))]
                      [3 (proc (if (eq? accessor matrix-row) outer #f)
                               (if (eq? accessor matrix-row) #f outer)
                               (accessor mtx outer))])
                    seq)))))

(module+ test
  (test-case "matrix-map tests"
             
             (let ([M0 (matrix '(1 2 3 4 5 6 7 8 9)
                               #:order 3)]
                   [M1 (matrix '(#t #f #t #f #t #f #t #f #t))]
                   [M2 (matrix '(0 1 2 1 2 3 2 3 4) #:order 3)]
                   [M3 (matrix '((0 0 1) 
                                 (0 1 2) 
                                 (0 2 3)
                                 (1 0 4)
                                 (1 1 5)
                                 (1 2 6)
                                 (2 0 7)
                                 (2 1 8)
                                 (2 2 9))
                               #:order 3)])
               (check-equal? (matrix-map odd? M0)
                             M1)
               
               (check-equal? (matrix-map (λ (r c) (+ r c)) 
                                         M0)
                             M2)
               
               (check-equal? (matrix-map (λ (r c v) (list r c v))
                                         M0)
                             M3))))

;; matrix-rotate: matrix number -> matrix
;; produces a new matrix rotated the indicated number of rotations 
;; clockwise (positive rotations) or counter-clockwise (negative rotations).
(define (matrix-rotate mtx (rotations 1))
  (let ([clockwise (positive? rotations)]
        [rotations (abs rotations)])
    (letrec (;; rotates the matrix r times
             [outer (λ (n xs)
                      (cond
                        [(zero? n) xs]
                        [else (outer (sub1 n) (inner xs (matrix-cols xs) empty))]))]
             
             ;; shifts the elements of a rotated matrix
             ;; clockwise or counter-clockwise.
             [inner (λ (xs n acc)
                      (cond
                        [(zero? n) (if clockwise acc (reverse acc))]
                        [else (inner xs
                                     (sub1 n)
                                     (cons ((if clockwise reverse identity)
                                            (matrix-col xs (sub1 n)))
                                           acc))]))])
      (outer rotations mtx))))

(module+ test
  (test-case "matrix-rotate tests"
             (let* ([LST '(a b c d e f)]
                    [mtx (matrix LST)])
               (check-equal? (matrix-rotate mtx 0)
                             mtx)
               (check-equal? (matrix-rotate mtx -4)
                             mtx)
               (check-equal? (matrix-rotate mtx 4)
                             mtx)
               (check-equal? (matrix-rotate
                              (matrix-rotate mtx -1) 1)
                             mtx)
               (check-equal? (matrix-rotate
                              (matrix-rotate mtx -2) 2)
                             mtx)
               (check-equal? (matrix-rotate (matrix))
                             (matrix)))))

;; matrix-transpose: mtx -> mtx
;; The transpose of a matrix is a new matrix whose rows are the columns
;; of the original. (This makes the columns of the new matrix the rows
;; of the original). Here is a matrix and its transpose:
;;
;; (matrix '(5 4 3 4 0 4 7 10 3) #:order 3)
;; (matrix '(5 4 7 4 0 10 3 4 3) #:order 3)
(define (matrix-transpose m) (reverse (matrix-rotate m -1)))

(module+ test
  (test-case "matrix-transpose tests"
             (define A (matrix '(1 2 3 4 5 6) #:rows 2))
             (define B (matrix '(1 4 2 5 3 6) #:rows 3))
             (check-equal? (matrix-transpose A) B)
             (check-true (matrix-equal? (matrix-transpose (matrix-transpose A))
                                        A))))

;; matrix->matrix: mtx1 mtx2 -> matrix?
;; Returns a new matrix, mapping mtx1 values onto mtx2. The mapping
;; is constrained by the minimum of the sizes of mtx1 and mtx2.
(define (matrix->matrix mtx1 mtx2)
  (define rows (min (matrix-rows mtx1) (matrix-rows mtx2)))
  (define cols (min (matrix-cols mtx1) (matrix-cols mtx2)))
  (for* ([r (range rows)]
         [c (range cols)])
    (set! mtx2 (matrix-set mtx2 r c (matrix-ref mtx1 r c))))
  mtx2)

(module+ test
  (test-case "matrix->matrix tests"
             (let ([M0 (matrix '(1 2 3 4 5 6)
                               #:rows 2
                               #:cols 3)]
                   [M1 (matrix #:rows 3
                               #:cols 2
                               #:fill-value #f)])
               (check-equal? (matrix->matrix M0 M1)
                             '((1 2) (4 5) (#f #f)))
               (check-equal? (matrix->matrix M1 M0)
                             '((#f #f 3) (#f #f 6)))
               (check-equal? (matrix->matrix (matrix)
                                             (matrix '(a b c d) #:order 2))
                             (matrix '(a b c d) #:order 2))
               (check-equal? (matrix->matrix (matrix '(a b c d) #:order 2)
                                             (matrix))
                             '()))))

;; matrix-accessor-find: pred? matrix proc -> number
;; returns the first ref of the matrix for which pred is true
;; using the specified traversal method. If no match is found
;; it returns #f. 
(define (matrix-accessor-find pred 
                              mtx 
                              #:accessor (accessor matrix-row)
                              #:values (values-flag 'list-pos+value))
  (let* ([len (matrix-length mtx #:accessor accessor)]
         [pos (let loop ([len len] [cnt 0])
                (cond
                  [(zero? len) #f]
                  [(pred (accessor mtx cnt)) cnt]
                  [else (loop (sub1 len) (add1 cnt))]))]
         [val (when pos (accessor mtx pos))])
    (if pos (match values-flag
              ['value (values val)]
              ['list-pos (values pos)]
              ['list-pos+value (values pos val)]
              ['matrix-pos (values (list-pos->matrix-pos pos #:matrix mtx))]
              ['matrix-pos+value 
               (values (list-pos->matrix-pos pos #:matrix mtx))])
        #f)))

(module+ test
  (test-case "matrix-accessor-find tests"
             (let ([M0 (matrix '(0 1 2 3 4 5 6 7 8)
                               #:order 3)]
                   [p (λ (r) (> (car r) 2))])
               (check-equal? (values->list (matrix-accessor-find p M0))
                             '(1 (3 4 5)))
               (check-false (matrix-accessor-find p (matrix))))))

;; matrix-findf: pred 
;;               mtx 
;;               [#:accessor]
;;               [#:inner-traversal?]
;;               [#:acc*]
;; -> (listof mtx-elm?)
;;
;; Filters the matrix using predicate, which can take the following
;; arguments: value, index, index+value. 
;; Returns #f if not found, or an mtx-elm, or a list of mtx-elms.
;;
;; When #:acc* #f the function returns the first match encountered in the traversal;
;; otherwise it will accumulate a list of all occurrences (See matrix-findf*)
;; 
;; Example: 
;; > (matrix-findf odd?
;;                 (matrix (range 12) #:rows 3 #:cols 4)
;;                 #:accessor matrix-col)
;; -> '(mtx-elm 1 0 1 1)
(define (matrix-findf pred
                      mtx
                      #:accessor (accessor matrix-row)
                      #:inner-traversal? (inner-traversal? #t)
                      #:acc* (acc* #f))
  (define ACC (if acc* empty #f))
  (define outer-len (matrix-length mtx #:accessor accessor))
  (define inner-len (if (empty? mtx) 0 (length (accessor mtx 0))))
  (define ROW #f)
  (define COL #f)
  (define VAL #f)
  
  (do ((outer 0 (add1 outer)))
    ((or (mtx-elm? ACC) (>= outer outer-len)) 
     (cond
       [(boolean? ACC) ACC]
       [(mtx-elm? ACC) ACC]
       [else (reverse ACC)]))
    
    (cond
      [inner-traversal?
       (do ((inner 0 (add1 inner)))
         ((or (mtx-elm? ACC) (>= inner inner-len)))
         
         (set! ROW (if (eq? accessor matrix-row) outer inner))
         (set! COL (if (eq? accessor matrix-row) inner outer))
         
         (set! VAL (matrix-ref mtx ROW COL))
         
         (when (match (procedure-arity pred)
                 [1 (pred VAL)]
                 [2 (pred ROW COL)]
                 [3 (pred ROW COL VAL)])
           (define ELM (mtx-elm (matrix-pos->list-pos ROW COL #:matrix mtx)
                                ROW
                                COL
                                VAL))
           (set! ACC (if (list? ACC) (cons ELM ACC) ELM))))]
      [else 
       (set! VAL (accessor mtx outer))
       (when (match (procedure-arity pred)
               [1 (pred VAL)]
               [2 (pred (if (eq? accessor matrix-row) outer #f)
                        (if (eq? accessor matrix-row) #f outer))]
               [3 (pred (if (eq? accessor matrix-row) outer #f)
                        (if (eq? accessor matrix-row) #f outer)
                        VAL)])
         (define ELM (mtx-elm (matrix-pos->list-pos ROW COL #:matrix mtx)
                              ROW
                              COL
                              VAL))
         (set! ACC (if (list? ACC) (cons ELM ACC) ELM)))])))

(module+ test
  (test-case "matrix-findf tests"
             (let ([M0 (matrix (range 12)
                               #:rows 3
                               #:cols 4)]
                   [VAL (mtx-elm 1 0 1 1)])
               ;; empty matrix
               (check-false (matrix-findf (λ (r) (eq? 'p r))
                                          (matrix)))
               (check-equal? (matrix-findf (λ (r) (eq? r 'p))
                                           (matrix)
                                           #:acc* #t)
                             '())
               ;; matrix no-match
               (check-false (matrix-findf (λ (r) (eq? 'w r))
                                          (matrix '(p q r) #:rows 1)))
               ;; matrix match on column
               (check-equal? (matrix-findf odd? 
                                           M0 
                                           #:accessor matrix-col)
                             VAL))))

;; matrix-findf*: pred 
;;                mtx 
;;                [#:accessor]
;;                [#:inner-traversal?]
;; -> (listof mtx-elm?)
;;
;; Filters the matrix using predicate, which can take the following
;; arguments: value, index, index+value. 
;; Returns a list of mtx-elms.
;;
;; Equivalent to matrix-findf when #:acc* #t. This function returns a list
;; of all matching occurrences for the specified traversal.
;;
;; Example: 
;; > (matrix-findf* odd?
;;                  (matrix (range 12) #:rows 3 #:cols 4)
;;                  #:accessor matrix-col)
;; -> '((mtx-elm 1 0 1 1) (mtx-elm 5 1 1 5) (mtx-elm 9 2 1 9)
;;      (mtx-elm 3 0 3 3) (mtx-elm 7 1 3 7) (mtx-elm 11 2 3 11))
(define (matrix-findf* pred
                       mtx
                       #:accessor (accessor matrix-row)
                       #:inner-traversal? (inner-traversal? #t))
  (matrix-findf pred
                mtx
                #:accessor accessor
                #:inner-traversal? inner-traversal?
                #:acc* #t))

(module+ test
  (test-case "matrix-findf* tests"
             (let ([M0 (matrix (range 12)
                               #:rows 3
                               #:cols 4)]
                   [LST (list
                         (mtx-elm 1 0 1 1)
                         (mtx-elm 5 1 1 5)
                         (mtx-elm 9 2 1 9)
                         (mtx-elm 3 0 3 3)
                         (mtx-elm 7 1 3 7)
                         (mtx-elm 11 2 3 11))])
               (check-equal? (matrix-findf* odd? 
                                            M0 
                                            #:accessor matrix-col)
                             LST))))

;; matrix-equal?: matrix  matrix -> boolean?
;; Returns #t if m1 equals m2.
(define (matrix-equal? m1 m2 (is-equal? equal?))
  (cond 
    [(equal? (matrix-size m1) (matrix-size m2))
     (let loop ([m1 (matrix->list m1)] [m2 (matrix->list m2)])
       (cond
         [(empty? m1) #t]
         [(not (is-equal? (first m1) (first m2))) #f]
         [else (loop (rest m1) (rest m2))]))]
    [else #f]))

(module+ test
  (test-case "matrix-equal? tests"
             (define m1 (matrix '(a b c d e f) #:rows 2))
             (define m2 (matrix '(a b c d e f) #:rows 3))
             (define m3 (matrix '(a b c d e f) #:rows 2))
             (check-true (matrix-equal? (matrix) (matrix)))
             (check-false (matrix-equal? m1 m2))
             (check-true (matrix-equal? m1 m3))))

;; matrix-square?: mtx -> boolean
;; Returns true if the matix is square, otherwise returns false.
(define (matrix-square? mtx)
  (equal? (matrix-rows mtx) (matrix-cols mtx)))

(module+ test
  (test-case "matrix-square tests"
             (define A (matrix '(1 2 3 4 5 6) #:rows 2))
             (define B (matrix '(1 4 2 5 3 6 2 10 13) #:rows 3))
             (check-false (matrix-square? A))
             (check-true (matrix-square? B))))

;; matrix-symmetric? mtx -> boolean
;; returns true if the matrix is symmetric; otherwise false.
(define (matrix-symmetric? mtx)
  (cond
    [(not (matrix-square? mtx)) #f]
    [else (matrix-equal? mtx (matrix-transpose mtx))]))

(module+ test
  (test-case "matrix-symmetric tests"
             (define A (matrix '(1 2 3 4 5 6) #:rows 2))
             (define B (matrix '(1 4 2 5 3 6 2 10 13) #:order 3))
             (define C (matrix '(5 4 3 4 0 7 3 7 -1) #:order 3))
             (check-false (matrix-symmetric? A))
             (check-false (matrix-symmetric? B))
             (check-true (matrix-symmetric? C))))

;; submatrix: mtx [#:row-pos #:row-len #:col-pos #:col-len] => list?
;; Returns a submatrix of mtx analogous to sublist. The matrix will
(define (submatrix mtx
                   #:row-pos (row-pos 0)
                   #:row-len (row-len (- (matrix-rows mtx) row-pos))
                   #:col-pos (col-pos 0)
                   #:col-len (col-len (- (matrix-cols mtx) col-pos)))
  (for/list ([m (sublist mtx row-pos row-len)]) 
    (sublist m col-pos col-len)))


(module+ test
  (test-case "submatrix tests"
             (check-equal? (submatrix (matrix (range 12))
                                      #:row-pos 1 #:row-len 2
                                      #:col-pos 1 #:col-len 2)
                           '((5 6) (9 10)))))
