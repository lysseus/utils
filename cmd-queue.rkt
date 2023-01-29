#lang racket

;;;
;;; CMD-QUEUE
;;;
;;; A process for executing procedures in sequential fashion. If the procdure
;;; throws an eror it is reloaded to the last-in/first-out pending stack and
;;; retried until it either:
;;; (a) succeeds (doesn't throw an error)
;;; (b) Exhaust its number of tries
;;; (c) Exhausts the number of pending iteration attempts
;;; 

(provide (struct-out cmd)
         process-pending         
         pending
         successful
         failed
         add-to-pending
         #;λadd-to-pending
         stack-empty?
         peek
         size
         initialize-cmd-tries
         stream-pending
         size-active
         peek-active
         processing-cmd)

(require (for-syntax syntax/parse)
         utils/stack)

(struct cmd (stmt# tries proc args) #:mutable #:transparent)

(define current-pending (make-parameter (Lifo)))
(define-syntax pending
  (λ (stx) (syntax-parse stx [pending:id #'(current-pending)])))

(define courrent-successful (make-parameter (Lifo)))
(define-syntax successful
  (λ (stx) (syntax-parse stx [successful:id #'(courrent-successful)])))

(define current-failed (make-parameter (Lifo)))
(define-syntax failed
  (λ (stx) (syntax-parse stx [failed:id #'(current-failed)])))

(define current-processing-cmd (make-parameter #f))
(define-syntax processing-cmd
  (λ (stx) (syntax-parse stx [processing-cmd:id #'(current-processing-cmd)])))

#;(define-syntax (add-to-pending stx)
  (syntax-parse stx
    [(_ tries proc arg ...)
     #'(push! pending (cmd (add1 (size pending)) tries proc (list arg ...)))]))

(define (add-to-pending tries proc . args)
  (push! pending (cmd (add1 (size pending)) tries proc args)))


(define (initialize-cmd-tries)
  (define default-tries (add1 (size pending)))
  (for ([n (range (size pending))])
    (define c (pop! pending))
    (define stmt# (cmd-stmt# c))
    (define tries (cmd-tries c))
    (define proc (cmd-proc c))
    (define args (cmd-args c))
    (if (false? tries)
        (push! pending (cmd stmt# default-tries proc args))
        (push! pending c))))

(define/contract (process-pending
                  #:attempts (attempts (size pending))
                  #:debug? (debug? #f)
                  #:stop-when (stop-when #f)
                  #:report? (report? #f)
                  #:stream? (stream? #f))
  (->* ()
       (#:attempts natural?
        #:debug? boolean?
        #:stop-when (or/c #f procedure?)
        #:report? boolean?
        #:stream? boolean?)
       any)
  
  ;; Initialize tries for each unspecified by the command
  (initialize-cmd-tries)

  ;; Process the pending stack.
  (define atts (cond
                 [stream? 1]
                 [else attempts]))
  (for ([attempt (range 1 (add1 atts))])
    ;; For failed commands with more tries left, load them to stack.
    (when debug? (printf "Attempt ~a:~%" attempt))
    (when (and report? (not (zero? (size failed))))
      (printf "[~a] Checking failed commands for requeue to pending~%"
              attempt))
    (for ([attempt (size failed)])
      ;; We only peek to check the number of tries lef.
      (define f (peek failed))
      (unless (zero? (cmd-tries f))
        (when (or debug? report?)
          (printf "\tFailed item [~a] command stmt #~a requeued~%"
                  attempt (cmd-stmt# f)))
        (push! pending (pop! failed))))
    (when debug? (printf "\tChecking if stack is empty or proces finished.~%"))
    #:break (or (stack-empty? pending)
                (and (procedure? stop-when) (stop-when)))
    (when (or debug? report?)
      (printf "[~a] Processing ~a pending command~a~%"
              attempt  (size pending)
              (if (> (size pending) 1) "s" "")))
    (define sz (size pending))    
    (define ns (cond
                 [(zero? sz) 0]
                 [stream? 1]
                 [else sz]))    
    (for ([n (range ns)])
      (when debug?
        (printf "\tChecking if stack empty or processing finished~%"))
      #:break (or (stack-empty? pending)
                  (and (procedure? stop-when) (stop-when)))
      (define c (pop! pending))
      (define stmt# (cmd-stmt# c))
      (define tries (sub1 (cmd-tries c)))
      (define proc (cmd-proc c))
      (define args (cmd-args c))
      (current-processing-cmd c)
      (define ans (with-handlers ([exn:fail:user?
                                   (λ (e)
                                     (when debug?
                                       (printf "~a~%" (exn-message e))) #f)])
                    (apply proc args)
                    #t))
      (when (or debug? report?)
        (printf "\tPending item [~a] command stmt #~a ~a~%"
                n stmt# (if ans 'successful 'failed))
        (when debug? (printf "\t\tproc: ~a~%\t\targs: ~a~%\t\tremaining tries; ~a~%" proc args tries)))
      (cond
        ;; This command successfully processed.
        [(not (false? ans)) (push! successful (cmd stmt# tries proc args))]
        ;; Otherwise the command failed
        [else (push! failed (cmd stmt# tries proc args))]))
    (when debug? (printf "Attempt ~a done.~%" attempt)))
  (when (or debug? report?)
    (printf "Processing finished with:
\t~a pending~%\t~a successful~%\t~a failed~%"
            (size pending)
            (size successful)
            (size failed))))

;;;
;;; STREAMING
;;;

;; Returns the stack size counting only active commands in the stac. 
(define (size-active stack)  
  (for/sum ([c (Lifo-data stack)])
    (if (and (cmd? c)
             (zero? (cmd-tries c))) 0 1)))

;; Peeks the first active command. If none found, returns #f.
(define (peek-active stack)  
  (for/first ([c (Lifo-data stack)])
    (if (and (cmd? c)
             (number? (cmd-tries c))
             (zero? (cmd-tries c))) #f c)))

;; Processes 1 pending record per call. Returns true if the command
;; was successful, otherwise false. When both pending and error queues
;; are empty an eof is returned.
;; NOTE: A command in the error queue may not actauly be moved to pending
;; due to it's number of tries having reached zero. 
(define/contract (stream-pending
                  #:debug? (debug? #f)
                  #:stop-when (stop-when #f)
                  #:report? (report? #f))
  (->* ()
       (#:debug? boolean?
        #:stop-when (or/c #f procedure?)
        #:report? boolean?)
       any)

  (define failed-size (size-active failed))
  (cond
    [(and (zero? (size pending))
          (zero? failed-size))
     eof]
    [else
     (process-pending #:debug? debug?
                      #:stop-when stop-when
                      #:report? report?
                      #:stream? #t)
     ;; Return whether command was successful or failed.
     (if (= (size failed) failed-size) #t #f)]))
