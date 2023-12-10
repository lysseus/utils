#lang racket

;;;
;;; STATES
;;;
;;; Implements a finite state-machine mechanism for
;;; handling events.
;;;

(provide (struct-out State)
         states-hash
         state-handler
         undefined
         current-pre-action
         current-post-action
         current-not-found)

(require (for-syntax syntax/parse)
         racket/undefined)

(define-syntax (states-hash stx)
  (syntax-parse stx
    [(_ (state (curr event action state-id next) ...) ...)
     #'(make-hash (list (list (quote state)
                              (Task (quote curr)
                                    event
                                    action
                                    (quote state-id)
                                    (quote next)) ...) ...))]))

;; Provides a hook for global pre-action that is only executed
;; when the action is executed.
(define current-pre-action (make-parameter #f))

;; Provides a hook for global post-action that is only executed
;; when the action is executed.
(define current-post-action (make-parameter #f))

;; Provides a hook for global not-found that is only executed
;; when no new state is found.
(define current-not-found (make-parameter #f))

(struct State (id task-id event)
  #:mutable #:transparent)

(struct Task (curr event action state-id next)
  #:transparent)

(define (push! states state task)
  (define val (cons (list state task) (hash-ref states '^ '())))
  (hash-set! states '^ val))

(define (pop! states (state-id #f))
  (define val (hash-ref states '^ '()))
  (cond
    [(empty? val) #f]
    [else
     (cond
       [(false? state-id)
        (define ans (car val))
        (hash-set! states '^ (rest val))
        ans]
       [else
        (define ans (memf (λ (v) (eq? (car v) state-id)) val))
        (cond [(false? ans) #f]
              [else
               (hash-set! states '^ (rest ans))
               (car ans)])])]))

(define (find-task tasks curr event)
  (for/first ([task tasks]
              #:when (and (eq? (Task-curr task) curr)
                          (cond
                            [(string? (Task-event task))                            
                             (string=? (Task-event task) event)]
                            [(procedure? (Task-event task))
                             ((Task-event task) event)]
                            [else #t])))
    task))

;; state-handler does the processing of a task for
;; a given state/event. It retrieves the current task
;; defined to handle the event for this state, performs
;; any specified action, and sets up the next state/task.
(define (state-handler ws states curr-state event)
  (define next-state (struct-copy State curr-state (event undefined)))
  (define (jump? sym)
    (cond
      [(symbol? sym)
       (define str (symbol->string sym))
       (define v? (string-prefix? str "^"))
       (define v2 (string-trim str "^"))
       (define s
         (if (non-empty-string? v2)
             (string->symbol v2)
             #f))
       (values v? s)]
      [else (values #f #f)]))
  (define tasks (hash-ref states (State-id curr-state)))
  (define task (find-task tasks (State-task-id curr-state) event))
  (cond
    [(Task? task)
    ;; Events are only saved on the curr-state when there is a task to handle them.
    (set-State-event! curr-state event)
    (define-values (j? st) (jump? (Task-state-id task)))
    ;; Save this task-id in our states-hash. 
    (unless (or (false? (Task-state-id task)) j?)
      (push! states (State-id curr-state) (State-task-id curr-state)))    
    ;; This task switches to a new state-id.
    (cond
      [(false? (Task-state-id task))
       (void)]
      [j?
       (define ans (pop! states st))
       (unless (false? ans)
         (set-State-id! next-state (first ans))
         (set-State-task-id! next-state (second ans)))]
      [else
       (set-State-id! next-state (Task-state-id task))])
    (unless (false? (Task-next task))
      (set-State-task-id! next-state (Task-next task)))
    (define action (Task-action task))
    (when (procedure? action)
      ;; When action raises a symbol it becomes the new task-id.
      (with-handlers ([symbol? (λ (next) (set-State-task-id! next-state next))]
                      [pair? (λ (pair)
                               (set-State-id! next-state (car pair))
                               (set-State-task-id! next-state (cdr pair)))])
        (unless (false? (current-pre-action))
          ((current-pre-action) ws curr-state next-state))
        (action ws curr-state next-state)
        (unless (false? (current-post-action))
          ((current-post-action) ws curr-state next-state))))]
    [else (unless (false? (current-not-found))
            ((current-not-found) ws curr-state))])
  next-state)
