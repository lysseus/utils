#lang racket

;;;
;;; STATES
;;;
;;; Implements a finite state-machine mechanism for
;;; handling events.
;;;

(provide (struct-out State)
         states-hash
         state-handler)

(require (for-syntax syntax/parse))

(define-syntax (states-hash stx)
  (syntax-parse stx
    [(_ (state (curr event action state-id next) ...) ...)
     #'(make-hash (list (list (quote state)
                              (Task (quote curr)
                                    event
                                    action
                                    (quote state-id)
                                    (quote next)) ...) ...))]))

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
  (for/last ([task tasks]
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
(define (state-handler ws states state event)
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
  (define tasks (hash-ref states (State-id state)))
  (define task (find-task tasks (State-task-id state) event))
  (when (Task? task)
    ;; Events are only saved when there is a task to handle them.
    (set-State-event! state event)
    (define-values (j? st) (jump? (Task-state-id task)))
    ;; Save this task-id in our states-hash. 
    (unless (or (false? (Task-state-id task)) j?)
      (push! states (State-id state) (State-task-id state)))    
    ;; This task switches to a new state-id.
    (cond
      [(false? (Task-state-id task))
       (void)]
      [j?
       (define ans (pop! states st))
       (unless (false? ans)
         (set-State-id! state (first ans))
         (set-State-task-id! state (second ans)))]
      [else
       (set-State-id! state (Task-state-id task))])
    (unless (false? (Task-next task))
      (set-State-task-id! state (Task-next task)))
    (define action (Task-action task))
    (when (procedure? action)
      ;; When action raises a symbol it becomes the new task-id.
      (with-handlers ([symbol? (λ (next) (set-State-task-id! state next))]
                      [pair? (λ (pair)
                               (set-State-id! state (car pair))
                               (set-State-task-id! state (cdr pair)))])
        (action ws state event))))
  state)
