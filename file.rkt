#lang racket

;; This module implements a set of directory utilities.

(provide 
 ; converts a directory string into a list of file paths.
 (contract-out
  [make-dir-path-list
   (->i ([dir string?])
        (#:error-log [error-log string?])
        (result (non-empty-listof path?)))]))

;;--------------------------------------
;; import and implementation section


(module+ test
  (require rackunit))


;; make-dir-path-list: string -> list?
;; Conversts directory string into a list of file paths. 
(define (make-dir-path-list dir #:error-log (error-log "error-log.txt"))
  (define PATH-LIST (cdr (with-handlers ([exn:fail? 
                                          (λ (e) (log-error e error-log) 
                                            (raise e))])
                           (directory-list dir))))
  (define FILE-STRINGS (map path->string PATH-LIST))
  (define PATH-STRINGS (map (λ (p) (string-append dir "/" p)) FILE-STRINGS))
  (map string->path PATH-STRINGS))

(define (log-error e file-name)
  (with-output-to-file file-name
    (λ () (write (exn-message e)))
    #:exists 'replace))

; ===========================================================================                                          
;                                          
;                                          
;                                          
;   ;;;;;;;                                
;   ;  ;  ;                  ;             
;      ;     ;;;;    ;;;;;  ;;;;;    ;;;;; 
;      ;    ;    ;  ;    ;   ;      ;    ; 
;      ;    ;;;;;;   ;;;;    ;       ;;;;  
;      ;    ;            ;   ;           ; 
;      ;    ;       ;    ;   ;   ;  ;    ; 
;     ;;;    ;;;;;  ;;;;;     ;;;   ;;;;;  
;                                          
;                                          
;                                          
; 


(module+ test
  (require (submod ".."))
  
  (test-case "make-dir-path-list tests"
             
             (check-not-exn (λ () (make-dir-path-list ".")))))
