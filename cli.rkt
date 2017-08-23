#lang typed/racket
(require "interpreter.rkt")

;; Provides the command-line methods of interpreting this 
;; method; runs automatically
(module+ main
  ;; Treat argument as filename (#t) or program (#f)?
  (define read-file? (make-parameter #t))
  
  ;; Rebuild the file. I can't just concatenate the lines, cause 
  ;; newlines are meaningful if they occur in push blocks 
  ;; (and illegal otherwise). So the fence-post work.
  ;; rebuild-file: Input-Port -> String
  ;; Read back file at input-port
  (: rebuild-file (-> Path-String String))
  (define (rebuild-file input-port)
    ;; FENCEPOST GARBAGE W/ ACCUM
    (: append-items (-> (Listof String) String String))
    (define (append-items ls accum)
      (cond [(empty? ls) accum]
            [(empty? (rest ls)) (string-append accum (first ls))]
            [else (append-items (rest ls) (string-append accum
                                                         (first ls) "\n"))]))
    (append-items (call-with-input-file input-port
                    (λ([in : Input-Port]) (sequence->list (in-lines in))))
                  ""))
  
  (: push-to-path-string (-> Any Path-String))
  (define (push-to-path-string input)
    (if (path-string? input) input
        (error (string-append "Argument "
                              " could not be interpreted as a "
                              "path string."))))
  
  (: push-to-string (-> Any String))
  (define (push-to-string input)
    (if (string? input) input 
        (error "Input " input "could not be interpreted as a string.")))
  
  ;; Sequencing for side-effects. Interpret has side-effects for reasons 
  ;; that I explained above, so it needs to be sequenced. ''begin'' can't 
  ;; be used, because at the module level (ie, here) it doesn't silence 
  ;; return values, it just splices them into the module. So ''begin0'' 
  ;; had to be used.
  (begin0
    (void)
    (interpret (command-line 
                #:program "underload"
                #:once-each
                [("--here" "-s") "Interpret given argument as a program, not a file"
                                 (read-file? #f)]
                #:args (input)
                (if (read-file?)
                    (rebuild-file (push-to-path-string input))
                    (push-to-string input))))
    (display "\n")))
