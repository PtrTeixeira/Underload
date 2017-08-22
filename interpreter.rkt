#lang typed/racket
(require "interpreter-service.rkt")

(provide interpret)
;; A Token is one of
;; - "(.*)"
;; - ":"
;; - "~"
;; - "*"
;; - "a"
;; - "^"
;; - "!"
;; - "S"
(define-type Token String)



;; A Classification is one of
;; - 'push
;; - 'concat
;; - 'duplicate
;; - 'swap
;; - 'enclose
;; - 'eval
;; - 'drop
;; - 'print
(define-type Classification 
  (U 'push
     'concat
     'duplicate
     'swap
     'enclose
     'eval
     'drop
     'print))

;; A State is a (make-state [List Token] [List String])
(define-struct state ([program : (Listof Token)]
                      [stack : (Listof String)]) #:transparent)
;; I don't like types having lowercase names.
(define-type State state)

;; classify: Token -> Classification
;; Classify each token by the action that is required
(: classify (-> Token Classification))
(define/match (classify token)
  [(":") 'duplicate]
  [("~") 'swap]
  [("*") 'concat]
  [("a") 'enclose]
  [("^") 'eval]
  [("!") 'drop]
  [("S") 'print]
  [(_)   'push])

;; print-state: State -> String
;; Do a print-out of the current state
(: print-state (-> State String))
(define (print-state state)
  ;; [List <String|Token>] -> String
  (: prgm-string (-> (Listof String) String))
  (define (prgm-string lst)
    (if (empty? lst) ""
        (string-append (first lst) (prgm-string (rest lst)))))
  (string-append (prgm-string (state-program state)) 
                 "\n" (prgm-string (state-stack state))))


;; Push: State -> State
;; Guarded: prgm non-empty
;; Push token from program to stack
(: push (-> State State))
(define (push state) 
  (let ((fst (substring 
              (first (state-program state)) 
              1 (- (string-length (first (state-program state))) 1))))
    (make-state (rest (state-program state))
                (cons fst (state-stack state)))))
;; Duplicate: State -> State
;; Guarded: prgm non-empty
;; Throw error if trying to duplicate on empty stack
;; Duplicate top element of stack
(: duplicate (-> State State))
(define (duplicate state)
  (if (empty? (state-stack state)) 
      (error 
       (string-append "Attempted to duplicate on empty stack: " 
                      (print-state state)))
      (make-state 
       (rest (state-program state))
       (cons (first (state-stack state)) (state-stack state)))))

;; swap: State -> State
;; Guard: prgm non-empty
;; Throw error if fewer than two elements on the stack
;; Swap top two elements on the stack
(: swap (-> State State))
(define (swap state)
  (match (state-stack state)
    [(list-rest a b c) (make-state (rest (state-program state))
                                   (cons b (cons a c)))]
    [_ (error (string-append 
                    "Attempted to swap with fewer than two elements "
                    "in the stack: " (print-state state)))]))

;; concat: State -> State
;; Guard: prgm non-empty
;; Throw error if fewer than two elements on the stack
;; Concatenate the top two elements of the stack
(: concat (-> State State))
(define (concat state)
  (match (state-stack state)
    [(list-rest a b c) (make-state (rest (state-program state))
                                   (cons (string-append b a) c))]
    [_ (error (string-append "Attempted to concatenate with fewer than two "
                             "elements in the stack: " (print-state state)))]))

;; enclose: State -> State
;; Guard: prgm non-empty
;; Throw error if stack is empty
;; Enclose top element of stack in parentheses
(: enclose (-> State State))
(define (enclose state)
  (match (state-stack state)
    [(list-rest a b) (make-state (rest (state-program state))
                                 (cons (string-append "(" a ")") b))]
    [_ (error (string-append "Attempted to enclose with no elements on the "
                             "stack:" (print-state state)))]))

;; eval: State -> State
;; Guard : prgm non-empty
;; Throw error if stack is empty OR if popped item is not a valid program
;; Pop the first element from the stack onto the program
(: eval (-> State State))
(define (eval state)
  (if (empty? (state-stack state))
      (error (string-append "Attempted to evaluate on the empty stack: "
                            (print-state state)))
      (make-state (append (tokenize (first (state-stack state)))
                          (rest (state-program state)))
                  (rest (state-stack state)))))

;; drop: State -> State
;; Guard: prgm non-empty
;; Throw error if stack is empty
;; Drop first element from the stack
(: drop (-> State State))
(define (drop state)
  (if (empty? (state-stack state))
      (error (string-append "Attempted to drop item from empty stack: "
                            (print-state state)))
      (make-state (rest (state-program state))
                  (rest (state-stack state)))))

;; print: State -> State
;; Guard: prgm is non-empty
;; Throw error if stack is empty
;; SIDE EFFECT: Will print to standard output
;; Print & drop first element from the stack
(: prints (-> State State))
(define (prints state)
  (if (empty? (state-stack state))
      (error (string-append "Attempted to print from empty stack: "
                            (print-state state)))
      (begin
        (display (first (state-stack state)))
        (make-state (rest (state-program state))
                    (rest (state-stack state))))))


;; run: State -> [List String]
;; Run the program from a state
;; Output from the program is immediately printed to sout, rather than 
;; -> returned. This has to do with how the interpreter should behave 
;; -> on infinite programs. The returned value is the stack after the 
;; -> program terminates.
(: run (-> State (Listof String)))
(define (run state)
  (define prgm (state-program state))
  (define stck (state-stack state))
  (if (empty? prgm) stck 
      (letrec ((fst (first prgm))
               (type (classify fst)))
        (cond [(symbol=? type 'push) (run (push state))]
              [(symbol=? type 'duplicate) (run (duplicate state))]
              [(symbol=? type 'swap) (run (swap state))]
              [(symbol=? type 'concat) (run (concat state))]
              [(symbol=? type 'enclose) (run (enclose state))]
              [(symbol=? type 'drop) (run (drop state))]
              [(symbol=? type 'print) (run (prints state))]
              [(symbol=? type 'eval) (run (eval state))]))))


;; interpret: String -> [List String]
;; Run program from string input
(: interpret (-> String (Listof String)))
(define (interpret program)
  (run (make-state (tokenize program) '())))

;; Testing module: This should only imported if you 
;; are planning on testing these functions, they are 
;; implementation details.
(module* testing #f
  (provide interpret
           run
           tokenize
           classify
           push
           duplicate
           swap
           enclose
           eval
           drop
           prints
           concat
           (struct-out state)))

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