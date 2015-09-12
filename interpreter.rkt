#! /usr/bin/env racket
#lang racket

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


;; A Classification is one of
;; - 'push
;; - 'concat
;; - 'duplicate
;; - 'swap
;; - 'enclose
;; - 'eval
;; - 'drop
;; - 'print

;; A State is a (make-state [List Token] [List String])
(define-struct state (program stack) #:transparent)

;; tokenize: String -> [List Token]
;; Breaks the input string into tokens, as per underload.
;; Throws an error upon hitting an unrecognized token
(define (tokenize str)
  ;; take-token: String -> Token
  ;; Produce the first token present in the string
  (define (take-token str)
    (let ((fst (substring str 0 1)))
      (cond [(or (string=? fst ":") (string=? fst "~")
                 (string=? fst "*") (string=? fst "a")
                 (string=? fst "^") (string=? fst "!")
                 (string=? fst "S")) fst]
            [(string=? fst "(") (take-push-token str)]
            [else (error (string-append "Unrecognized character: " fst))])))
  
  ;; take-push-token-helper: String String Int -> Token
  ;; Helper method to count off paren depth
  (define (take-push-token-helper str build depth)
  (cond [(zero? depth) build]
        [else 
         (let ((fst (substring str 0 1))
               (rst (substring str 1)))
                (cond [(string=? fst "(") 
                       (take-push-token-helper 
                        rst (string-append build fst) (+ depth 1))]
                      [(string=? fst ")")
                       (take-push-token-helper 
                        rst (string-append build fst) (- depth 1))]
                      [else 
                       (take-push-token-helper 
                        rst (string-append build fst) depth)]))]))

  ;; take-push-token: String -> Token
  ;; Parse and return a push token
  (define (take-push-token str)
    (take-push-token-helper (substring str 1) "(" 1))

  ;; drop-token: String -> String
  ;; Return the string, absent the first token present
  (define (drop-token str token)
    (substring str (string-length token)))

  (cond [(string=? str "") '()]
        [else (let ((token (take-token str)))
                (cons token (tokenize (drop-token str token))))]))

;; classify: Token -> Classification
;; Classify each token by the action that is required
(define (classify token)
  (cond [(string=? token ":") 'duplicate]
        [(string=? token "~") 'swap]
        [(string=? token "*") 'concat]
        [(string=? token "a") 'enclose]
        [(string=? token "^") 'eval]
        [(string=? token "!") 'drop]
        [(string=? token "S") 'print]
        [else 'push]))

;; print-state: State -> String
;; Do a print-out of the current state
(define (print-state state)
  ;; [List <String|Token>] -> String
  (define (prgm-string lst)
    (if (empty? lst) ""
        (string-append (first lst) (prgm-string (rest lst)))))
  (string-append (prgm-string (state-program state)) 
                 "\n" (prgm-string (state-stack state))))


;; Push: State -> State
;; Guarded: prgm non-empty
;; Push token from program to stack
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
  ;; rebuild-file: input-port -> String
  ;; Read back file at input-port
  (define (rebuild-file input-port)
    ;; FENCEPOST GARBAGE W/ ACCUM
    (define (append-items ls accum)
      (cond [(empty? ls) accum]
            [(empty? (rest ls)) (string-append accum (first ls))]
            [else (append-items (rest ls) (string-append accum
                                                         (first ls) "\n"))]))
    (append-items (call-with-input-file input-port
                    (λ(in) (sequence->list (in-lines in))))
                  ""))
  
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
                    (rebuild-file input)
                    input)))
    (display "\n")))