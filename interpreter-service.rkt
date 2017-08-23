#lang typed/racket

(provide interpret)

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

;; Group together characters in input into tokens, throwing error if impossible
(: take-tokens (-> (Listof Char) (Listof Token)))
(define/match (take-tokens chars)
  [('()) '()]
  [((cons (and token (or #\: #\~ #\* #\a #\^ #\! #\S)) chars))
   (cons (string token) (take-tokens chars))]
  [((cons #\( xs)) (let ([tokens (take-push-token xs "(" 1)])
                   (cons (car tokens) (take-tokens (cdr tokens))))]
  [((cons a _)) (error (~a "Unrecognized character: " a))])
  

;; Helper method to count off paren depth
(: take-push-token (-> (Listof Char) String Integer (Pairof Token (Listof Char))))
(define/match (take-push-token chars result depth)
  [(_ _ 0) (cons result chars)]
  [((cons #\( xs) _ _) (take-push-token xs (~a result #\() (add1 depth))]
  [((cons #\) xs) _ _) (take-push-token xs (~a result #\)) (sub1 depth))]
  [((list-rest #\" (and escaped (or #\< #\> #\[ #\] #\")) xs) _ _)
   (take-push-token xs (~a result escaped) depth)]
  [((cons (and escape-char (or #\< #\> #\[ #\] #\")) _) _ _)
   (error (~a "Unquoted " escape-char))]
  [((cons char chars) _ _) (take-push-token chars (~a result char) depth)]
  [(_ _ _) (error "Ran into unmatched parens!")])

;; tokenize: String -> [List Token]
;; Breaks the input string into tokens, as per underload.
;; Throws an error upon hitting an unrecognized token
(: tokenize (-> String (Listof Token)))
(define (tokenize str)
  (take-tokens (string->list str)))

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


;; Strip any leading or trailing parens off of a string
(: trim-parens (-> String String))
(define (trim-parens str)
  (string-trim (string-trim str "(") ")"))

;; Push token from program to stack
(: push (-> State State))
(define/match (push world)
  [((state '() _)) (error "Program cannot be empty")]
  [((state (cons next others) stack))
   (let ([token (trim-parens next)])
     (make-state others
                 (cons token stack)))])

 
;; Throw error if trying to duplicate on empty stack
;; Duplicate top element of stack
(: duplicate (-> State State))
(define/match (duplicate world)
  [((state '() _)) (error "Program cannot be empty")]
  [((state _ '())) (error "Attempted to duplicate on empty stack: "
                          (print-state world))]
  [((state (cons _ tokens) (cons stack-top stack-rest)))
   (make-state tokens
               (cons stack-top (cons stack-top stack-rest)))])
               

;; swap: State -> State
;; Guard: prgm non-empty
;; Throw error if fewer than two elements on the stack
;; Swap top two elements on the stack
(: swap (-> State State))
(define/match (swap world)
  [((state '() _)) (error "Program cannot be empty")]
  [((state (cons _ tokens) (list-rest fst snd rest)))
   (make-state tokens
               `(,snd ,fst ,@rest))]
  [(_) (error "Attempted to swap with fewer than two elements in the stack: "
              (print-state world))])
   

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

