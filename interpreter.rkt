#lang racket

;; I want a macro that transforms (equal?| x (or "a" "b" "c")) => (or (equal x "a") (equal x "b") (equal x "c"))


(provide (all-defined-out))

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

;; A State is a (make-state [List Token] [List Token] String)
(define-struct state (program stack output))

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

  ;; take-push-token: String -> Token
  ;; Parse and return a push token
  (define (take-push-token str)
    "") ; TODO

  ;; drop-token: String -> String
  ;; Return the string, absent the first token present
  (define (drop-token str) "") ; TODO

  ;; drop-push-token: String -> String
  ;; Parse and drop a push token
  (define (drop-push-token str) "") ; TODO
  
  (cond [(string=? str "") '()]
        [else (cons (take-token str) (tokenize (drop-token str)))]))

;; classify: Token -> Classification
;; Classify each token by the action that is required
(define (classify token)
  token)

;; run: State -> String
;; Run the program from the currently defined program state
(define (run state)
  state)
