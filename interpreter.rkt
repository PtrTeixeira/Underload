#lang racket

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
  str)

;; classify: Token -> Classification
;; Classify each token by the action that is required
(define (classify token)
  token)

;; run: State -> String
;; Run the program from the currently defined program state
(define (run state)
  state)