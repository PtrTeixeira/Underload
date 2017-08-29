#lang typed/racket

(provide Token
         (struct-out push-token))


;; A Program is a (Listof Token)


;; A Token is one of
;; - "($1)" -> (push-token $1)
;; - ":"      -> 'duplicate
;; - "~"      -> 'swap
;; - "*"      -> 'concat
;; - "a"      -> 'enclose
;; - "^"      -> 'eval
;; - "!"      -> 'drop
;; - "S"      -> 'print
(struct push-token ([value : String])
  #:transparent)
(define-type Token
  (U push-token
     'concat
     'duplicate
     'swap
     'enclose
     'eval
     'drop
     'print))