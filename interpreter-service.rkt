#lang typed/racket

(provide tokenize)

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

