#lang typed/racket

(require typed/racket/unsafe)
(unsafe-require/typed parsack
                      [#:struct (T) Ok ([parsed : T])]
                      [#:struct Error ()])

(define-type (Parse-Result T) (U Error (Ok T)))

(unsafe-require/typed parsack
                      [#:struct (T) Empty ([reply : (Parse-Result T)])]
                      [#:struct (T) Consumed ([reply : (Parse-Result T)])])

(define-type (Parser T) (Input-Port -> T))

(unsafe-require/typed parsack
                      [parse-result (All (T) ((Parser T) Input-Port -> T))]
                      [parse
                       (All (T) ((Parser T) Input-Port -> (Parse-Result T)))]
                      [return (All (T) (T -> (Parser T)))])
