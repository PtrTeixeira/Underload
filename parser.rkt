#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         "ast.rkt")

; (Listof Char) -> (Parser Char)
(define (none/p chars)
  (satisfy/p (λ(next) (not (member next chars)))))

(define (between/p start end inside)
  (do start
    [body <- inside]
    end
    (pure body)))

; -> Char
(define escape-char/p
  (do (char/p #\\)
    (or/p (char/p #\<)
          (char/p #\>)
          (char/p #\[)
          (char/p #\])
          (char/p #\"))))

(define string-char/p
  (do [result <- (or/p escape-char/p
                       (none/p '(#\( #\) #\\ #\< #\> #\[ #\[)))]
    (pure (string result))))
(define push-raw/p
  (do (char/p #\()
    [body <- (many/p (or/p push-raw/p string-char/p))]
    (char/p #\))
    (pure (apply string-append `("(" ,@body ")")))))
(define push/p
  (do [body <- (between/p (char/p #\() (char/p #\))
                          (many/p (or/p push-raw/p string-char/p)))]
    (pure (push-token (apply string-append body)))))

(define duplicate/p
  (do (char/p #\:)
    (pure 'duplicate)))
(define concat/p
  (do (char/p #\*)
    (pure 'concat)))
(define swap/p
  (do (char/p #\~)
    (pure 'swap)))
(define enclose/p
  (do (char/p #\a)
    (pure 'enclose)))
(define eval/p
  (do (char/p #\^)
    (pure 'eval)))
(define drop/p
  (do (char/p #\!)
    (pure 'drop)))
(define print/p
  (do (char/p #\S)
    (pure 'print)))

(define command/p
  (or/p push/p
        duplicate/p
        concat/p
        swap/p
        enclose/p
        eval/p
        drop/p
        print/p))

(define program/p
  (do [program <- (many/p command/p)]
    eof/p
    [pure program]))