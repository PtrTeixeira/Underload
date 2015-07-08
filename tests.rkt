#lang racket
(require rackunit
         rackunit/text-ui
         "interpreter.rkt")

(define tokenizer-tests
  (test-suite
   "Tokenizer Checking"
   (check-pred list? (tokenize ""))
   (check-pred list? (tokenize "(hello):~*a^!S"))
   (check-exn #rx".*Unquoted \\[" (λ()(tokenize "([)")))
   (check-exn #rx".*Unquoted \\]" (λ()(tokenize "(])")))
   (check-exn #rx".*Unquoted <" (λ()(tokenize "(<)")))
   (check-exn #rx".*Unquoted >" (λ()(tokenize "(>)")))
   (check-exn #rx".*Unrecognized character" (λ()(tokenize "q")))
   (check-not-exn (λ()(tokenize "(\"[\"]\"<\">\"\")")))
   (check-equal? (tokenize "") '())
   (check-equal? (tokenize "(hello):~a*^!S") 
                 '("(hello)" ":" "~" "a" "*" "^" "!" "S"))
   (check-equal? (tokenize "(he()o)")
                 '("(he()o)"))
   (check-equal? (tokenize "(he)(l):(o)***S")
                 '("(he)" "(l)" ":" "(o)" "*" "*" "*" "S"))))


(run-tests tokenizer-tests)