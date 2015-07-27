#lang racket
(require rackunit
         rackunit/gui
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
   (check-exn #rx".*Unrecognized character: q" (λ()(tokenize "q")))
   (check-not-exn (λ()(tokenize "(\"[\"]\"<\">\"\")")))
   (check-equal? (tokenize "") '())
   (check-equal? (tokenize "(hello):~a*^!S") 
                 '("(hello)" ":" "~" "a" "*" "^" "!" "S"))
   (check-equal? (tokenize "(he()o)")
                 '("(he()o)"))
   (check-equal? (tokenize "(he)(l):(o)***S")
                 '("(he)" "(l)" ":" "(o)" "*" "*" "*" "S"))))


(define classifier-tests 
  (test-suite 
   "Classifier Tests"
   (check-equal? (classify "(hello)") 'push)
   (check-equal? (classify "*") 'concat)
   (check-equal? (classify ":") 'duplicate)
   (check-equal? (classify "~") 'swap)
   (check-equal? (classify "a") 'enclose)
   (check-equal? (classify "^") 'eval)
   (check-equal? (classify "!") 'drop)
   (check-equal? (classify "S") 'print)))

(define runner-tests
  (test-suite
   "Runner tests"
   (check-equal? (run (make-state '() '() "")) "")
   (check-equal? (run (make-state 
                  '("(he)" "(l)" ":" "(o)" "*" "*" "*" "S")
                  '() ""))
                 "hello")
   ;; Using a builtin that requires something on the stack
   ;; without something on the stack fails
   (check-exn exn:fail? 
              (λ()(run (make-state '("*") '() ""))))
   (check-exn exn:fail?
              (λ()(run (make-state '(":") '() ""))))
   (check-exn exn:fail? 
              (λ()(run (make-state '("!") '() ""))))
   (check-exn exn:fail? 
              (λ()(run (make-state '("a") '() ""))))
   (check-exn exn:fail? 
              (λ()(run (make-state '("~") '() ""))))
   (check-exn exn:fail? 
              (λ()(run (make-state '("^") '() ""))))))


((make-gui-runner) tokenizer-tests classifier-tests runner-tests)
;(run-tests classifier-tests)
;(run-tests runner-tests)