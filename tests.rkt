#! /usr/bin/env racket
#lang racket
(require rackunit
         rackunit/gui
         "interpreter.rkt"
         (submod "interpreter.rkt" testing))

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

(define helper-tests
  (test-suite 
   "Helper method tests"
   (check-equal? (push (make-state '("(h)") '()))
                 (make-state '() '("h")))
   (check-equal? (push (make-state '("(h)" ":") '()))
                 (make-state '(":") '("h")))
   (check-equal? (duplicate (make-state '(":") '("h")))
                 (make-state '() '("h" "h")))
   (check-equal? (swap (make-state '("~") '("a" "b")))
                 (make-state '() '("b" "a")))
   (check-equal? (concat (make-state '("*") '("b" "a")))
                 (make-state '() '("ab")))
   (check-equal? (enclose (make-state '("a" "S") '("x")))
                 (make-state '("S") '("(x)")))
   (check-equal? (eval (make-state '("^" "S") '(":" "l")))
                 (make-state '(":" "S") '("l")))
   (check-equal? (drop (make-state '("!") '("x")))
                 (make-state '() '()))
   (check-equal? (prints (make-state '("S") '("hello")))
                 (make-state '() '()))))

(define runner-tests
  (test-suite
   "Runner tests"
   (check-equal? (run (make-state '() '())) '())
   (check-equal? (run (make-state 
                  '("(x)" "(he)" "(l)" ":" "(o)" "*" "*" "*" "~" "S")
                  '()))
                 '("hello"))
   (check-equal? (run (make-state 
                  '("(x)" "(he)" "(l)" ":" "(o)" "*" "*" "*" "S")
                  '()))
                 '("x"))
   ;; Using a builtin that requires something on the stack
   ;; without something on the stack fails
   (check-exn exn:fail? 
              (λ()(run (make-state '("*") '()))))
   (check-exn exn:fail?
              (λ()(run (make-state '(":") '()))))
   (check-exn exn:fail? 
              (λ()(run (make-state '("!") '()))))
   (check-exn exn:fail? 
              (λ()(run (make-state '("a") '()))))
   (check-exn exn:fail? 
              (λ()(run (make-state '("~") '()))))
   (check-exn exn:fail? 
              (λ()(run (make-state '("^") '()))))))


((make-gui-runner) tokenizer-tests
                   classifier-tests
                   helper-tests
                   runner-tests)