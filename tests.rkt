#! /usr/bin/env racket
#lang racket
(require rackunit
         rackunit/text-ui
         (submod "interpreter-service.rkt" testing)
         "interpreter.rkt")

(define tokenizer-tests
  (test-suite
   "Tokenizer Checking"
   (check-pred list? (tokenize "") "Sanity check: Tokenizing the empty string gives a list")
   (check-pred list? (tokenize "(hello):~*a^!S") "Sanity check: Tokenizing a non-empty string gives a list")
   (check-exn #rx".*Unquoted \\[" (λ()(tokenize "([)")) "[ character must be escaped")
   (check-exn #rx".*Unquoted \\]" (λ()(tokenize "(])")) "] character must be escaped")
   (check-exn #rx".*Unquoted <" (λ()(tokenize "(<)")) "< character must be escaped")
   (check-exn #rx".*Unquoted >" (λ()(tokenize "(>)")) "> character must be escaped")
   (check-exn #rx".*Unrecognized character: q" (λ()(tokenize "q")) "non-commands should not be at the top level")
   (check-not-exn (λ()(tokenize "(\"[\"]\"<\">\"\")")) "Escaped characters do not raise exceptions")
   (check-equal? (tokenize "") '() "Tokenizing the empty string gives back an empty list")
   (check-equal? (tokenize "(hello):~a*^!S") 
                 '("(hello)" ":" "~" "a" "*" "^" "!" "S") "Different commands are split into different tokens")
   (check-equal? (tokenize "(he()o)")
                 '("(he()o)") "Tokenizer permits nested parens")
   (check-equal? (tokenize "(he)(l):(o)***S")
                 '("(he)" "(l)" ":" "(o)" "*" "*" "*" "S")
                 "Different commands are split into different tokens")))


(define classifier-tests 
  (test-suite 
   "Classifier Tests"
   (check-equal? (classify "(hello)") 'push "Push token recognized")
   (check-equal? (classify "*") 'concat "Concat token recognized")
   (check-equal? (classify ":") 'duplicate "Duplicate token recognized")
   (check-equal? (classify "~") 'swap "Swap token recognized")
   (check-equal? (classify "a") 'enclose "Enclose token recognized")
   (check-equal? (classify "^") 'eval "Eval token recognized")
   (check-equal? (classify "!") 'drop "Drop token recognized")
   (check-equal? (classify "S") 'print "Print token recognized")))

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
   (check-equal? (run (make-state '() '())) '() "Running from empty state gives empty state")
   (check-equal? (run (make-state 
                  '("(x)" "(he)" "(l)" ":" "(o)" "*" "*" "*" "~" "S")
                  '()))
                 '("hello")
                 "Running from a non-empty state yields a result")
   (check-equal? (run (make-state 
                  '("(x)" "(he)" "(l)" ":" "(o)" "*" "*" "*" "S")
                  '()))
                 '("x")
                 "Running from a non-empty state yields a result")
   ;; Using a builtin that requires something on the stack
   ;; without something on the stack fails
   (check-exn exn:fail? 
              (λ()(run (make-state '("*") '())))
              "Concatenating on an empty stack fails")
   (check-exn exn:fail?
              (λ()(run (make-state '(":") '())))
              "Duplicating on an empty stack fails")
   (check-exn exn:fail? 
              (λ()(run (make-state '("!") '())))
              "Dropping on an empty stack fails")
   (check-exn exn:fail? 
              (λ()(run (make-state '("a") '())))
              "Enclosing on an empty stack fails")
   (check-exn exn:fail? 
              (λ()(run (make-state '("~") '())))
              "Swapping on an empty stack fails")
   (check-exn exn:fail? 
              (λ()(run (make-state '("^") '())))
              "Evaluating an empty stack fails")))

(define all-tests
  (test-suite "Full Test Suite"
              tokenizer-tests
              classifier-tests
              helper-tests
              runner-tests))

(run-tests all-tests)