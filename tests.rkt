#! /usr/bin/env racket
#lang racket
(require rackunit
         rackunit/text-ui
         (submod "interpreter.rkt" testing)
         "cli.rkt")

(define tokenizer-tests
  (test-suite
   "Tokenizer Checking"
   (check-pred list? (tokenize "")
               "Sanity check: Tokenizing the empty string gives a list")
   (check-pred list? (tokenize "(hello):~*a^!S")
               "Sanity check: Tokenizing a non-empty string gives a list")
   (check-exn #rx".*Unquoted \\[" (λ()(tokenize "([)"))
              "[ character must be escaped")
   (check-exn #rx".*Unquoted \\]" (λ()(tokenize "(])"))
              "] character must be escaped")
   (check-exn #rx".*Unquoted <" (λ()(tokenize "(<)"))
              "< character must be escaped")
   (check-exn #rx".*Unquoted >" (λ()(tokenize "(>)"))
              "> character must be escaped")
   (check-exn #rx".*Unrecognized character: q" (λ()(tokenize "q"))
              "non-commands should not be at the top level")
   (check-not-exn (λ()(tokenize "(\"[\"]\"<\">\"\")"))
                  "Escaped characters do not raise exceptions")
   (check-equal? (tokenize "") '()
                 "Tokenizing the empty string gives back an empty list")
   (check-equal? (tokenize "(hello):~a*^!S") 
                 `(,(push-token "hello")
                   duplicate
                   swap
                   enclose
                   concat
                   eval
                   drop
                   print)
                 "Different commands are split into different tokens")
   (check-equal? (tokenize "(he()o)")
                 `(,(push-token "he()o"))
                 "Tokenizer permits nested parens")
   (check-equal? (tokenize "(he)(l):(o)***S")
                 `(,(push-token "he")
                   ,(push-token "l")
                   duplicate
                   ,(push-token "o")
                   concat
                   concat
                   concat
                   print)
                 "Different commands are split into different tokens")))

(define helper-tests
  (test-suite 
   "Helper method tests"
   (check-equal? (push (push-token "h") '() '())
                 (state '() '("h")))
   (check-equal? (push (push-token "h") '(duplicate) '())
                 (make-state '(duplicate) '("h")))
   (check-equal? (duplicate (make-state '(duplicate) '("h")))
                 (make-state '() '("h" "h")))
   (check-equal? (swap (make-state '(swap) '("a" "b")))
                 (make-state '() '("b" "a")))
   (check-equal? (concat (make-state '(concat) '("b" "a")))
                 (make-state '() '("ab")))
   (check-equal? (enclose (make-state '(enclose print) '("x")))
                 (make-state '(print) '("(x)")))
   (check-equal? (eval (make-state '(eval print) '(":" "l")))
                 (make-state '(duplicate print) '("l")))
   (check-equal? (drop (make-state '(drop) '("x")))
                 (make-state '() '()))
   (let* ([output (open-output-string)]
         [result (prints (make-state '(print) '("hello")) output)])
     (check-equal? (get-output-string output) "hello")
     (check-equal? result (make-state '() '())))))

(define runner-tests
  (test-suite
   "Runner tests"
   (let* ([output (open-output-string)]
          [result (run (make-state '() '()) output)])
     (check-equal? result '() "Running from empty state gives empty list"))

   (let* ([output (open-output-string)]
          [result (run (make-state
                        `(,(push-token "x")
                          ,(push-token "he")
                          ,(push-token "l")
                          duplicate
                          ,(push-token "o")
                          concat
                          concat
                          concat
                          swap
                          print)
                        '()) output)])
     (check-equal? result '("hello")
                   "It returns everything left on the stack")
     (check-equal? (get-output-string output) "x"
                   "It prints the output to the given port"))

   ;; Using a builtin that requires something on the stack
   ;; without something on the stack fails
   (check-exn exn:fail? 
              (λ()(run (make-state '(concat) '())))
              "Concatenating on an empty stack fails")
   (check-exn exn:fail?
              (λ()(run (make-state '(duplicate) '())))
              "Duplicating on an empty stack fails")
   (check-exn exn:fail? 
              (λ()(run (make-state '(drop) '())))
              "Dropping on an empty stack fails")
   (check-exn exn:fail? 
              (λ()(run (make-state '(enclose) '())))
              "Enclosing on an empty stack fails")
   (check-exn exn:fail? 
              (λ()(run (make-state '(swap) '())))
              "Swapping on an empty stack fails")
   (check-exn exn:fail? 
              (λ()(run (make-state '(eval) '())))
              "Evaluating an empty stack fails")))

(define all-tests
  (test-suite "Full Test Suite"
              tokenizer-tests
              helper-tests
              runner-tests))

(run-tests all-tests)
