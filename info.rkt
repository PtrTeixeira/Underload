#lang info

(define collection "underload")
(define version "1.0.0")
(define pkg-authors '(PtrTeixeira))
(define pkg-desc 
  "Interpreter for the Underload esoteric programming language")

(define deps '("base"
               "rackunit-lib"
               ("typed-racket-lib" #:version "1.7")))

