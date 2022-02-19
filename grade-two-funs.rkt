#lang racket

(require rackunit) ;; WARNING: Use the test- forms, NOT the check- forms!

(require "lib-grade.rkt")

(define-var add-one from "code.rkt")
(define-var sub-one from "code.rkt")

(define tests
  (test-suite
   ""
   (test-suite
    "add-one test suite"
    (test-suite
     "extra nesting for testing"
     (test-equal? "+1 0" (add-one 0) 1))
    (test-equal? "+1 1" (add-one 1) 2))
   (test-suite
    "sub-one test suite"
    (test-equal? "-1 1" (sub-one 1) 0)
    (test-equal? "-1 0" (sub-one 0) -1))))

(generate-results tests)
