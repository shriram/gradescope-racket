#lang racket

(require rackunit) ;; WARNING: Use the test- forms, NOT the check- forms!

(require "lib-grade.rkt")

(define-var sq from "defs.rkt")

(define-test-suite sq-tests
  (test-equal? "0" (sq 0) 0)
  (test-equal? "1" (sq 1) 1)
  (test-equal? "-1" (sq -1) 1)
  (test-equal? "2" (sq 2) 4)
  (test-equal? "3" (sq 3) 9))

(generate-results sq-tests)
