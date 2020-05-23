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

#|
(define-var add-one from "code.rkt")
(define-var sub-one from "code.rkt")

(define-test-suite tests
  (test-equal? "0" (add-one 0) 1)
  (test-equal? "1" (add-one 1) 2)
  (test-equal? "-1" (sub-one 1) 0)
  (test-equal? "2" (sub-one 0) 0))

(generate-results tests)
|#
