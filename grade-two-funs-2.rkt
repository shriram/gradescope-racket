#lang racket

(require rackunit) ;; WARNING: Use the test- forms, NOT the check- forms!

(require
 racket/hash
 "lib-grade.rkt")

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

(define test-results-hash (generate-results/hash tests))

(produce-report/exit
 (hash-union
  test-results-hash
  #hasheq((output . "Some additional output\n"))
  #:combine/key (lambda (k v1 v2)
                  (cond
                    [(eq? k 'output)
                     (string-append v1 v2)]))))
