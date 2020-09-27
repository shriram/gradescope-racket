#lang racket

(require rackunit) ;; WARNING: Use the test- forms, NOT the check- forms!

(require "lib-grade.rkt")

(mirror-macro foo from "student-code.rkt")
(mirror-macro lett from "student-code.rkt")
(mirror-macro with-a-kwd from "student-code.rkt")
(mirror-macro mycond from "student-code.rkt")

(define-test-suite macro-tests
  (test-equal? "foo"
               (foo)                 ;; note that `foo` comes from student-code.rkt
               "hello this is foo")
  (test-equal? "lett"
               (lett ([x 3]
                      [y 4])
                     (+ x y))
               7)
  (test-equal? "with-a-kwd"
               (with-a-kwd this x that) ;; note that `x` comes from student-code.rkt
               3)
  (test-equal? "mycond immediate else"
               (mycond [else 23])
               23)
  (test-equal? "mycond eventual else"
               (mycond [false 4]
                       [true 6]
                       [else 9])
               6))

(generate-results macro-tests)
