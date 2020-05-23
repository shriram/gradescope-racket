#lang racket/base

(define (sq n)
  (cond
    [(= n 2) (+ "hi" "bye")]
    [(= n 1) 27]
    [else (* n n)]))
