#lang typed/racket

(define (sq n)
  (if (= n 2)
      (+ "hi" "bye")
      (* n n)))
