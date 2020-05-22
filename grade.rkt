#lang racket

(require lang/prim)
(require rackunit rackunit/log)
(require json)

(define filename "/autograder/submission/defs.rkt")

(define (load-ns filename)
  (dynamic-require `(file ,filename) #f)
  (module->namespace `(file ,filename)))

(define-syntax extract-var
  (syntax-rules ()
    [(_ name ns)
     (define name (eval `(,#'first-order->higher-order name) ns))]))

(define ns (load-ns filename))

(extract-var sq ns)

(check-equal? (sq 0) 0)
(check-equal? (sq 1) 1)
(check-equal? (sq -1) 1)
(check-equal? (sq 2) 4)
(check-equal? (sq 3) 9)

(define result (test-log))

(define failed (car result))
(define total (cdr result))

(with-output-to-file "/autograder/results/results.json"
  (lambda ()
    (write-json
     `#hasheq((score . ,(number->string
			 (exact->inexact
			  (* 100 (/ (- total failed) total)))))))))

