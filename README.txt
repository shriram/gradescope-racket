https://gradescope-autograders.readthedocs.io/en/latest/specs/
https://gradescope-autograders.readthedocs.io/en/latest/getting_started/
https://gradescope-autograders.readthedocs.io/en/latest/manual_docker/
https://gradescope-autograders.readthedocs.io/en/latest/git_pull/

s1 = correct, BSL
s2 = incorrect, BSL (computes cube rather than square)
s3 = correct, racket/base
s4 = correct, typed/racket
s5 = incorrect, defines square instead of sq
s6 = incorrect, run-time error
s7 = incorrect, missing defs.rkt file
s8 = incorrect, type errors (written in typed/racket)
s9 = incorrect, mostly correct but run-time error when given 2

The Gradescope autograder "spec" is here:

https://gradescope-autograders.readthedocs.io/en/latest/specs/

This code *may* end up depending on the Racket handin server, which is obtained by

raco pkg install handin

and documented here:

https://github.com/racket/handin
https://docs.racket-lang.org/handin-server/Handin-Server_and_Client.html




Best so far:

(require racket/sandbox)
(define e (make-module-evaluator (string->path "s1/defs.rkt")))
(e '(require rackunit/log))
(e '(require rackunit))
(e '(check-equal? (sq 3) 9))
(e '(check-equal? (sq 3) 8))
(e '(test-log))


Better:

#lang racket

(require lang/prim)
(require rackunit rackunit/log)

(define filename "/Users/sk/Desktop/Dockerplay/s2/defs.rkt")

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

(test-log)

