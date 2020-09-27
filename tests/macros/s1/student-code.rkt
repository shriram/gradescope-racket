#lang racket

(provide x foo lett with-a-kwd)
(provide [rename-out (mycond newcond)])

(require syntax/parse)

(define x 3)

(define-syntax foo
  (syntax-rules ()
    [(foo) "hello this is foo"]))

(define-syntax lett
  (syntax-rules ()
    [(_ ([var val] ...) E)
     ((lambda (var ...) E) val ...)]))

(define-syntax (with-a-kwd E)
  (syntax-case E (this that)
    [(_ this x that) #'x]))

(define-syntax mycond
  (syntax-rules (else)
    [(_ [else E]) E]
    [(_ (C T)) (if C T (error 'mycond "fell through"))]
    [(_ (C T) Clause ...)
     (if C T (mycond Clause ...))]))
