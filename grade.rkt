#lang racket

(require lang/prim)
(require rackunit rackunit/log)
(require json)

(define base-filename "defs.rkt")
(define filename (string-append "/autograder/submission/" base-filename))

(define (produce-report/exit grade-hash)
  (with-output-to-file "/autograder/results/results.json"
    #:exists 'replace
    (lambda ()
      (write-json grade-hash)))
  (exit))

(define (load-ns filename)
  (if (file-exists? filename)
      (with-handlers ([exn:fail?
		       (lambda (e)
			 (produce-report/exit
			  `#hasheq((score . 0)
				   (output . ,(string-append "Loading failed with error\n"
							     (exn-message e))))))])
	  (dynamic-require `(file ,filename) #f)
	  (module->namespace `(file ,filename)))
      (produce-report/exit
       `#hasheq((score . 0)
		(output . ,(string-append "File " base-filename " not found: please check your submission"))))))

(define-syntax extract-var
  (syntax-rules ()
    [(_ name ns)
     (define name (eval `(,#'first-order->higher-order name) ns))]))

(define ns (load-ns filename))

(with-handlers ([exn:fail?
		 (lambda (e)
		   (produce-report/exit
		    `#hasheq((score . 0)
			     (output . ,(string-append "Run failed with error\n"
						       (exn-message e))))))])

  (extract-var sq ns)

  (check-equal? (sq 0) 0)
  (check-equal? (sq 1) 1)
  (check-equal? (sq -1) 1)
  (check-equal? (sq 2) 4)
  (check-equal? (sq 3) 9))

(define result (test-log))

(define failed (car result))
(define total (cdr result))

(produce-report/exit
 `#hasheq((score . ,(number->string
		     (exact->inexact
		      (* 100 (/ (- total failed) total)))))))

