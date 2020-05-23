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
     (define name
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (produce-report/exit
                           `#hasheq((score . 0)
                                    (output . ,(string-append "Run failed with error\n"
                                                              (exn-message e))))))])
         (eval `(,#'first-order->higher-order name) ns)))]))

(define ns (load-ns filename))

(extract-var sq ns)

(define-test-suite sq-tests
  (test-equal? "0" (sq 0) 0)
  (test-equal? "1" (sq 1) 1)
  (test-equal? "-1" (sq -1) 1)
  (test-equal? "2" (sq 2) 4)
  (test-equal? "3" (sq 3) 9))

(let ([test-results (fold-test-results cons empty sq-tests)])
  (let ([score (number->string
		(exact->inexact
		 (* 100
		    (/ (length (filter test-success? test-results)) (length test-results)))))])
    (produce-report/exit
     `#hasheq((score . ,score)
              (tests . ,(append
                         (map (λ (t)
                                `#hasheq((output
                                          . ,(string-append "Error: " (test-result-test-case-name t)))))
                              (filter test-error? test-results))
                         (map (λ (t)
                                `#hasheq((output
                                          . ,(string-append "Wrong: " (test-result-test-case-name t)))))
                              (filter test-failure? test-results))))))))
