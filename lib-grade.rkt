#lang racket

(require lang/prim)
(require rackunit)
(require json)

(provide produce-report/exit define-var generate-results)

(define (produce-report/exit grade-hash)
  (with-output-to-file "/autograder/results/results.json"
    #:exists 'replace
    (lambda ()
      (write-json grade-hash)))
  (exit))

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

(define (load-ns base-filename)
  (define filename (string-append "/autograder/submission/" base-filename))
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

(define-syntax define-var
  (syntax-rules (from)
    [(_ var-name from file-name)
     (extract-var var-name (load-ns file-name))]))

(define (generate-results test-suite)
  (let ([test-results (fold-test-results cons empty test-suite)])
    (let ([score (number->string
                  (exact->inexact
                   (* 100
                      (/ (length (filter test-success? test-results)) (length test-results)))))])
      (produce-report/exit
       `#hasheq((score . ,score)
                (tests . ,(append
                           (map (λ (t)
                                  `#hasheq((output
                                            . ,(string-append "Execution error in test named «"
                                                              (test-result-test-case-name t)
                                                              "»"))))
                                (filter test-error? test-results))
                           (map (λ (t)
                                  `#hasheq((output
                                            . ,(string-append "Incorrect answer from test named «"
                                                              (test-result-test-case-name t)
                                                              "»"))))
                                (filter test-failure? test-results)))))))))
