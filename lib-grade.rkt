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

(define-syntax define-var
  (syntax-rules (from)
    [(_ var-name from base-filename)
     (define var-name
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (produce-report/exit
                           `#hasheq((score . 0)
                                    (output . ,(string-append "Run failed with error\n"
                                                              (exn-message e))))))])
         (define bfn base-filename)
         (define filename (string-append "/autograder/submission/" bfn))
         (if (file-exists? filename)
             (with-handlers ([exn:fail?
                              (lambda (e)
                                (produce-report/exit
                                 `#hasheq((score . 0)
                                          (output . ,(string-append "Loading failed with error\n"
                                                                    (exn-message e))))))])
               (dynamic-require `(file ,filename) 'var-name
                                (thunk
                                 (dynamic-require `(file ,filename) #f)
                                 (define ns (module->namespace `(file ,filename)))
                                 (eval `(,#'first-order->higher-order var-name) ns))))
             (produce-report/exit
              `#hasheq((score . 0)
                       (output . ,(string-append "File " bfn " not found: please check your submission")))))))]))

(define (generate-results test-suite)
  (let* ([test-results (fold-test-results cons empty test-suite)]
         [raw-score (* 100
                       (/ (length (filter test-success? test-results)) (length test-results)))]
         [score-str (number->string (exact->inexact raw-score))])
    (if (= raw-score 100)
        (produce-report/exit
         `#hasheq((score . "100")
                  (output . "Looks shipshape, all tests passed, mate!")))
        (produce-report/exit
         `#hasheq((score . ,score-str)
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
