#lang racket

(require lang/prim)
(require rackunit)
(require json)

(provide produce-report/exit define-var generate-results)

(provide mirror-macro)

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

(define-syntax mirror-macro
  (syntax-rules (from)
    [(_ macro-name from base-filename)
     (define-syntax macro-name
       (syntax-rules ()
         [(_ E (... ...))
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
                  (eval '(macro-name E (... ...))
                        (module->namespace
                         (begin
                           (dynamic-require `(file , filename) #f)
                           `(file ,filename)))))
                (produce-report/exit
                 `#hasheq((score . 0)
                          (output . ,(string-append "File " bfn " not found: please check your submission"))))))]))]))

(define (generate-results test-suite)
  (struct fold-state (successes errors failures names) #:transparent)

  (define init-state (fold-state (list) (list) (list) (list)))

  (define (push-suite-name name state)
    (struct-copy fold-state state [names (cons name (fold-state-names state))]))

  (define (pop-suite-name name state)
    (struct-copy fold-state state [names (rest (fold-state-names state))]))

  (define (make-name state result)
    (define new-name
      (string-join
       (filter (lambda (s) (and s (not (equal? s ""))))
               (append (reverse (fold-state-names state)) (list (test-result-test-case-name result))))
       ":"))
    (if (equal? new-name "")
        #f
        new-name))

  (define (add-error state result)
    (struct-copy fold-state
                 state
                 [errors (cons (make-name state result)
                               (fold-state-errors state))]))

  (define (add-failure state result)
    (struct-copy fold-state
                 state
                 [failures (cons (make-name state result)
                                 (fold-state-failures state))]))

  (define (add-success state result)
    (struct-copy fold-state
                 state
                 [successes (cons (make-name state result)
                                  (fold-state-successes state))]))

  (define (add-result result state)
    (cond
      [(test-failure? result)
       (add-failure state result)]
      [(test-error? result)
       (add-error state result)]
      [(test-success? result)
       (add-success state result)]))

  (define (fold-state-total-results state)
    (+
     (length (fold-state-successes state))
     (length (fold-state-errors state))
     (length (fold-state-failures state))))

  (let* ([test-results (fold-test-results add-result init-state test-suite
                                          #:fdown push-suite-name
                                          #:fup pop-suite-name)]
         [raw-score (* 100
                       (/ (length (fold-state-successes test-results))
                          (fold-state-total-results test-results)))]
         [score-str (number->string (exact->inexact raw-score))])
    (if (= raw-score 100)
        (produce-report/exit
         `#hasheq((score . "100")
                  (output . "Looks shipshape, all tests passed, mate!")))
        (produce-report/exit
         `#hasheq((score . ,score-str)
                  (tests . ,(append
                             (map (λ (name)
                                    `#hasheq((output
                                              . ,(cond
                                                   [name =>
                                                    (lambda (test-case-name)
                                                      (string-append "Execution error in test named «"
                                                                     test-case-name
                                                                     "»"))]
                                                   [else "Execution error in unnamed test"]))))
                                  (fold-state-errors test-results))
                             (map (λ (name)
                                    `#hasheq((output
                                              . ,(cond
                                                   [name =>
                                                    (lambda (test-case-name)
                                                      (string-append "Incorrect answer from test named «"
                                                                     test-case-name
                                                                     "»"))]
                                                   [else "Incorrect answer from unnamed test"]))))
                                  (fold-state-failures test-results)))))))))
