#lang racket

(require lang/prim)
(require rackunit)
(require json)
(require racket/engine)


(require (for-syntax syntax/parse))

(provide produce-report/exit  define-var generate-results generate-results/hash test-case-timeout submission-directory results-path autodetect-extension)

(provide mirror-macro)

;; test-case-timeout determines maximum amount of time an individual test-case may run in seconds
(define test-case-timeout (make-parameter #f))

(struct test-timeout test-result () #:transparent)


;; This is mostly useful for testing, where if it is #f,
;; we print to the current port
(define results-path (make-parameter "/autograder/results/results.json"))


(define (produce-report/exit grade-hash)
  (if (results-path)
      (with-output-to-file (results-path)
        #:exists 'replace
        (lambda ()
          (write-json grade-hash)))
      (write-json grade-hash))
  (exit))

;; When using define-var without specifying filename, this controls
;; what file extension to look for.
(define autodetect-extension (make-parameter ".rkt"))
(define autodetected-file #f)

;; This is mostly useful for testing, where you don't want
;; to have to put things into /autograder/submission!
(define submission-directory (make-parameter "/autograder/submission/"))


(define (autodetect-file)
  (if (and autodetected-file
           (string-suffix? autodetected-file (autodetect-extension)))
      autodetected-file
      (let [(fs (filter (lambda (f) (string-suffix? f (autodetect-extension)))
                        (map path->string
                             (directory-list (submission-directory)))))]
        (if (empty? fs)
            (produce-report/exit
              `#hasheq((score . "0")
                       (output . ,(string-append "File with extension " (autodetect-extension) " not found: please check your submission"))))
            (begin (set! autodetected-file (first fs))
                   autodetected-file)))))

(define-syntax (define-var stx)
  (syntax-parse stx 
      [(_ var-name (~optional (~literal from)) (~optional base-filename))
       #'(define var-name
           (let* [(bfn (~? base-filename
                          (autodetect-file)))
                 (filename (string-append (submission-directory) bfn))]
             (with-handlers ([exn:fail?
                              (lambda (e)
                                (produce-report/exit
                                 `#hasheq((score . "0")
                                          (output . ,(string-append "Run failed with error\n"
                                                                    (exn-message e))))))])
               (if (file-exists? filename)
                   (with-handlers ([exn:fail?
                                    (lambda (e)
                                      (produce-report/exit
                                       `#hasheq((score . "0")
                                                (output . ,(string-append "Loading failed with error\n"
                                                                          (exn-message e))))))])
                     (dynamic-require `(file ,filename) 'var-name
                                      (thunk
                                       (dynamic-require `(file ,filename) #f)
                                       (define ns (module->namespace `(file ,filename)))
                                    (eval `(,#'first-order->higher-order var-name) ns))))
                   (produce-report/exit
                    `#hasheq((score . "0")
                             (output . ,(string-append "File " bfn " not found: please check your submission"))))))))]))
  

(define-syntax mirror-macro
  (syntax-rules (from)
    [(_ macro-name from base-filename)
     (define-syntax macro-name
       (syntax-rules ()
         [(_ E (... ...))
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (produce-report/exit
                              `#hasheq((score . "0")
                                       (output . ,(string-append "Run failed with error\n"
                                                                 (exn-message e))))))])
            (define bfn base-filename)
            (define filename (string-append "/autograder/submission/" bfn))
            (if (file-exists? filename)
                (with-handlers ([exn:fail?
                                 (lambda (e)
                                   (produce-report/exit
                                    `#hasheq((score . "0")
                                             (output . ,(string-append "Loading failed with error\n"
                                                                       (exn-message e))))))])
                  (eval '(macro-name E (... ...))
                        (module->namespace
                         (begin
                           (dynamic-require `(file , filename) #f)
                           `(file ,filename)))))
                (produce-report/exit
                 `#hasheq((score . "0")
                          (output . ,(string-append "File " bfn " not found: please check your submission"))))))]))]))

(define (generate-results test-suite)
  (produce-report/exit
   (generate-results/hash
    test-suite)))

(define (generate-results/hash test-suite)
  (struct fold-state (successes errors failures timeouts names) #:transparent)

  (define init-state (fold-state (list) (list) (list) (list) (list)))

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
  (define (add-timeout state result)
    (struct-copy fold-state
                 state
                 [timeouts (cons (make-name state result)
                                  (fold-state-timeouts state))]))

  (define (add-result result state)
    (cond
      [(test-timeout? result)
       (add-timeout state result)]
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
     (length (fold-state-failures state))
     (length (fold-state-timeouts state))))


  ;; run-test-case/timeout : (U String #f) [-> Any] -> (U TestResult TestTimeout)
  ;; runs the test case, but produces test-timeout if test case does not finish in time
  (define (run-test-case/timeout name action)
    (define eng (engine (lambda (_) (run-test-case name action))))
    (if (engine-run (* (test-case-timeout) 1000) eng)
                       (engine-result eng)
                       (test-timeout name)))

  (let* ([test-results (fold-test-results add-result init-state test-suite
                                          #:run (if (test-case-timeout) run-test-case/timeout run-test-case)
                                          #:fdown push-suite-name
                                          #:fup pop-suite-name)]
         [raw-score (if (zero? (fold-state-total-results test-results))
                        100
                        (* 100
                           (/ (length (fold-state-successes test-results))
                              (fold-state-total-results test-results))))]
         [score-str (number->string (exact->inexact raw-score))])
    (if (= raw-score 100)
        `#hasheq((score . "100")
                 (output . "Looks shipshape, all tests passed, mate!"))
        `#hasheq((score . ,score-str)
                 (tests . ,(append
                            (map (λ (name)
                                    `#hasheq((output
                                              . ,(cond
                                                   [name =>
                                                    (lambda (test-case-name)
                                                      (string-append "Execution timed out in test named «"
                                                                     test-case-name
                                                                     "»"))]
                                                   [else "Execution timed out in unnamed test"]))))
                                  (fold-state-timeouts test-results))
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
                                 (fold-state-failures test-results))))))))
