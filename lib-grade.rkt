#lang racket

(require lang/prim)
(require rackunit)
(require json)
(require racket/async-channel)

(provide produce-report/exit define-var generate-results generate-results/hash timeout-in-seconds)

(provide mirror-macro)

;; Determines the time limit to generate results for a test-suite in seconds, #f represents no timeout (default).
(define timeout-in-seconds (make-parameter #f))

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
               (dynamic-require `(file ,filename) 'var-name
                                (thunk
                                 (dynamic-require `(file ,filename) #f)
                                 (define ns (module->namespace `(file ,filename)))
                                 (eval `(,#'first-order->higher-order var-name) ns))))
             (produce-report/exit
              `#hasheq((score . "0")
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

  (let* ([test-results (call/timeout (λ () (fold-test-results add-result init-state test-suite
                                                              #:fdown push-suite-name
                                                              #:fup pop-suite-name)))]
         [raw-score (* 100
                       (/ (length (fold-state-successes test-results))
                          (fold-state-total-results test-results)))]
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



;; call/timeout : {X} [-> X] -> X
;; produces the result of calling the given thunk, but sends a user break after timeout-in-seconds
;; timeout is determined by provided parameter timeout-in-seconds, if #f then call/timeout just calls proc
(define (call/timeout proc)

  ;; call-proc/timeout : -> ()
  ;; ASSUMES: (timeout-in-seconds) is a number (not #f)
  (define (call-proc/timeout)
    ;; async channel allows non-blocking test suites to succeed immediately
    (define results-channel (make-async-channel 1))
    (define thd (thread (λ () (async-channel-put results-channel (proc)))))
    (sync/timeout (timeout-in-seconds) thd)
    (async-channel-try-get-break results-channel thd))

  ;; async-channel-try-get-break : AsyncChannel Thread -> Any
  ;; attempts to try-get from a channel, otherwise breaks the given thread and gets
  (define (async-channel-try-get-break channel thd)
    (or (async-channel-try-get channel)
        ; otherwise, the test suite is hanging: break the thread and force results
        (begin
          (break-thread thd)
          (begin0 (async-channel-get channel)
                  (thread-wait thd)))))

  (if (timeout-in-seconds) (call-proc/timeout) (proc)))
