#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Facilitates regression testing of a program.

  This code has been purposely left primitive.

  The programmer first hooks tests, and then runs (test-all) to run regression.

  The regression starts with a test of the regression system, this will show a passing
  test, a test failing, and a test failing due to throwing an exception.  After this the
  real regression starts.

  A logging facility is also supplied, it writes into the file "log.txt". Writing the
  log is purely under programmer control.  Running regression will not write the log.

  
|#
  (in-package #:tm)

;;--------------------------------------------------------------------------------
;; test/debug
;;
;;  a test takes no arguments and returns #f if and only if it fails
;;
;;  in general a test should not print any output or explanation, instead the programmer
;;  can go back to failed tests after regression is complete, and then flip switches and/or
;;  run trace on the test (depending on the design of the test) and debug it.
;;
;;  to be included in regression a test calls (test-hook a-test)
;;
;;  (test-all)  to run the tests
;;
;;
  (defvar *test-routines* (list))

  (defmacro test-hook (a-test)
    `(progn
       (princ "hooking test: ")
       (princ (symbol-name ',a-test))
       (nl)
       (push ',a-test tm::*test-routines*)
       t
       ))

  (defmacro test-remove (a-test) 
    `(progn
       (princ "removing test: ")
       (princ (symbol-name ',a-test))
       (nl)
       (setq 
         tm::*test-routines*
         (delete-if (λ(e)(eq ',a-test e)) tm::*test-routines*)
         )
       t
       ))

  ;; generally we don't print specific error messages, because this is regression
  ;; instead we expect that the programmer will go back and run the failing tests
  ;; individually to get more information.  However, I have added code to print
  ;; exception strings because they are not appearing in the REPL when a test is
  ;; run individually.
  ;;
    (defun test (a-test)
      (let(
            (test-name (symbol-name a-test))
            cc
            )
        (let(
              ;; see test-handler.lisp
              (test-result (handler-case (apply a-test '()) (condition (c)(setq cc c) ':exception)))
              )
          (princ " ")
          (cond
            ((eq test-result ':exception) (princ " exception"))
            ((eq test-result ∅)         (princ "    failed"))
            ((eq test-result t)           (princ "         +"))
            (t                            (princ "   illegal"))
            )
          (princ " ")
          (princ test-name)
          (cond
            ((eq test-result ':exception) (princ " - ") (princ cc))
            ((eq test-result ∅)         ∅)
            ((eq test-result t)           ∅)
            (t                            (princ " - returned other than t or ∅")
              ))
          (nl)
          (list test-name (eq test-result t))
          )))

  ;; + means the test ran and passed
  ;; 'failed' means the test ran and failed
  ;; returns true if all tests pass, otherwise false
  (defun test-all ()
    (let*(
          (results (map 'list #'test (reverse *test-routines*)))
          (no-tests (length results))
          (result-flags (map 'list #'cadr results))
          (error-count (count ∅ result-flags))
          (all-passed (every (λ(e)e) result-flags))
          )
      (cond
        (all-passed (princ "all ")(princ no-tests) (princ " passed")(nl))
        (t
          (princ "------")
          (nl)
          (princ "failed: ") 
          (princ error-count)
          (princ " of ") 
          (princ no-tests)
          (nl)
          )
        )
      all-passed
      ))
 
  (defun example-pass-test() (= (+ 1 1) 2))
  (defun example-fail-test() (= (+ 1 1) 3))
  (defun example-exception-test() (error 'intentional-error :text "intentional-error"))

  (defun example-illegal-test() 3) ; fails due to not returning t or ∅

  (defun test-examples ()
    (test-hook example-pass-test)
    (test-hook example-fail-test)
    (test-hook example-exception-test)
    (test-hook example-illegal-test)
    (princ "running example tests...")
    (nl)(nl)
    (let (
           (result (test-all))
           )
      (test-remove example-pass-test)
      (test-remove example-fail-test)
      (test-remove example-exception-test)
      (test-remove example-illegal-test)
      (nl)
      result
      ))

#|


;;--------------------------------------------------------------------------------
;; defines a trace-able module interface
;;
;;  (provide-with-trace "lib-name" provided-function ...)
;;
;;   lib-name-trace to turn on tracing
;;   lib-name-untrace to turn off tracing
;;
  (define-syntax (provide-with-trace stx)
    (let(
           (datum  (syntax->datum stx))
          )
      (let(
            (prefix              (cadr datum))
            (interface-functions (cddr datum))    
           )
        (let(
              (name-trace-fun   (string->symbol (string-append prefix "-trace")))
              (name-untrace-fun (string->symbol (string-append prefix "-untrace")))
              )
          #|
          (displayln name-trace-fun)
          (displayln name-untrace-fun)
          |#
          (let(
                (code-block `(begin))
                (trace-require '(require racket/trace))
                (trace-fun
                  (append
                    '(define)
                    (list (list name-trace-fun))
                    (map (λ(e)`(trace ,e)) interface-functions)
                    )
                  )
                (untrace-fun
                  (append
                    '(define)
                    (list (list name-untrace-fun))
                    (map (λ(e)`(untrace ,e)) interface-functions)
                    )
                  )
                (provide-calls (map (λ(e)`(provide ,e)) interface-functions))
                (provide-trace `(provide ,name-trace-fun))
                (provide-untrace `(provide ,name-untrace-fun))
                )
            #|
            (displayln code-block)
            (displayln trace-require)
            (displayln trace-fun)
            (displayln untrace-fun)
            (displayln provide-calls)
            |#
            (let*(
                   (program 
                     (append
                       code-block
                       (list trace-require)
                       (list trace-fun)
                       (list untrace-fun)
                       provide-calls
                       (list provide-trace)
                       (list provide-untrace)
                       )
                     )
                   )
              ;;(displayln program)
              (datum->syntax stx program)
              ))))))
|#

;;--------------------------------------------------------------------------------
;; log utils
;;

 (defparameter *log-default-file-name* "log.txt")
  
 (defun print-to-log (mess &optional (filename *log-default-file-name*))

    (when mess

      (with-open-file (*standard-output* filename
                        :direction :output 
                        :if-exists :append
                        :if-does-not-exist :create
                        )
        
        (defun pl (mess)
          (princ " ")
          (princ mess)
          (nl)
          )

        (princ (now))
        (if (consp mess)
          (progn
            (nl)
            (dolist (a-mess mess) (pl a-mess))
            )
          (pl mess)
          )
        (nl)
        )))
    

