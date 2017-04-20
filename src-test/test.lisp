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
       (nl)
       (princ "hooking test: ")
       (princ (symbol-name ',a-test))
       (push ',a-test tm::*test-routines*)
       t
       ))

  (defmacro test-remove (a-test) 
    `(progn
       (nl)
       (princ "removing test: ")
       (princ (symbol-name ',a-test))
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
          (nl)
          (cond
            ((eq test-result ':exception) (princ " exception"))
            ((eq test-result ∅)           (princ "    failed"))
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
          (list test-name (eq test-result t))
          )))

  ;; + means the test ran and passed
  ;; 'failed' means the test ran and failed
  ;; returns true if no test failed, otherwise false
  (defun test-all (&optional (verbose t))
    (let*(
          (results (map 'list #'test (reverse *test-routines*)))
          (test-count (length results))
          (result-flags (map 'list #'cadr results))
          (error-count (count ∅ result-flags))
          )
      (cond
        ((= error-count 0)
          (if
            (= test-count 0)
            (when verbose
              (nl)
              (princ "0 tests total. None passed, but also none failed.")
              )
            (when verbose
              (nl)
              (princ "all ")
              (princ test-count)
              (princ " passed")
              )))
        (t
          (when verbose
            (nl)
            (princ "------")
            (nl)
            (princ "failed: ") 
            (princ error-count)
            (princ " of ") 
            (princ test-count)
            ))
        )
      {error-count test-count}
      ))
 
  (defun example-pass-test() (= (+ 1 1) 2))
  (defun example-fail-test() (= (+ 1 1) 3))
  (defun example-exception-test() (error 'intentional-error :text "intentional-error"))

  (defun example-illegal-test() 3) ; fails due to not returning t or ∅

  (defun test-all-and-self (&optional verbose)
    (test-hook example-pass-test)
    (test-hook example-fail-test)
    (test-hook example-exception-test)
    (test-hook example-illegal-test)
    (nl)(nl)
    (princ "running tests...")
    (let(
          (result (test-all verbose))
          )
      (test-remove example-pass-test)
      (test-remove example-fail-test)
      (test-remove example-exception-test)
      (test-remove example-illegal-test)
      result ; should have exactly three fails
      ))


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
    

