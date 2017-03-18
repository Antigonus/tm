#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  SBCL condition reporting behavior tests.  Initially the handler set to 
  'condition'  reports an exception thrown from  #'call-xxx but not from
  #'call-www.  But the only difference is where the function xx (or ww)
  is defined.  What gives?

  When changing to 'serious-condition' no exception is thrown
  from #'call-xxx.
|#


(defun test (test-function)

  ;;; -> this does not report an exception
  ;;; (handler-case (apply test-function '()) (serious-condition () ':exception))

  ;;; -> this reports an exception
  ;;; (handler-case (apply test-function '()) (condition () ':exception))

  (handler-case (apply test-function '()) (condition (x) (print x) ':exception))

  (apply test-function '())
  )

;;--------------------------------------------------------------------------------
;; throws exception, but shouldn't (?)
;;
  (defun xxx ()
    (defun xx () t)
    (xx)
    )

  (defun call-xxx ()
    (xxx)  ;; #'xxx must be called twice for the exception to appear
    (xxx)
    t
    )

  ;; call-xxx throws exception when run from test, but shouldn't
  (defun run-test-call-xxx ()
    (test #'call-xxx)
    )

  ;; no problem here, call-xxx returns t when called directly
  (defun run-call-xxx ()
    (call-xxx)
    )

;;--------------------------------------------------------------------------------
;; works fine  
;;  only difference, took out the nested definition of #'ww from #'www
;;
  (defun ww () t)

  (defun www ()
    (ww)
    )

  (defun call-www ()
    (www) 
    (www)
    t
    )

  (defun run-test-call-www ()
    (test #'call-www)
    )

